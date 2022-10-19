(ns gridfire.outputs
  (:require [clojure.data.csv     :as csv]
            [clojure.java.io      :as io]
            [clojure.string       :as str]
            [gridfire.utils.async :as gf-async]
            [magellan.core        :refer [matrix-to-raster write-raster]]
            [manifold.deferred    :as mfd]
            [matrix-viz.core      :refer [save-matrix-as-png]]
            [tech.v3.datatype     :as d]))

(set! *unchecked-math* :warn-on-boxed)

(defn output-filename [name outfile-suffix simulation-id output-time ext]
  (as-> [name outfile-suffix simulation-id (when output-time (str "t" output-time))] $
    (remove str/blank? $)
    (str/join "_" $)
    (str $ ext)))

(defonce ^:private outputs-writing-exectr*
  (delay
    ;; This is expensive to create, hence the (defonce ...) and (delay ...) combination.
    (let [;; AFAICT our outputs-writing tends to be CPU-bound,
          ;; hence this choice of parallelism.
          ;; Benchmarks on 4 cores have shown an only-2x performance enhancement,
          ;; so there might be some contention at play here,
          ;; and the optimal parallelism might be even lower.
          writing-parallelism (.availableProcessors (Runtime/getRuntime))]
      (gf-async/executor-with-n-named-threads
        "gridfire-outputs-writing"
        writing-parallelism
        {:onto? true}))))

(defn outputs-writing-executor
  "(Advanced) returns the pool of worker Java Threads dedicated to writing GridFire outputs.

   This enables us to parallelize the outputs-writing work (improving throughput)
   while controlling the degree of parallelism,
   and making it easy to monitor,
   as well as straightforward to orchestrate without blocking threads,
   thanks to the Manifold library.

   To achieve this, the heavy-lifting of output-writing
   must be done in tasks scheduled into this pool by calling
   (exec-in-outputs-writing-pool ...)"
  []
  @outputs-writing-exectr*)

;; Most of the following functions return either nil or Manifold Deferred,
;; resolved to nil upon completion of the function's side-effects,
;; so that they can easily be run in parallel,
;; and coordinated with minimal thread-blocking.
;; Manifold Deferreds are semantically equivalent to Promises in other languages;
;; using them makes async flow control straightforward to implement,
;; while reaping the benefits of parallelizing
;; into well-instrumented Thread pools without blocking threads.
;; Manifold was chosen because it is battle-tested, expressive,
;; well-suited to this sort of 'hierarchical' async flow control,
;; and offers good facilities for controlling thread pools.
;; It is harder to achieve these benefits cleanly by using core.async channels,
;; which force you into a side-effectful programming model,
;; and offer little opportunities for fine-tuning;
;; that said, Manifold is easy to compose with core.async,
;; which is interesting if you want to benefit from its (go ...) primitive.

(defn exec-in-outputs-writing-pool
  "Schedules the given 0-arg function to be executed in the thread pool
  dedicated to GridFire outputs-writing.

  Returns a Manifold Deferred, which will be completed with the return
  value of f.

  The function f may itself return a Manifold Deferred."
  [f]
  (letfn [(run-output-task []
            ;; Wrapping in a named fn makes it easy to spot in stacktraces,
            ;; thread dumps, flame graphs etc.
            (f))]
    (->
      (mfd/future-with (outputs-writing-executor)
        (run-output-task))
      ;; To advanced users: this helps ensure that (mfd/chain ...) callbacks
      ;; attached to this Deferred will execute in the same pool.
      ;; Even with that, it's not guaranteed to happen that way,
      ;; in situations where (f) executes so fast that this Deferred
      ;; gets completed even before the mfd/chain function is invoked:
      ;; in such cases, the callback will execute in the Thread invoking mfd/chain.
      ;; When it doubt: call (exec-in-outputs-writing-pool ...) again inside your callback.
      (mfd/onto (outputs-writing-executor)))))

(defn output-geotiff
  ([config matrix name envelope]
   (output-geotiff config matrix name envelope nil nil))

  ([config matrix name envelope simulation-id]
   (output-geotiff config matrix name envelope simulation-id nil))

  ([{:keys [output-directory outfile-suffix] :as config}
    matrix name envelope simulation-id output-time]
   (exec-in-outputs-writing-pool
     (fn []
       (let [file-name (output-filename name
                                        outfile-suffix
                                        (str simulation-id)
                                        output-time
                                        ".tif")]
         (-> (matrix-to-raster name matrix envelope)
             (write-raster (if output-directory
                             (str/join "/" [output-directory file-name])
                             file-name))))))))

(defn output-png
  ([config matrix name envelope]
   (output-png config matrix name envelope nil nil))

  ([config matrix name envelope simulation-id]
   (output-png config matrix name envelope simulation-id nil))

  ([{:keys [output-directory outfile-suffix]}
    matrix name envelope simulation-id output-time]
   (exec-in-outputs-writing-pool
     (fn []
       (let [file-name (output-filename name
                                        outfile-suffix
                                        (str simulation-id)
                                        output-time
                                        ".png")]
         (save-matrix-as-png :color 4 -1.0
                             matrix
                             (if output-directory
                               (str/join "/" [output-directory file-name])
                               (file-name))))))))

(defn write-landfire-layers!
  [{:keys [output-landfire-inputs? outfile-suffix landfire-rasters envelope]}]
  (when output-landfire-inputs?
    (->> landfire-rasters
         (mapv
           (fn [[layer matrix]]
             (exec-in-outputs-writing-pool
               (fn []
                 (-> (matrix-to-raster (name layer) matrix envelope)
                     (write-raster (str (name layer) outfile-suffix ".tif")))))))
         (gf-async/nil-when-all-completed))))

(defn write-burn-probability-layer!
  [{:keys [output-burn-probability simulations envelope output-pngs? burn-count-matrix] :as outputs}]
  (when-let [timestep output-burn-probability]
    (let [output-name        "burn_probability"
          simulations        (long simulations)
          div-by-simulations (fn ^double [^long burn-count]
                               (double (/ burn-count simulations)))]
      (if (int? timestep)
        (let [timestep (long timestep)]
          (->> (map-indexed vector burn-count-matrix)
               (mapv
                (fn [[band matrix]]
                  (->
                   (exec-in-outputs-writing-pool
                    (fn []
                      (let [output-time        (* (long band) timestep)
                            probability-matrix (d/clone (d/emap div-by-simulations nil matrix))]
                        [output-time probability-matrix])))
                   (mfd/chain
                    (fn [[output-time probability-matrix]]
                      (mfd/zip
                       (output-geotiff outputs probability-matrix output-name envelope nil output-time)
                       (when output-pngs?
                         (output-png outputs probability-matrix output-name envelope nil output-time))))))))
               (gf-async/nil-when-all-completed)))
        (->
         (exec-in-outputs-writing-pool
          (fn []
            (d/clone (d/emap div-by-simulations nil burn-count-matrix))))
         (mfd/chain
          (fn [probability-matrix]
            (mfd/zip
             (output-geotiff outputs probability-matrix output-name envelope)
             (when output-pngs?
               (output-png outputs probability-matrix output-name envelope)))))
         (gf-async/nil-when-completed))))))

(defn write-flame-length-sum-layer!
  [{:keys [envelope output-flame-length-sum flame-length-sum-matrix] :as outputs}]
  (when output-flame-length-sum
    (output-geotiff outputs flame-length-sum-matrix "flame_length_sum" envelope)))

(defn write-flame-length-max-layer!
  [{:keys [envelope output-flame-length-max flame-length-max-matrix] :as outputs}]
  (when output-flame-length-max
    (output-geotiff outputs flame-length-max-matrix "flame_length_max" envelope)))

(defn write-burn-count-layer!
  [{:keys [envelope output-burn-count? burn-count-matrix] :as outputs}]
  (when output-burn-count?
    (output-geotiff outputs burn-count-matrix "burn_count" envelope)))

(defn write-spot-count-layer!
  [{:keys [envelope output-spot-count? spot-count-matrix] :as outputs}]
  (when output-spot-count?
    (output-geotiff outputs spot-count-matrix "spot_count" envelope)))

(defn write-aggregate-layers!
  [outputs]
  (->>
    [(write-burn-probability-layer! outputs)
     (write-flame-length-sum-layer! outputs)
     (write-flame-length-max-layer! outputs)
     (write-burn-count-layer! outputs)
     (write-spot-count-layer! outputs)]
    (gf-async/nil-when-all-completed)))

(defn write-csv-outputs!
  [{:keys [output-csvs? output-directory outfile-suffix summary-stats]}]
  (when output-csvs?
    (exec-in-outputs-writing-pool
      (fn []
        (let [output-filename (str "summary_stats" outfile-suffix ".csv")]
          (with-open [out-file (io/writer (if output-directory
                                            (str/join "/" [output-directory output-filename])
                                            output-filename))]
            (->> summary-stats
                 (sort-by #(vector (:ignition-row %) (:ignition-col %)))
                 (mapv (fn [{:keys [ignition-row ignition-col max-runtime temperature relative-humidity
                                    wind-speed-20ft wind-from-direction foliar-moisture ellipse-adjustment-factor
                                    fire-size flame-length-mean flame-length-stddev fire-line-intensity-mean
                                    fire-line-intensity-stddev simulation surface-fire-size crown-fire-size spot-count]}]
                         [simulation
                          ignition-row
                          ignition-col
                          max-runtime
                          temperature
                          relative-humidity
                          wind-speed-20ft
                          wind-from-direction
                          foliar-moisture
                          ellipse-adjustment-factor
                          fire-size
                          flame-length-mean
                          flame-length-stddev
                          fire-line-intensity-mean
                          fire-line-intensity-stddev
                          crown-fire-size
                          spot-count
                          surface-fire-size]))
                 (cons ["simulation" "ignition-row" "ignition-col" "max-runtime" "temperature" "relative-humidity"
                        "wind-speed-20ft" "wind-from-direction" "foliar-moisture" "ellipse-adjustment-factor"
                        "fire-size" "flame-length-mean" "flame-length-stddev" "fire-line-intensity-mean"
                        "fire-line-intensity-stddev" "crown-fire-size" "spot-count" "surface-fire-size"])
                 (csv/write-csv out-file))))))))
