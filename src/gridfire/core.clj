;; [[file:../../org/GridFire.org::gridfire-core][gridfire-core]]
(ns gridfire.core
  (:require [clojure.core.reducers        :as r]
            [clojure.edn                  :as edn]
            [clojure.spec.alpha           :as spec]
            [gridfire.fire-spread-optimal :refer [rothermel-fast-wrapper-optimal]]
            [gridfire.inputs              :as inputs]
            [gridfire.outputs             :as outputs]
            [gridfire.simulations         :as simulations]
            [gridfire.spec.config         :as config-spec]
            [taoensso.tufte               :as tufte]
            [triangulum.logging           :refer [log log-str]]))

(set! *unchecked-math* :warn-on-boxed)

(defn write-outputs!
  [outputs]
  (outputs/write-landfire-layers! outputs)
  (outputs/write-aggregate-layers! outputs)
  (outputs/write-csv-outputs! outputs)

  :success)

(defmacro with-multithread-profiling
  [& body]
  `(do (tufte/remove-handler! :accumulating)
       (let [stats-accumulator# (tufte/add-accumulating-handler! {:handler-id :accumulating})
             result#            (do ~@body)]
         (Thread/sleep 1000)
         (as-> {:format-pstats-opts {:columns [:n-calls :min :max :mean :mad :clock :total]}} $#
           (tufte/format-grouped-pstats @stats-accumulator# $#)
           (log $# :truncate? false))
         result#)))

(defn run-simulations!
  [{:keys [^long simulations parallel-strategy] :as inputs}]
  (with-multithread-profiling ; TODO: Disable this to see how much performance is gained.
    (log-str "Running simulations")
    (dotimes [_ 30] ;; did this instead of (assoc :simulations 30), as it caused a OutOfMemory error (Val, 07 Jul 2022)
      (let [parallel-bin-size 1
            reducer-fn        (if (= parallel-strategy :between-fires)
                                #(into [] (r/fold parallel-bin-size r/cat r/append! %))
                                #(into [] %))
            summary-stats     (with-redefs [rothermel-fast-wrapper-optimal (memoize rothermel-fast-wrapper-optimal)]
                                (->> (range simulations)
                                     (vec)
                                     (r/map #(do
                                               (simulations/run-simulation! % inputs)
                                               ;; to avoid consuming all memory.
                                               nil))
                                     (r/remove nil?)
                                     (reducer-fn)))]
        (assoc inputs :summary-stats summary-stats)))))

(defn force-pixel-pertubations
  [config]
  (reduce-kv
    (fn [config1 k rng]
      (update config1 :perturbations merge
              {k {:spatial-type :pixel
                  :range        rng}}))
    config
    {:aspect                        [0. 10.]
     ;:canopy-base-height []
     ;:canopy-cover
     :canopy-height                 [0. 1.]
     ;:crown-bulk-density
     :elevation                     [0. 1.]
     ;:foliar-moisture
     ;:fuel-model
     :fuel-moisture-dead-100hr      [0. 5e-3]
     :fuel-moisture-dead-10hr       [0. 5e-3]
     :fuel-moisture-dead-1hr        [0. 5e-3]
     :fuel-moisture-live-herbaceous [0. 5e-3]
     :fuel-moisture-live-woody      [0. 5e-3]
     :relative-humidity             [0. 20.]
     ;:slope
     :temperature                   [-10. 10.]
     :wind-from-direction           [-30. 30.]
     :wind-speed-20ft               [-2. 2.]}))

(defn load-inputs!
  [config]
  (-> config
      (force-pixel-pertubations)                            ;; HACK to avoid changing config files.
      (inputs/add-input-layers)
      (inputs/add-misc-params)
      (inputs/add-ignition-csv)
      (inputs/add-sampled-params)
      (inputs/add-perturbation-params)
      (inputs/add-weather-params)
      (inputs/add-fuel-moisture-params)
      (inputs/add-random-ignition-sites)
      (inputs/add-aggregate-matrices)
      (inputs/add-burn-period-params)))

(defn load-config!
  [config-file-path]
  (let [config (edn/read-string (slurp config-file-path))]
    (if (spec/valid? ::config-spec/config config)
      (assoc config :config-file-path config-file-path)
      (log-str (format "Invalid config file [%s]:\n%s"
                       config-file-path
                       (spec/explain-str ::config-spec/config config))))))

(defn process-config-file!
  [config-file-path]
  (try
    (some-> config-file-path
            (load-config!)
            (load-inputs!)
            (run-simulations!)
            #_ ;; removing the I/O overhead to get a better signal-to-noise ratio for :run-simulation benchmarking.
            (write-outputs!))
    (catch Exception e
      (log-str (ex-message e)))))
;; gridfire-core ends here

(def benchmark-config-file "../benchmark_for_mark/gridfire.edn")

(defn run-my-benchmark!
  []
  (process-config-file! benchmark-config-file))


(defn -main [& _args]
  (run-my-benchmark!)
  (System/exit 0))

(comment

  ;;;; Results:

  ;; CONTROL
  ;07/07 12:41:01 Running simulations
  ;07/07 12:43:34 :run-simulation,
  ;pId                  nCalls        Min        Max       Mean   MAD      Clock  Total
  ;
  ;:run-fire-spread         30     4.78s      8.10s      5.06s    ±6%     2.53m    100%
  ;
  ;Accounted                                                              2.53m    100%
  ;Clock                                                                  2.53m    100%

  ;; TREATMENT
  ;07/07 12:58:14 Running simulations
  ;07/07 12:59:46 :run-simulation,
  ;pId                  nCalls        Min        Max       Mean   MAD      Clock  Total
  ;
  ;:run-fire-spread         30     2.83s      5.73s      3.00s    ±7%     1.50m    100%
  ;
  ;Accounted                                                              1.50m    100%
  ;Clock                                                                  1.50m    100%

  ;;;; What configuration are we benchmarking?
  (-> (slurp benchmark-config-file)
      (read-string)
      (force-pixel-pertubations))
  =>
  {:srid "EPSG:32611",
   :cell-size 98.425,
   :foliar-moisture 80,
   :wind-speed-20ft 5,
   :ignition-row 792,
   :ignition-col 800,
   :random-seed 1234567890,
   :ellipse-adjustment-factor 1.0,
   :output-landfire-inputs? true,
   :perturbations {:fuel-moisture-live-woody {:spatial-type :pixel, :range [0.0 0.005]},
                   :aspect {:spatial-type :pixel, :range [0.0 10.0]},
                   :elevation {:spatial-type :pixel, :range [0.0 1.0]},
                   :canopy-height {:spatial-type :pixel, :range [0.0 1.0]},
                   :wind-speed-20ft {:spatial-type :pixel, :range [-2.0 2.0]},
                   :fuel-moisture-dead-1hr {:spatial-type :pixel, :range [0.0 0.005]},
                   :fuel-moisture-dead-10hr {:spatial-type :pixel, :range [0.0 0.005]},
                   :fuel-moisture-dead-100hr {:spatial-type :pixel, :range [0.0 0.005]},
                   :relative-humidity {:spatial-type :pixel, :range [0.0 20.0]},
                   :wind-from-direction {:spatial-type :pixel, :range [-30.0 30.0]},
                   :temperature {:spatial-type :pixel, :range [-10.0 10.0]},
                   :fuel-moisture-live-herbaceous {:spatial-type :pixel, :range [0.0 0.005]}},
   :fuel-moisture {:dead {:1hr 0.02, :10hr 0.02, :100hr 0.02}, :live {:herbaceous 0.3, :woody 0.6}},
   :max-runtime 4320,
   :landfire-layers {:aspect {:type :geotiff,
                              :source "/Users/val/projects/SIG/benchmark_for_mark/fuels_and_topography/asp.tif"},
                     :canopy-base-height {:type :geotiff,
                                          :source "/Users/val/projects/SIG/benchmark_for_mark/fuels_and_topography/cbh.tif"},
                     :canopy-cover {:type :geotiff,
                                    :source "/Users/val/projects/SIG/benchmark_for_mark/fuels_and_topography/cc.tif"},
                     :canopy-height {:type :geotiff,
                                     :source "/Users/val/projects/SIG/benchmark_for_mark/fuels_and_topography/ch.tif"},
                     :crown-bulk-density {:type :geotiff,
                                          :source "/Users/val/projects/SIG/benchmark_for_mark/fuels_and_topography/cbd.tif"},
                     :elevation {:type :geotiff,
                                 :source "/Users/val/projects/SIG/benchmark_for_mark/fuels_and_topography/dem.tif"},
                     :fuel-model {:type :geotiff,
                                  :source "/Users/val/projects/SIG/benchmark_for_mark/fuels_and_topography/fbfm40.tif"},
                     :slope {:type :geotiff,
                             :source "/Users/val/projects/SIG/benchmark_for_mark/fuels_and_topography/slp.tif"}},
   :relative-humidity 17,
   :wind-from-direction 270,
   :output-geotiffs? true,
   :simulations 1,
   :output-csvs? true,
   :output-directory "/Users/val/projects/SIG/benchmark_for_mark/outputs",
   :output-pngs? true,
   :temperature 85}

  *e)
