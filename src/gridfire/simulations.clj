(ns gridfire.simulations
  (:require [clojure.java.io                              :as io]
            [gridfire.binary-output                       :as binary]
            [gridfire.common                              :refer [get-neighbors in-bounds?]]
            [gridfire.conversion                          :refer [min->hour kebab->snake snake->kebab]]
            [gridfire.fire-spread-optimal                 :refer [run-fire-spread]]
            [gridfire.grid-lookup                         :as grid-lookup]
            [gridfire.outputs                             :as outputs]
            [gridfire.perturbations.pixel.hash-determined :as pixel-hdp]
            [gridfire.utils.async                         :as gf-async]
            [gridfire.utils.random                        :refer [my-rand-range]]
            [manifold.deferred                            :as mfd]
            [taoensso.tufte                               :as tufte]
            [tech.v3.datatype                             :as d]
            [tech.v3.datatype.functional                  :as dfn]
            [tech.v3.tensor                               :as t])
  (:import java.util.Random))

(set! *unchecked-math* :warn-on-boxed)

(def ^:dynamic *log-performance-metrics*
  "Whether to log performance monitoring metrics,
  which adds latency to simulations,
  and verbosity to the logs."
  (= "true"
     ;; TIP: to re-def this Var as true in your REPL,
     ;; comment out the following expr and reload the code, using #_
     (System/getProperty "gridfire.simulations.log-performance-metrics")))

(defn layer-snapshot [burn-time-matrix layer-matrix ^double t]
  (d/clone
   (d/emap (fn [^double layer-value ^double burn-time]
             (if (and (not (neg? burn-time)) (<= burn-time t))
               layer-value
               0.0))
           :float64
           layer-matrix
           burn-time-matrix)))

(defn previous-active-perimeter?
  [[i j :as here] matrix]
  (let [[num-rows num-cols] (:shape (t/tensor->dimensions matrix))]
    (and
     (= (t/mget matrix i j) -1.0)
     (->> (get-neighbors here)
          (filter #(in-bounds? num-rows num-cols %))
          (map #(apply t/mget matrix %))
          (some pos?)))))

(defn to-color-map-values [burn-time-matrix ^double current-clock]
  (t/compute-tensor
   (:shape (t/tensor->dimensions burn-time-matrix))
   (fn [i j]
     (let [^double burn-time (t/mget burn-time-matrix i j)
           delta-hours       (->> (- current-clock burn-time)
                                  min->hour)]
       (cond
         (previous-active-perimeter? [i j] burn-time-matrix) 201
         (= burn-time -1.0)                                  200
         (< 0 delta-hours 5)                                 delta-hours
         (>= delta-hours 5)                                  5
         :else                                               0)))
   :int64))

(defn process-output-layers-timestepped
  [{:keys [simulation-id] :as config}
   {:keys [global-clock burn-time-matrix] :as fire-spread-results}
   name layer timestep envelope]
  (let [global-clock (double global-clock)
        output-times (range 0 (inc global-clock) timestep)]
    (->> output-times
         (mapv
           (fn [output-time]
             (->
               (outputs/exec-in-outputs-writing-pool
                 (fn []
                   (let [matrix (if (= name "burn_history")
                                  (to-color-map-values layer output-time)
                                  (fire-spread-results layer))]
                     (layer-snapshot burn-time-matrix matrix output-time))))
               (mfd/chain
                 (fn [filtered-matrix]
                   (mfd/zip
                     (outputs/output-geotiff config filtered-matrix name envelope simulation-id output-time)
                     (outputs/output-png config filtered-matrix name envelope simulation-id output-time))))
               (gf-async/nil-when-completed))))
         (gf-async/nil-when-all-completed))))

(def layer-name+matrix-key+is-optional
  [["fire_spread"              :fire-spread-matrix]
   ["flame_length"             :flame-length-matrix]
   ["directional_flame_length" :directional-flame-length-matrix true]
   ["fire_line_intensity"      :fire-line-intensity-matrix]
   ["burn_history"             :burn-time-matrix]
   ["spread_rate"              :spread-rate-matrix]
   ["fire_type"                :fire-type-matrix]])

(defn filter-output-layers [output-layers]
  (let [layers-to-filter (set (map (comp kebab->snake name) (keys output-layers)))]
    (filter (fn [[name _]] (contains? layers-to-filter name)) layer-name+matrix-key+is-optional)))

(defn process-output-layers!
  [{:keys [output-layers output-geotiffs? output-pngs?] :as config}
   {:keys [global-clock] :as fire-spread-results}
   envelope
   simulation-id]
  (let [layers (if output-layers
                 (filter-output-layers output-layers)
                 layer-name+matrix-key+is-optional)]
    (->> layers
         (mapv
           (fn [[name layer is-optional?]]
             (let [kw       (keyword (snake->kebab name))
                   timestep (get output-layers kw)]
               (if (int? timestep)
                 (process-output-layers-timestepped config
                                                    fire-spread-results
                                                    name
                                                    layer
                                                    timestep
                                                    envelope)
                 (when-some [matrix0 (or (get fire-spread-results layer)
                                         (if is-optional?
                                           nil
                                           (throw (ex-info (format "missing layer %s in fire-spread-results" (pr-str layer))
                                                           {::layer-key layer}))))]
                   (-> (outputs/exec-in-outputs-writing-pool
                        (fn []
                          (-> matrix0
                              ;; TODO that check will never be true, (Val, 03 Nov 2022)
                              ;; since we are comparing a Keyword to a String,
                              ;; but I suspect we've been relying on that bug until now.
                              (cond-> (= layer "burn_history") (to-color-map-values global-clock)))))
                       (mfd/chain
                        (fn [matrix]
                          (mfd/zip
                           (when output-geotiffs?
                             (outputs/output-geotiff config matrix name envelope simulation-id))
                           (when output-pngs?
                             (outputs/output-png config matrix name envelope simulation-id)))))
                       (gf-async/nil-when-completed)))))))
         (gf-async/nil-when-all-completed))))

(defn process-burn-count!
  [{:keys [fire-spread-matrix burn-time-matrix global-clock]}
   burn-count-matrix
   timestep]
  (if (int? timestep)
    (let [global-clock (double global-clock)
          timestep     (long timestep)]
      (doseq [^double clock (range 0 (inc global-clock) timestep)]
        (let [filtered-fire-spread (d/clone
                                    (d/emap (fn [^double layer-value ^double burn-time]
                                              (if (<= burn-time clock)
                                                layer-value
                                                0.0))
                                            :float64
                                            fire-spread-matrix
                                            burn-time-matrix))
              band                 (int (quot clock timestep))
              burn-count-matrix-i  (nth burn-count-matrix band)]
          (d/copy! (dfn/+ burn-count-matrix-i filtered-fire-spread) burn-count-matrix-i))))
    (d/copy! (dfn/+ burn-count-matrix fire-spread-matrix) burn-count-matrix)))

(defn process-aggregate-output-layers!
  [{:keys [output-flame-length-sum output-flame-length-max burn-count-matrix flame-length-max-matrix flame-length-sum-matrix
           output-burn-probability spot-count-matrix]} fire-spread-results]
  (when-let [timestep output-burn-probability]
    (process-burn-count! fire-spread-results burn-count-matrix timestep))
  (when flame-length-sum-matrix
    (if (= output-flame-length-sum :directional)
      (d/copy! (dfn/+ flame-length-sum-matrix (:directional-flame-length-matrix fire-spread-results))
               flame-length-sum-matrix)
      (d/copy! (dfn/+ flame-length-sum-matrix (:flame-length-matrix fire-spread-results))
               flame-length-sum-matrix)))
  (when flame-length-max-matrix
    (if (= output-flame-length-max :directional)
      (d/copy! (dfn/max flame-length-max-matrix (:directional-flame-length-matrix fire-spread-results))
               flame-length-max-matrix)
      (d/copy! (dfn/max flame-length-max-matrix (:flame-length-matrix fire-spread-results))
               flame-length-max-matrix)))
  (when spot-count-matrix
    (d/copy! (dfn/+ spot-count-matrix (:spot-matrix fire-spread-results))
             spot-count-matrix)))

(defn process-binary-output!
  [{:keys [output-binary? output-directory]}
   {:keys [burn-time-matrix flame-length-matrix spread-rate-matrix fire-type-matrix]}
   ^long simulation]
  (when output-binary?
    (outputs/exec-in-outputs-writing-pool
      (fn []
        (let [output-name (format "toa_0001_%05d.bin" (inc simulation))
              output-path (if output-directory
                            (.getPath (io/file output-directory output-name))
                            output-name)]
          ;; NOTE that's not parallelizable, as the matrices get written to the same file.
          (binary/write-matrices-as-binary output-path
                                           [:float :float :float :int]
                                           [(->> burn-time-matrix
                                                 (d/emap (fn ^double [^double x] (if (pos? x) (* 60.0 x) x)) :float64)
                                                 (d/clone))
                                            flame-length-matrix
                                            spread-rate-matrix
                                            fire-type-matrix]))))))
(defn cells-to-acres ^double
  [^double cell-size ^long num-cells]
  (let [acres-per-cell (/ (* cell-size cell-size) 43560.0)]
    (* acres-per-cell num-cells)))

(defn summarize-fire-spread-results
  [fire-spread-results cell-size]
  (let [flame-lengths              (filterv pos? (t/tensor->buffer (:flame-length-matrix fire-spread-results)))
        fire-line-intensities      (filterv pos? (t/tensor->buffer (:fire-line-intensity-matrix fire-spread-results)))
        burned-cells               (count flame-lengths)
        surface-fire-size          (cells-to-acres cell-size (:surface-fire-count fire-spread-results))
        crown-fire-size            (cells-to-acres cell-size (:crown-fire-count fire-spread-results))
        flame-length-mean          (/ (dfn/sum flame-lengths) burned-cells)
        fire-line-intensity-mean   (/ (dfn/sum fire-line-intensities) burned-cells)
        flame-length-stddev        (->> flame-lengths
                                        (d/emap #(Math/pow (- flame-length-mean ^double %) 2.0) nil)
                                        (dfn/sum)
                                        (#(/ ^double % burned-cells))
                                        (Math/sqrt))
        fire-line-intensity-stddev (->> fire-line-intensities
                                        (d/emap #(Math/pow (- fire-line-intensity-mean ^double %) 2.0) nil)
                                        (dfn/sum)
                                        (#(/ ^double % burned-cells))
                                        (Math/sqrt))]
    {:fire-size                  (+ surface-fire-size crown-fire-size)
     :surface-fire-size          surface-fire-size
     :crown-fire-size            crown-fire-size
     :flame-length-mean          flame-length-mean
     :flame-length-stddev        flame-length-stddev
     :fire-line-intensity-mean   fire-line-intensity-mean
     :fire-line-intensity-stddev fire-line-intensity-stddev
     :spot-count                 (:spot-count fire-spread-results)}))

(defn- matrix-or-i
  [inputs layer-name i]
  (or (get inputs (-> (name layer-name)
                      (str "-matrix")
                      keyword))
      (get-in inputs [(-> (name layer-name)
                          (str "-samples")
                          keyword)
                      i])))

(defn- get-index-multiplier
  [inputs layer-name]
  (get inputs (-> (name layer-name)
                  (str "-index-multiplier")
                  keyword)))

(defn tensor-cell-getter
  "Returns a function roughly similar to (partial t/mget m),
  but is more tolerant of both m (may be a number)
  the subsequently passed indices (the band index will be ignored
  if m is 2D.)"
  [m]
  {:post [(grid-lookup/suitable-for-primitive-lookup? %)]}
  (if (number? m)
    (let [v (double m)]
      (fn get0d
        (^double [_b] v)
        (^double [_i _j] v)
        (^double [_b _i _j] v)))
    (case (count (d/shape m))
      2 (fn get2d
          (^double [i j] (t/mget m i j))
          ;; This case is important, because some input tensors (Val, 03 Nov 2022)
          ;; like moisture will be provided sometimes in 2d, sometimes in 3d.
          (^double [_b i j] (t/mget m i j)))
      3 (fn get3d
          (^double [i j] (t/mget m 0 i j))
          (^double [b i j] (t/mget m b i j))))))

(def n-buckets 1024)

(defmulti perturbation-getter (fn [perturb-config _rand-gen] (:spatial-type perturb-config)))

(defn- sample-h->perturb
  [perturb-config rand-gen]
  (let [[range-min range-max] (:range perturb-config)
        gen-perturbation      (fn gen-in-range [_h]
                                (my-rand-range rand-gen range-min range-max))
        h->perturb            (pixel-hdp/gen-hash->perturbation n-buckets gen-perturbation)]
    h->perturb))

(defmethod perturbation-getter :global
  [perturb-config rand-gen]
  (let [h->perturb (sample-h->perturb perturb-config rand-gen)]
    (fn get-global-perturbation
      (^double [_i _j]
       ;; NOTE we used to resolve {:spatial-type :global} perturbations using an atom-held cache,
       ;; which is not a problem performance-wise,
       ;; but even in this case a Hash-Determined strategy is better,
       ;; because it makes the perturbation more robustly reproducible,
       ;; as it won't be affected by other uses of the random generator
       ;; during the simulation loop.
       (pixel-hdp/resolve-perturbation-for-coords h->perturb -1 -1 -1))
      (^double [b _i _j]
       (pixel-hdp/resolve-perturbation-for-coords h->perturb b -1 -1)))))

(defmethod perturbation-getter :pixel
  [perturb-config rand-gen]
  (let [h->perturb (sample-h->perturb perturb-config rand-gen)]
    (fn get-pixel-perturbation
      (^double [i j]
       (pixel-hdp/resolve-perturbation-for-coords h->perturb i j))
      (^double [b i j]
       (pixel-hdp/resolve-perturbation-for-coords h->perturb b i j)))))

(defn- grid-getter
  "Pre-computes a 'getter' for resolving perturbed input values in the GridFire space-time grid.

  Returns a 'getter' object, to be called with (gridfire.grid-lookup/double-at getter b i j)
  in order to resolve the input value at coordinates [b i j].
  At the time of writing, this getter object will be a function, but that's an implementation detail,
  do not rely on it.

  The perturbed value might be lazily computed, and its computation may involve pseudo-randomness,
  but calling (grid-lookup/double-at ...) several times at the same coordinates
  will always return the same result."
  [{:keys [perturbations] :as inputs} rand-gen layer-name i]
  {:post [(or (nil? %) (grid-lookup/suitable-for-primitive-lookup? %))]}
  (when-let [matrix-or-num (matrix-or-i inputs layer-name i)]
    (let [index-multiplier (get-index-multiplier inputs layer-name)
          tensor-lookup    (tensor-cell-getter matrix-or-num)
          get-unperturbed  (if (number? matrix-or-num)
                             tensor-lookup
                             (if (nil? index-multiplier)
                               tensor-lookup
                               (let [index-multiplier (double index-multiplier)]
                                 (fn multiplied-tensor-lookup
                                   (^double [i j]
                                    (let [i   (long i)
                                          j   (long j)
                                          row (long (* i index-multiplier))
                                          col (long (* j index-multiplier))]
                                      (grid-lookup/double-at tensor-lookup row col)))
                                   (^double [b i j]
                                    (let [i   (long i)
                                          j   (long j)
                                          row (long (* i index-multiplier))
                                          col (long (* j index-multiplier))]
                                      (grid-lookup/double-at tensor-lookup b row col)))))))
          get-perturbation (if-some [perturb-config (get perturbations layer-name)]
                             (perturbation-getter perturb-config rand-gen)
                             nil)]
      (fn grid-getter
        (^double [i j]
         (cond-> (grid-lookup/double-at get-unperturbed i j)
           (some? get-perturbation)
           (-> (+ (grid-lookup/double-at get-perturbation i j))
               (max 0.))))
        (^double [b i j]
         (cond-> (grid-lookup/double-at get-unperturbed b i j)
           (some? get-perturbation)
           (-> (+ (grid-lookup/double-at get-perturbation b i j))
               (max 0.))))))))

(defrecord SimulationInputs
    [^long num-rows
     ^long num-cols
     ^double cell-size
     ^Random rand-gen
     initial-ignition-site
     ^double ignition-start-time
     ignition-start-timestamp
     burn-period-start
     burn-period-end
     ^double max-runtime
     compute-directional-values?
     fuel-number->spread-rate-adjustment-array-lookup
     get-aspect
     get-canopy-base-height
     get-canopy-cover
     get-canopy-height
     get-crown-bulk-density
     get-elevation
     get-foliar-moisture
     get-fuel-model
     get-fuel-moisture-dead-100hr
     get-fuel-moisture-dead-10hr
     get-fuel-moisture-dead-1hr
     get-fuel-moisture-live-herbaceous
     get-fuel-moisture-live-woody
     get-relative-humidity
     get-slope
     get-suppression-difficulty-index
     get-temperature
     get-wind-from-direction
     get-wind-speed-20ft
     ^double ellipse-adjustment-factor
     ^boolean crowning-disabled?
     ^boolean grass-suppression?
     sdi-containment-overwhelming-area-growth-rate
     sdi-reference-suppression-speed
     sdi-sensitivity-to-difficulty
     spotting
     suppression-coefficient
     suppression-dt])

(defn prepare-simulation-inputs
  [^long i
   {:keys
    [num-rows num-cols crowning-disabled? grass-suppression? ignition-matrix cell-size max-runtime-samples
     ignition-rows ignition-cols ellipse-adjustment-factor-samples random-seed ignition-start-times spotting
     burn-period-samples ignition-start-timestamps
     output-flame-length-sum output-flame-length-max output-layers]
    :as inputs}]
  (let [rand-gen           (if random-seed (Random. (+ ^long random-seed i)) (Random.))
        burn-period        (burn-period-samples i)
        simulation-inputs  {:num-rows                                         num-rows
                            :num-cols                                         num-cols
                            :cell-size                                        cell-size
                            :rand-gen                                         rand-gen
                            :initial-ignition-site                            (or ignition-matrix
                                                                                  [(ignition-rows i) (ignition-cols i)])
                            :ignition-start-time                              (get ignition-start-times i 0.0)
                            :ignition-start-timestamp                         (get ignition-start-timestamps i)
                            :burn-period-start                                (:burn-period-start burn-period)
                            :burn-period-end                                  (:burn-period-end burn-period)
                            :max-runtime                                      (max-runtime-samples i)
                            :compute-directional-values?                      (or (= output-flame-length-max :directional)
                                                                                  (= output-flame-length-sum :directional)
                                                                                  (:directional-flame-length output-layers))
                            :get-aspect                                       (grid-getter inputs rand-gen :aspect i)
                            :get-canopy-base-height                           (grid-getter inputs rand-gen :canopy-base-height i)
                            :get-canopy-cover                                 (grid-getter inputs rand-gen :canopy-cover i)
                            :get-canopy-height                                (grid-getter inputs rand-gen :canopy-height i)
                            :get-crown-bulk-density                           (grid-getter inputs rand-gen :crown-bulk-density i)
                            :get-elevation                                    (grid-getter inputs rand-gen :elevation i)
                            :get-foliar-moisture                              (grid-getter inputs rand-gen :foliar-moisture i)
                            :get-fuel-model                                   (grid-getter inputs rand-gen :fuel-model i)
                            :get-fuel-moisture-dead-100hr                     (grid-getter inputs rand-gen :fuel-moisture-dead-100hr i)
                            :get-fuel-moisture-dead-10hr                      (grid-getter inputs rand-gen :fuel-moisture-dead-10hr i)
                            :get-fuel-moisture-dead-1hr                       (grid-getter inputs rand-gen :fuel-moisture-dead-1hr i)
                            :get-fuel-moisture-live-herbaceous                (grid-getter inputs rand-gen :fuel-moisture-live-herbaceous i)
                            :get-fuel-moisture-live-woody                     (grid-getter inputs rand-gen :fuel-moisture-live-woody i)
                            :get-relative-humidity                            (grid-getter inputs rand-gen :relative-humidity i)
                            :get-slope                                        (grid-getter inputs rand-gen :slope i)
                            :get-suppression-difficulty-index                 (grid-getter inputs rand-gen :suppression-difficulty-index i)
                            :get-temperature                                  (grid-getter inputs rand-gen :temperature i)
                            :get-wind-from-direction                          (grid-getter inputs rand-gen :wind-from-direction i)
                            :get-wind-speed-20ft                              (grid-getter inputs rand-gen :wind-speed-20ft i)
                            :crowning-disabled?                               (true? crowning-disabled?) ; NOTE not using samples yet. Might never be needed.
                            :ellipse-adjustment-factor                        (ellipse-adjustment-factor-samples i)
                            :grass-suppression?                               (true? grass-suppression?)
                            :sdi-containment-overwhelming-area-growth-rate    (some-> (:sdi-containment-overwhelming-area-growth-rate-samples inputs) (get i))
                            :sdi-reference-suppression-speed                  (some-> (:sdi-reference-suppression-speed-samples inputs) (get i))
                            :sdi-sensitivity-to-difficulty                    (some-> (:sdi-sensitivity-to-difficulty-samples inputs) (get i))
                            :spotting                                         spotting
                            :fuel-number->spread-rate-adjustment-array-lookup (some-> (:fuel-number->spread-rate-adjustment-array-lookup-array-lookup-samples inputs) (get i))
                            :suppression-coefficient                          (some-> (:suppression-coefficient-samples inputs) (get i))
                            :suppression-dt                                   (some-> (:suppression-dt-samples inputs) (get i))}]
    (map->SimulationInputs simulation-inputs)))

(defn process-simulation-results!
  [^long i
   {:keys [output-csvs? envelope cell-size ignition-rows ignition-cols] :as inputs}
   simulation-inputs
   simulation-results]
  (when simulation-results
    (->
      (mfd/zip
       (process-output-layers! inputs simulation-results envelope i)
       (process-aggregate-output-layers! inputs simulation-results)
       (process-binary-output! inputs simulation-results i))
      (gf-async/nil-when-completed)
      (deref)))
  (when output-csvs?
    (-> simulation-inputs
        (dissoc :get-aspect
                :get-canopy-height
                :get-canopy-base-height
                :get-canopy-cover
                :get-crown-bulk-density
                :get-fuel-model
                :get-slope
                :get-elevation
                :get-temperature
                :get-relative-humidity
                :get-wind-speed-20ft
                :get-wind-from-direction
                :get-fuel-moisture-dead-1hr
                :get-fuel-moisture-dead-10hr
                :get-fuel-moisture-dead-100hr
                :get-fuel-moisture-live-herbaceous
                :get-fuel-moisture-live-woody
                :get-foliar-moisture)
        (merge {:simulation         (inc i)
                :ignition-row       (get ignition-rows i)
                :ignition-col       (get ignition-cols i)
                :global-clock       (:global-clock simulation-results)
                :exit-condition     (:exit-condition simulation-results :no-fire-spread)
                :surface-fire-count (:surface-fire-count simulation-results)
                :crown-fire-count   (:crown-fire-count simulation-results)
                :spot-count         (:spot-count simulation-results)})
        (merge
         (if simulation-results
           (tufte/p
            :summarize-fire-spread-results
            (summarize-fire-spread-results simulation-results cell-size))
           {:fire-size                  0.0
            :flame-length-mean          0.0
            :flame-length-stddev        0.0
            :fire-line-intensity-mean   0.0
            :fire-line-intensity-stddev 0.0})))))

(defn run-simulation!
  [^long i inputs]
  (tufte/profile
   {:id :run-simulation}
   (let [simulation-inputs  (prepare-simulation-inputs i inputs)
         simulation-results (tufte/p :run-fire-spread
                                     (run-fire-spread simulation-inputs))]
     (process-simulation-results! i inputs simulation-inputs simulation-results))))
