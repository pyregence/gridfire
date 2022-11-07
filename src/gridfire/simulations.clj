(ns gridfire.simulations
  (:require [clojure.java.io              :as io]
            [gridfire.binary-output       :as binary]
            [gridfire.common              :refer [get-neighbors in-bounds?]]
            [gridfire.conversion          :refer [min->hour kebab->snake snake->kebab]]
            [gridfire.fire-spread-optimal :refer [run-fire-spread]]
            [gridfire.outputs             :as outputs]
            [gridfire.utils.async         :as gf-async]
            [gridfire.utils.random        :refer [my-rand-range]]
            [manifold.deferred            :as mfd]
            [taoensso.tufte               :as tufte]
            [tech.v3.datatype             :as d]
            [tech.v3.datatype.functional  :as dfn]
            [tech.v3.tensor               :as t])
  (:import java.util.Random))

(set! *unchecked-math* :warn-on-boxed)

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

(def layer-name->matrix
  [["fire_spread"              :fire-spread-matrix]
   ["flame_length"             :flame-length-matrix]
   ["directional_flame_length" :directional-flame-length-matrix]
   ["fire_line_intensity"      :fire-line-intensity-matrix]
   ["burn_history"             :burn-time-matrix]
   ["spread_rate"              :spread-rate-matrix]
   ["fire_type"                :fire-type-matrix]])

(defn filter-output-layers [output-layers]
  (let [layers-to-filter (set (map (comp kebab->snake name) (keys output-layers)))]
    (filter (fn [[name _]] (contains? layers-to-filter name)) layer-name->matrix)))

(defn process-output-layers!
  [{:keys [output-layers output-geotiffs? output-pngs?] :as config}
   {:keys [global-clock] :as fire-spread-results}
   envelope
   simulation-id]
  (let [layers (if output-layers
                 (filter-output-layers output-layers)
                 layer-name->matrix)]
    (->> layers
         (mapv
           (fn [[name layer]]
             (let [kw       (keyword (snake->kebab name))
                   timestep (get output-layers kw)]
               (if (int? timestep)
                 (process-output-layers-timestepped config
                                                    fire-spread-results
                                                    name
                                                    layer
                                                    timestep
                                                    envelope)
                 (->
                   (outputs/exec-in-outputs-writing-pool
                     (fn []
                       (if (= layer "burn_history")
                         (to-color-map-values layer global-clock)
                         (fire-spread-results layer))))
                   (mfd/chain
                     (fn [matrix]
                       (mfd/zip
                         (when output-geotiffs?
                           (outputs/output-geotiff config matrix name envelope simulation-id))
                         (when output-pngs?
                           (outputs/output-png config matrix name envelope simulation-id)))))
                   (gf-async/nil-when-completed))))))
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

(defn- get-value-fn
  [{:keys [perturbations] :as inputs} rand-gen layer-name i]
  (when-let [matrix-or-num (matrix-or-i inputs layer-name i)]
    (let [index-multiplier             (get-index-multiplier inputs layer-name)
          {:keys [spatial-type range]} (get perturbations layer-name)
          [range-min range-max]        range]
      (cond
        (and (number? matrix-or-num) (nil? (get perturbations layer-name)))
        (fn
          (^double [_ _] matrix-or-num)
          (^double [_ _ _] matrix-or-num))

        (and (not (number? matrix-or-num)) (nil? (get perturbations layer-name)))
        (if index-multiplier
          (let [index-multiplier (double index-multiplier)]
            (fn
              (^double [^long i ^long j]
               (let [row (long (* i index-multiplier))
                     col (long (* j index-multiplier))]
                 (t/mget matrix-or-num row col)))
              (^double [^long b ^long i ^long j]
               (let [row (long (* i index-multiplier))
                     col (long (* j index-multiplier))]
                 (t/mget matrix-or-num b row col)))))
          (fn
            (^double [i j]
             (t/mget matrix-or-num i j))
            (^double [b i j]
             (t/mget matrix-or-num b i j))))

        (and (number? matrix-or-num) (= spatial-type :pixel))
        (let [band-cache            (atom 0)
              perturbed-value-cache (atom {})
              matrix-or-num         (double matrix-or-num)]
          (fn
            (^double [i j]
             (or (get @perturbed-value-cache [i j])
                 (let [new-value (max 0.0 (+ matrix-or-num (my-rand-range rand-gen range-min range-max)))]
                   (swap! perturbed-value-cache assoc [i j] new-value)
                   new-value)))
            (^double [b i j]
             (when-not (= b @band-cache)
               (reset! band-cache b)
               (reset! perturbed-value-cache {}))
             (or (get @perturbed-value-cache [i j]) ;TODO benchmark [i j] vs string i j
                 (let [new-value (max 0.0 (+ matrix-or-num (my-rand-range rand-gen range-min range-max)))] ;TODO document we are snapping negative values to 0
                   (swap! perturbed-value-cache assoc [i j] new-value)
                   new-value)))))

        (and (number? matrix-or-num) (= spatial-type :global))
        (let [perturbed-value-cache (atom nil)
              matrix-or-num         (double matrix-or-num)]
          (fn
            (^double [_ _]
             (or @perturbed-value-cache
                 (let [new-value (max 0.0 (+ matrix-or-num (my-rand-range rand-gen range-min range-max)))]
                   (reset! perturbed-value-cache new-value)
                   new-value)))
            (^double [b _ _]
             (or (get @perturbed-value-cache b)
                 (let [new-value (max 0.0 (+ matrix-or-num (my-rand-range rand-gen range-min range-max)))]
                   (reset! perturbed-value-cache {b new-value})
                   new-value)))))

        (and (not (number? matrix-or-num)) (= spatial-type :pixel))
        (let [band-cache            (atom 0)
              perturbed-value-cache (atom {})]
          (if index-multiplier
            (let [index-multiplier (double index-multiplier)]
              (fn
                (^double [^long i ^long j]
                 (let [row (long (* i index-multiplier))
                       col (long (* j index-multiplier))]
                   (or (get @perturbed-value-cache [row col])
                       (let [new-value (max 0.0 (+ ^double (t/mget matrix-or-num row col)
                                                   (my-rand-range rand-gen range-min range-max)))]
                         (swap! perturbed-value-cache assoc [row col] new-value)
                         new-value))))
                (^double [^long b ^long i ^long j]
                 (when-not (= b @band-cache)
                   (reset! band-cache b)
                   (reset! perturbed-value-cache {}))
                 (let [row (long (* i index-multiplier))
                       col (long (* j index-multiplier))]
                   (or (get @perturbed-value-cache [row col])
                       (let [new-value (max 0.0 (+ ^double (t/mget matrix-or-num b row col)
                                                   (my-rand-range rand-gen range-min range-max)))]
                         (swap! perturbed-value-cache assoc [row col] new-value)
                         new-value))))))
            (fn
              (^double [i j]
               (or (get @perturbed-value-cache [i j])
                   (let [new-value (max 0.0 (+ ^double (t/mget matrix-or-num i j)
                                               (my-rand-range rand-gen range-min range-max)))]
                     (swap! perturbed-value-cache assoc [i j] new-value)
                     new-value)))
              (^double [b i j]
               (when-not (= b @band-cache)
                 (reset! band-cache b)
                 (reset! perturbed-value-cache {}))
               (or (get @perturbed-value-cache [i j])
                   (let [new-value (max 0.0 (+ ^double (t/mget matrix-or-num b i j)
                                               (my-rand-range rand-gen range-min range-max)))]
                     (swap! perturbed-value-cache assoc [i j] new-value)
                     new-value))))))

        (and (not (number? matrix-or-num)) (= spatial-type :global))
        (let [offset-cache (atom nil)]
          (if index-multiplier
            (let [index-multiplier (double index-multiplier)]
              (fn
                (^double [^long i ^long j]
                 (let [row            (long (* i index-multiplier))
                       col            (long (* j index-multiplier))
                       ^double offset (or @offset-cache
                                          (reset! offset-cache (my-rand-range rand-gen range-min range-max)))]
                   (max 0.0 (+ ^double (t/mget matrix-or-num row col) offset))))
                (^double [^long b ^long i ^long j]
                 (let [row            (long (* i index-multiplier))
                       col            (long (* j index-multiplier))
                       ^double offset (or (get @offset-cache b)
                                          (let [new-offset (my-rand-range rand-gen range-min range-max)]
                                            (reset! offset-cache {b new-offset})
                                            new-offset))]
                   (max 0.0 (+ ^double (t/mget matrix-or-num b row col) offset))))))
            (fn
              (^double [i j]
               (let [^double offset (or @offset-cache
                                        (reset! offset-cache (my-rand-range rand-gen range-min range-max)))]
                 (max 0.0 (+ ^double (t/mget matrix-or-num i j) offset))))
              (^double [b i j]
               (let [^double offset (or (get @offset-cache b)
                                        (let [new-offset (my-rand-range rand-gen range-min range-max)]
                                          (reset! offset-cache {b new-offset})
                                          new-offset))]
                 (max 0.0 (+ ^double (t/mget matrix-or-num b i j) offset)))))))))))

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

(defn run-simulation!
  [^long i
   {:keys
    [num-rows num-cols crowning-disabled? grass-suppression? output-csvs? envelope ignition-matrix cell-size max-runtime-samples
     ignition-rows ignition-cols ellipse-adjustment-factor-samples random-seed ignition-start-times spotting
     burn-period-start burn-period-end ignition-start-timestamps output-flame-length-sum output-flame-length-max
     output-layers fuel-number->spread-rate-adjustment-array-lookup-array-lookup-samples suppression-dt-samples suppression-coefficient-samples
     sdi-sensitivity-to-difficulty-samples sdi-containment-overwhelming-area-growth-rate-samples
     sdi-reference-suppression-speed-samples]
    :as inputs}]
  (tufte/profile
   {:id :run-simulation}
   (let [rand-gen           (if random-seed (Random. (+ ^long random-seed i)) (Random.))
         simulation-inputs  {:num-rows                                         num-rows
                             :num-cols                                         num-cols
                             :cell-size                                        cell-size
                             :rand-gen                                         rand-gen
                             :initial-ignition-site                            (or ignition-matrix
                                                                                   [(ignition-rows i) (ignition-cols i)])
                             :ignition-start-time                              (get ignition-start-times i 0.0)
                             :ignition-start-timestamp                         (get ignition-start-timestamps i)
                             :burn-period-start                                burn-period-start
                             :burn-period-end                                  burn-period-end
                             :max-runtime                                      (max-runtime-samples i)
                             :compute-directional-values?                      (or (= output-flame-length-max :directional)
                                                                                   (= output-flame-length-sum :directional)
                                                                                   (:directional-flame-length output-layers))
                             :get-aspect                                       (get-value-fn inputs rand-gen :aspect i)
                             :get-canopy-base-height                           (get-value-fn inputs rand-gen :canopy-base-height i)
                             :get-canopy-cover                                 (get-value-fn inputs rand-gen :canopy-cover i)
                             :get-canopy-height                                (get-value-fn inputs rand-gen :canopy-height i)
                             :get-crown-bulk-density                           (get-value-fn inputs rand-gen :crown-bulk-density i)
                             :get-elevation                                    (get-value-fn inputs rand-gen :elevation i)
                             :get-foliar-moisture                              (get-value-fn inputs rand-gen :foliar-moisture i)
                             :get-fuel-model                                   (get-value-fn inputs rand-gen :fuel-model i)
                             :get-fuel-moisture-dead-100hr                     (get-value-fn inputs rand-gen :fuel-moisture-dead-100hr i)
                             :get-fuel-moisture-dead-10hr                      (get-value-fn inputs rand-gen :fuel-moisture-dead-10hr i)
                             :get-fuel-moisture-dead-1hr                       (get-value-fn inputs rand-gen :fuel-moisture-dead-1hr i)
                             :get-fuel-moisture-live-herbaceous                (get-value-fn inputs rand-gen :fuel-moisture-live-herbaceous i)
                             :get-fuel-moisture-live-woody                     (get-value-fn inputs rand-gen :fuel-moisture-live-woody i)
                             :get-relative-humidity                            (get-value-fn inputs rand-gen :relative-humidity i)
                             :get-slope                                        (get-value-fn inputs rand-gen :slope i)
                             :get-suppression-difficulty-index                 (get-value-fn inputs rand-gen :suppression-difficulty-index i)
                             :get-temperature                                  (get-value-fn inputs rand-gen :temperature i)
                             :get-wind-from-direction                          (get-value-fn inputs rand-gen :wind-from-direction i)
                             :get-wind-speed-20ft                              (get-value-fn inputs rand-gen :wind-speed-20ft i)
                             :crowning-disabled?                               (true? crowning-disabled?) ; NOTE not using samples yet. Might never be needed.
                             :ellipse-adjustment-factor                        (ellipse-adjustment-factor-samples i)
                             :grass-suppression?                               (true? grass-suppression?)
                             :sdi-containment-overwhelming-area-growth-rate    (some-> sdi-containment-overwhelming-area-growth-rate-samples (get i))
                             :sdi-reference-suppression-speed                  (some-> sdi-reference-suppression-speed-samples (get i))
                             :sdi-sensitivity-to-difficulty                    (some-> sdi-sensitivity-to-difficulty-samples (get i))
                             :spotting                                         spotting
                             :fuel-number->spread-rate-adjustment-array-lookup (some-> fuel-number->spread-rate-adjustment-array-lookup-array-lookup-samples (get i))
                             :suppression-coefficient                          (some-> suppression-coefficient-samples (get i))
                             :suppression-dt                                   (some-> suppression-dt-samples (get i))}
         simulation-results (tufte/p :run-fire-spread
                                     (run-fire-spread (map->SimulationInputs simulation-inputs)))]
     (when simulation-results
       (-> (mfd/zip
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
               :fire-line-intensity-stddev 0.0})))))))
