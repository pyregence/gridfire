;; [[file:../../org/GridFire.org::fire-spread-algorithm][fire-spread-algorithm]]
(ns gridfire.fire-spread
  (:require [clojure.core.reducers         :as r]
            [gridfire.common               :refer [burnable-fuel-model?
                                                   burnable?
                                                   calc-fuel-moisture
                                                   in-bounds?
                                                   burnable-neighbors?
                                                   get-neighbors
                                                   distance-3d
                                                   non-zero-indices]]
            [gridfire.conversion          :refer [mph->fpm]]
            [gridfire.crown-fire          :refer [crown-fire-eccentricity
                                                  crown-fire-line-intensity
                                                  cruz-crown-fire-spread
                                                  van-wagner-crown-fire-initiation?]]
            [gridfire.fuel-models         :refer [build-fuel-model moisturize]]
            [gridfire.spotting            :as spot]
            [gridfire.surface-fire        :refer [anderson-flame-depth
                                                  byram-fire-line-intensity
                                                  byram-flame-length
                                                  rothermel-surface-fire-spread-any
                                                  rothermel-surface-fire-spread-max
                                                  rothermel-surface-fire-spread-no-wind-no-slope
                                                  wind-adjustment-factor]]
            [gridfire.fuel-models-optimal  :as f-opt]
            [gridfire.surface-fire-optimal :as s-opt]
            [tech.v3.datatype             :as d]
            [tech.v3.datatype.functional  :as dfn]
            [tech.v3.tensor               :as t]
            [taoensso.tufte :as tufte]))

;; for surface fire, tau = 10 mins, t0 = 0, and t = global-clock
;; for crown fire, tau = 20 mins, t0 = time of first torch, t = global-clock
;; (defn lautenberger-spread-acceleration
;;   [equilibrium-spread-rate t0 t tau]
;;   (* equilibrium-spread-rate (- 1.0 (Math/exp (/ (- t0 t 0.2) tau)))))
;;
;; Note: Because of our use of adaptive timesteps, if the spread rate on
;;       the first timestep is not at least 83 ft/min, then the timestep will
;;       be calculated as greater than 60 minutes, which will terminate the
;;       one hour fire simulation instantly.

(defn random-cell
  "Returns a random [i j] pair with i < num-rows and j < num-cols."
  [num-rows num-cols]
  [(rand-int num-rows)
   (rand-int num-cols)])

(def offset-to-degrees
  "Returns clockwise degrees from north."
  {[-1  0]   0.0   ; N
   [-1  1]  45.0   ; NE
   [ 0  1]  90.0   ; E
   [ 1  1] 135.0   ; SE
   [ 1  0] 180.0   ; S
   [ 1 -1] 225.0   ; SW
   [ 0 -1] 270.0   ; W
   [-1 -1] 315.0}) ; NW

(defn rothermel-fast-wrapper
  [fuel-model-number fuel-moisture grass-suppression?]
  (let [fuel-model      (-> (build-fuel-model (int fuel-model-number))
                            (moisturize fuel-moisture))
        spread-info-min (rothermel-surface-fire-spread-no-wind-no-slope fuel-model grass-suppression?)]
    [fuel-model spread-info-min]))

(defn rothermel-fast-wrapper-optimal
  [fuel-model-number fuel-moisture grass-suppression?]
  (let [fuel-model                       (-> (f-opt/fuel-models-precomputed (long fuel-model-number))
                                             (f-opt/moisturize fuel-moisture))
        [spread-info-min wind-slope-fns] (s-opt/rothermel-surface-fire-spread-no-wind-no-slope
                                          fuel-model grass-suppression?)]
    [fuel-model spread-info-min wind-slope-fns]))

(defrecord BurnTrajectory
    [cell
     source
     trajectory
     ^double terrain-distance
     ^double spread-rate
     ^double fire-line-intensity
     ^double flame-length
     fractional-distance
     fire-type
     crown-fire?])

(defn compute-burn-trajectory
  [neighbor here spread-info-min spread-info-max fuel-model crown-bulk-density
   canopy-cover canopy-height canopy-base-height foliar-moisture crown-spread-max
   crown-eccentricity elevation-matrix cell-size overflow-trajectory overflow-heat
   crown-type]
  (let [trajectory                (mapv - neighbor here)
        spread-direction          (offset-to-degrees trajectory)
        surface-spread-rate       (rothermel-surface-fire-spread-any spread-info-max
                                                                     spread-direction)
        residence-time            (:residence-time spread-info-min)
        reaction-intensity        (:reaction-intensity spread-info-min)
        surface-intensity         (->> (anderson-flame-depth surface-spread-rate residence-time)
                                       (byram-fire-line-intensity reaction-intensity))
        crown-fire?               (van-wagner-crown-fire-initiation? canopy-cover
                                                                     canopy-base-height
                                                                     foliar-moisture
                                                                     surface-intensity)
        ^double crown-spread-rate (when crown-fire?
                                    (rothermel-surface-fire-spread-any
                                     (assoc spread-info-max
                                            :max-spread-rate crown-spread-max
                                            :eccentricity crown-eccentricity)
                                     spread-direction))
        ^double crown-intensity   (when crown-fire?
                                    (crown-fire-line-intensity
                                     crown-spread-rate
                                     crown-bulk-density
                                     (- canopy-height canopy-base-height)
                                     ((fuel-model :h) 0))) ; 0 = dead-1hr
        spread-rate               (if crown-fire?
                                    (max surface-spread-rate crown-spread-rate)
                                    surface-spread-rate)
        fire-line-intensity       (if crown-fire?
                                    (+ surface-intensity crown-intensity)
                                    surface-intensity)
        flame-length              (byram-flame-length fire-line-intensity)]
    (->BurnTrajectory neighbor
                      here
                      trajectory
                      (distance-3d elevation-matrix cell-size here neighbor)
                      spread-rate
                      fire-line-intensity
                      flame-length
                      (volatile! (if (= trajectory overflow-trajectory)
                                   overflow-heat
                                   0.0))
                      (if crown-fire? crown-type :surface)
                      crown-fire?)))

;;TODO Optimize me!
(defn compute-neighborhood-fire-spread-rates!
  "Returns a vector of entries of the form:
  {:cell [i j],
   :trajectory [di dj],
   :terrain-distance ft,
   :spread-rate ft/min,
   :fire-line-intensity Btu/ft/s,
   :flame-length ft,
   :fractional-distance [0-1]}, one for each cell adjacent to here."
  [{:keys
    [get-aspect get-canopy-base-height get-canopy-cover get-canopy-height get-crown-bulk-density
     get-fuel-model get-slope elevation-matrix fuel-model-matrix get-wind-speed-20ft
     get-wind-from-direction get-temperature get-relative-humidity get-foliar-moisture
     ellipse-adjustment-factor cell-size num-rows num-cols get-fuel-moisture-dead-1hr
     get-fuel-moisture-dead-10hr get-fuel-moisture-dead-100hr get-fuel-moisture-live-herbaceous
     get-fuel-moisture-live-woody grass-suppression?]}
   fire-spread-matrix
   [i j :as here]
   overflow-trajectory
   overflow-heat
   global-clock]
  (let [band                                  (int (/ global-clock 60.0))
        ^double aspect                        (get-aspect i j)
        ^double canopy-base-height            (get-canopy-base-height i j)
        ^double canopy-height                 (get-canopy-height i j)
        ^double canopy-cover                  (get-canopy-cover i j)
        ^double crown-bulk-density            (get-crown-bulk-density i j)
        ^double fuel-model                    (get-fuel-model i j)
        ^double slope                         (get-slope i j)
        ^double relative-humidity             (get-relative-humidity band i j)
        ^double temperature                   (get-temperature band i j)
        ^double wind-speed-20ft               (get-wind-speed-20ft band i j)
        ^double wind-from-direction           (get-wind-from-direction band i j)
        ^double fuel-moisture-dead-1hr        (if get-fuel-moisture-dead-1hr
                                                (get-fuel-moisture-dead-1hr band i j)
                                                (calc-fuel-moisture relative-humidity temperature :dead :1hr))
        ^double fuel-moisture-dead-10hr       (if get-fuel-moisture-dead-10hr
                                                (get-fuel-moisture-dead-10hr band i j)
                                                (calc-fuel-moisture relative-humidity temperature :dead :10hr))
        ^double fuel-moisture-dead-100hr      (if get-fuel-moisture-dead-100hr
                                                (get-fuel-moisture-dead-100hr band i j)
                                                (calc-fuel-moisture relative-humidity temperature :dead :100hr))
        ^double fuel-moisture-live-herbaceous (if get-fuel-moisture-live-herbaceous
                                                (get-fuel-moisture-live-herbaceous i j)
                                                (calc-fuel-moisture relative-humidity temperature :live :herbaceous))
        ^double fuel-moisture-live-woody      (if get-fuel-moisture-live-woody
                                                (get-fuel-moisture-live-woody i j)
                                                (calc-fuel-moisture relative-humidity temperature :live :woody))
        ^double foliar-moisture               (get-foliar-moisture band i j)
        [fuel-model spread-info-min wind-slope-fns] (rothermel-fast-wrapper-optimal
                                                     fuel-model
                                                     [fuel-moisture-dead-1hr
                                                      fuel-moisture-dead-10hr
                                                      fuel-moisture-dead-100hr
                                                      0.0 ; fuel-moisture-dead-herbaceous
                                                      fuel-moisture-live-herbaceous
                                                      fuel-moisture-live-woody]
                                                     grass-suppression?)
        midflame-wind-speed                   (mph->fpm
                                               (* wind-speed-20ft
                                                  (wind-adjustment-factor ^double (:delta fuel-model)
                                                                          canopy-height
                                                                          canopy-cover)))
        spread-info-max                       (rothermel-surface-fire-spread-max spread-info-min
                                                                                 wind-slope-fns
                                                                                 midflame-wind-speed
                                                                                 wind-from-direction
                                                                                 slope
                                                                                 aspect
                                                                                 ellipse-adjustment-factor)
        [crown-type crown-spread-max]         (cruz-crown-fire-spread wind-speed-20ft crown-bulk-density fuel-moisture-dead-1hr)
        crown-eccentricity                    (crown-fire-eccentricity wind-speed-20ft
                                                                       ellipse-adjustment-factor)]
    (into []
          (comp
           (filter #(and (in-bounds? num-rows num-cols %)
                         (burnable? fire-spread-matrix fuel-model-matrix here %)))
           (map #(compute-burn-trajectory % here spread-info-min spread-info-max fuel-model
                                          crown-bulk-density canopy-cover canopy-height
                                          canopy-base-height foliar-moisture crown-spread-max
                                          crown-eccentricity elevation-matrix cell-size
                                          overflow-trajectory overflow-heat crown-type)))
          (get-neighbors here))))

(defn- get-old-fractional-distance
  [{:keys [trajectory-combination]} {:keys [fractional-distance]} fractional-distance-matrix [i j]]
  (if (= trajectory-combination :sum)
    (t/mget fractional-distance-matrix i j)
    @fractional-distance))

(defn- update-fractional-distance-matrix!
  "Update the fractional distance matrix with the largest fractional distance calculated."
  [fractional-distance-matrix max-fractionals]
  (doseq [[cell fractional-distance] @max-fractionals]
    (let [[i j] cell]
      (t/mset! fractional-distance-matrix i j fractional-distance))))

(defn- update-fractional-distance!
  "Update fractional distance for given trajectory into the current cell. Return a tuple of [old-value new-value]"
  [{:keys [trajectory-combination] :as inputs} max-fractionals trajectory fractional-distance-matrix timestep cell]
  (let [terrain-distance    (double (:terrain-distance trajectory))
        spread-rate         (double (:spread-rate trajectory))
        new-spread-fraction (/ (* spread-rate timestep) terrain-distance)
        old-total           (get-old-fractional-distance inputs trajectory fractional-distance-matrix cell)
        new-total           (+ old-total new-spread-fraction)]
    (if (= trajectory-combination :sum)
      (let [max-fractional-distance (max (get @max-fractionals cell 0.0) new-total)]
        (swap! max-fractionals assoc cell max-fractional-distance))
      (vreset! (:fractional-distance trajectory) new-total))
    [old-total new-total]))

(defn- update-overflow-heat
  [{:keys [num-rows num-cols]} fractional-distance-matrix {:keys [cell trajectory]} fractional-distance]
  (let [[i j :as target] (mapv + cell trajectory)]
    (when (in-bounds? num-rows num-cols target)
      (t/mset! fractional-distance-matrix i j (- fractional-distance 1.0)))))

(defn ignition-event-reducer
  [inputs max-fractionals fractional-distance-matrix timestep trajectory-combination fire-spread-matrix
   acc trajectory]
  (let [{:keys [source cell]}                 trajectory
        [i j]                                 source
        [^double old-total ^double new-total] (tufte/p
                                               :update-fractional-distance!
                                               (update-fractional-distance! inputs
                                                                            max-fractionals
                                                                            trajectory
                                                                            fractional-distance-matrix
                                                                            timestep
                                                                            cell))]
    (if (and (>= new-total 1.0)
             (> new-total ^double (tufte/p
                                   :get
                                   (get-in acc [cell :fractional-distance] 0.0))))
      (do (when (and (= trajectory-combination :sum) (> new-total 1.0))
            (update-overflow-heat inputs fractional-distance-matrix trajectory new-total))
          (assoc! acc cell (merge trajectory {:fractional-distance  new-total
                                              :dt-adjusted          (* (/ (- 1.0 old-total) (- new-total old-total))
                                                                       timestep)
                                              :ignition-probability (t/mget fire-spread-matrix i j)})))
      acc)))

;;TODO Optimize Tufte 62%
(defn identify-ignition-events
  [{:keys [trajectory-combination] :as inputs} ignited-cells timestep fire-spread-matrix fractional-distance-matrix]
  (let [timestep        (double timestep)
        max-fractionals (atom {})
        reducer-fn      (fn [acc trajectory]
                          (ignition-event-reducer inputs max-fractionals fractional-distance-matrix
                                                  timestep trajectory-combination fire-spread-matrix
                                                  acc trajectory))
        ignition-events (->> ignited-cells
                             (reduce reducer-fn (transient {}))
                             persistent!
                             vals)]
    (when (= trajectory-combination :sum)
      (update-fractional-distance-matrix! fractional-distance-matrix max-fractionals))
    ignition-events))

;; TODO optimize Tufte 31%
(defn update-ignited-cells
  [{:keys [fuel-model-matrix num-rows num-cols parallel-strategy] :as constants}
   ignited-cells
   ignition-events
   fire-spread-matrix
   global-clock]
  (let [parallel-bin-size    (max 1 (quot (count ignition-events) (.availableProcessors (Runtime/getRuntime))))
        newly-ignited-cells  (into #{} (map :cell) ignition-events)
        pruned-ignited-cells (into [] (remove #(contains? newly-ignited-cells (:cell %))) ignited-cells)
        reducer-fn           (if (= parallel-strategy :within-fires)
                               #(->> (r/fold parallel-bin-size r/cat r/append! %)
                                     (reduce (fn [acc v] (into acc v)) pruned-ignited-cells))
                               #(reduce (fn [acc v] (into acc v)) pruned-ignited-cells %))]
    (->> ignition-events
         (r/map (fn [{:keys [cell trajectory fractional-distance]}]
                  (let [fractional-distance (double fractional-distance)]
                    (when (burnable-neighbors? fire-spread-matrix
                                               fuel-model-matrix
                                               num-rows num-cols
                                               cell)
                      (compute-neighborhood-fire-spread-rates!
                       constants
                       fire-spread-matrix
                       cell
                       trajectory
                       (- fractional-distance 1.0)
                       global-clock)))))
         (r/remove nil?)
         (reducer-fn))))

(defn generate-ignited-cells
  [inputs fire-spread-matrix cells]
  (reduce (fn [ignited-cells cell]
            (into ignited-cells
                  (compute-neighborhood-fire-spread-rates! inputs
                                                           fire-spread-matrix
                                                           cell
                                                           nil
                                                           0.0
                                                           0.0)))
          []
          cells))

(defn identify-spot-ignition-events
  [global-clock spot-ignitions]
  (let [to-ignite-now (group-by (fn [[_ [time _]]]
                                  (let [time (double time)]
                                    (>= ^double global-clock time)))
                                spot-ignitions)
        ignite-later  (into {} (get to-ignite-now false))
        ignite-now    (into {} (get to-ignite-now true))]
    [ignite-later ignite-now]))

(defn spot-ignited-cells
  "Updates matrices for spot ignited cells
  Returns a map of ignited cells"
  [constants
   global-clock
   {:keys [fire-spread-matrix burn-time-matrix spread-rate-matrix fire-type-matrix
           flame-length-matrix fire-line-intensity-matrix spot-matrix]}
   spot-ignite-now]
  (let [ignited?        (fn [[k v]]
                          (let [[i j] k
                                [_ p] v]
                            (> ^double (t/mget fire-spread-matrix i j) ^double p)))
        spot-ignite-now (remove ignited? spot-ignite-now)
        ignited-cells   (generate-ignited-cells constants
                                                fire-spread-matrix
                                                (keys spot-ignite-now))]
    (doseq [cell spot-ignite-now
            :let [[i j]                    (key cell)
                  [_ ignition-probability] (val cell)]]
      (t/mset! fire-spread-matrix i j ignition-probability)
      (t/mset! burn-time-matrix i j global-clock)
      (t/mset! flame-length-matrix i j 1.0)
      (t/mset! fire-line-intensity-matrix i j 1.0)
      (t/mset! spread-rate-matrix i j -1.0)
      (t/mset! fire-type-matrix i j -1.0)
      (t/mset! spot-matrix i j 1.0))
    ignited-cells))

(defn new-spot-ignitions
  "Returns a map of [x y] locations to [t p] where:
  t: time of ignition
  p: ignition-probability"
  [{:keys [spotting] :as inputs} matrices ignition-events global-clock]
  (when spotting
    (reduce (fn [acc ignition-event]
              (merge-with (partial min-key first)
                          acc
                          (->> (spot/spread-firebrands
                                inputs
                                matrices
                                ignition-event
                                global-clock)
                               (into {}))))
            {}
            ignition-events)))

(def fire-type-to-value
  {:surface       1.0
   :passive-crown 2.0
   :active-crown  3.0})

(defn- find-max-spread-rate ^double
  [^double max-spread-rate ^BurnTrajectory ignited-cell]
  (Math/max max-spread-rate ^double (:spread-rate ignited-cell)))

(defn- compute-dt ^double
  [^double cell-size ignited-cells]
  (if (seq ignited-cells)
    (let [max-spread-rate (double (reduce find-max-spread-rate 0.0 ignited-cells))]
      (/ cell-size max-spread-rate))
    10.0))

(defn- count-crown-fire-cells
  [fire-type-matrix]
  (-> fire-type-matrix (dfn/> 1.0) dfn/sum (d/unchecked-cast :int64)))

(defn run-loop
  [{:keys [max-runtime cell-size ignition-start-time initial-ignition-site] :as inputs}
   {:keys
    [fire-spread-matrix
     flame-length-matrix
     fire-line-intensity-matrix
     burn-time-matrix
     spread-rate-matrix
     fire-type-matrix
     fractional-distance-matrix
     spot-matrix] :as matrices}
   perimeter-cells]
  (let [max-runtime         (double max-runtime)
        cell-size           (double cell-size)
        ignition-start-time (double ignition-start-time)
        ignition-stop-time  (+ ignition-start-time max-runtime)]
    (loop [global-clock   ignition-start-time
           ignited-cells  (generate-ignited-cells inputs fire-spread-matrix (or perimeter-cells [initial-ignition-site]))
           spot-ignitions {}
           spot-count     0]
      (if (and (< global-clock ignition-stop-time)
               (or (seq ignited-cells) (seq spot-ignitions)))
        (let [timestep          (tufte/p
                                 :calc-timestep
                                 (Math/min (compute-dt cell-size ignited-cells)
                                           (- ignition-stop-time global-clock)))
              ignition-events   (tufte/p
                                 :identify-ignition-events
                                 (identify-ignition-events inputs ignited-cells timestep
                                                           fire-spread-matrix fractional-distance-matrix))]
          ;; [{:cell :trajectory :fractional-distance
          ;;   :flame-length :fire-line-intensity} ...]
          (doseq [{:keys
                   [cell flame-length fire-line-intensity
                    ignition-probability spread-rate fire-type
                    dt-adjusted]} ignition-events] ;TODO investigate using records for ignition-events
            (let [[i j] cell]
              (t/mset! fire-spread-matrix         i j ignition-probability)
              (t/mset! flame-length-matrix        i j flame-length)
              (t/mset! fire-line-intensity-matrix i j fire-line-intensity)
              (t/mset! burn-time-matrix           i j (+ global-clock ^double dt-adjusted))
              (t/mset! spread-rate-matrix         i j spread-rate)
              (t/mset! fire-type-matrix           i j (fire-type fire-type-to-value)))) ;TODO Use number
          (let [new-spot-ignitions (new-spot-ignitions inputs ;TODO optimize
                                                       matrices
                                                       ignition-events
                                                       global-clock)
                [spot-ignite-later
                 spot-ignite-now]  (identify-spot-ignition-events global-clock ;TODO optimize
                                                                  (merge-with (partial min-key first)
                                                                              spot-ignitions
                                                                              new-spot-ignitions))
                spot-ignited-cells (spot-ignited-cells inputs ;TODO optimize
                                                       global-clock
                                                       matrices
                                                       spot-ignite-now)]
            (recur (+ global-clock timestep)
                   (tufte/p
                    :update-ignited-cells
                    (update-ignited-cells inputs
                                          (into spot-ignited-cells ignited-cells)
                                          ignition-events
                                          fire-spread-matrix
                                          global-clock))
                   spot-ignite-later
                   (+ spot-count (count spot-ignited-cells)))))
        {:global-clock               global-clock
         :exit-condition             (if (seq ignited-cells) :max-runtime-reached :no-burnable-fuels)
         :fire-spread-matrix         fire-spread-matrix
         :flame-length-matrix        flame-length-matrix
         :fire-line-intensity-matrix fire-line-intensity-matrix
         :burn-time-matrix           burn-time-matrix
         :spot-matrix                spot-matrix
         :spread-rate-matrix         spread-rate-matrix
         :fire-type-matrix           fire-type-matrix
         :crown-fire-count           (count-crown-fire-cells fire-type-matrix)
         :spot-count                 spot-count}))))

(defmulti run-fire-spread
  "Runs the raster-based fire spread model with a map of these arguments:
  - max-runtime: double (minutes)
  - cell-size: double (feet)
  - elevation-matrix: core.matrix 2D double array (feet)
  - slope-matrix: core.matrix 2D double array (vertical feet/horizontal feet)
  - aspect-matrix: core.matrix 2D double array (degrees clockwise from north)
  - fuel-model-matrix: core.matrix 2D double array (fuel model numbers 1-256)
  - canopy-height-matrix: core.matrix 2D double array (feet)
  - canopy-base-height-matrix: core.matrix 2D double array (feet)
  - crown-bulk-density-matrix: core.matrix 2D double array (lb/ft^3)
  - canopy-cover-matrix: core.matrix 2D double array (0-100)
  - wind-speed-20ft: double (miles/hour)
  - wind-from-direction: double (degrees clockwise from north)
  - fuel-moisture: doubles (0-1) {:dead {:1hr :10hr :100hr} :live {:herbaceous :woody}}
  - foliar-moisture: double (0-1)
  - ellipse-adjustment-factor: (< 1.0 = more circular, > 1.0 = more elliptical)
  - initial-ignition-site: One of the following:
     - point represented as [row col]
     - a core.matrix 2D double array (0-2)
  - num-rows: integer
  - num-cols: integer"
  (fn [{:keys [initial-ignition-site]}]
    (if (vector? initial-ignition-site)
      :ignition-point
      :ignition-perimeter)))

(defn- initialize-point-ignition-matrices
  [{:keys [num-rows num-cols initial-ignition-site ignition-start-time spotting trajectory-combination]}]
  (let [[i j]                      initial-ignition-site
        shape                      [num-rows num-cols]
        burn-time-matrix           (t/new-tensor shape)
        fire-line-intensity-matrix (t/new-tensor shape)
        fire-spread-matrix         (t/new-tensor shape)
        fire-type-matrix           (t/new-tensor shape)
        firebrand-count-matrix     (when spotting (t/new-tensor shape))
        flame-length-matrix        (t/new-tensor shape)
        fractional-distance-matrix (when (= trajectory-combination :sum) (t/new-tensor shape))
        spot-matrix                (t/new-tensor shape) ;;TODO check if spot-matrix requires spotting
        spread-rate-matrix         (t/new-tensor shape)]
    (t/mset! burn-time-matrix i j ignition-start-time)
    (t/mset! fire-line-intensity-matrix i j 1.0)       ;TODO should this be zero?
    (t/mset! fire-spread-matrix i j 1.0)
    (t/mset! fire-type-matrix i j -1.0)                ;TODO should this be zero?
    (t/mset! flame-length-matrix i j 1.0)              ;TODO should this be zero?
    (t/mset! spread-rate-matrix i j -1.0)              ;TODO should this be zero?
    {:burn-time-matrix           burn-time-matrix
     :fire-line-intensity-matrix fire-line-intensity-matrix
     :fire-spread-matrix         fire-spread-matrix
     :fire-type-matrix           fire-type-matrix
     :firebrand-count-matrix     firebrand-count-matrix
     :flame-length-matrix        flame-length-matrix
     :fractional-distance-matrix fractional-distance-matrix
     :spot-matrix                spot-matrix
     :spread-rate-matrix         spread-rate-matrix}))

(defmethod run-fire-spread :ignition-point
  [inputs]
  (run-loop inputs (initialize-point-ignition-matrices inputs) nil))

(defn- initialize-matrix
  [shape indices]
  (let [matrix (t/new-tensor shape)]
    (doseq [[i j] indices] ;TODO can we optimize this loop with compute-tensor?
      (t/mset! matrix i j -1.0))
    matrix))

(defn- initialize-perimeter-ignition-matrices
  [{:keys [num-rows num-cols spotting trajectory-combination initial-ignition-site]} non-zero-indices]
  (let [shape                      [num-rows num-cols]
        burn-time-matrix           (initialize-matrix shape non-zero-indices)
        fire-line-intensity-matrix (initialize-matrix shape non-zero-indices)
        fire-spread-matrix         (d/clone initial-ignition-site)
        fire-type-matrix           (initialize-matrix shape non-zero-indices)
        firebrand-count-matrix     (when spotting (t/new-tensor shape))
        flame-length-matrix        (initialize-matrix shape non-zero-indices)
        fractional-distance-matrix (when (= trajectory-combination :sum)
                                     (initialize-matrix shape non-zero-indices))
        spot-matrix                (t/new-tensor shape) ;TODO check if spot-matrix requires spotting
        spread-rate-matrix         (initialize-matrix shape non-zero-indices)]
    {:burn-time-matrix           burn-time-matrix
     :fire-line-intensity-matrix fire-line-intensity-matrix
     :fire-spread-matrix         fire-spread-matrix
     :fire-type-matrix           fire-type-matrix
     :firebrand-count-matrix     firebrand-count-matrix
     :flame-length-matrix        flame-length-matrix
     :fractional-distance-matrix fractional-distance-matrix
     :spot-matrix                spot-matrix
     :spread-rate-matrix         spread-rate-matrix}))

(defn- get-non-zero-indices [m]
  (let [{:keys [row-idxs col-idxs]} (non-zero-indices m)]
    (map vector row-idxs col-idxs)))

(defmethod run-fire-spread :ignition-perimeter
  [{:keys [num-rows num-cols initial-ignition-site fuel-model-matrix] :as inputs}]
  (let [non-zero-indices (get-non-zero-indices initial-ignition-site)
        perimeter-cells  (filter #(burnable-neighbors? initial-ignition-site
                                                       fuel-model-matrix
                                                       num-rows
                                                       num-cols
                                                       %)
                                 non-zero-indices)]
    (when (seq perimeter-cells)
      (let [matrices (initialize-perimeter-ignition-matrices inputs non-zero-indices)]
        (run-loop inputs matrices perimeter-cells)))))
;; fire-spread-algorithm ends here

(comment

  (def lot (t/new-tensor [10 10] :datatype :byte))

  (defn get-n-s? [i j]
    (-> (t/mget lot i j)
        (bit-test 3)))

  (defn get-ne-sw? [i j]
    (-> (t/mget lot i j)
        (bit-test 2)))

  (defn get-e-w? [i j]
    (-> (t/mget lot i j)
        (bit-test 1)))

  (defn get-se-nw? [i j]
    (-> (t/mget lot i j)
        (bit-test 0)))

  (defn set-n-s! [i j]
    (as-> (t/mget lot i j) %
      (bit-set % 3)
      (t/mset! lot i j %)))

  (defn set-ne-sw! [i j]
    (as-> (t/mget lot i j) %
      (bit-set % 2)
      (t/mset! lot i j %)))

  (defn set-e-w! [i j]
    (as-> (t/mget lot i j) %
      (bit-set % 1)
      (t/mset! lot i j %)))

  (defn set-se-nw! [i j]
    (as-> (t/mget lot i j) %
      (bit-set % 0)
      (t/mset! lot i j %)))


  (def lot-map (into {} (for [i (range 10) j (range 10)] [[i j] 2r0000])))

  (defn get-n-s-map? [i j]
    (-> (get lot-map [i j])
        (bit-test 3)))

  (defn set-n-s-map! [i j]
    (as-> (get lot-map [i j]) %
      (bit-set % 3)
      (assoc lot-map [i j] %)))

  (def lot-atom-map (into {} (for [i (range 10) j (range 10)] [[i j] (atom 2r0000)])))

  (defn get-n-s-atom-map? [i j]
    (-> (get lot-atom-map [i j])
        (deref)
        (bit-test 3)))

  (defn set-n-s-atom-map! [i j]
    (as-> (get lot-atom-map [i j]) %
      (swap! % bit-set 3)))

  (definterface ABurnTrajectory
    (^long getI [])
    (^long getJ [])
    (^byte getDirection [])
    (^double getSpreadRate [])
    (^double getTerrainDistance [])
    (^double getFractionalDistance [])
    (^double getBurnProbability [])
    (^void setI [^long i])
    (^void setJ [^long j])
    (^void setDirection [^byte direction])
    (^void setSpreadRate [^double spread-rate])
    (^void setTerrainDistance [^double terrain-distance])
    (^void setFractionalDistance [^double fractional-distance])
    (^void setBurnProbability [^double burn-probability]))

  (deftype MutableBurnTrajectory
      [^:volatile-mutable ^long  i
       ^:volatile-mutable ^long  j
       ^:volatile-mutable ^byte  direction
       ^:volatile-mutable ^double spread-rate
       ^:volatile-mutable ^double terrain-distance
       ^:volatile-mutable ^double fractional-distance
       ^:volatile-mutable ^double burn-probability]
      ABurnTrajectory
      (^long getI [this] i)
      (^long getJ [this] j)
      (^byte getDirection [this] direction)
      (^double getSpreadRate [this] spread-rate)
      (^double getTerrainDistance [this] terrain-distance)
      (^double getFractionalDistance [this] fractional-distance)
      (^double getBurnProbability [this] burn-probability)
      (^void setI [this ^long new-i] (set! i new-i))
      (^void setJ [this ^long new-j] (set! j new-j))
      (^void setDirection [this ^byte new-direction] (set! direction new-direction))
      (^void setSpreadRate [this ^double new-spread-rate] (set! spread-rate new-spread-rate))
      (^void setTerrainDistance [this ^double new-terrain-distance] (set! terrain-distance new-terrain-distance))
      (^void setFractionalDistance [this ^double new-fractional-distance] (set! fractional-distance new-fractional-distance))
      (^void setBurnProbability [this ^double new-burn-probability] (set! burn-probability new-burn-probability)))

    (deftype MutableBurnTrajectoryNew
      [^:unsynchronized-mutable ^long  i
       ^:unsynchronized-mutable ^long  j
       ^:unsynchronized-mutable ^byte  direction
       ^:unsynchronized-mutable ^double spread-rate
       ^:unsynchronized-mutable ^double terrain-distance
       ^:unsynchronized-mutable ^double fractional-distance
       ^:unsynchronized-mutable ^double burn-probability]
    ABurnTrajectory
    (^long getI [this] i)
    (^long getJ [this] j)
    (^byte getDirection [this] direction)
    (^double getSpreadRate [this] spread-rate)
    (^double getTerrainDistance [this] terrain-distance)
    (^double getFractionalDistance [this] fractional-distance)
    (^double getBurnProbability [this] burn-probability)
    (^void setI [this ^long new-i] (set! i new-i))
    (^void setJ [this ^long new-j] (set! j new-j))
    (^void setDirection [this ^byte new-direction] (set! direction new-direction))
    (^void setSpreadRate [this ^double new-spread-rate] (set! spread-rate new-spread-rate))
    (^void setTerrainDistance [this ^double new-terrain-distance] (set! terrain-distance new-terrain-distance))
    (^void setFractionalDistance [this ^double new-fractional-distance] (set! fractional-distance new-fractional-distance))
    (^void setBurnProbability [this ^double new-burn-probability] (set! burn-probability new-burn-probability)))

  )
