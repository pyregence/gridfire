;; [[file:../../org/GridFire.org::fire-spread-algorithm][fire-spread-algorithm]]
(ns gridfire.fire-spread
  (:require [clojure.core.matrix           :as m]
            [clojure.core.reducers         :as r]
            [gridfire.common               :refer [burnable-fuel-model?
                                                   burnable?
                                                   calc-fuel-moisture
                                                   in-bounds?
                                                   burnable-neighbors?
                                                   get-neighbors
                                                   distance-3d]]
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
            [gridfire.utils.random        :as random])
  (:import java.util.Random))

(m/set-current-implementation :vectorz)

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
  [fuel-model-number fuel-moisture]
  (let [fuel-model      (-> (build-fuel-model (int fuel-model-number))
                            (moisturize fuel-moisture))
        spread-info-min (rothermel-surface-fire-spread-no-wind-no-slope fuel-model)]
    [fuel-model spread-info-min]))

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
                                     (-> fuel-model :h :dead :1hr)))
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
     get-fuel-moisture-live-woody]}
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
        [fuel-model spread-info-min]          (rothermel-fast-wrapper fuel-model
                                                                      {:dead {:1hr   fuel-moisture-dead-1hr
                                                                              :10hr  fuel-moisture-dead-10hr
                                                                              :100hr fuel-moisture-dead-100hr}
                                                                       :live {:herbaceous fuel-moisture-live-herbaceous
                                                                              :woody      fuel-moisture-live-woody}})
        midflame-wind-speed                   (mph->fpm
                                               (* wind-speed-20ft
                                                  (wind-adjustment-factor ^long (:delta fuel-model)
                                                                          canopy-height
                                                                          canopy-cover)))
        spread-info-max                       (rothermel-surface-fire-spread-max spread-info-min
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
    (m/mget fractional-distance-matrix i j)
    @fractional-distance))

(defn- update-fractional-distance-matrix!
  "Update the fractional distance matrix with the largest fractional distance calculated."
  [fractional-distance-matrix max-fractionals]
  (doseq [[cell fractional-distance] @max-fractionals]
    (let [[i j] cell]
      (m/mset! fractional-distance-matrix i j fractional-distance))))

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
      (m/mset! fractional-distance-matrix i j (- fractional-distance 1.0)))))

(defn ignition-event-reducer
  [inputs max-fractionals fractional-distance-matrix timestep trajectory-combination fire-spread-matrix
   acc trajectory]
  (let [{:keys [source cell]}                 trajectory
        [i j]                                 source
        [^double old-total ^double new-total] (update-fractional-distance! inputs
                                                                           max-fractionals
                                                                           trajectory
                                                                           fractional-distance-matrix
                                                                           timestep
                                                                           cell)]
    (if (and (>= new-total 1.0)
             (> new-total ^double (get-in acc [cell :fractional-distance] 0.0)))
      (do (when (and (= trajectory-combination :sum) (> new-total 1.0))
            (update-overflow-heat inputs fractional-distance-matrix trajectory new-total))
          (assoc! acc cell (merge trajectory {:fractional-distance  new-total
                                              :dt-adjusted          (* (/ (- 1.0 old-total) (- new-total old-total))
                                                                       timestep)
                                              :ignition-probability (m/mget fire-spread-matrix i j)})))
      acc)))

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
  [constants fire-spread-matrix cells]
  (when (seq cells)
    (reduce (fn [ignited-cells cell]
              (into ignited-cells
                    (compute-neighborhood-fire-spread-rates! constants
                                                             fire-spread-matrix
                                                             cell
                                                             nil
                                                             0.0
                                                             0.0)))
            []
            cells)))

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
                            (> ^double (m/mget fire-spread-matrix i j) ^double p)))
        spot-ignite-now (remove ignited? spot-ignite-now)
        ignited-cells   (generate-ignited-cells constants
                                                fire-spread-matrix
                                                (keys spot-ignite-now))]
    (doseq [cell spot-ignite-now
            :let [[i j]                    (key cell)
                  [_ ignition-probability] (val cell)]]
      (m/mset! fire-spread-matrix i j ignition-probability)
      (m/mset! burn-time-matrix i j global-clock)
      (m/mset! flame-length-matrix i j 1.0)
      (m/mset! fire-line-intensity-matrix i j 1.0)
      (m/mset! spread-rate-matrix i j -1.0)
      (m/mset! fire-type-matrix i j -1.0)
      (m/mset! spot-matrix i j 1.0))
    ignited-cells))

(defn new-spot-ignitions
  "Returns a map of [x y] locations to [t p] where:
  t: time of ignition
  p: ignition-probability"
  [{:keys [spotting] :as inputs} matrices ignition-events]
  (when spotting
    (reduce (fn [acc ignition-event]
              (merge-with (partial min-key first)
                          acc
                          (->> (spot/spread-firebrands
                                inputs
                                matrices
                                ignition-event)
                               (into {}))))
            {}
            ignition-events)))

(def fire-type-to-value
  {:surface       1.0
   :passive-crown 2.0
   :active-crown  3.0})

(defn- reducer-fn ^double
  [^double max-spread-rate ignited-cell]
  (Math/max max-spread-rate (double (:spread-rate ignited-cell))))

(defn run-loop
  [{:keys [max-runtime cell-size ignition-start-time] :as inputs}
   {:keys
    [fire-spread-matrix
     flame-length-matrix
     fire-line-intensity-matrix
     burn-time-matrix
     spread-rate-matrix
     fire-type-matrix
     fractional-distance-matrix
     spot-matrix] :as matrices}
   ignited-cells]
  (let [max-runtime        (double max-runtime)
        cell-size          (double cell-size)
        crown-fire-count   (atom 0)
        spot-count         (atom 0)
        ignition-stop-time (+ ignition-start-time max-runtime)]
    (loop [global-clock   ignition-start-time
           ignited-cells  ignited-cells
           spot-ignitions {}]
      ;; FIXME: Combine ignited cells and spot ignitions
      (if (and (< global-clock ignition-stop-time)
               (or (seq ignited-cells) (seq spot-ignitions)))
        (let [dt                (if (seq ignited-cells)
                                  (->> ignited-cells
                                       (reduce reducer-fn 0.0)
                                       (/ cell-size)
                                       double)
                                  10.0)
              timestep          (min dt (- ignition-stop-time global-clock))
              next-global-clock (+ global-clock timestep)
              ignition-events   (identify-ignition-events inputs ignited-cells timestep
                                                          fire-spread-matrix fractional-distance-matrix)]
          ;; [{:cell :trajectory :fractional-distance
          ;;   :flame-length :fire-line-intensity} ...]
          (doseq [{:keys
                   [cell flame-length fire-line-intensity
                    ignition-probability spread-rate fire-type
                    dt-adjusted crown-fire?]} ignition-events]
            (let [[i j]       cell
                  dt-adjusted (double dt-adjusted)]
              (when crown-fire? (swap! crown-fire-count inc))
              (m/mset! fire-spread-matrix         i j ignition-probability)
              (m/mset! flame-length-matrix        i j flame-length)
              (m/mset! fire-line-intensity-matrix i j fire-line-intensity)
              (m/mset! burn-time-matrix           i j (+ global-clock dt-adjusted))
              (m/mset! spread-rate-matrix         i j spread-rate)
              (m/mset! fire-type-matrix           i j (fire-type fire-type-to-value))))
          (let [new-spot-ignitions (new-spot-ignitions (assoc inputs :global-clock global-clock)
                                                       matrices
                                                       ignition-events)
                [spot-ignite-later
                 spot-ignite-now]  (identify-spot-ignition-events global-clock
                                                                  (merge-with (partial min-key first)
                                                                              spot-ignitions
                                                                              new-spot-ignitions))
                spot-ignited-cells (spot-ignited-cells inputs
                                                       global-clock
                                                       matrices
                                                       spot-ignite-now)]
            (reset! spot-count (+ @spot-count (count spot-ignited-cells)))
            (recur next-global-clock
                   (update-ignited-cells inputs
                                         (into spot-ignited-cells ignited-cells)
                                         ignition-events
                                         fire-spread-matrix
                                         global-clock)
                   spot-ignite-later)))
        {:global-clock               global-clock
         :exit-condition             (if (seq ignited-cells) :max-runtime-reached :no-burnable-fuels)
         :fire-spread-matrix         fire-spread-matrix
         :flame-length-matrix        flame-length-matrix
         :fire-line-intensity-matrix fire-line-intensity-matrix
         :burn-time-matrix           burn-time-matrix
         :spot-matrix                spot-matrix
         :spread-rate-matrix         spread-rate-matrix
         :fire-type-matrix           fire-type-matrix
         :crown-fire-count           @crown-fire-count
         :spot-count                 @spot-count}))))

(defn- initialize-matrix
  [num-rows num-cols indices]
  (let [matrix (m/zero-matrix num-rows num-cols)]
    (doseq [[i j] indices
            :when (in-bounds? num-rows num-cols [i j])]
      (m/mset! matrix i j -1.0))
    matrix))

(defn- get-non-zero-indices [m]
  (for [[r cols] (map-indexed vector (m/non-zero-indices m))
        c        cols]
    [r c]))

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
     - map containing a :matrix field of type core.matrix 2D double array (0-2)
     - nil (this causes GridFire to select a random ignition-point)
  - num-rows: integer
  - num-cols: integer"
  (fn [{:keys [initial-ignition-site]}]
    (condp = (type initial-ignition-site)
      clojure.lang.PersistentHashMap :ignition-perimeter
      clojure.lang.PersistentVector  :ignition-point)))

(defmethod run-fire-spread :ignition-point
  [{:keys [num-rows num-cols initial-ignition-site spotting trajectory-combination] :as inputs}]
  (let [[i j]                      initial-ignition-site
        fire-spread-matrix         (m/zero-matrix num-rows num-cols)
        flame-length-matrix        (m/zero-matrix num-rows num-cols)
        fire-line-intensity-matrix (m/zero-matrix num-rows num-cols)
        burn-time-matrix           (m/zero-matrix num-rows num-cols)
        firebrand-count-matrix     (when spotting (m/zero-matrix num-rows num-cols))
        spread-rate-matrix         (m/zero-matrix num-rows num-cols)
        fire-type-matrix           (m/zero-matrix num-rows num-cols)
        spot-matrix                (m/zero-matrix num-rows num-cols)
        fractional-distance-matrix (when (= trajectory-combination :sum) (m/zero-matrix num-rows num-cols))]
    (m/mset! fire-spread-matrix i j 1.0)
    (m/mset! flame-length-matrix i j 1.0)
    (m/mset! fire-line-intensity-matrix i j 1.0)
    (m/mset! burn-time-matrix i j -1.0)
    (m/mset! spread-rate-matrix i j -1.0)
    (m/mset! fire-type-matrix i j -1.0)
    (let [ignited-cells (compute-neighborhood-fire-spread-rates!
                         inputs
                         fire-spread-matrix
                         initial-ignition-site
                         nil
                         0.0
                         0.0)]
      (run-loop inputs
                {:fire-spread-matrix         fire-spread-matrix
                 :spread-rate-matrix         spread-rate-matrix
                 :flame-length-matrix        flame-length-matrix
                 :fire-line-intensity-matrix fire-line-intensity-matrix
                 :firebrand-count-matrix     firebrand-count-matrix
                 :burn-time-matrix           burn-time-matrix
                 :fire-type-matrix           fire-type-matrix
                 :fractional-distance-matrix fractional-distance-matrix
                 :spot-matrix                spot-matrix}
                ignited-cells))))

(defmethod run-fire-spread :ignition-perimeter
  [{:keys [num-rows num-cols initial-ignition-site fuel-model-matrix spotting trajectory-combination] :as inputs}]
  (let [fire-spread-matrix (first (m/mutable (:matrix initial-ignition-site)))
        non-zero-indices   (get-non-zero-indices fire-spread-matrix)
        perimeter-indices  (filter #(burnable-neighbors? fire-spread-matrix
                                                         fuel-model-matrix
                                                         num-rows
                                                         num-cols
                                                         %)
                                   non-zero-indices)]
    (when (seq perimeter-indices)
      (let [flame-length-matrix        (initialize-matrix num-rows num-cols non-zero-indices)
            fire-line-intensity-matrix (initialize-matrix num-rows num-cols non-zero-indices)
            burn-time-matrix           (initialize-matrix num-rows num-cols non-zero-indices)
            firebrand-count-matrix     (when spotting (m/zero-matrix num-rows num-cols))
            spread-rate-matrix         (initialize-matrix num-rows num-cols non-zero-indices)
            fire-type-matrix           (initialize-matrix num-rows num-cols non-zero-indices)
            fractional-distance-matrix (when (= trajectory-combination :sum)
                                         (initialize-matrix num-rows num-cols non-zero-indices))
            spot-matrix                (m/zero-matrix num-rows num-cols)
            ignited-cells              (generate-ignited-cells inputs fire-spread-matrix perimeter-indices)]
        (when (seq ignited-cells)
          (run-loop inputs
                    {:fire-spread-matrix         fire-spread-matrix
                     :spread-rate-matrix         spread-rate-matrix
                     :flame-length-matrix        flame-length-matrix
                     :fire-line-intensity-matrix fire-line-intensity-matrix
                     :firebrand-count-matrix     firebrand-count-matrix
                     :burn-time-matrix           burn-time-matrix
                     :fire-type-matrix           fire-type-matrix
                     :fractional-distance-matrix fractional-distance-matrix
                     :spot-matrix                spot-matrix}
                    ignited-cells))))))
;; fire-spread-algorithm ends here
