(ns gridfire.fire-spread-optimal
  (:require [clojure.core.matrix :as m]
            [gridfire.common :refer [burnable-fuel-model?
                                     burnable-neighbors?
                                     burnable?
                                     distance-3d
                                     fuel-moisture-from-raster
                                     get-fuel-moisture
                                     get-neighbors
                                     get-value-at
                                     in-bounds?
                                     sample-at]]
            [gridfire.crown-fire :refer [crown-fire-eccentricity
                                         crown-fire-line-intensity
                                         cruz-crown-fire-spread
                                         van-wagner-crown-fire-initiation?]]
            [gridfire.fuel-models :refer [build-fuel-model moisturize]]
            [gridfire.surface-fire :refer [anderson-flame-depth
                                           byram-fire-line-intensity
                                           byram-flame-length
                                           rothermel-surface-fire-spread-any
                                           rothermel-surface-fire-spread-max
                                           rothermel-surface-fire-spread-no-wind-no-slope
                                           wind-adjustment-factor]]
            [gridfire.utils.primitive :refer [double-reduce]]
            [gridfire.utils.random :as random]))

(def fire-type-to-value
  {:surface       1.0
   :passive-crown 2.0
   :active-crown  3.0})

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

(def degrees-to-offset
  "Returns clockwise degrees from north."
  {0.0   [-1  0]   ; N
   45.0  [-1  1]   ; NE
   90.0  [ 0  1]   ; E
   135.0 [ 1  1]   ; SE
   180.0 [ 1  0]   ; S
   225.0 [ 1 -1]   ; SW
   270.0 [ 0 -1]   ; W
   315.0 [-1 -1]}) ; NW


(def rothermel-fast-wrapper
  (memoize
   (fn [fuel-model-number fuel-moisture]
     (let [fuel-model      (-> (build-fuel-model (int fuel-model-number))
                               (moisturize fuel-moisture))
           spread-info-min (rothermel-surface-fire-spread-no-wind-no-slope fuel-model)]
       [fuel-model spread-info-min]))))

(defn- compute-burn-trajectory!
  "Return target cell"
  [{:keys [fire-probability-matrix max-distance-matrix
           max-rate-matrix max-direction-matrix max-probability-matrix]}
   spread-info-min spread-info-max wind-speed-20ft ellipse-adjustment-factor
   canopy-cover canopy-base-height foliar-moisture crown-spread-max
   landfire-rasters cell-size timestep [x y :as source] [i j :as target]]
  (let [trajectory                (mapv - target source)
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
        crown-eccentricity        (crown-fire-eccentricity wind-speed-20ft
                                                           ellipse-adjustment-factor)
        ^double crown-spread-rate (when crown-fire?
                                    (rothermel-surface-fire-spread-any
                                     (assoc spread-info-max
                                            :max-spread-rate crown-spread-max
                                            :eccentricity crown-eccentricity)
                                     spread-direction))
        spread-rate               (if crown-fire?
                                    (max surface-spread-rate crown-spread-rate)
                                    surface-spread-rate)
        terrain-distance          (distance-3d (:elevation landfire-rasters) cell-size source target) ;TODO Precalculate this in load-inputs phase
        spread-distance           (/ (* spread-rate timestep) terrain-distance)]
    (when (>= spread-distance (m/mget max-distance-matrix i j))
      (m/mset! max-distance-matrix i j spread-distance)
      (m/mset! max-direction-matrix i j spread-direction)
      (m/mset! max-rate-matrix i j spread-rate)
      (let [source-probability (m/mget fire-probability-matrix x y)]
        (m/mset! max-probability-matrix i j (if (= -1.0 source-probability) 1.0 source-probability))))
    target))

(defn- compute-fire-behavior-values
  [{:keys [spread-rate-matrix fire-line-intensity-matrix flame-length-matrix fire-type-matrix]}
   {:keys [residence-time reaction-intensity]} spread-info-max canopy-cover canopy-base-height foliar-moisture
   crown-bulk-density canopy-height fuel-model crown-type crown-spread-max [i j]]
  (let [surface-intensity       (->> (anderson-flame-depth (:max-spread-rate spread-info-max) residence-time)
                                     (byram-fire-line-intensity reaction-intensity))
        crown-fire?             (van-wagner-crown-fire-initiation? canopy-cover
                                                                   canopy-base-height
                                                                   foliar-moisture
                                                                   surface-intensity)
        ^double crown-intensity (when crown-fire?
                                  (crown-fire-line-intensity
                                   crown-spread-max
                                   crown-bulk-density
                                   canopy-height
                                   canopy-base-height
                                   (-> fuel-model :h :dead :1hr)))
        fire-line-intensity     (if crown-fire?
                                  (+ surface-intensity crown-intensity)
                                  surface-intensity)
        flame-length            (byram-flame-length fire-line-intensity)
        fire-type               (if crown-fire? (fire-type-to-value crown-type) 1.0)]

    (m/mset! spread-rate-matrix i j (:max-spread-rate spread-info-max))
    (m/mset! fire-line-intensity-matrix i j fire-line-intensity)
    (m/mset! flame-length-matrix i j flame-length)
    (m/mset! fire-type-matrix i j fire-type)))

(defn- compute-neighborhood-trajectories!
  [{:keys
    [landfire-rasters multiplier-lookup perturbations wind-speed-20ft wind-from-direction
     temperature relative-humidity foliar-moisture ellipse-adjustment-factor cell-size num-rows
     num-cols] :as constants}
   {:keys [fire-probability-matrix fire-line-intensity-matrix] :as matrices}
   target-cells
   global-clock
   timestep
   [i j :as here]]
  (let [^double aspect                (sample-at here
                                                 global-clock
                                                 (:aspect landfire-rasters)
                                                 (:aspect multiplier-lookup)
                                                 (:aspect perturbations))
        ^double canopy-base-height    (sample-at here
                                                 global-clock
                                                 (:canopy-base-height landfire-rasters)
                                                 (:canopy-base-height multiplier-lookup)
                                                 (:canopy-base-height perturbations))
        ^double canopy-cover          (sample-at here
                                                 global-clock
                                                 (:canopy-cover landfire-rasters)
                                                 (:canopy-cover multiplier-lookup)
                                                 (:canopy-cover perturbations))
        ^double canopy-height         (sample-at here
                                                 global-clock
                                                 (:canopy-height landfire-rasters)
                                                 (:canopy-height multiplier-lookup)
                                                 (:canopy-height perturbations))
        ^double crown-bulk-density    (sample-at here
                                                 global-clock
                                                 (:crown-bulk-density landfire-rasters)
                                                 (:crown-bulk-density multiplier-lookup)
                                                 (:crown-bulk-density perturbations))
        ^long fuel-model              (sample-at here
                                                 global-clock
                                                 (:fuel-model landfire-rasters)
                                                 (:fuel-model multiplier-lookup)
                                                 (:fuel-model perturbations))
        ^double slope                 (sample-at here
                                                 global-clock
                                                 (:slope landfire-rasters)
                                                 (:slope multiplier-lookup)
                                                 (:slope perturbations))
        ^double relative-humidity     (get-value-at here
                                                    global-clock
                                                    relative-humidity
                                                    (:relative-humidity multiplier-lookup)
                                                    (:relative-humidity perturbations))
        ^double temperature           (get-value-at here
                                                    global-clock
                                                    temperature
                                                    (:temperature multiplier-lookup)
                                                    (:temperature perturbations))
        ^double wind-from-direction   (get-value-at here
                                                    global-clock
                                                    wind-from-direction
                                                    (:wind-from-direction multiplier-lookup)
                                                    (:wind-from-direction perturbations))
        ^double wind-speed-20ft       (get-value-at here
                                                    global-clock
                                                    wind-speed-20ft
                                                    (:wind-speed-20ft multiplier-lookup)
                                                    (:wind-speed-20ft perturbations))
        ^double fuel-moisture         (or (fuel-moisture-from-raster constants here global-clock)
                                          (get-fuel-moisture relative-humidity temperature))
        [fuel-model spread-info-min]  (rothermel-fast-wrapper fuel-model fuel-moisture)
        waf                           (wind-adjustment-factor ^long (:delta fuel-model)
                                                              canopy-height
                                                              canopy-cover)
        midflame-wind-speed           (* wind-speed-20ft 88.0 waf) ; mi/hr -> ft/min
        spread-info-max               (rothermel-surface-fire-spread-max spread-info-min
                                                                         midflame-wind-speed
                                                                         wind-from-direction
                                                                         slope
                                                                         aspect
                                                                         ellipse-adjustment-factor)
        [crown-type crown-spread-max] (cruz-crown-fire-spread wind-speed-20ft crown-bulk-density
                                                              (-> fuel-moisture :dead :1hr))]
    (when (<= (m/mget fire-line-intensity-matrix i j) 0.0)
      (compute-fire-behavior-values matrices spread-info-min spread-info-max canopy-cover canopy-base-height
                                    foliar-moisture crown-bulk-density canopy-height fuel-model crown-type
                                    crown-spread-max here))
    ;; (compute-fire-behavior-values matrices spread-info-min spread-info-max canopy-cover canopy-base-height
    ;;                               foliar-moisture crown-bulk-density canopy-height fuel-model crown-type
    ;;                               crown-spread-max here)
    (into #{}
          (comp
           (filter #(and (in-bounds? num-rows num-cols %)
                         (burnable? fire-probability-matrix (:fuel-model landfire-rasters) here %)))
           (map #(compute-burn-trajectory! matrices spread-info-min spread-info-max wind-speed-20ft
                                           ellipse-adjustment-factor canopy-cover canopy-base-height
                                           foliar-moisture crown-spread-max landfire-rasters cell-size
                                           timestep here %))
           (remove #(contains? target-cells %)))
          (get-neighbors here))))

(defn- update-overflow-targets
  [{:keys [num-rows num-cols]}
   {:keys [max-direction-matrix max-probability-matrix max-distance-matrix total-distance-matrix]}
   [x y] overflow-targets overflow-direction fractional-distance]
  (let [overflow-count (count overflow-targets)]
    (doseq [n     (range overflow-count)
            :let  [[i j :as overflow-target] (nth overflow-targets n)
                   overflow-heat  (if (= i (dec overflow-count)) (rem fractional-distance 1.0) 1.0)]
            :when (in-bounds? num-rows num-cols overflow-target)]
      (when  (> overflow-heat (m/mget max-distance-matrix i j))
        (m/mset! max-distance-matrix i j overflow-heat)
        (m/mset! max-direction-matrix i j overflow-direction)
        (m/mset! max-probability-matrix i j (m/mget max-probability-matrix x y)))
      (when (= (m/mget total-distance-matrix i j) (m/mget max-distance-matrix i j) 0.0)
        overflow-target))))

(defn- identify-overflow-targets
  [inputs
   {:keys [max-direction-matrix max-distance-matrix total-distance-matrix] :as matrices}
   source-cells]
  (loop [results                   #{}
         [[x y :as source] & rest] source-cells]
    (if source
      (let [total-distance (m/mget total-distance-matrix x y)
            max-distance   (m/mget max-distance-matrix x y)
            new-total      (+ total-distance max-distance)]
        (if (> new-total 1.0)
          (let [overflow-count     (int (Math/floor (- new-total 1.0)))
                overflow-direction (m/mget max-direction-matrix x y)
                overflow-offset    (degrees-to-offset overflow-direction)
                overflow-targets   (reductions #(mapv + %1 %2) source (repeat overflow-count overflow-offset))]
            (update-overflow-targets inputs matrices source overflow-targets overflow-direction new-total)
            (recur (into results (filterv (fn [[i j]]
                                            (= (m/mget total-distance-matrix i j)
                                               (m/mget max-distance-matrix i j)
                                               0.0))
                                          overflow-targets))
                   rest))
          (recur results rest)))
      results)))

(defn- identify-ignitions
  [{:keys [total-distance-matrix max-distance-matrix burn-time-matrix fire-probability-matrix
           max-probability-matrix]}
   global-clock
   timestep
   target-cells]
  (for [[i j :as target] target-cells
        :let             [total-distance (m/mget total-distance-matrix i j)
                          max-distance   (m/mget max-distance-matrix i j)
                          new-total      (+ total-distance max-distance)
                          _              (m/mset! total-distance-matrix i j new-total)
                          _              (m/mset! max-distance-matrix i j 0.0)]
        :when            (>= new-total 1.0)]
    (let [dt-ignition (* (/ (- 1.0 total-distance) max-distance) timestep)]
      (m/mset! burn-time-matrix i j (+ global-clock dt-ignition))
      (m/mset! fire-probability-matrix i j (m/mget max-probability-matrix i j))
      target)))

(defn- reducer-fn ^double
  [spread-rate-matrix ^double max-spread-rate [i j]]
  (Math/max max-spread-rate (double (m/mget spread-rate-matrix i j))))

(defn- calc-dt ^double
  [spread-rate-matrix cell-size source-cells]
  (->> source-cells
       (double-reduce (partial reducer-fn spread-rate-matrix) 0.0)
       (/ cell-size)
       double))

(defn- run-loop
  [{:keys [max-runtime cell-size landfire-rasters num-rows num-cols] :as inputs}
   {:keys
    [flame-length-matrix
     fire-line-intensity-matrix
     burn-time-matrix
     spread-rate-matrix
     fire-type-matrix
     fire-probability-matrix] :as matrices}
   source-cells
   target-cells]
  (let [max-runtime (double max-runtime)
        cell-size   (double cell-size)]
    (loop [global-clock 0.0
           source-cells source-cells
           target-cells target-cells
           dt           (calc-dt spread-rate-matrix cell-size source-cells)]
      (if (and (< global-clock max-runtime)
               (seq source-cells))
        (let [timestep         (if (> (+ global-clock dt) max-runtime)
                                 (- max-runtime global-clock)
                                 dt)
              updated-targets  (transduce (map #(compute-neighborhood-trajectories!
                                                 inputs matrices target-cells global-clock timestep %))
                                          into
                                          target-cells
                                          source-cells)
              overflow-targets (identify-overflow-targets inputs matrices updated-targets)
              new-sources      (identify-ignitions matrices global-clock timestep (into updated-targets overflow-targets))
              updated-sources  (into source-cells new-sources)]
          (recur (+ global-clock timestep)
                 (filter #(burnable-neighbors? fire-probability-matrix
                                               (:fuel-model landfire-rasters)
                                               num-rows
                                               num-cols
                                               %)
                         updated-sources)
                 (apply disj updated-targets new-sources)
                 (calc-dt spread-rate-matrix cell-size updated-sources)))
        {:global-clock               global-clock
         :exit-condition             (if (seq source-cells) :max-runtime-reached :no-burnable-fuels)
         :fire-spread-matrix         (:fire-probability-matrix matrices)
         :flame-length-matrix        flame-length-matrix
         :fire-line-intensity-matrix fire-line-intensity-matrix
         :burn-time-matrix           burn-time-matrix
         :spread-rate-matrix         spread-rate-matrix
         :fire-type-matrix           fire-type-matrix}))))

(defn- initialize-matrices
  ([inputs]
   (initialize-matrices inputs nil))

  ([{:keys [num-rows num-cols]} non-zero-indices]
   (let [initial-matrix (m/zero-matrix num-rows num-cols)]
     (when non-zero-indices
       (doseq [[i j :as here] non-zero-indices
               :when          (in-bounds? num-rows num-cols here)]
         (m/mset! initial-matrix i j -1.0)))
     {:fire-probability-matrix    (m/mutable initial-matrix)
      :spread-rate-matrix         (m/mutable initial-matrix)
      :flame-length-matrix        (m/mutable initial-matrix)
      :fire-line-intensity-matrix (m/mutable initial-matrix)
      :burn-time-matrix           (m/mutable initial-matrix)
      :fire-type-matrix           (m/mutable initial-matrix)
      :total-distance-matrix      (m/mutable initial-matrix)
      :max-distance-matrix        (m/mutable initial-matrix)
      :max-direction-matrix       (m/mutable initial-matrix)
      :max-rate-matrix            (m/mutable initial-matrix)
      :max-spread-matrix          (m/mutable initial-matrix)
      :max-probability-matrix     (m/mutable initial-matrix)})))

(defmulti run-fire-spread
  "Runs the raster-based fire spread model with a map of these arguments:
  - max-runtime: double (minutes)
  - cell-size: double (feet)
  - landfire-rasters: map containing these entries;
    - elevation: core.matrix 2D double array (feet)
    - slope: core.matrix 2D double array (vertical feet/horizontal feet)
    - aspect: core.matrix 2D double array (degrees clockwise from north)
    - fuel-model: core.matrix 2D double array (fuel model numbers 1-256)
    - canopy-height: core.matrix 2D double array (feet)
    - canopy-base-height: core.matrix 2D double array (feet)
    - crown-bulk-density: core.matrix 2D double array (lb/ft^3)
    - canopy-cover: core.matrix 2D double array (0-100)
  - wind-speed-20ft: double (miles/hour)
  - wind-from-direction: double (degrees clockwise from north)
  - fuel-moisture: doubles (%){:dead {:1hr :10hr :100hr} :live {:herbaceous :woody}}
  - foliar-moisture: double (%)
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
      clojure.lang.PersistentVector  :ignition-point
      :random-ignition-point)))

(defmethod run-fire-spread :random-ignition-point
  [{:keys [ignitable-sites rand-gen] :as inputs}]
  (.nextDouble rand-gen)
  (run-fire-spread (assoc inputs
                          :initial-ignition-site
                          (random/my-rand-nth rand-gen ignitable-sites))))

(defmethod run-fire-spread :ignition-point
  [{:keys [landfire-rasters num-rows num-cols initial-ignition-site] :as inputs}]
  (let [[i j]                                       initial-ignition-site
        fuel-model-matrix                           (:fuel-model landfire-rasters)
        {:keys [fire-probability-matrix] :as matrices} (initialize-matrices inputs)]
    (when (and (in-bounds? num-rows num-cols initial-ignition-site)
               (burnable-fuel-model? (m/mget fuel-model-matrix i j))
               (burnable-neighbors? fire-probability-matrix fuel-model-matrix
                                    num-rows num-cols initial-ignition-site))
      (m/mset! fire-probability-matrix i j 1.0)
      (let [source-cells #{initial-ignition-site}
            target-cells (compute-neighborhood-trajectories! inputs
                                                             matrices
                                                             #{}
                                                             0.0
                                                             0.0
                                                             initial-ignition-site)]
        (run-loop inputs matrices source-cells target-cells)))))
