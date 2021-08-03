(ns gridfire.fire-spread-optimal
  (:require [clojure.core.matrix :as m]
            [gridfire.common :refer [burnable-neighbors?
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
            [gridfire.utils.random :as random]
            [taoensso.tufte :as tufte]))

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

(defn- get-fire-behavior-values
  [{:keys [crown-eccentricity-matrix crown-rate-matrix fire-line-intensity-matrix fire-type-matrix
           flame-length-matrix reaction-intensity-matrix residence-time-matrix surface-eccentricity-matrix
           spread-rate-matrix surface-max-direction-matrix surface-max-rate-matrix]}
   [i j]]
  (let [fire-type (m/mget fire-type-matrix i j)]
    {:crown-eccentricity    (m/mget crown-eccentricity-matrix i j)
     :crown-fire?           (> fire-type 1.0)
     :crown-max-rate        (m/mget crown-rate-matrix i j)
     :fire-line-intensity   (m/mget fire-line-intensity-matrix i j)
     :fire-type             (m/mget fire-type-matrix i j)
     :flame-length          (m/mget flame-length-matrix i j)
     :reaction-intensity    (m/mget reaction-intensity-matrix i j)
     :residence-time        (m/mget residence-time-matrix i j)
     :spread-rate           (m/mget spread-rate-matrix i j)
     :surface-eccentricity  (m/mget surface-eccentricity-matrix i j)
     :surface-max-direction (m/mget surface-max-direction-matrix i j)
     :surface-max-rate      (m/mget surface-max-rate-matrix i j)})) ;TODO return record

(defn- compute-fire-behavior-values
  "Returns Map of fire behavior values:
  {:crown-eccentricity    [0.0 1.0]
   :crown-fire?           boolean
   :crown-max-rate        (ft/min)
   :fire-line-intensity   (Btu/ft*s)
   :fire-type             #{1.0 2.0 3.0}
   :flame-length          (ft)
   :reaction-intensity    (Btu/ft)^2*min
   :residence-time        (min)
   :spread-rate           (ft/min)
   :surface-eccentricity  [0.0 1.0]
   :surface-max-direction [0.0 360.0]
   :surface-max-rate      (ft/min)}"
  [{:keys
    [landfire-rasters multiplier-lookup perturbations wind-speed-20ft wind-from-direction
     temperature relative-humidity foliar-moisture ellipse-adjustment-factor] :as inputs}
   global-clock
   here]
  (let [^double aspect               (sample-at here
                                                global-clock
                                                (:aspect landfire-rasters)
                                                (:aspect multiplier-lookup)
                                                (:aspect perturbations))
        ^double canopy-base-height   (sample-at here
                                                global-clock
                                                (:canopy-base-height landfire-rasters)
                                                (:canopy-base-height multiplier-lookup)
                                                (:canopy-base-height perturbations))
        ^double canopy-cover         (sample-at here
                                                global-clock
                                                (:canopy-cover landfire-rasters)
                                                (:canopy-cover multiplier-lookup)
                                                (:canopy-cover perturbations))
        ^double canopy-height        (sample-at here
                                                global-clock
                                                (:canopy-height landfire-rasters)
                                                (:canopy-height multiplier-lookup)
                                                (:canopy-height perturbations))
        ^double crown-bulk-density   (sample-at here
                                                global-clock
                                                (:crown-bulk-density landfire-rasters)
                                                (:crown-bulk-density multiplier-lookup)
                                                (:crown-bulk-density perturbations))
        ^long fuel-model             (sample-at here
                                                global-clock
                                                (:fuel-model landfire-rasters)
                                                (:fuel-model multiplier-lookup)
                                                (:fuel-model perturbations))
        ^double slope                (sample-at here
                                                global-clock
                                                (:slope landfire-rasters)
                                                (:slope multiplier-lookup)
                                                (:slope perturbations))
        ^double relative-humidity    (get-value-at here
                                                   global-clock
                                                   relative-humidity
                                                   (:relative-humidity multiplier-lookup)
                                                   (:relative-humidity perturbations))
        ^double temperature          (get-value-at here
                                                   global-clock
                                                   temperature
                                                   (:temperature multiplier-lookup)
                                                   (:temperature perturbations))
        ^double wind-from-direction  (get-value-at here
                                                   global-clock
                                                   wind-from-direction
                                                   (:wind-from-direction multiplier-lookup)
                                                   (:wind-from-direction perturbations))
        ^double wind-speed-20ft      (get-value-at here
                                                   global-clock
                                                   wind-speed-20ft
                                                   (:wind-speed-20ft multiplier-lookup)
                                                   (:wind-speed-20ft perturbations))
        ^double fuel-moisture        (or (fuel-moisture-from-raster inputs here global-clock)
                                         (get-fuel-moisture relative-humidity temperature))
        [fuel-model spread-info-min] (rothermel-fast-wrapper fuel-model fuel-moisture)
        waf                          (wind-adjustment-factor ^long (:delta fuel-model)
                                                             canopy-height
                                                             canopy-cover)
        midflame-wind-speed          (* wind-speed-20ft 88.0 waf) ; mi/hr -> ft/min
        spread-info-max              (rothermel-surface-fire-spread-max spread-info-min
                                                                        midflame-wind-speed
                                                                        wind-from-direction
                                                                        slope
                                                                        aspect
                                                                        ellipse-adjustment-factor)
        surface-max-rate             (:max-spread-rate spread-info-max)
        surface-max-direction        (:max-spread-direction spread-info-max)
        [crown-type crown-max-rate]  (cruz-crown-fire-spread wind-speed-20ft crown-bulk-density
                                                             (-> fuel-moisture :dead :1hr))
        residence-time               (:residence-time spread-info-min)
        reaction-intensity           (:reaction-intensity spread-info-min)
        surface-intensity            (->> (anderson-flame-depth surface-max-rate residence-time)
                                          (byram-fire-line-intensity reaction-intensity))
        crown-fire?                  (van-wagner-crown-fire-initiation? canopy-cover
                                                                        canopy-base-height
                                                                        foliar-moisture
                                                                        surface-intensity)
        ^double crown-intensity      (when crown-fire?
                                       (crown-fire-line-intensity
                                        crown-max-rate
                                        crown-bulk-density
                                        canopy-height
                                        canopy-base-height
                                        (-> fuel-model :h :dead :1hr)))
        fire-line-intensity          (if crown-fire?
                                       (+ surface-intensity crown-intensity)
                                       surface-intensity)
        fire-type                    (if crown-fire? crown-type :surface)]
    {:crown-eccentricity    (crown-fire-eccentricity wind-speed-20ft ellipse-adjustment-factor) ;TODO return a record
     :crown-fire?           (not= fire-type :surface)
     :crown-max-rate        crown-max-rate
     :fire-line-intensity   fire-line-intensity
     :fire-type             (fire-type-to-value fire-type)
     :flame-length          (byram-flame-length fire-line-intensity)
     :reaction-intensity    reaction-intensity
     :residence-time        residence-time
     :spread-rate           (if crown-fire? (max surface-max-rate crown-max-rate) surface-max-rate)
     :surface-eccentricity  (:eccentricity spread-info-max)
     :surface-max-direction surface-max-direction
     :surface-max-rate      surface-max-rate}))

(defn- store-fire-behavior-values!
  [{:keys [crown-eccentricity-matrix crown-rate-matrix fire-line-intensity-matrix fire-type-matrix
           flame-length-matrix reaction-intensity-matrix residence-time-matrix spread-rate-matrix
           surface-eccentricity-matrix surface-max-direction-matrix surface-max-rate-matrix]}
   [i j]
   {:keys [crown-eccentricity crown-max-rate fire-line-intensity fire-type flame-length reaction-intensity
           residence-time spread-rate surface-eccentricity surface-max-direction surface-max-rate]}]
  (m/mset! crown-eccentricity-matrix    i j crown-eccentricity)
  (m/mset! crown-rate-matrix            i j crown-max-rate)
  (m/mset! fire-line-intensity-matrix   i j fire-line-intensity)
  (m/mset! fire-type-matrix             i j fire-type)
  (m/mset! flame-length-matrix          i j flame-length)
  (m/mset! reaction-intensity-matrix    i j reaction-intensity)
  (m/mset! residence-time-matrix        i j residence-time)
  (m/mset! spread-rate-matrix           i j spread-rate)
  (m/mset! surface-eccentricity-matrix  i j surface-eccentricity)
  (m/mset! surface-max-direction-matrix i j surface-max-direction)
  (m/mset! surface-max-rate-matrix      i j surface-max-rate))

(defn- update-target-cell!
  "Return target cell"
  [{:keys [distance-3d-matrices]}
   {:keys [fire-probability-matrix max-distance-matrix max-direction-matrix max-probability-matrix]}
   {:keys [surface-max-rate surface-max-direction surface-eccentricity crown-max-rate crown-eccentricity crown-fire?]}
   timestep
   [x y :as source]
   [i j :as target]]
  (let [trajectory                (mapv - target source)
        spread-direction          (offset-to-degrees trajectory)
        surface-spread-rate       (rothermel-surface-fire-spread-any surface-max-rate
                                                                     surface-max-direction
                                                                     surface-eccentricity
                                                                     spread-direction)
        ^double crown-spread-rate (when crown-fire?
                                    (rothermel-surface-fire-spread-any crown-max-rate
                                                                       surface-max-direction
                                                                       crown-eccentricity
                                                                       spread-direction))
        spread-rate               (if crown-fire?
                                    (max surface-spread-rate crown-spread-rate)
                                    surface-spread-rate)
        terrain-distance          (tufte/p
                                   :distance-3d
                                   (m/mget (get distance-3d-matrices spread-direction) x y))
        spread-distance           (/ (* spread-rate timestep) terrain-distance)]
    (when (> spread-distance (m/mget max-distance-matrix i j))
      (m/mset! max-distance-matrix i j spread-distance)
      (m/mset! max-direction-matrix i j spread-direction)
      (m/mset! max-probability-matrix i j (m/mget fire-probability-matrix x y)))))

(defn- process-source-cell!
  [inputs matrices global-clock recompute-fn source-cell]
  (let [recompute?           (recompute-fn source-cell)
        fire-behavior-values (if recompute?
                               (compute-fire-behavior-values inputs global-clock source-cell)
                               (get-fire-behavior-values matrices source-cell))]
    (when recompute?
      (store-fire-behavior-values! matrices source-cell fire-behavior-values))
    fire-behavior-values))

(defn- process-target-cells!
  [{:keys [num-rows num-cols landfire-rasters] :as inputs}
   {:keys [fire-probability-matrix] :as matrices}
   timestep]
  (fn [source-cell fire-behavior-values]
    (let [fuel-model-matrix (:fuel-model landfire-rasters)
          target-cells      (filterv #(and (in-bounds? num-rows num-cols %) ;FIXME drop inbounds
                                           (burnable? fire-probability-matrix
                                                      fuel-model-matrix
                                                      source-cell
                                                      %))
                                     (get-neighbors source-cell))]
                                        ;FIXME stamp updated neighbor target-cells back into target-matrix
      (doseq [target-cell target-cells]
        (update-target-cell! inputs
                             matrices
                             fire-behavior-values
                             timestep
                             source-cell
                             target-cell))
      target-cells)))

(defn- update-overflow-targets!
  [{:keys [num-rows num-cols landfire-rasters]}
   {:keys [max-direction-matrix max-probability-matrix max-distance-matrix fire-probability-matrix]}
   [x y :as source] overflow-direction overflow-heat overflow-offset overflow-count]
  (loop [source                source
         overflow-count        overflow-count
         overflow-heat         overflow-heat
         burned-overflow-cells #{}]
    (let [[i j :as overflow-target] (mapv + source overflow-offset)]
      (if (and (pos? overflow-count)
               (in-bounds? num-rows num-cols overflow-target)
               (burnable? fire-probability-matrix (:fuel-model landfire-rasters) source overflow-target)
               (> overflow-heat (m/mget max-distance-matrix i j)))
        (let [old-max-distance (m/mget max-distance-matrix i j)]
          (m/mset! max-distance-matrix    i j overflow-heat)
          (m/mset! max-direction-matrix   i j overflow-direction)
          (m/mset! max-probability-matrix i j (m/mget max-probability-matrix x y))
          (recur overflow-target
                 (dec overflow-count)
                 (dec overflow-heat)
                 (if (= old-max-distance 0.0)
                   (conj burned-overflow-cells overflow-target)
                   burned-overflow-cells)))
        burned-overflow-cells))))

(defn- update-all-overflow-targets!
  [inputs
   {:keys [max-direction-matrix max-distance-matrix total-distance-matrix] :as matrices}
   target-cells]
  (loop [[[x y :as target] & remain] target-cells
         ignited-overflow-cells      #{}]
    (if target
      (let [total-distance (m/mget total-distance-matrix x y)
            max-distance   (m/mget max-distance-matrix x y)
            new-total      (+ total-distance max-distance)]
        (if (> new-total 1.0)
          (let [overflow-heat      (dec new-total)
                overflow-count     (int (Math/floor new-total)) ;;FIXME Take into account terrain-distance-ratio
                overflow-direction (m/mget max-direction-matrix x y)
                overflow-offset    (degrees-to-offset overflow-direction)]
            (recur remain
                   (into ignited-overflow-cells
                         (update-overflow-targets! inputs matrices target overflow-direction
                                                   overflow-heat overflow-offset overflow-count))))
          (recur remain ignited-overflow-cells)))
      ignited-overflow-cells)))

(defn- identify-new-sources!
  [{:keys [total-distance-matrix max-distance-matrix burn-time-matrix fire-probability-matrix
           max-probability-matrix]}
   global-clock
   timestep
   target-cells]
  (persistent!
   (reduce (fn [acc [i j :as target]]
             (let [total-distance (m/mget total-distance-matrix i j)
                   max-distance   (m/mget max-distance-matrix i j)
                   new-total      (+ total-distance max-distance)]
               (m/mset! total-distance-matrix i j new-total)
               (m/mset! max-distance-matrix i j 0.0)
               (if (>= new-total 1.0)
                 (let [dt-ignition (* (/ (- 1.0 total-distance) max-distance) timestep)]
                   (m/mset! burn-time-matrix i j (+ global-clock dt-ignition))
                   (m/mset! fire-probability-matrix i j (m/mget max-probability-matrix i j))
                   (conj! acc target))
                 acc)))
           (transient #{})
           target-cells)))

(defn- reducer-fn ^double
  [spread-rate-matrix ^double max-spread-rate [i j]]
  (Math/max max-spread-rate (double (m/mget spread-rate-matrix i j))))

(defn- calc-dt ^double
  [spread-rate-matrix cell-size source-cells]
  (->> source-cells
       (double-reduce (partial reducer-fn spread-rate-matrix) 0.0)
       (/ cell-size)
       double))

(defn- recompute?
  [global-clock prev-temporal-index temporal-index new-sources source-cell]
  (or (contains? new-sources source-cell)
      (zero? global-clock)
      (not= temporal-index prev-temporal-index)))

(defn- run-loop
  [{:keys [max-runtime cell-size landfire-rasters num-rows num-cols recompute-dt] :as inputs}
   {:keys
    [burn-time-matrix
     fire-line-intensity-matrix
     fire-probability-matrix
     fire-type-matrix
     flame-length-matrix
     spread-rate-matrix] :as matrices}
   source-cells]
  (let [max-runtime (double max-runtime)
        cell-size   (double cell-size)]
    (loop [global-clock        0.0
           source-cells        source-cells
           new-sources         #{}
           prev-temporal-index 0]
      (if (and (< global-clock max-runtime)
               (seq source-cells))
        (let [temporal-index         (int (quot global-clock recompute-dt)) ;TODO Check temporal-index is working as expected. print outs
              recompute-fn           #(recompute? global-clock prev-temporal-index temporal-index new-sources %)
              fire-behavior-values   (mapv #(process-source-cell! inputs matrices global-clock recompute-fn %)
                                           source-cells)
              dt                     (calc-dt spread-rate-matrix cell-size source-cells)
              timestep               (if (> (+ global-clock dt) max-runtime)
                                       (- max-runtime global-clock)
                                       dt)
              immediate-target-cells (tufte/p
                                      :immediate-target-cells
                                      (into #{}
                                            (mapcat (process-target-cells! inputs matrices timestep)
                                                    source-cells
                                                    fire-behavior-values)))
              overflow-target-cells  (update-all-overflow-targets! inputs matrices immediate-target-cells)
              new-target-cells       (into immediate-target-cells overflow-target-cells)
              new-sources            (identify-new-sources! matrices global-clock timestep new-target-cells)]
          (recur (+ global-clock timestep)
                 (into #{}
                       (filter #(burnable-neighbors? fire-probability-matrix
                                                     (:fuel-model landfire-rasters)
                                                     num-rows
                                                     num-cols
                                                     %))
                       (into source-cells new-sources))
                 new-sources
                 temporal-index))
        {:global-clock               global-clock
         :exit-condition             (if (seq source-cells) :max-runtime-reached :no-burnable-fuels)
         :fire-spread-matrix         (:fire-probability-matrix matrices)
         :flame-length-matrix        flame-length-matrix
         :fire-line-intensity-matrix fire-line-intensity-matrix
         :burn-time-matrix           burn-time-matrix
         :spread-rate-matrix         spread-rate-matrix
         :fire-type-matrix           fire-type-matrix}))))

                                        ;FIXME init target-matrix
                                        ; set all interior cells to 2r11111111 = 255
                                        ; Along edge leave certain cells 0

;; Bit representation of target cells
;;   N E S W
;; 2r11111111

(defn- initialize-matrices
  [{:keys [num-rows num-cols]} non-zero-indices]
  (let [initial-matrix (m/zero-matrix num-rows num-cols)]
    (when non-zero-indices
      (doseq [[i j] non-zero-indices]
        (m/mset! initial-matrix i j -1.0)))
    {:burn-time-matrix             (m/mutable initial-matrix)
     :crown-eccentricity-matrix    (m/mutable initial-matrix)
     :crown-rate-matrix            (m/mutable initial-matrix)
     :fire-line-intensity-matrix   (m/mutable initial-matrix)
     :fire-probability-matrix      (m/mutable initial-matrix) ;FIXME init/add target-matrix (for byte arith)
     :fire-type-matrix             (m/mutable initial-matrix)
     :flame-length-matrix          (m/mutable initial-matrix)
     :max-direction-matrix         (m/mutable initial-matrix)
     :max-distance-matrix          (m/mutable initial-matrix)
     :max-probability-matrix       (m/mutable initial-matrix)
     :reaction-intensity-matrix    (m/mutable initial-matrix)
     :residence-time-matrix        (m/mutable initial-matrix)
     :surface-eccentricity-matrix  (m/mutable initial-matrix)
     :surface-max-direction-matrix (m/mutable initial-matrix)
     :surface-max-rate-matrix      (m/mutable initial-matrix)
     :spread-rate-matrix           (m/mutable initial-matrix)
     :total-distance-matrix        (m/mutable initial-matrix)}))

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
  - ellipse-adjustment-factor: (< 1.0 = more circular, >
  gridfire.cli> (def inputs (load-inputs config))
  Jul 26, 2021 10:01:41 AM org.hsqldb.persist.Logger logInfoEvent
  INFO: dataFileCache open start 1.0 = more elliptical)
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

(defmethod run-fire-spread :ignition-point ;FIXME Calculate recompute-dt  (min)
  [{:keys [landfire-rasters num-rows num-cols initial-ignition-site] :as inputs}]
  (let [[i j]                                          initial-ignition-site
        fuel-model-matrix                              (:fuel-model landfire-rasters)
        {:keys [fire-probability-matrix] :as matrices} (initialize-matrices inputs [initial-ignition-site])
        _                                              (m/mset! fire-probability-matrix i j 1.0)]
    (when (and (in-bounds? num-rows num-cols initial-ignition-site)

               (burnable-neighbors? fire-probability-matrix fuel-model-matrix
                                    num-rows num-cols initial-ignition-site))
      (run-loop inputs matrices #{initial-ignition-site}))))
