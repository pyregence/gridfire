(ns gridfire.fire-spread-optimal
  (:require [gridfire.common               :refer [burnable-cell?
                                                   calc-fuel-moisture
                                                   compute-terrain-distance
                                                   non-zero-indices]]
            [gridfire.conversion           :refer [mph->fpm]]
            [gridfire.crown-fire           :refer [crown-fire-eccentricity
                                                   crown-fire-line-intensity
                                                   cruz-crown-fire-spread
                                                   van-wagner-crown-fire-initiation?]]
            [gridfire.fuel-models-optimal  :refer [fuel-models-precomputed
                                                   moisturize]]
            [gridfire.spotting-optimal     :as spot-optimal]
            [gridfire.surface-fire-optimal :refer [rothermel-surface-fire-spread-no-wind-no-slope
                                                   rothermel-surface-fire-spread-max
                                                   compute-spread-rate
                                                   anderson-flame-depth
                                                   byram-fire-line-intensity
                                                   byram-flame-length
                                                   wind-adjustment-factor]]
            [taoensso.tufte                :as tufte]
            [tech.v3.datatype              :as d]
            [tech.v3.datatype.functional   :as dfn]
            [tech.v3.tensor                :as t]))

(set! *unchecked-math* :warn-on-boxed)

(defrecord BurnVector
    [^long   i
     ^long   j
     ^double direction
     ^double fractional-distance
     ^double spread-rate
     ^double terrain-distance
     ^double burn-probability])

;;-----------------------------------------------------------------------------
;; Fire spread
;;-----------------------------------------------------------------------------

(defn- find-max-spread-rate ^double
  [^double max-spread-rate burn-vector]
  (max max-spread-rate ^double (:spread-rate burn-vector)))

(defn- compute-dt ^double
  [^double cell-size burn-vectors]
  (if (pos? (count burn-vectors))
    (/ cell-size (double (reduce find-max-spread-rate 0.0 burn-vectors)))
    10.0)) ; Wait 10 minutes for spot ignitions to smolder and catch fire

#_(defn- compute-terrain-distance-slow
    [inputs ^long i ^long j ^double direction]
    (let [cell-size          (double (:cell-size inputs))
          cell-size-diagonal (double (:cell-size-diagonal inputs))
          get-aspect         (:get-aspect inputs)
          get-slope          (:get-slope inputs)
          ^double aspect     (get-aspect i j)
          ^double slope      (get-slope i j)
          theta              (Math/abs (- aspect direction))
          slope-factor       (/
                              (if (<= theta 90.0)
                                (- 90.0 theta)
                                (if (<= theta 180.0)
                                  (- theta 90.0)
                                  (if (<= theta 270.0)
                                    (- 270.0 theta)
                                    (- theta 270.0))))
                              90.0)
          run                (case direction
                               0.0   cell-size
                               45.0  cell-size-diagonal
                               90.0  cell-size
                               135.0 cell-size-diagonal
                               180.0 cell-size
                               225.0 cell-size-diagonal
                               270.0 cell-size
                               315.0 cell-size-diagonal)
          rise               (* run slope slope-factor)]
      (Math/sqrt (+ (* run run) (* rise rise)))))

(defn ^:redef rothermel-fast-wrapper-optimal
  [fuel-model-number fuel-moisture grass-suppression?]
  (-> (long fuel-model-number)
      (fuel-models-precomputed)
      (moisturize fuel-moisture)
      (rothermel-surface-fire-spread-no-wind-no-slope grass-suppression?)))

(defn- store-if-max!
  [matrix i j ^double new-value]
  (let [^double old-value (t/mget matrix i j)]
    (when (> new-value old-value)
      (t/mset! matrix i j new-value))))

(defn- compute-max-in-situ-values!
  [inputs matrices band i j]
  (let [get-slope                             (:get-slope inputs)
        get-aspect                            (:get-aspect inputs)
        get-canopy-cover                      (:get-canopy-cover inputs)
        get-canopy-height                     (:get-canopy-height inputs)
        get-canopy-base-height                (:get-canopy-base-height inputs)
        get-crown-bulk-density                (:get-crown-bulk-density inputs)
        get-fuel-model                        (:get-fuel-model inputs)
        get-temperature                       (:get-temperature inputs)
        get-relative-humidity                 (:get-relative-humidity inputs)
        get-wind-speed-20ft                   (:get-wind-speed-20ft inputs)
        get-wind-from-direction               (:get-wind-from-direction inputs)
        get-fuel-moisture-dead-1hr            (:get-fuel-moisture-dead-1hr inputs)
        get-fuel-moisture-dead-10hr           (:get-fuel-moisture-dead-10hr inputs)
        get-fuel-moisture-dead-100hr          (:get-fuel-moisture-dead-100hr inputs)
        get-fuel-moisture-live-herbaceous     (:get-fuel-moisture-live-herbaceous inputs)
        get-fuel-moisture-live-woody          (:get-fuel-moisture-live-woody inputs)
        get-foliar-moisture                   (:get-foliar-moisture inputs)
        ellipse-adjustment-factor             (:ellipse-adjustment-factor inputs)
        grass-suppression?                    (:grass-suppression? inputs)
        max-spread-rate-matrix                (:max-spread-rate-matrix matrices)
        max-spread-direction-matrix           (:max-spread-direction-matrix matrices)
        spread-rate-matrix                    (:spread-rate-matrix matrices)
        flame-length-matrix                   (:flame-length-matrix matrices)
        fire-line-intensity-matrix            (:fire-line-intensity-matrix matrices)
        fire-type-matrix                      (:fire-type-matrix matrices)
        modified-time-matrix                  (:modified-time-matrix matrices)
        eccentricity-matrix                   (:eccentricity-matrix matrices)
        band                                  (long band)
        ^double slope                         (get-slope i j)
        ^double aspect                        (get-aspect i j)
        ^double canopy-cover                  (get-canopy-cover i j)
        ^double canopy-height                 (get-canopy-height i j)
        ^double canopy-base-height            (get-canopy-base-height i j)
        ^double crown-bulk-density            (get-crown-bulk-density i j)
        ^double fuel-model                    (get-fuel-model i j)
        ^double temperature                   (get-temperature band i j)
        ^double relative-humidity             (get-relative-humidity band i j)
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
        surface-fire-min                      (rothermel-fast-wrapper-optimal fuel-model
                                                                              [fuel-moisture-dead-1hr
                                                                               fuel-moisture-dead-10hr
                                                                               fuel-moisture-dead-100hr
                                                                               0.0 ; fuel-moisture-dead-herbaceous
                                                                               fuel-moisture-live-herbaceous
                                                                               fuel-moisture-live-woody]
                                                                              grass-suppression?)
        midflame-wind-speed                   (mph->fpm
                                               (* wind-speed-20ft
                                                  (wind-adjustment-factor ^double (:fuel-bed-depth surface-fire-min)
                                                                          canopy-height
                                                                          canopy-cover)))
        surface-fire-max                      (rothermel-surface-fire-spread-max surface-fire-min
                                                                                 midflame-wind-speed
                                                                                 wind-from-direction
                                                                                 slope
                                                                                 aspect
                                                                                 ellipse-adjustment-factor)
        max-spread-rate                       (:max-spread-rate surface-fire-max)
        max-spread-direction                  (:max-spread-direction surface-fire-max)
        eccentricity                          (:eccentricity surface-fire-max)
        max-surface-intensity                 (->> (anderson-flame-depth max-spread-rate ^double (:residence-time surface-fire-min))
                                                   (byram-fire-line-intensity ^double (:reaction-intensity surface-fire-min)))]
    (if (van-wagner-crown-fire-initiation? canopy-cover
                                           canopy-base-height
                                           foliar-moisture
                                           max-surface-intensity)
      (let [crown-spread-max        (cruz-crown-fire-spread wind-speed-20ft
                                                            crown-bulk-density
                                                            fuel-moisture-dead-1hr)
            crown-type              (if (neg? crown-spread-max) 2.0 3.0) ; 2=passive, 3=active
            crown-spread-max        (Math/abs crown-spread-max)
            max-crown-intensity     (crown-fire-line-intensity crown-spread-max
                                                               crown-bulk-density
                                                               (- canopy-height canopy-base-height)
                                                               (:heat-of-combustion surface-fire-min))
            max-fire-line-intensity (+ max-surface-intensity max-crown-intensity)
            max-eccentricity        (if (> ^double max-spread-rate crown-spread-max)
                                      eccentricity
                                      (crown-fire-eccentricity wind-speed-20ft ellipse-adjustment-factor))
            max-spread-rate         (max ^double max-spread-rate crown-spread-max)]
        (t/mset! max-spread-rate-matrix           i j max-spread-rate)
        (t/mset! max-spread-direction-matrix      i j max-spread-direction)
        (t/mset! eccentricity-matrix              i j max-eccentricity)
        (t/mset! modified-time-matrix             i j (inc band))
        (store-if-max! spread-rate-matrix         i j max-spread-rate)
        (store-if-max! flame-length-matrix        i j (byram-flame-length max-fire-line-intensity))
        (store-if-max! fire-line-intensity-matrix i j max-fire-line-intensity)
        (store-if-max! fire-type-matrix           i j crown-type))
      (do
        (t/mset! max-spread-rate-matrix           i j max-spread-rate)
        (t/mset! max-spread-direction-matrix      i j max-spread-direction)
        (t/mset! eccentricity-matrix              i j eccentricity)
        (t/mset! modified-time-matrix             i j (inc band))
        (store-if-max! spread-rate-matrix         i j max-spread-rate)
        (store-if-max! flame-length-matrix        i j (byram-flame-length max-surface-intensity))
        (store-if-max! fire-line-intensity-matrix i j max-surface-intensity)
        (store-if-max! fire-type-matrix           i j 1.0)))))

(defn- burnable-neighbors?
  [get-fuel-model fire-spread-matrix burn-probability num-rows num-cols i j]
  (let [i  (long i)
        j  (long j)
        i- (- i 1)
        i+ (+ i 1)
        j- (- j 1)
        j+ (+ j 1)]
    (or (burnable-cell? get-fuel-model fire-spread-matrix burn-probability num-rows num-cols i- j-)
        (burnable-cell? get-fuel-model fire-spread-matrix burn-probability num-rows num-cols i- j)
        (burnable-cell? get-fuel-model fire-spread-matrix burn-probability num-rows num-cols i- j+)
        (burnable-cell? get-fuel-model fire-spread-matrix burn-probability num-rows num-cols i  j-)
        (burnable-cell? get-fuel-model fire-spread-matrix burn-probability num-rows num-cols i  j+)
        (burnable-cell? get-fuel-model fire-spread-matrix burn-probability num-rows num-cols i+ j-)
        (burnable-cell? get-fuel-model fire-spread-matrix burn-probability num-rows num-cols i+ j)
        (burnable-cell? get-fuel-model fire-spread-matrix burn-probability num-rows num-cols i+ j+))))

(defn- calc-new-spread-rate!
  [inputs matrices modified-time-matrix max-spread-rate-matrix
   max-spread-direction-matrix eccentricity-matrix band direction i j]
  (when (> ^long band (dec ^long (t/mget modified-time-matrix i j)))
    (compute-max-in-situ-values! inputs matrices band i j))
  (let [max-spread-rate      (t/mget max-spread-rate-matrix i j)
        max-spread-direction (t/mget max-spread-direction-matrix i j)
        eccentricity         (t/mget eccentricity-matrix i j)]
    (compute-spread-rate max-spread-rate
                         max-spread-direction
                         eccentricity
                         direction)))

(defn make-burn-vector-constructor
  [num-rows num-cols get-fuel-model fire-spread-matrix burn-probability max-spread-rate-matrix
   max-spread-direction-matrix eccentricity-matrix cell-size get-elevation i j]
  (let [i                    (long i)
        j                    (long j)
        max-spread-rate      (t/mget max-spread-rate-matrix i j)
        max-spread-direction (t/mget max-spread-direction-matrix i j)
        eccentricity         (t/mget eccentricity-matrix i j)]
    (fn [direction]
      (let [new-i (case direction
                    0.0   (- i 1)
                    45.0  (- i 1)
                    90.0  i
                    135.0 (+ i 1)
                    180.0 (+ i 1)
                    225.0 (+ i 1)
                    270.0 i
                    315.0 (- i 1))
            new-j (case direction
                    0.0   j
                    45.0  (+ j 1)
                    90.0  (+ j 1)
                    135.0 (+ j 1)
                    180.0 j
                    225.0 (- j 1)
                    270.0 (- j 1)
                    315.0 (- j 1))]
        (when (burnable-cell? get-fuel-model fire-spread-matrix burn-probability num-rows num-cols new-i new-j)
          (let [spread-rate      (compute-spread-rate max-spread-rate
                                                      max-spread-direction
                                                      eccentricity
                                                      direction)
                terrain-distance (compute-terrain-distance cell-size get-elevation
                                                           num-rows num-cols i j new-i new-j)]
            (->BurnVector i j direction 0.5 spread-rate terrain-distance burn-probability)))))))

(def ^:private bits [0 1 2 3 4 5 6 7])

(defn- create-new-burn-vectors!
  [acc num-rows num-cols cell-size get-elevation fire-spread-matrix travel-lines-matrix
   max-spread-rate-matrix max-spread-direction-matrix eccentricity-matrix get-fuel-model
   i j burn-probability]
  (let [travel-lines       (t/mget travel-lines-matrix i j)
        create-burn-vector (make-burn-vector-constructor num-rows num-cols get-fuel-model fire-spread-matrix burn-probability
                                                         max-spread-rate-matrix max-spread-direction-matrix eccentricity-matrix
                                                         cell-size get-elevation i j)]
    (reduce (fn [acc bit]
              (if (bit-test travel-lines bit)
                acc
                (let [direction       (case bit
                                        0 0.0
                                        1 45.0
                                        2 90.0
                                        3 135.0
                                        4 180.0
                                        5 225.0
                                        6 270.0
                                        7 315.0)
                      new-burn-vector (create-burn-vector direction)]
                  (if new-burn-vector
                    (do
                      ;; TODO move into function
                      (as-> (t/mget travel-lines-matrix i j) $
                        (bit-set $ bit)
                        (t/mset! travel-lines-matrix i j $))
                      (conj! acc new-burn-vector))
                    acc))))
            acc
            bits)))

(defn- identify-spot-ignition-events
  [new-clock spot-ignitions]
  (loop [to-process   spot-ignitions
         ignite-later (transient {})
         ignite-now   (transient {})]
    (if (seq to-process)
      (let [[cell spot-info] (first to-process)
            [t _]            spot-info]
        (if (>= ^double new-clock ^double t)
          (recur (rest to-process) ignite-later (assoc! ignite-now cell spot-info))
          (recur (rest to-process) (assoc! ignite-later cell spot-info) ignite-now)))
      [(persistent! ignite-later) (persistent! ignite-now)])))

(defn- merge-spot-ignitions [a b]
  (reduce (fn [acc [cell spot-info]]
            (if-let [existing-entry (acc cell)]
              (let [[cur-t cur-p] existing-entry
                    [new-t new-p] spot-info]
                (cond (> ^double cur-p ^double new-p)  acc
                      (< ^double cur-p ^double new-p)  (assoc! acc cell spot-info)
                      (<= ^double cur-t ^double new-t) acc
                      :else                            (assoc! acc cell spot-info)))
              (assoc! acc cell spot-info)))
          a
          b))

(defn- compute-new-spot-ignitions
  "Returns a map of [x y] locations to [t p] where:
  t: time of ignition
  p: ignition-probability"
  [inputs matrices ignited-cells]
  (when (:spotting inputs)
    (persistent!
     (reduce (fn [acc [i j]]
               (merge-spot-ignitions acc (spot-optimal/spread-firebrands inputs matrices i j)))
             (transient {})
             ignited-cells))))

(defn- compute-spot-burn-vectors!
  [inputs matrices spot-ignitions ignited-cells band new-clock]
  (let [num-rows                    (:num-rows inputs)
        num-cols                    (:num-cols inputs)
        cell-size                   (:cell-size inputs)
        get-elevation               (:get-elevation inputs)
        get-fuel-model              (:get-fuel-model inputs)
        fire-spread-matrix          (:fire-spread-matrix matrices)
        burn-time-matrix            (:burn-time-matrix matrices)
        travel-lines-matrix         (:travel-lines-matrix matrices)
        max-spread-rate-matrix      (:max-spread-rate-matrix matrices)
        max-spread-direction-matrix (:max-spread-direction-matrix matrices)
        eccentricity-matrix         (:eccentricity-matrix matrices)
        modified-time-matrix        (:modified-time-matrix matrices)
        band                        (long band)
        new-clock                   (double new-clock)
        new-spot-ignitions          (compute-new-spot-ignitions inputs matrices ignited-cells)
        [spot-ignite-later
         spot-ignite-now]           (identify-spot-ignition-events new-clock (persistent!
                                                                              (merge-spot-ignitions
                                                                               (transient spot-ignitions)
                                                                               new-spot-ignitions)))
        ignited?                    (fn [[k v]]
                                      (let [[i j] k
                                            [_ p] v]
                                        (>= ^double (t/mget fire-spread-matrix i j) ^double p)))
        pruned-spot-ignite-later    (into {} (remove ignited? spot-ignite-later)) ; TODO move to identify-spot-ignition
        pruned-spot-ignite-now      (filterv #(not (ignited? %)) spot-ignite-now) ; TODO move to identify-spot-ignition
        spot-burn-vectors           (persistent!
                                     (reduce
                                      (fn [acc [cell spot-info]]
                                        (let [[i j]                        cell
                                              [burn-time burn-probability] spot-info]
                                          (when (> band (dec ^long (t/mget modified-time-matrix i j)))
                                            (compute-max-in-situ-values! inputs matrices band i j))
                                          (t/mset! fire-spread-matrix i j burn-probability)
                                          (t/mset! burn-time-matrix i j burn-time)
                                          (create-new-burn-vectors! acc num-rows num-cols cell-size get-elevation fire-spread-matrix
                                                                    travel-lines-matrix max-spread-rate-matrix max-spread-direction-matrix
                                                                    eccentricity-matrix get-fuel-model i j burn-probability)))
                                      (transient [])
                                      pruned-spot-ignite-now))]
    [spot-burn-vectors (count pruned-spot-ignite-now) pruned-spot-ignite-later]))

(defn- grow-burn-vectors!
  [matrices global-clock timestep burn-vectors]
  (let [fire-spread-matrix (:fire-spread-matrix matrices)
        burn-time-matrix   (:burn-time-matrix matrices)
        global-clock       (double global-clock)
        timestep           (double timestep)]
    (mapv
     persistent!
     (reduce (fn [[burn-vectors ignited-cells] burn-vector]
               (let [i                         (long (:i burn-vector))
                     j                         (long (:j burn-vector))
                     direction                 (double (:direction burn-vector))
                     fractional-distance       (double (:fractional-distance burn-vector))
                     spread-rate               (double (:spread-rate burn-vector))
                     terrain-distance          (double (:terrain-distance burn-vector))
                     burn-probability          (double (:burn-probability burn-vector))
                     fractional-distance-delta (/ (* spread-rate timestep) terrain-distance)
                     new-fractional-distance   (+ fractional-distance fractional-distance-delta)
                     crossed-center?           (and (< fractional-distance 0.5)
                                                    (>= new-fractional-distance 0.5))
                     ^double local-burn-time   (t/mget burn-time-matrix i j)]
                 (when crossed-center?
                   (let [^double local-burn-probability (t/mget fire-spread-matrix i j)]
                     (when (>= burn-probability local-burn-probability)
                       (let [burn-time (-> 0.5
                                           (- fractional-distance)
                                           (/ fractional-distance-delta)
                                           (* timestep)
                                           (+ global-clock))]
                         (if (> burn-probability local-burn-probability)
                           (do
                             (t/mset! fire-spread-matrix i j burn-probability)
                             (t/mset! burn-time-matrix i j burn-time))
                           (when (< burn-time local-burn-time)
                             (t/mset! burn-time-matrix i j burn-time)))))))
                 [(conj! burn-vectors (->BurnVector i j direction new-fractional-distance
                                                    spread-rate terrain-distance burn-probability))
                  (if (and crossed-center? (<= local-burn-time global-clock)) ; first to cross center of the cell this timestep
                    (conj! ignited-cells [i j])
                    ignited-cells)]))
             [(transient []) (transient [])]
             ;;      (if crossed-center? ; first to cross center of the cell this timestep
             ;;        (conj! ignited-cells [i j])
             ;;        ignited-cells)]))
             ;; [(transient []) (transient #{})]
             burn-vectors))))

(defn- ignited-cells->burn-vectors
  [inputs matrices ignited-cells burn-vectors]
  (let [num-rows                    (:num-rows inputs)
        num-cols                    (:num-cols inputs)
        cell-size                   (:cell-size inputs)
        get-elevation               (:get-elevation inputs)
        get-fuel-model              (:get-fuel-model inputs)
        fire-spread-matrix          (:fire-spread-matrix matrices)
        travel-lines-matrix         (:travel-lines-matrix matrices)
        max-spread-rate-matrix      (:max-spread-rate-matrix matrices)
        max-spread-direction-matrix (:max-spread-direction-matrix matrices)
        eccentricity-matrix         (:eccentricity-matrix matrices)]
    (persistent!
     (reduce
      (fn [acc [i j]]
        (create-new-burn-vectors! acc num-rows num-cols cell-size get-elevation fire-spread-matrix travel-lines-matrix
                                  max-spread-rate-matrix max-spread-direction-matrix eccentricity-matrix get-fuel-model
                                  i j (t/mget fire-spread-matrix i j)))
      (transient burn-vectors)
      ignited-cells))))

(defn- promote-burn-vectors
  [inputs matrices global-clock new-clock max-fractional-distance burn-vectors]
  (let [num-rows                (:num-rows inputs)
        num-cols                (:num-cols inputs)
        get-fuel-model          (:get-fuel-model inputs)
        fire-spread-matrix      (:fire-spread-matrix matrices)
        burn-time-matrix        (:burn-time-matrix matrices)
        travel-lines-matrix     (:travel-lines-matrix matrices)
        global-clock            (double global-clock)
        new-clock               (double new-clock)
        max-fractional-distance (double max-fractional-distance)]
    (persistent!
     (reduce
      (fn [acc burn-vector]
        (let [i                      (long (:i burn-vector))
              j                      (long (:j burn-vector))
              burn-probability       (double (:burn-probability burn-vector))
              local-burn-probability (double (t/mget fire-spread-matrix i j))
              local-burn-time        (double (t/mget burn-time-matrix i j))]
          (if (and (> local-burn-time global-clock) ; cell was ignited this timestep
                   (<= burn-probability local-burn-probability))
            (let [direction (double (:direction burn-vector))
                  new-i     (case direction
                              0.0   (- i 1)
                              45.0  (- i 1)
                              90.0  i
                              135.0 (+ i 1)
                              180.0 (+ i 1)
                              225.0 (+ i 1)
                              270.0 i
                              315.0 (- i 1))
                  new-j     (case direction
                              0.0   j
                              45.0  (+ j 1)
                              90.0  (+ j 1)
                              135.0 (+ j 1)
                              180.0 j
                              225.0 (- j 1)
                              270.0 (- j 1)
                              315.0 (- j 1))]
              (if (burnable-cell? get-fuel-model fire-spread-matrix burn-probability
                                  num-rows num-cols new-i new-j)
                (let [spread-rate             (double (:spread-rate burn-vector))
                      terrain-distance        (double (:terrain-distance burn-vector))
                      dt-after-ignition       (- new-clock local-burn-time)
                      new-fractional-distance (min max-fractional-distance
                                                   (+ 0.5 (/ (* spread-rate dt-after-ignition) terrain-distance)))
                      new-spread-rate         (if (< new-fractional-distance max-fractional-distance)
                                                spread-rate
                                                (/ (* (- new-fractional-distance 0.5) terrain-distance) dt-after-ignition))]
                  (conj! acc (->BurnVector i j direction new-fractional-distance new-spread-rate terrain-distance local-burn-probability)))
                (do
                  (t/mset! travel-lines-matrix i j ; TODO make into function
                           (bit-clear (t/mget travel-lines-matrix i j)
                                      (case direction
                                        0.0   0
                                        45.0  1
                                        90.0  2
                                        135.0 3
                                        180.0 4
                                        225.0 5
                                        270.0 6
                                        315.0 7)))
                  acc)))
            (conj! acc burn-vector))))
      (transient [])
      burn-vectors))))

#_(defn- fd->dt
    [fractional-distance terrain-distance spread-rate]
    (-> fractional-distance
        (* terrain-distance)
        (/ spread-rate)))

(defn- transition-burn-vectors
  [inputs matrices band global-clock new-clock max-fractional-distance burn-vectors]
  (let [cell-size                   (:cell-size inputs)
        get-elevation               (:get-elevation inputs)
        num-rows                    (:num-rows inputs)
        num-cols                    (:num-cols inputs)
        get-fuel-model              (:get-fuel-model inputs)
        travel-lines-matrix         (:travel-lines-matrix matrices)
        fire-spread-matrix          (:fire-spread-matrix matrices)
        modified-time-matrix        (:modified-time-matrix matrices)
        max-spread-rate-matrix      (:max-spread-rate-matrix matrices)
        max-spread-direction-matrix (:max-spread-direction-matrix matrices)
        eccentricity-matrix         (:eccentricity-matrix matrices)
        burn-time-matrix            (:burn-time-matrix matrices)
        band                        (long band)
        global-clock                (double global-clock)
        new-clock                   (double new-clock)
        max-fractional-distance     (double max-fractional-distance)]
    (mapv
     persistent!
     (reduce
      (fn [[untransitioned-bvs transitioned-bvs ignited-cells] burn-vector]
        (let [fractional-distance (double (:fractional-distance burn-vector))]
          (if (< fractional-distance 1.0)
            [(conj! untransitioned-bvs burn-vector) transitioned-bvs ignited-cells]
            (let [i                (long (:i burn-vector))
                  j                (long (:j burn-vector))
                  direction        (double (:direction burn-vector))
                  burn-probability (double (:burn-probability burn-vector))
                  direction-bit    (case direction
                                     0.0   0
                                     45.0  1
                                     90.0  2
                                     135.0 3
                                     180.0 4
                                     225.0 5
                                     270.0 6
                                     315.0 7)]
              ;; TODO make into function
              (as-> (t/mget travel-lines-matrix i j) $
                (bit-clear $ direction-bit)
                (t/mset! travel-lines-matrix i j $))
              (let [new-i (case direction
                            0.0   (- i 1)
                            45.0  (- i 1)
                            90.0  i
                            135.0 (+ i 1)
                            180.0 (+ i 1)
                            225.0 (+ i 1)
                            270.0 i
                            315.0 (- i 1))
                    new-j (case direction
                            0.0   j
                            45.0  (+ j 1)
                            90.0  (+ j 1)
                            135.0 (+ j 1)
                            180.0 j
                            225.0 (- j 1)
                            270.0 (- j 1)
                            315.0 (- j 1))]
                (if-not (burnable-cell? get-fuel-model fire-spread-matrix burn-probability
                                        num-rows num-cols new-i new-j)
                  [untransitioned-bvs transitioned-bvs ignited-cells]
                  (do
                    (when (> band (dec ^long (t/mget modified-time-matrix new-i new-j)))
                      ;; vector is first in this timestep to compute
                      (compute-max-in-situ-values! inputs matrices band new-i new-j))
                    ;; TODO move to function
                    (as-> (t/mget travel-lines-matrix new-i new-j) $
                      (bit-set $ direction-bit)
                      (t/mset! travel-lines-matrix new-i new-j $))
                    (let [spread-rate             (double (:spread-rate burn-vector))
                          terrain-distance        (double (:terrain-distance burn-vector))
                          max-spread-rate         (double (t/mget max-spread-rate-matrix new-i new-j))
                          max-spread-direction    (double (t/mget max-spread-direction-matrix new-i new-j))
                          eccentricity            (double (t/mget eccentricity-matrix new-i new-j))
                          new-spread-rate         (compute-spread-rate max-spread-rate max-spread-direction eccentricity direction)
                          ;; TODO move case form to functions
                          new-terrain-distance    (double (compute-terrain-distance cell-size get-elevation num-rows num-cols new-i new-j
                                                                                    (case direction
                                                                                      0.0   (- new-i 1)
                                                                                      45.0  (- new-i 1)
                                                                                      90.0  new-i
                                                                                      135.0 (+ new-i 1)
                                                                                      180.0 (+ new-i 1)
                                                                                      225.0 (+ new-i 1)
                                                                                      270.0 new-i
                                                                                      315.0 (- new-i 1))
                                                                                    (case direction
                                                                                      0.0   new-j
                                                                                      45.0  (+ new-j 1)
                                                                                      90.0  (+ new-j 1)
                                                                                      135.0 (+ new-j 1)
                                                                                      180.0 new-j
                                                                                      225.0 (- new-j 1)
                                                                                      270.0 (- new-j 1)
                                                                                      315.0 (- new-j 1))))
                          dt-in-neighbor          (-> fractional-distance (- 1.0) (* terrain-distance) (/ spread-rate))
                          new-fractional-distance (min max-fractional-distance
                                                       (/ (* new-spread-rate dt-in-neighbor) new-terrain-distance))
                          new-spread-rate         (if (< new-fractional-distance max-fractional-distance)
                                                    new-spread-rate
                                                    (/ (* new-fractional-distance new-terrain-distance) dt-in-neighbor))
                          new-transitioned-bvs    (conj! transitioned-bvs
                                                         (->BurnVector new-i new-j direction new-fractional-distance new-spread-rate
                                                                       new-terrain-distance burn-probability))]
                      (if (< new-fractional-distance 0.5)
                        [untransitioned-bvs new-transitioned-bvs ignited-cells]
                        (let [^double local-burn-probability (t/mget fire-spread-matrix new-i new-j)]
                          (if (< burn-probability local-burn-probability)
                            [untransitioned-bvs new-transitioned-bvs ignited-cells]
                            (let [relative-burn-time      (-> (/ dt-in-neighbor new-fractional-distance)
                                                              (* 0.5))
                                  burn-time               (-> new-clock
                                                              (- dt-in-neighbor)
                                                              (+ relative-burn-time))
                                  ^double local-burn-time (t/mget burn-time-matrix new-i new-j)]
                              (if (> burn-probability local-burn-probability)
                                (do
                                  (t/mset! fire-spread-matrix new-i new-j burn-probability)
                                  (t/mset! burn-time-matrix new-i new-j burn-time)
                                  [untransitioned-bvs new-transitioned-bvs (if (<= local-burn-time global-clock)
                                                                             (conj! ignited-cells [new-i new-j])
                                                                             ignited-cells)])
                                ;; [untransitioned-bvs new-transitioned-bvs (conj! ignited-cells [new-i new-j])]) ; TODO check set vs vectors
                                (do
                                  (when (< burn-time local-burn-time)
                                    (t/mset! burn-time-matrix new-i new-j burn-time))
                                  [untransitioned-bvs new-transitioned-bvs ignited-cells]))))))))))))))
      [(transient []) (transient []) (transient [])]
      ;; [(transient []) (transient []) (transient #{})]
      burn-vectors))))

;; FIXME Update target spread rate on burn-vectors if new band > band
(defn- run-loop
  [inputs matrices ignited-cells]
  (let [cell-size                   (double (:cell-size inputs))
        max-runtime                 (double (:max-runtime inputs))
        ignition-start-time         (double (:ignition-start-time inputs))
        ignition-stop-time          (+ ignition-start-time max-runtime)
        band                        (long (/ ignition-start-time 60.0))
        modified-time-matrix        (:modified-time-matrix matrices)
        max-spread-rate-matrix      (:max-spread-rate-matrix matrices)
        max-spread-direction-matrix (:max-spread-direction-matrix matrices)
        eccentricity-matrix         (:eccentricity-matrix matrices)]
    (doseq [[i j] ignited-cells]
      (compute-max-in-situ-values! inputs matrices band i j))
    (loop [global-clock   ignition-start-time
           band           band
           burn-vectors   (ignited-cells->burn-vectors inputs matrices ignited-cells [])
           spot-ignitions {}
           spot-count     0]
      (if (and (< global-clock ignition-stop-time)
               (or (seq burn-vectors) (seq spot-ignitions)))
        (let [dt-until-new-hour            (- 60.0 (rem global-clock 60.0))
              dt-until-max-runtime         (- ignition-stop-time global-clock)
              burn-vectors                 (if (= dt-until-new-hour 60.0)
                                             (mapv (fn [burn-vector]
                                                     (let [i (:i burn-vector)
                                                           j (:j burn-vector)]
                                                       (when (> band (dec ^long (t/mget modified-time-matrix i j)))
                                                         (compute-max-in-situ-values! inputs matrices band i j))
                                                       (let [direction            (double (:direction burn-vector))
                                                             max-spread-rate      (double (t/mget max-spread-rate-matrix i j))
                                                             max-spread-direction (double (t/mget max-spread-direction-matrix i j))
                                                             eccentricity         (double (t/mget eccentricity-matrix i j))
                                                             new-spread-rate      (compute-spread-rate max-spread-rate max-spread-direction eccentricity direction)]
                                                         (assoc burn-vector :spread-rate new-spread-rate))))
                                                   burn-vectors)
                                             burn-vectors)
              timestep                     (-> (compute-dt cell-size burn-vectors)
                                               (min dt-until-new-hour)
                                               (min dt-until-max-runtime))
              new-clock                    (+ global-clock timestep)
              [burn-vectors ignited-cells] (grow-burn-vectors! matrices global-clock timestep burn-vectors)
              promoted-burn-vectors        (->> burn-vectors
                                                (ignited-cells->burn-vectors inputs matrices ignited-cells)
                                                (promote-burn-vectors inputs matrices global-clock new-clock 1.99))
              [untransitioned-bvs
               transitioned-bvs
               transition-ignited-cells]   (transition-burn-vectors inputs matrices band global-clock new-clock 0.99 promoted-burn-vectors)
              promoted-transitioned-bvs    (->> transitioned-bvs
                                                (ignited-cells->burn-vectors inputs matrices transition-ignited-cells)
                                                (promote-burn-vectors inputs matrices global-clock new-clock 0.99))
              [spot-burn-vectors
               spot-ignite-now-count
               spot-ignite-later]          (compute-spot-burn-vectors! inputs
                                                                       matrices
                                                                       spot-ignitions
                                                                       (into ignited-cells transition-ignited-cells)
                                                                       band
                                                                       new-clock)
              promoted-spot-bvs            (promote-burn-vectors inputs matrices global-clock new-clock 1.49 spot-burn-vectors)
              [untransitioned-spot-bvs
               transitioned-spot-bvs
               _]                          (transition-burn-vectors inputs matrices band global-clock new-clock 0.49 promoted-spot-bvs)
              new-burn-vectors             (-> (into untransitioned-bvs promoted-transitioned-bvs)
                                               (into untransitioned-spot-bvs)
                                               (into transitioned-spot-bvs))]
          (recur new-clock
                 (long (/ new-clock 60.0))
                 new-burn-vectors
                 spot-ignite-later
                 (+ spot-count ^long spot-ignite-now-count)))
        (let [fire-type-matrix (:fire-type-matrix matrices)]
          {:exit-condition             (if (>= global-clock ignition-stop-time) :max-runtime-reached :no-burnable-fuels)
           :global-clock               global-clock
           :burn-time-matrix           (:burn-time-matrix matrices)
           :fire-line-intensity-matrix (:fire-line-intensity-matrix matrices)
           :fire-spread-matrix         (:fire-spread-matrix matrices)
           :fire-type-matrix           fire-type-matrix
           :flame-length-matrix        (:flame-length-matrix matrices)
           :spot-matrix                (:spot-matrix matrices)
           :spread-rate-matrix         (:spread-rate-matrix matrices)
           :crown-fire-count           (as-> fire-type-matrix $
                                         (d/emap #(if (>= ^double % 2.0) 1.0 0.0) nil $)
                                         (dfn/sum $))
           :surface-fire-count         (as-> fire-type-matrix $
                                         (d/emap #(if (= ^double % 1.0) 1.0 0.0) nil $)
                                         (dfn/sum $))
           :spot-count                 spot-count})))))

;;-----------------------------------------------------------------------------
;; SimulationMatrices Record
;;-----------------------------------------------------------------------------

(defrecord SimulationMatrices
    [burn-time-matrix
     eccentricity-matrix
     fire-line-intensity-matrix
     fire-spread-matrix
     fire-type-matrix
     firebrand-count-matrix
     flame-length-matrix
     max-spread-direction-matrix
     max-spread-rate-matrix
     modified-time-matrix
     spot-matrix
     spread-rate-matrix
     travel-lines-matrix])

;;-----------------------------------------------------------------------------
;; Main Simulation Entry Point - Dispatches to Point/Perimeter Ignition
;;-----------------------------------------------------------------------------

;; TODO: Move this multimethod check into run-simulations to avoid running it in every thread
(defmulti run-fire-spread
  "Runs the raster-based fire spread model with a SimulationInputs record containing these fields:
  |------------------------------------+--------------------+-----------------------------------------------------------|
  | Key                                | Value Type         | Value Units                                               |
  |------------------------------------+--------------------+-----------------------------------------------------------|
  | :num-rows                          | long               | column count of fuel-model-matrix                         |
  | :num-cols                          | long               | row count of fuel-model-matrix                            |
  | :cell-size                         | double             | feet                                                      |
  |------------------------------------+--------------------+-----------------------------------------------------------|
  | :ignition-start-time               | double             | minutes                                                   |
  | :max-runtime                       | double             | minutes                                                   |
  |------------------------------------+--------------------+-----------------------------------------------------------|
  | :initial-ignition-site             | [i,j] or 2D tensor | [y,x] coordinate or categories 0-2 in tensor              |
  |------------------------------------+--------------------+-----------------------------------------------------------|
  | :ellipse-adjustment-factor         | double             | < 1.0 = more circular, > 1.0 = more elliptical            |
  | :grass-suppression?                | boolean            | true or false                                             |
  |------------------------------------+--------------------+-----------------------------------------------------------|
  | :rand-gen                          | java.util.Random   | uniform sample [0-1]                                      |
  |------------------------------------+--------------------+-----------------------------------------------------------|
  | :get-elevation                     | (i,j) -> v         | feet                                                      |
  | :get-slope                         | (i,j) -> v         | vertical feet/horizontal feet                             |
  | :get-aspect                        | (i,j) -> v         | degrees clockwise from north [0-360)                      |
  |------------------------------------+--------------------+-----------------------------------------------------------|
  | :get-canopy-cover                  | (i,j) -> v         | percent [0-100]                                           |
  | :get-canopy-height                 | (i,j) -> v         | feet                                                      |
  | :get-canopy-base-height            | (i,j) -> v         | feet                                                      |
  | :get-crown-bulk-density            | (i,j) -> v         | lb/ft^3                                                   |
  |------------------------------------+--------------------+-----------------------------------------------------------|
  | :get-fuel-model                    | (i,j) -> v         | fuel model numbers [1-256]                                |
  |------------------------------------+--------------------+-----------------------------------------------------------|
  | :get-temperature                   | (b,i,j) -> v       | degrees Fahrenheit                                        |
  | :get-relative-humidity             | (b,i,j) -> v       | percent [0-100]                                           |
  | :get-wind-speed-20ft               | (b,i,j) -> v       | miles/hour                                                |
  | :get-wind-from-direction           | (b,i,j) -> v       | degrees clockwise from north                              |
  |------------------------------------+--------------------+-----------------------------------------------------------|
  | :get-fuel-moisture-dead-1hr        | (b,i,j) -> v       | ratio [0-1]                                               |
  | :get-fuel-moisture-dead-10hr       | (b,i,j) -> v       | ratio [0-1]                                               |
  | :get-fuel-moisture-dead-100hr      | (b,i,j) -> v       | ratio [0-1]                                               |
  | :get-fuel-moisture-live-herbaceous | (b,i,j) -> v       | ratio [0-1]                                               |
  | :get-fuel-moisture-live-woody      | (b,i,j) -> v       | ratio [0-1]                                               |
  | :get-foliar-moisture               | (b,i,j) -> v       | ratio [0-1]                                               |
  |------------------------------------+--------------------+-----------------------------------------------------------|
  | :spotting                          | map                | :decay-constant -> double                                 |
  |                                    |                    | :num-firebrands -> long                                   |
  |                                    |                    | :surface-fire-spotting -> map                             |
  |                                    |                    | :crown-fire-spotting-percent -> double or [double double] |
  |------------------------------------+--------------------+-----------------------------------------------------------|"
  (fn [inputs]
    (if (vector? (:initial-ignition-site inputs))
      :ignition-point
      :ignition-perimeter)))

;;-----------------------------------------------------------------------------
;; Point Ignition
;;-----------------------------------------------------------------------------

(defn- initialize-point-ignition-matrices
  [inputs]
  (let [num-rows            (:num-rows inputs)
        num-cols            (:num-cols inputs)
        [i j]               (:initial-ignition-site inputs)
        ignition-start-time (:ignition-start-time inputs)
        spotting            (:spotting inputs)
        shape               [num-rows num-cols]
        burn-time-matrix    (-> (t/new-tensor shape) (t/mset! i j ignition-start-time))]
    (map->SimulationMatrices
     {:burn-time-matrix            burn-time-matrix
      :eccentricity-matrix         (t/new-tensor shape)
      :fire-line-intensity-matrix  (t/new-tensor shape)
      :fire-spread-matrix          (-> (t/new-tensor shape) (t/mset! i j 1.0))
      :fire-type-matrix            (t/new-tensor shape)
      :firebrand-count-matrix      (when spotting (t/new-tensor shape :datatype :int32))
      :flame-length-matrix         (t/new-tensor shape)
      :max-spread-direction-matrix (t/new-tensor shape)
      :max-spread-rate-matrix      (t/new-tensor shape)
      :modified-time-matrix        (t/new-tensor shape :datatype :int32)
      :spot-matrix                 (when spotting (t/new-tensor shape))
      :spread-rate-matrix          (t/new-tensor shape)
      :travel-lines-matrix         (t/new-tensor shape :datatype :short)})))

(defmethod run-fire-spread :ignition-point
  [inputs]
  (run-loop inputs
            (initialize-point-ignition-matrices inputs)
            [(:initial-ignition-site inputs)]))

;;-----------------------------------------------------------------------------
;; Perimeter Ignition
;;-----------------------------------------------------------------------------

(defn- add-ignited-cells!
  [matrix ignited-cells value]
  (doseq [[i j] ignited-cells]
    (t/mset! matrix i j value))
  matrix)

(defn- initialize-perimeter-ignition-matrices
  [inputs ignited-cells]
  (let [num-rows            (:num-rows inputs)
        num-cols            (:num-cols inputs)
        positive-burn-scar  (:initial-ignition-site inputs)
        ignition-start-time (:ignition-start-time inputs)
        spotting            (:spotting inputs)
        shape               [num-rows num-cols]
        negative-burn-scar  (d/clone (dfn/* -1.0 positive-burn-scar))
        burn-time-matrix    (add-ignited-cells! (d/clone negative-burn-scar)
                                                ignited-cells
                                                ignition-start-time)]
    (map->SimulationMatrices
     {:burn-time-matrix            burn-time-matrix
      :eccentricity-matrix         (d/clone negative-burn-scar)
      :fire-line-intensity-matrix  negative-burn-scar
      :fire-spread-matrix          (d/clone positive-burn-scar)
      :fire-type-matrix            (d/clone negative-burn-scar)
      :firebrand-count-matrix      (when spotting (t/new-tensor shape :datatype :int32))
      :flame-length-matrix         (d/clone negative-burn-scar)
      :max-spread-direction-matrix (d/clone negative-burn-scar)
      :max-spread-rate-matrix      (d/clone negative-burn-scar)
      :modified-time-matrix        (t/new-tensor shape :datatype :int32)
      :spot-matrix                 (when spotting (t/new-tensor shape))
      :spread-rate-matrix          (d/clone negative-burn-scar)
      :travel-lines-matrix         (t/new-tensor shape :datatype :short)})))

;; TODO: Move this step into run-simulations to avoid running it in every thread
(defn- get-perimeter-cells
  [inputs]
  (let [num-rows                    (:num-rows inputs)
        num-cols                    (:num-cols inputs)
        initial-ignition-site       (:initial-ignition-site inputs)
        get-fuel-model              (:get-fuel-model inputs)
        {:keys [row-idxs col-idxs]} (non-zero-indices initial-ignition-site)
        num-idxs                    (d/ecount row-idxs)]
    (loop [idx 0
           acc (transient [])]
      (if (< idx num-idxs)
        (let [i (row-idxs idx)
              j (col-idxs idx)]
          (if (burnable-neighbors? get-fuel-model
                                   initial-ignition-site
                                   1.0
                                   num-rows
                                   num-cols
                                   i
                                   j)
            (recur (inc idx)
                   (conj! acc [i j]))
            (recur (inc idx)
                   acc)))
        (persistent! acc)))))

(defmethod run-fire-spread :ignition-perimeter
  [inputs]
  (let [ignited-cells (get-perimeter-cells inputs)]
    (when (seq ignited-cells)
      (run-loop inputs
                (initialize-perimeter-ignition-matrices inputs ignited-cells)
                ignited-cells))))
