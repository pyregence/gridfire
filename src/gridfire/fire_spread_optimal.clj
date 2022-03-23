(ns gridfire.fire-spread-optimal
  (:require [gridfire.common               :refer [burnable-fuel-model?
                                                   calc-fuel-moisture
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
     ^double old-spread-rate
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

(defn in-bounds?
  "Returns true if the point lies within the bounds [0,rows) by [0,cols)."
  [^long rows ^long cols ^long i ^long j]
  (and (>= i 0)
       (>= j 0)
       (< i rows)
       (< j cols)))

(defn- compute-terrain-distance
  [cell-size get-elevation num-rows num-cols i j new-i new-j]
  (let [cell-size (double cell-size)
        i         (long i)
        j         (long j)
        new-i     (long new-i)
        new-j     (long new-j)
        di        (* cell-size (- i new-i))
        dj        (* cell-size (- j new-j))]
    (if (in-bounds? num-rows num-cols new-i new-j)
      (let [dz (- ^double (get-elevation i j)
                  ^double (get-elevation new-i new-j))]
        (Math/sqrt (+ (* di di) (* dj dj) (* dz dz))))
      (Math/sqrt (+ (* di di) (* dj dj))))))

#_(defn- compute-terrain-distance-slow
    [{:keys [cell-size cell-size-diagonal get-aspect get-slope]} ^long i ^long j ^double direction]
    (let [cell-size          (double cell-size)
          cell-size-diagonal (double cell-size-diagonal)
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
  [{:keys [get-aspect get-canopy-base-height get-canopy-cover get-canopy-height get-crown-bulk-density
           get-fuel-model get-slope get-wind-speed-20ft get-wind-from-direction get-temperature
           get-relative-humidity get-foliar-moisture ellipse-adjustment-factor get-fuel-moisture-dead-1hr
           get-fuel-moisture-dead-10hr get-fuel-moisture-dead-100hr get-fuel-moisture-live-herbaceous
           get-fuel-moisture-live-woody grass-suppression?]}
   {:keys [max-spread-rate-matrix max-spread-direction-matrix spread-rate-matrix flame-length-matrix
           fire-line-intensity-matrix fire-type-matrix modified-time-matrix eccentricity-matrix]}
   clock
   i
   j]
  (let [band                                  (long (/ ^double clock 60.0))
        ^double aspect                        (get-aspect i j)
        ^double canopy-base-height            (get-canopy-base-height i j)
        ^double canopy-cover                  (get-canopy-cover i j)
        ^double canopy-height                 (get-canopy-height i j)
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
        (t/mset! modified-time-matrix             i j clock)
        (store-if-max! spread-rate-matrix         i j max-spread-rate)
        (store-if-max! flame-length-matrix        i j (byram-flame-length max-fire-line-intensity))
        (store-if-max! fire-line-intensity-matrix i j max-fire-line-intensity)
        (store-if-max! fire-type-matrix           i j crown-type))
      (do
        (t/mset! max-spread-rate-matrix           i j max-spread-rate)
        (t/mset! max-spread-direction-matrix      i j max-spread-direction)
        (t/mset! eccentricity-matrix              i j eccentricity)
        (t/mset! modified-time-matrix             i j clock)
        (store-if-max! spread-rate-matrix         i j max-spread-rate)
        (store-if-max! flame-length-matrix        i j (byram-flame-length max-surface-intensity))
        (store-if-max! fire-line-intensity-matrix i j max-surface-intensity)
        (store-if-max! fire-type-matrix           i j 1.0)))))

(defn burnable-cell?
  [get-fuel-model fire-spread-matrix burn-probability num-rows num-cols i j]
  (and (in-bounds? num-rows num-cols i j)
       (burnable-fuel-model? (get-fuel-model i j))
       (> (double burn-probability) ^double (t/mget fire-spread-matrix i j))))

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

(def ^:private bits [0 1 2 3 4 5 6 7])

(defn- create-new-burn-vectors!
  [acc num-rows num-cols cell-size get-elevation fire-spread-matrix travel-lines-matrix
   max-spread-rate-matrix max-spread-direction-matrix eccentricity-matrix get-fuel-model i j burn-probability]
  (let [i                    (long i)
        j                    (long j)
        travel-lines         (t/mget travel-lines-matrix i j)
        max-spread-rate      (t/mget max-spread-rate-matrix i j)
        max-spread-direction (t/mget max-spread-direction-matrix i j)
        eccentricity         (t/mget eccentricity-matrix i j)
        create-burn-vector   (fn [acc ^long bit]
                               (let [direction (case bit
                                                 0 0.0
                                                 1 45.0
                                                 2 90.0
                                                 3 135.0
                                                 4 180.0
                                                 5 225.0
                                                 6 270.0
                                                 7 315.0)
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
                                   (let [spread-rate      (compute-spread-rate max-spread-rate
                                                                               max-spread-direction
                                                                               eccentricity
                                                                               direction)
                                         terrain-distance (compute-terrain-distance cell-size get-elevation
                                                                                    num-rows num-cols i j new-i new-j)]
                                     (as-> (t/mget travel-lines-matrix i j) $
                                       (bit-set $ bit)
                                       (t/mset! travel-lines-matrix i j $))
                                     (conj! acc
                                            (->BurnVector i j direction 0.5 spread-rate spread-rate
                                                          terrain-distance burn-probability)))
                                   acc)))]

    ;; FIXME: Use loop/recur to track changes to travel-lines in each step.
    ;;        Don't use t/mget at all. Only call t/mset! once at the end of the loop.
    ;; FIXME: Replace inner reduce with inlined calls to the reducer-fn for the 2 directions.
    (reduce (fn [acc bit]
              (if (bit-test travel-lines bit)
                acc
                (create-burn-vector acc bit)))
            acc
            bits)))

(defn- calc-new-spread-rate!
  [inputs
   {:keys
    [modified-time-matrix max-spread-rate-matrix max-spread-direction-matrix
     eccentricity-matrix] :as matrices}
   new-clock direction i j]
  (when (> ^double new-clock ^double (t/mget modified-time-matrix i j))
    (compute-max-in-situ-values! inputs matrices new-clock i j))
  (let [max-spread-rate      (t/mget max-spread-rate-matrix i j)
        max-spread-direction (t/mget max-spread-direction-matrix i j)
        eccentricity         (t/mget eccentricity-matrix i j)]
    (compute-spread-rate max-spread-rate
                         max-spread-direction
                         eccentricity
                         direction)))

;; TODO re-write using loop-recur
(defn- identify-spot-ignition-events
  [new-clock spot-ignitions]
  (let [to-ignite-now (group-by (fn [[_ [time _]]]
                                  (let [time (double time)]
                                    (>= ^double new-clock time)))
                                spot-ignitions)
        ignite-later  (into {} (get to-ignite-now false))
        ignite-now    (into {} (get to-ignite-now true))]
    [ignite-later ignite-now]))

(defn- merge-spot-ignitions [a b]
  (persistent!
   (reduce (fn [acc [cell spot-info]]
             (if-let [existing-entry (acc cell)]
               (let [[cur-t cur-p] existing-entry
                     [new-t new-p] spot-info]
                 (cond (> ^double cur-p ^double new-p)  acc
                       (< ^double cur-p ^double new-p)  (assoc! acc cell spot-info)
                       (<= ^double cur-t ^double new-t) acc
                       :else                            (assoc! acc cell spot-info)))
               (assoc! acc cell spot-info)))
           (transient a)
           b)))

(defn- compute-new-spot-ignitions
  "Returns a map of [x y] locations to [t p] where:
  t: time of ignition
  p: ignition-probability"
  [{:keys [spotting] :as inputs} matrices ignited-cells]
  (when spotting
    (reduce (fn [acc [i j]]
              (merge-spot-ignitions acc (->> (spot-optimal/spread-firebrands
                                              inputs
                                              matrices
                                              i j)
                                             (into {}))))
            {}
            ignited-cells)))

(defn- compute-spot-burn-vectors!
  [{:keys [num-rows num-cols cell-size get-elevation get-fuel-model] :as inputs}
   {:keys
    [fire-spread-matrix burn-time-matrix travel-lines-matrix max-spread-rate-matrix
     max-spread-direction-matrix eccentricity-matrix modified-time-matrix] :as matrices}
   spot-ignitions ignited-cells new-clock new-hour?]
  (let [new-clock                (double new-clock)
        new-spot-ignitions       (compute-new-spot-ignitions inputs matrices ignited-cells)
        [spot-ignite-later
         spot-ignite-now]        (identify-spot-ignition-events new-clock (merge-spot-ignitions
                                                                           spot-ignitions
                                                                           new-spot-ignitions))
        ignited?                 (fn [[k v]]
                                   (let [[i j] k
                                         [_ p] v]
                                     (>= ^double (t/mget fire-spread-matrix i j) ^double p)))
        pruned-spot-ignite-later (into {} (remove ignited? spot-ignite-later)) ; TODO move to identify-spot-ignition
        pruned-spot-ignite-now   (remove ignited? spot-ignite-now)   ; TODO move to identify-spot-ignition
        spot-burn-vectors        (persistent!
                                  (reduce
                                   (fn [acc [cell spot-info]]
                                     (let [[i j]                        cell
                                           [burn-time burn-probability] spot-info
                                           ^double modified-time        (t/mget modified-time-matrix i j)]
                                       (when (or (zero? modified-time)
                                                 (and new-hour? (> new-clock modified-time)))
                                         (compute-max-in-situ-values! inputs matrices new-clock i j))
                                       (t/mset! fire-spread-matrix i j burn-probability)
                                       (t/mset! burn-time-matrix i j burn-time)
                                       (create-new-burn-vectors! acc num-rows num-cols cell-size get-elevation
                                                                 fire-spread-matrix travel-lines-matrix
                                                                 max-spread-rate-matrix max-spread-direction-matrix
                                                                 eccentricity-matrix get-fuel-model i j
                                                                 burn-probability)))
                                   (transient [])
                                   pruned-spot-ignite-now))]
    [spot-burn-vectors (count pruned-spot-ignite-now) pruned-spot-ignite-later]))

(defn- grow-burn-vectors!
  [inputs
   {:keys [fire-spread-matrix burn-time-matrix] :as matrices}
   global-clock
   timestep
   new-clock
   new-hour?
   t1 t2
   burn-vectors]
  (let [global-clock    (double global-clock)
        timestep        (double timestep)
        new-clock       (double new-clock)
        t1              (double t1)
        t2              (double t2)
        [burn-vectors
         ignited-cells] (reduce (fn [[burn-vectors ignited-cells] burn-vector]
                                  (let [i                         (long (:i burn-vector))
                                        j                         (long (:j burn-vector))
                                        direction                 (double (:direction burn-vector))
                                        fractional-distance       (double (:fractional-distance burn-vector))
                                        spread-rate               (double (:spread-rate burn-vector))
                                        terrain-distance          (double (:terrain-distance burn-vector))
                                        burn-probability          (double (:burn-probability burn-vector))
                                        new-spread-rate           (if new-hour?
                                                                    (double (calc-new-spread-rate! inputs matrices new-clock direction i j))
                                                                    spread-rate)
                                        fractional-distance-delta (if new-hour?
                                                                    (/ (+ (* t1 spread-rate)
                                                                          (* t2 new-spread-rate))
                                                                       terrain-distance)
                                                                    (/ (* spread-rate timestep) terrain-distance))
                                        new-fractional-distance   (+ fractional-distance fractional-distance-delta)
                                        crossed-center?           (and (< fractional-distance 0.5)
                                                                       (>= new-fractional-distance 0.5))
                                        ^double local-burn-time   (t/mget burn-time-matrix i j)]
                                    (when crossed-center?
                                      (let [^double local-burn-probability (t/mget fire-spread-matrix i j)
                                            burn-time                      (if new-hour?
                                                                             (let [distance-to-center (-> 0.5
                                                                                                          (- fractional-distance)
                                                                                                          (* terrain-distance))
                                                                                   d1                 (* t1 spread-rate)]
                                                                               (if (< distance-to-center d1)
                                                                                 ;; ignition occurs in first hour
                                                                                 (-> distance-to-center
                                                                                     (/ spread-rate)
                                                                                     (+ global-clock))
                                                                                 ;; ignition occurs in second hour
                                                                                 (-> distance-to-center
                                                                                     (- d1)
                                                                                     (/ new-spread-rate)
                                                                                     (+ t1)
                                                                                     (+ global-clock))))
                                                                             (-> 0.5
                                                                                 (- fractional-distance)
                                                                                 (/ fractional-distance-delta)
                                                                                 (* timestep)
                                                                                 (+ global-clock)))]
                                        (if (> burn-probability local-burn-probability)
                                          (do
                                            (t/mset! fire-spread-matrix i j burn-probability)
                                            (t/mset! burn-time-matrix i j burn-time))
                                          (when (and (= burn-probability local-burn-probability)
                                                     (< burn-time local-burn-time))
                                            (t/mset! burn-time-matrix i j burn-time)))))
                                    [(conj! burn-vectors (->BurnVector i j direction new-fractional-distance new-spread-rate
                                                                       spread-rate terrain-distance burn-probability))
                                     (if (and crossed-center? (<= local-burn-time global-clock)) ; first to cross center of the cell this timestep
                                       (conj! ignited-cells [i j])
                                       ignited-cells)]))
                                [(transient []) (transient [])]
                                ;;      (if crossed-center? ; first to cross center of the cell this timestep
                                ;;        (conj! ignited-cells [i j])
                                ;;        ignited-cells)]))
                                ;; [(transient []) (transient #{})]
                                burn-vectors)]
    [(persistent! burn-vectors) (persistent! ignited-cells)]))

(defn- ignited-cells->burn-vectors
  [{:keys [num-rows num-cols cell-size get-elevation get-fuel-model]}
   {:keys [fire-spread-matrix travel-lines-matrix max-spread-rate-matrix
           max-spread-direction-matrix eccentricity-matrix]}
   ignited-cells
   burn-vectors]
  (persistent!
   (reduce
    (fn [acc [i j]]
      (create-new-burn-vectors! acc num-rows num-cols cell-size get-elevation fire-spread-matrix
                                travel-lines-matrix max-spread-rate-matrix max-spread-direction-matrix
                                eccentricity-matrix get-fuel-model i j (t/mget fire-spread-matrix i j)))
    (transient burn-vectors)
    ignited-cells)))

(defn- promote-burn-vectors
  "t1: time spent in first hour"
  [{:keys [num-rows num-cols get-fuel-model]}
   {:keys [fire-spread-matrix burn-time-matrix]}
   global-clock new-clock new-hour? top-of-hour burn-vectors]
  (let [global-clock (double global-clock)
        new-clock    (double new-clock)
        top-of-hour  (double top-of-hour)]
    (persistent!
     (reduce
      (fn [acc burn-vector]
        (let [i                (long (:i burn-vector))
              j                (long (:j burn-vector))
              direction        (double (:direction burn-vector))
              burn-probability (double (:burn-probability burn-vector))
              new-i            (case direction
                                 0.0   (- i 1)
                                 45.0  (- i 1)
                                 90.0  i
                                 135.0 (+ i 1)
                                 180.0 (+ i 1)
                                 225.0 (+ i 1)
                                 270.0 i
                                 315.0 (- i 1))
              new-j            (case direction
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
            (let [^double local-burn-probability (t/mget fire-spread-matrix i j)
                  ^double local-burn-time        (t/mget burn-time-matrix i j)]
              (if (and (> local-burn-time global-clock) ; cell was ignited this timestep
                       (<= burn-probability local-burn-probability))
                (let [spread-rate             (double (:spread-rate burn-vector))
                      old-spread-rate         (double (:old-spread-rate burn-vector))
                      terrain-distance        (double (:terrain-distance burn-vector))
                      dt-after-ignition       (- new-clock local-burn-time)
                      new-fractional-distance (if new-hour?
                                                (if (< local-burn-time top-of-hour)
                                                  ;; ignition occured in first hour
                                                  (let [dt-until-hour (- top-of-hour local-burn-time)
                                                        dt-after-hour (- dt-after-ignition dt-until-hour)]
                                                    (+ 0.5 (/ (+ (* old-spread-rate dt-until-hour)
                                                                 (* spread-rate dt-after-hour))
                                                              terrain-distance)))
                                                  ;; ignition occured in second hour
                                                  (+ 0.5 (/ (* spread-rate dt-after-ignition) terrain-distance)))
                                                (+ 0.5 (/ (* spread-rate dt-after-ignition) terrain-distance)))]
                  (conj! acc (->BurnVector i j direction new-fractional-distance spread-rate old-spread-rate terrain-distance local-burn-probability)))
                (conj! acc burn-vector)))
            acc)))
      (transient [])
      burn-vectors))))

(defn- transition-burn-vectors
  [{:keys [cell-size get-elevation num-rows num-cols get-fuel-model] :as inputs}
   {:keys
    [travel-lines-matrix fire-spread-matrix modified-time-matrix max-spread-rate-matrix
     max-spread-direction-matrix eccentricity-matrix burn-time-matrix] :as matrices}
   global-clock
   new-clock
   new-hour?
   burn-vectors]
  (let [global-clock    (double global-clock)
        new-clock       (double new-clock)
        [untransitioned-bvs
         transitioned-bvs
         ignited-cells] (reduce
                         (fn [[untransitioned-bvs transitioned-bvs ignited-cells] burn-vector]
                           (let [fractional-distance (double (:fractional-distance burn-vector))]
                             (if (>= fractional-distance 1.0)
                               ;;transition
                               (let [i                (long (:i burn-vector))
                                     j                (long (:j burn-vector))
                                     direction        (double (:direction burn-vector))
                                     burn-probability (double (:burn-probability burn-vector))]
                                 (t/mset! travel-lines-matrix i j
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
                                   (if (burnable-cell? get-fuel-model fire-spread-matrix burn-probability num-rows num-cols new-i new-j)
                                     ;; Neighbor is burnable: transition burn vector to next cell
                                     (let [^double modified-time (t/mget modified-time-matrix new-i new-j)]
                                       (when (or (zero? modified-time)
                                                 (and new-hour? (> new-clock modified-time))) ;;crossed new hour and vector is first in this timestep to compute
                                         (compute-max-in-situ-values! inputs matrices new-clock new-i new-j))
                                       (t/mset! travel-lines-matrix new-i new-j
                                                (bit-set (t/mget travel-lines-matrix new-i new-j)
                                                         (case direction
                                                           0.0   0
                                                           45.0  1
                                                           90.0  2
                                                           135.0 3
                                                           180.0 4
                                                           225.0 5
                                                           270.0 6
                                                           315.0 7)))
                                       (let [spread-rate             (double (:spread-rate burn-vector))
                                             terrain-distance        (double (:terrain-distance burn-vector))
                                             max-spread-rate         (double (t/mget max-spread-rate-matrix new-i new-j))
                                             max-spread-direction    (double (t/mget max-spread-direction-matrix new-i new-j))
                                             eccentricity            (double (t/mget eccentricity-matrix new-i new-j))
                                             new-spread-rate         (compute-spread-rate max-spread-rate max-spread-direction eccentricity direction)
                                             new-terrain-distance    (double (compute-terrain-distance cell-size get-elevation num-rows num-cols i j new-i new-j))
                                             dt-in-neighbor          (-> fractional-distance (- 1.0) (* terrain-distance) (/ spread-rate))
                                             new-fractional-distance (/ (* new-spread-rate dt-in-neighbor) new-terrain-distance)
                                             new-transitioned-bvs    (conj! transitioned-bvs
                                                                            (->BurnVector new-i new-j direction new-fractional-distance new-spread-rate spread-rate
                                                                                          new-terrain-distance burn-probability))]
                                         (if (>= new-fractional-distance 0.5) ; ignites neighbor's cell
                                           (let [relative-burn-time             (-> dt-in-neighbor
                                                                                    (* 0.5)
                                                                                    (/ new-fractional-distance))
                                                 burn-time                      (-> new-clock
                                                                                    (- dt-in-neighbor)
                                                                                    (+ relative-burn-time))
                                                 ^double local-burn-probability (t/mget fire-spread-matrix new-i new-j)
                                                 ^double local-burn-time        (t/mget burn-time-matrix new-i new-j)]
                                             (if (> burn-probability local-burn-probability)
                                               (do
                                                 (t/mset! fire-spread-matrix new-i new-j burn-probability)
                                                 (t/mset! burn-time-matrix new-i new-j burn-time)
                                                 [untransitioned-bvs new-transitioned-bvs (if (<= local-burn-time global-clock)
                                                                                            (conj! ignited-cells [new-i new-j])
                                                                                            ignited-cells)])
                                               ;; [untransitioned-bvs new-transitioned-bvs (conj! ignited-cells [new-i new-j])]) ; TODO check set vs vectors
                                               (do
                                                 (when (and (= burn-probability local-burn-probability)
                                                            (< burn-time local-burn-time))
                                                   (t/mset! burn-time-matrix new-i new-j burn-time))
                                                 [untransitioned-bvs new-transitioned-bvs ignited-cells])))
                                           [untransitioned-bvs new-transitioned-bvs ignited-cells])))
                                     ;; Neighbor not burnable: burn vector should disapear
                                     [untransitioned-bvs transitioned-bvs ignited-cells])))
                               ;; fractional distance < 1.0: retain current burn vector unchanged
                               [(conj! untransitioned-bvs burn-vector) transitioned-bvs ignited-cells])))
                         [(transient []) (transient []) (transient [])]
                         ;; [(transient []) (transient []) (transient #{})]
                         burn-vectors)]
    [(persistent! untransitioned-bvs) (persistent! transitioned-bvs) (persistent! ignited-cells)]))

(defn- run-loop
  [{:keys [cell-size ignition-start-time max-runtime] :as inputs} matrices ignited-cells]
  (let [cell-size           (double cell-size)
        max-runtime         (double max-runtime)
        ignition-start-time (double ignition-start-time)
        ignition-stop-time  (+ ignition-start-time max-runtime)]
    (doseq [[i j] ignited-cells]
      (compute-max-in-situ-values! inputs matrices ignition-start-time i j))
    (loop [global-clock                        ignition-start-time
           burn-vectors                        (ignited-cells->burn-vectors inputs matrices ignited-cells [])
           spot-ignitions                      {}
           spot-count                          0]
      (if (and (< global-clock ignition-stop-time)
               (or (seq burn-vectors) (seq spot-ignitions)))
        (let [timestep                     (min (compute-dt cell-size burn-vectors)
                                                (- ignition-stop-time global-clock))
              new-clock                    (+ global-clock timestep)
              t1                           (min timestep (- 60.0 (rem global-clock 60.0)))
              t2                           (- timestep t1)
              new-hour?                    (pos? t2)
              top-of-hour                  (+ global-clock t1)
              [burn-vectors ignited-cells] (grow-burn-vectors! inputs matrices global-clock timestep
                                                               new-clock new-hour? t1 t2 burn-vectors)
              promoted-burn-vectors        (->> burn-vectors
                                                (ignited-cells->burn-vectors inputs matrices ignited-cells)
                                                (promote-burn-vectors inputs matrices global-clock
                                                                      new-clock new-hour? top-of-hour))
              [untransitioned-bvs
               transitioned-bvs
               transition-ignited-cells]   (transition-burn-vectors inputs matrices global-clock new-clock
                                                                    new-hour? promoted-burn-vectors)
              promoted-transitioned-bvs    (->> transitioned-bvs
                                                (ignited-cells->burn-vectors inputs matrices transition-ignited-cells)
                                                (promote-burn-vectors inputs matrices global-clock new-clock
                                                                      new-hour? top-of-hour))
              ;; [spot-burn-vectors
              ;;  spot-ignite-now-count
              ;;  spot-ignite-later]          (compute-spot-burn-vectors! inputs
              ;;                                                          matrices
              ;;                                                          spot-ignitions
              ;;                                                          (into ignited-cells transition-ignited-cells)
              ;;                                                          new-clock
              ;;                                                          new-hour?)
              ;; promoted-spot-bvs            (promote-burn-vectors inputs matrices global-clock new-clock
              ;;                                                    new-hour? top-of-hour spot-burn-vectors)
              ;; [untransitioned-spot-bvs
              ;;  transitioned-spot-bvs
              ;;  _]                          (transition-burn-vectors inputs matrices global-clock new-clock
              ;;                                                       new-hour? promoted-spot-bvs)
              burn-vectors'                (-> (into untransitioned-bvs promoted-transitioned-bvs)
                                               #_(into untransitioned-spot-bvs)
                                               #_(into transitioned-spot-bvs))]
          (recur (+ global-clock timestep)
                 burn-vectors'
                 {}
                 0
                 ;; spot-ignite-later
                 #_(+ spot-count ^long spot-ignite-now-count)))
        {:exit-condition             (if (>= global-clock ignition-stop-time) :max-runtime-reached :no-burnable-fuels)
         :global-clock               global-clock
         :burn-time-matrix           (matrices :burn-time-matrix)
         :fire-line-intensity-matrix (matrices :fire-line-intensity-matrix)
         :fire-spread-matrix         (matrices :fire-spread-matrix)
         :fire-type-matrix           (matrices :fire-type-matrix)
         :flame-length-matrix        (matrices :flame-length-matrix)
         :spot-matrix                (matrices :spot-matrix)
         :spread-rate-matrix         (matrices :spread-rate-matrix)
         :crown-fire-count           0 ; TODO Calculate using tensor ops
         :spot-count                 spot-count}))))

;;-----------------------------------------------------------------------------
;; Main Simulation Entry Point - Dispatches to Point/Perimeter Ignition
;;-----------------------------------------------------------------------------

;; TODO: Move this multimethod check into run-simulations to avoid running it in every thread
(defmulti run-fire-spread
  "Runs the raster-based fire spread model with an input map containing these fields:
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
| :elevation-matrix                  | 2D tensor          | feet                                                      |
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
  (fn [{:keys [initial-ignition-site]}]
    (if (vector? initial-ignition-site)
      :ignition-point
      :ignition-perimeter)))

;;-----------------------------------------------------------------------------
;; Point Ignition
;;-----------------------------------------------------------------------------

(defn- initialize-point-ignition-matrices
  [{:keys [num-rows num-cols initial-ignition-site ignition-start-time spotting]}]
  (let [shape            [num-rows num-cols]
        [i j]            initial-ignition-site
        burn-time-matrix (-> (t/new-tensor shape) (t/mset! i j ignition-start-time))]
    {:burn-time-matrix            burn-time-matrix
     :eccentricity-matrix         (t/new-tensor shape)
     :fire-line-intensity-matrix  (t/new-tensor shape)
     :fire-spread-matrix          (-> (t/new-tensor shape) (t/mset! i j 1.0))
     :fire-type-matrix            (t/new-tensor shape)
     :firebrand-count-matrix      (when spotting (t/new-tensor shape))
     :flame-length-matrix         (t/new-tensor shape)
     :max-spread-direction-matrix (t/new-tensor shape)
     :max-spread-rate-matrix      (t/new-tensor shape)
     :modified-time-matrix        (d/clone burn-time-matrix)
     :spot-matrix                 (when spotting (t/new-tensor shape))
     :spread-rate-matrix          (t/new-tensor shape)
     :travel-lines-matrix         (t/new-tensor shape :datatype :short)}))

(defmethod run-fire-spread :ignition-point
  [{:keys [initial-ignition-site] :as inputs}]
  (run-loop inputs
            (initialize-point-ignition-matrices inputs)
            [initial-ignition-site]))

;;-----------------------------------------------------------------------------
;; Perimeter Ignition
;;-----------------------------------------------------------------------------

(defn- add-ignited-cells!
  [burn-time-matrix ignited-cells ignition-start-time]
  (doseq [[i j] ignited-cells]
    (t/mset! burn-time-matrix i j ignition-start-time))
  burn-time-matrix)

(defn- initialize-perimeter-ignition-matrices
  [{:keys [num-rows num-cols initial-ignition-site ignition-start-time spotting]}
   ignited-cells]
  (let [shape              [num-rows num-cols]
        positive-burn-scar initial-ignition-site
        negative-burn-scar (d/clone (dfn/* -1.0 positive-burn-scar))
        burn-time-matrix   (add-ignited-cells! (d/clone negative-burn-scar)
                                               ignited-cells
                                               ignition-start-time)]
    {:burn-time-matrix            burn-time-matrix
     :eccentricity-matrix         (d/clone negative-burn-scar)
     :fire-line-intensity-matrix  negative-burn-scar
     :fire-spread-matrix          (d/clone positive-burn-scar)
     :fire-type-matrix            (d/clone negative-burn-scar)
     :firebrand-count-matrix      (when spotting (t/new-tensor shape))
     :flame-length-matrix         (d/clone negative-burn-scar)
     :max-spread-direction-matrix (d/clone negative-burn-scar)
     :max-spread-rate-matrix      (d/clone negative-burn-scar)
     :modified-time-matrix        (d/clone burn-time-matrix)
     :spot-matrix                 (when spotting (t/new-tensor shape))
     :spread-rate-matrix          (d/clone negative-burn-scar)
     :travel-lines-matrix         (t/new-tensor shape :datatype :short)}))

;; TODO: Move this step into run-simulations to avoid running it in every thread
(defn- get-perimeter-cells
  [{:keys [num-rows num-cols initial-ignition-site get-fuel-model]}]
  (let [{:keys [row-idxs col-idxs]} (non-zero-indices initial-ignition-site)
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
