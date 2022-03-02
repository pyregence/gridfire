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
                                                   rothermel-surface-fire-spread-any
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
     ^double spread-rate
     ^double terrain-distance
     ^double fractional-distance
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

(def ^:private compute-spread-rate rothermel-surface-fire-spread-any)

(defn- in-bounds?
  "Returns true if the point lies within the bounds [0,rows) by [0,cols)."
  [^long rows ^long cols ^long i ^long j]
  (and (>= i 0)
       (>= j 0)
       (< i rows)
       (< j cols)))

(defn- compute-terrain-distance ^double
  [{:keys [cell-size get-elevation num-rows num-cols]} ^long i ^long j ^double direction]
  (let [cell-size (double cell-size)
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
    (if (in-bounds? num-rows num-cols new-i new-j)
      (let [di (* cell-size (- i new-i))
            dj (* cell-size (- j new-j))
            dz (- ^double (get-elevation i j)
                  ^double (get-elevation new-i new-j))]
        (Math/sqrt (+ (* di di) (* dj dj) (* dz dz))))
      (/ cell-size 2))))

(defn- compute-terrain-distance-slow
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

(defn- rothermel-fast-wrapper-optimal
  [fuel-model-number fuel-moisture grass-suppression?]
  (let [fuel-model       (-> (fuel-models-precomputed (long fuel-model-number))
                             (moisturize fuel-moisture))
        surface-fire-min (rothermel-surface-fire-spread-no-wind-no-slope
                          fuel-model grass-suppression?)]
    [fuel-model surface-fire-min]))

(defn- store-if-max!
  [^double value matrix i j]
  (->> value
       (max ^double (t/mget matrix i j))
       (t/mset! matrix i j)))

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
        [fuel-model
         surface-fire-min]                    (rothermel-fast-wrapper-optimal fuel-model
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
      (let [[crown-type ^double crown-spread-max] (cruz-crown-fire-spread wind-speed-20ft
                                                                          crown-bulk-density
                                                                          fuel-moisture-dead-1hr)
            max-crown-intensity                   (crown-fire-line-intensity crown-spread-max
                                                                             crown-bulk-density
                                                                             (- canopy-height canopy-base-height)
                                                                             ((:h fuel-model) 0))
            max-fire-line-intensity               (+ max-surface-intensity max-crown-intensity)
            max-eccentricity                      (if (> ^double max-spread-rate crown-spread-max)
                                                    eccentricity
                                                    (crown-fire-eccentricity wind-speed-20ft ellipse-adjustment-factor))
            max-spread-rate                       (max ^double max-spread-rate crown-spread-max)]

        (t/mset! max-spread-rate-matrix      i j max-spread-rate)
        (t/mset! max-spread-direction-matrix i j max-spread-direction)
        (t/mset! eccentricity-matrix         i j max-eccentricity)
        (t/mset! modified-time-matrix        i j clock)

        (-> max-spread-rate
            (store-if-max! spread-rate-matrix i j))

        (-> (byram-flame-length max-fire-line-intensity)
            (store-if-max! flame-length-matrix i j))

        (-> max-fire-line-intensity
            (store-if-max! fire-line-intensity-matrix i j))

        (-> (if (= crown-type :passive-crown) 2.0 3.0) ; FIXME: Make cruz-crown-fire-spread return 2.0 or 3.0
            (store-if-max! fire-type-matrix i j)))
      (do
        (t/mset! max-spread-rate-matrix      i j max-spread-rate)
        (t/mset! max-spread-direction-matrix i j max-spread-direction)
        (t/mset! eccentricity-matrix         i j eccentricity)
        (t/mset! modified-time-matrix        i j clock)

        (-> max-spread-rate
            (store-if-max! spread-rate-matrix i j))

        (-> (byram-flame-length max-surface-intensity)
            (store-if-max! flame-length-matrix i j))

        (-> max-surface-intensity
            (store-if-max! fire-line-intensity-matrix i j))

        (-> 1.0
            (store-if-max! fire-type-matrix i j))))))

(defn- create-new-burn-vector
  [inputs burn-probability max-spread-rate max-spread-direction eccentricity direction i j]
  (let [spread-rate      (compute-spread-rate max-spread-rate
                                              max-spread-direction
                                              eccentricity
                                              direction)
        terrain-distance (compute-terrain-distance inputs i j direction)]
    (->BurnVector i j direction spread-rate terrain-distance 0.5 burn-probability)))

(def ^:private bits [0 1 2 3])

                                        ;TODO write tests
(defn- create-new-burn-vectors!
  [acc
   inputs
   {:keys [travel-lines-matrix max-spread-rate-matrix max-spread-direction-matrix eccentricity-matrix]}
   burn-probability i j]
  (let [travel-lines         (t/mget travel-lines-matrix i j)
        max-spread-rate      (t/mget max-spread-rate-matrix i j)
        max-spread-direction (t/mget max-spread-direction-matrix i j)
        eccentricity         (t/mget eccentricity-matrix i j)]
    (reduce (fn [acc bit]
              (if (bit-test travel-lines bit)
                acc
                (case bit
                  0
                  (if (bit-test travel-lines 4)
                    acc
                    (do
                      (t/mset! travel-lines-matrix i j (bit-or ^long (t/mget travel-lines-matrix i j) 2r00010001))
                      (-> acc
                          (conj! (create-new-burn-vector inputs burn-probability max-spread-rate
                                                         max-spread-direction eccentricity 0.0 i j)) ;; N
                          (conj! (create-new-burn-vector inputs burn-probability max-spread-rate
                                                         max-spread-direction eccentricity 180.0 i j))))) ; S
                  1
                  (if (bit-test travel-lines 5)
                    acc
                    (do
                      (t/mset! travel-lines-matrix i j (bit-or ^long (t/mget travel-lines-matrix i j) 2r00100010))
                      (-> acc
                          (conj! (create-new-burn-vector inputs burn-probability max-spread-rate
                                                         max-spread-direction eccentricity 45.0 i j)) ; NE
                          (conj! (create-new-burn-vector inputs burn-probability max-spread-rate
                                                         max-spread-direction eccentricity 225.0 i j))))) ; SW
                  2
                  (if (bit-test travel-lines 6)
                    acc
                    (do
                      (t/mset! travel-lines-matrix i j (bit-or ^long (t/mget travel-lines-matrix i j) 2r01000100))
                      (-> acc
                          (conj! (create-new-burn-vector inputs burn-probability max-spread-rate
                                                         max-spread-direction eccentricity 90.0 i j)) ; E
                          (conj! (create-new-burn-vector inputs burn-probability max-spread-rate
                                                         max-spread-direction eccentricity 270.0 i j))))) ; W
                  3
                  (if (bit-test travel-lines 7)
                    acc
                    (do
                      (t/mset! travel-lines-matrix i j (bit-or ^long (t/mget travel-lines-matrix i j) 2r10001000))
                      (-> acc
                          (conj! (create-new-burn-vector inputs burn-probability max-spread-rate
                                                         max-spread-direction eccentricity 135.0 i j)) ; SE
                          (conj! (create-new-burn-vector inputs burn-probability max-spread-rate
                                                         max-spread-direction eccentricity 315.0 i j)))))))) ; NW
            acc
            bits)))

(defn- burnable-cell?
  [fuel-model-matrix fire-spread-matrix burn-probability num-rows num-cols i j]
  (and (in-bounds? num-rows num-cols i j)
       (burnable-fuel-model? (t/mget fuel-model-matrix i j))
       (> (double burn-probability) ^double (t/mget fire-spread-matrix i j))))

(defn- burnable-neighbors?
  [fuel-model-matrix fire-spread-matrix burn-probability num-rows num-cols i j]
  (let [i  (long i)
        j  (long j)
        i- (- i 1)
        i+ (+ i 1)
        j- (- j 1)
        j+ (+ j 1)]
    (or (burnable-cell? fuel-model-matrix fire-spread-matrix burn-probability num-rows num-cols i- j-)
        (burnable-cell? fuel-model-matrix fire-spread-matrix burn-probability num-rows num-cols i- j)
        (burnable-cell? fuel-model-matrix fire-spread-matrix burn-probability num-rows num-cols i- j+)
        (burnable-cell? fuel-model-matrix fire-spread-matrix burn-probability num-rows num-cols i  j-)
        (burnable-cell? fuel-model-matrix fire-spread-matrix burn-probability num-rows num-cols i  j+)
        (burnable-cell? fuel-model-matrix fire-spread-matrix burn-probability num-rows num-cols i+ j-)
        (burnable-cell? fuel-model-matrix fire-spread-matrix burn-probability num-rows num-cols i+ j)
        (burnable-cell? fuel-model-matrix fire-spread-matrix burn-probability num-rows num-cols i+ j+))))

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

(defn- identify-spot-ignition-events
  [global-clock spot-ignitions]
  (let [to-ignite-now (group-by (fn [[_ [time _]]]
                                  (let [time (double time)]
                                    (>= ^double global-clock time)))
                                spot-ignitions)
        ignite-later  (into {} (get to-ignite-now false))
        ignite-now    (into {} (get to-ignite-now true))]
    [ignite-later ignite-now]))

(defn- compute-new-spot-ignitions
  "Returns a map of [x y] locations to [t p] where:
  t: time of ignition
  p: ignition-probability"
  [{:keys [spotting] :as inputs} matrices ignited-cells]
  (when spotting
    (reduce (fn [acc [i j]]
              (merge-with (partial min-key first)
                          acc
                          (->> (spot-optimal/spread-firebrands
                                inputs
                                matrices
                                i j)
                               (into {}))))
            {}
            ignited-cells)))

(defn- compute-spot-burn-vectors!
  [inputs {:keys [fire-spread-matrix burn-time-matrix] :as matrices}
   burn-vectors spot-ignitions ignited-cells global-clock]
  (let [new-spot-ignitions     (compute-new-spot-ignitions inputs matrices ignited-cells)
        [spot-ignite-later
         spot-ignite-now]      (identify-spot-ignition-events global-clock
                                                              (merge-with (partial min-key first)
                                                                          spot-ignitions
                                                                          new-spot-ignitions))
        ignited?               (fn [[k v]]
                                 (let [[i j] k
                                       [_ p] v]
                                   (> ^double (t/mget fire-spread-matrix i j) ^double p)))
        pruned-spot-ignite-now (remove ignited? spot-ignite-now)
        updated-burn-vectors   (reduce
                                (fn [acc [cell spot-info]]
                                  (let [[i j]                cell
                                        [_ burn-probability] spot-info]
                                    (compute-max-in-situ-values! inputs matrices global-clock i j)
                                    (t/mset! fire-spread-matrix i j burn-probability)
                                    (t/mset! burn-time-matrix i j global-clock)
                                    (create-new-burn-vectors! acc inputs matrices burn-probability i j)))
                                burn-vectors
                                pruned-spot-ignite-now)]
    [updated-burn-vectors (count pruned-spot-ignite-now) spot-ignite-later]))

(defn- grow-burn-vectors!
  [{:keys [fire-spread-matrix burn-time-matrix]}
   burn-vectors
   global-clock
   timestep]
  (let [global-clock (double global-clock)
        timestep     (double timestep)]
    (reduce (fn [[burn-vectors ignited-cells] burn-vector]
              (let [i                              (long (:i burn-vector))
                    j                              (long (:j burn-vector))
                    spread-rate                    (double (:spread-rate burn-vector))
                    terrain-distance               (double (:terrain-distance burn-vector))
                    fractional-distance            (double (:fractional-distance burn-vector))
                    fractional-distance-delta      (/ (* spread-rate timestep) terrain-distance)
                    burn-probability               (double (:burn-probability burn-vector))
                    ^double local-burn-probability (t/mget fire-spread-matrix i j)
                    ^double local-burn-time        (t/mget burn-time-matrix i j)
                    new-fractional-distance        (+ fractional-distance fractional-distance-delta)
                    crossed-center?                (and (< fractional-distance 0.5)
                                                        (>= new-fractional-distance 0.5))]
                (when crossed-center?
                  (let [burn-time (-> 0.5
                                      (- fractional-distance)
                                      (/ fractional-distance-delta)
                                      (* timestep)
                                      (+ global-clock))]
                    (if (> burn-probability local-burn-probability)
                      (do
                        (t/mset! fire-spread-matrix i j burn-probability)
                        (t/mset! burn-time-matrix i j burn-time))
                      (when (and (= burn-probability local-burn-probability)
                                 (< burn-time local-burn-time))
                        (t/mset! burn-time-matrix i j burn-time)))))
                [(conj! burn-vectors (assoc burn-vector :fractional-distance new-fractional-distance))
                 (if (and crossed-center? (<= local-burn-time global-clock)) ; first to cross center of the cell this timestep
                   (conj! ignited-cells [i j])
                   ignited-cells)]))
            [(transient []) (transient [])]
            burn-vectors)))

(defn- promote-burn-vectors
  [{:keys [fire-spread-matrix burn-time-matrix]} ^double global-clock ^double timestep burn-vectors]
  (mapv
   (fn [burn-vector]
     (let [i                              (long (:i burn-vector))
           j                              (long (:j burn-vector))
           spread-rate                    (double (:spread-rate burn-vector))      ;TODO recalc new-spread-rate
           terrain-distance               (double (:terrain-distance burn-vector)) ;TODO calc new-terrain-distance
           burn-probability               (double (:burn-probability burn-vector))
           ^double local-burn-probability (t/mget fire-spread-matrix i j)
           ^double local-burn-time        (t/mget burn-time-matrix i j)
           dt-after-ignition              (- (+ global-clock timestep) local-burn-time)]
       (if (and (> local-burn-time global-clock) ; cell was ignited this timestep
                (<= burn-probability local-burn-probability))
         (-> burn-vector
             (assoc :fractional-distance (+ 0.5 (/ (* spread-rate dt-after-ignition) terrain-distance)))
             (assoc :burn-probability    local-burn-probability))
         burn-vector)))
   burn-vectors))

(defn transition-burn-vectors
  "transition vectors if they cross into the next cell"
  [{:keys [num-rows num-cols] :as inputs}
   {:keys
    [travel-lines-matrix fuel-model-matrix fire-spread-matrix modified-time-matrix max-spread-rate-matrix
     max-spread-direction-matrix eccentricity-matrix] :as matrices}
   global-clock
   timestep
   burn-vectors]
  (mapv
   (fn [burn-vector]
     (let [i                   (long (:i burn-vector))
           j                   (long (:j burn-vector))
           direction           (double (:direction burn-vector))
           burn-probability    (double (:burn-probability burn-vector))
           fractional-distance (double (:fractional-distance burn-vector))]
       (if (>= fractional-distance 1.0)
         ;;transition
         (do
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
             (if (burnable-cell? fuel-model-matrix fire-spread-matrix burn-probability
                                 num-rows num-cols new-i new-j)
               ;; Neighbor is burnable: transition burn vector to next cell
               (let [global-clock          (double global-clock)
                     timestep              (double timestep)
                     ^double modified-time (t/mget modified-time-matrix new-i new-j)
                     new-clock             (+ global-clock timestep)
                     new-hour?             (> (+ (rem global-clock 60.0) timestep) 60.0)]
                 (when (or (zero? modified-time)
                           (and new-hour? (> new-clock modified-time)))
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
                 (let [max-spread-rate         (t/mget max-spread-rate-matrix new-i new-j)
                       max-spread-direction    (t/mget max-spread-direction-matrix new-i new-j)
                       eccentricity            (t/mget eccentricity-matrix new-i new-j)
                       new-spread-rate         (compute-spread-rate max-spread-rate max-spread-direction eccentricity direction)
                       new-terrain-distance    (compute-terrain-distance inputs new-i new-j direction)
                       new-fractional-distance (- fractional-distance 1.0)]
                   (->BurnVector new-i new-j direction new-spread-rate new-terrain-distance
                                 new-fractional-distance burn-probability)))
               ;; Neighbor not burnable: return current burn vector unchanged
               burn-vector)))
         ;; fractional distance < 1.0: return current burn vector unchanged
         burn-vector)))
   burn-vectors))

(defn create-burn-vectors
  ;; Create child burn-vectors for any burn vecotors with fractional distance >= 0.5
  [])

#_(defn promote-burn-vectors []
    (if (< new-fractional-distance 1.0)
      [(conj! new-acc-burn-vectors (->BurnVector i j direction new-spread-rate terrain-distance
                                                 new-fractional-distance burn-probability))
       new-acc-ignited-cells]
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
        (if (burnable-cell? fuel-model-matrix fire-spread-matrix burn-probability
                            num-rows num-cols new-i new-j)
          (let [^double modified-time (t/mget modified-time-matrix new-i new-j)]
            ;; FIXME: Should I set the fire-spread-matrix with my burn-probability to stop other vectors?
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
            (when (or (zero? modified-time)
                      (and new-hour? (> new-clock modified-time)))
              (compute-max-in-situ-values! inputs matrices new-clock new-i new-j))
            (let [max-spread-rate                    (t/mget max-spread-rate-matrix new-i new-j)
                  max-spread-direction               (t/mget max-spread-direction-matrix new-i new-j)
                  eccentricity                       (t/mget eccentricity-matrix new-i new-j)
                  new-spread-rate                    (compute-spread-rate max-spread-rate
                                                                          max-spread-direction
                                                                          eccentricity
                                                                          direction)
                  new-terrain-distance               (compute-terrain-distance inputs new-i new-j direction)
                  new-fractional-distance            (- new-fractional-distance 1.0)
                  new-acc-burn-vectors               (conj! new-acc-burn-vectors
                                                            (->BurnVector new-i new-j direction new-spread-rate new-terrain-distance
                                                                          new-fractional-distance burn-probability))
                  ^double new-local-burn-probability (t/mget fire-spread-matrix new-i new-j)
                  ^double new-local-burn-time        (t/mget burn-time-matrix new-i new-j)]
              (if (>= new-fractional-distance 0.5)
                (let [burn-time (-> 1.5
                                    (- fractional-distance)
                                    (/ fractional-distance-delta)
                                    (* timestep)
                                    (+ global-clock))]
                  (if (> burn-probability new-local-burn-probability)
                    (do
                      (t/mset! fire-spread-matrix new-i new-j burn-probability)
                      (t/mset! burn-time-matrix new-i new-j burn-time)
                      (if (> new-local-burn-time global-clock)
                        [new-acc-burn-vectors new-acc-ignited-cells]
                        [(create-new-burn-vectors! new-acc-burn-vectors inputs matrices burn-probability new-i new-j)
                         (if spotting
                           (conj! new-acc-ignited-cells [new-i new-j])
                           new-acc-ignited-cells)]))
                    (if (and (= burn-probability new-local-burn-probability)
                             (< burn-time new-local-burn-time))
                      (do
                        (t/mset! burn-time-matrix new-i new-j burn-time)
                        [new-acc-burn-vectors new-acc-ignited-cells])
                      [new-acc-burn-vectors new-acc-ignited-cells])))
                [new-acc-burn-vectors new-acc-ignited-cells])))
          [new-acc-burn-vectors new-acc-ignited-cells])))
    )


#_(defn- progress-burn-vectors!
    [{:keys [num-rows num-cols fuel-model-matrix spotting] :as inputs}
     {:keys
      [fire-spread-matrix modified-time-matrix max-spread-rate-matrix
       max-spread-direction-matrix travel-lines-matrix burn-time-matrix
       eccentricity-matrix] :as matrices}
     burn-vectors
     global-clock
     timestep]
    (let [global-clock (double global-clock)
          timestep     (double timestep)
          new-clock    (+ global-clock timestep)
          new-hour?    (> (+ (rem global-clock 60.0) timestep) 60.0)]
      ;;TODO insert promotion code that returns the same burn vectors with updated fractional-distance
      (reduce (fn [[acc-burn-vectors acc-ignited-cells] burn-vector]
                (let [i                              (long (:i burn-vector))
                      j                              (long (:j burn-vector))
                      direction                      (double (:direction burn-vector))
                      spread-rate                    (double (:spread-rate burn-vector))
                      terrain-distance               (double (:terrain-distance burn-vector))
                      fractional-distance            (double (:fractional-distance burn-vector))
                      burn-probability               (double (:burn-probability burn-vector))
                      ^double local-burn-probability (t/mget fire-spread-matrix i j)
                      ^double local-burn-time        (t/mget burn-time-matrix i j)
                      ;; fractional-distance            (if (> burn-probability local-burn-probability)
                      ;;                                  fractional-distance
                      ;;                                  (+ 0.5 (/ (* spread-rate (- global-clock local-burn-time)) terrain-distance)))
                      (- 10 12)
                      ;; burn-probability               (max burn-probability local-burn-probability)
                      new-spread-rate                (if new-hour? ;TODO don't need to calculate this here. Move to where new burn vectors are created in the same cell
                                                       (calc-new-spread-rate! inputs matrices new-clock direction i j)
                                                       spread-rate)
                      fractional-distance-delta      (/ (* spread-rate timestep) terrain-distance)
                      new-fractional-distance        (+ fractional-distance fractional-distance-delta)
                      new-acc-burn-vectors           (if (and (< fractional-distance 0.5)
                                                              (>= new-fractional-distance 0.5)) ; crosssing the center of a cell
                                                       (let [burn-time (-> 0.5
                                                                           (- fractional-distance)
                                                                           (/ fractional-distance-delta)
                                                                           (* timestep)
                                                                           (+ global-clock))]
                                                         (if (> burn-probability local-burn-probability)
                                                           (do
                                                             (t/mset! fire-spread-matrix i j burn-probability)
                                                             (t/mset! burn-time-matrix i j burn-time)
                                                             (if (> local-burn-time global-clock)
                                                               acc-burn-vectors
                                                               (create-new-burn-vectors! acc-burn-vectors inputs matrices burn-probability i j)))
                                                           (if (and (= burn-probability local-burn-probability)
                                                                    (< burn-time local-burn-time))
                                                             (do
                                                               (t/mset! burn-time-matrix i j burn-time)
                                                               acc-burn-vectors)
                                                             acc-burn-vectors)))
                                                       acc-burn-vectors)
                      new-acc-ignited-cells          (if (and spotting
                                                              ;; (> burn-probability local-burn-probability)
                                                              (<= local-burn-time global-clock) ; first to cross center of the cell this timestep
                                                              (< fractional-distance 0.5)
                                                              (>= new-fractional-distance 0.5))
                                                       (conj! acc-ignited-cells [i j])
                                                       acc-ignited-cells)]
                  (if (< new-fractional-distance 1.0)
                    [(conj! new-acc-burn-vectors (->BurnVector i j direction new-spread-rate terrain-distance
                                                               new-fractional-distance burn-probability))
                     new-acc-ignited-cells]
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
                      (if (burnable-cell? fuel-model-matrix fire-spread-matrix burn-probability
                                          num-rows num-cols new-i new-j)
                        (let [^double modified-time (t/mget modified-time-matrix new-i new-j)]
                          ;; FIXME: Should I set the fire-spread-matrix with my burn-probability to stop other vectors?
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
                          (when (or (zero? modified-time)
                                    (and new-hour? (> new-clock modified-time)))
                            (compute-max-in-situ-values! inputs matrices new-clock new-i new-j))
                          (let [max-spread-rate                    (t/mget max-spread-rate-matrix new-i new-j)
                                max-spread-direction               (t/mget max-spread-direction-matrix new-i new-j)
                                eccentricity                       (t/mget eccentricity-matrix new-i new-j)
                                new-spread-rate                    (compute-spread-rate max-spread-rate
                                                                                        max-spread-direction
                                                                                        eccentricity
                                                                                        direction)
                                new-terrain-distance               (compute-terrain-distance inputs new-i new-j direction)
                                new-fractional-distance            (- new-fractional-distance 1.0)
                                new-acc-burn-vectors               (conj! new-acc-burn-vectors
                                                                          (->BurnVector new-i new-j direction new-spread-rate new-terrain-distance
                                                                                        new-fractional-distance burn-probability))
                                ^double new-local-burn-probability (t/mget fire-spread-matrix new-i new-j)
                                ^double new-local-burn-time        (t/mget burn-time-matrix new-i new-j)]
                            (if (>= new-fractional-distance 0.5)
                              (let [burn-time (-> 1.5
                                                  (- fractional-distance)
                                                  (/ fractional-distance-delta)
                                                  (* timestep)
                                                  (+ global-clock))]
                                (if (> burn-probability new-local-burn-probability)
                                  (do
                                    (t/mset! fire-spread-matrix new-i new-j burn-probability)
                                    (t/mset! burn-time-matrix new-i new-j burn-time)
                                    (if (> new-local-burn-time global-clock)
                                      [new-acc-burn-vectors new-acc-ignited-cells]
                                      [(create-new-burn-vectors! new-acc-burn-vectors inputs matrices burn-probability new-i new-j)
                                       (if spotting
                                         (conj! new-acc-ignited-cells [new-i new-j])
                                         new-acc-ignited-cells)]))
                                  (if (and (= burn-probability new-local-burn-probability)
                                           (< burn-time new-local-burn-time))
                                    (do
                                      (t/mset! burn-time-matrix new-i new-j burn-time)
                                      [new-acc-burn-vectors new-acc-ignited-cells])
                                    [new-acc-burn-vectors new-acc-ignited-cells])))
                              [new-acc-burn-vectors new-acc-ignited-cells])))
                        [new-acc-burn-vectors new-acc-ignited-cells])))))
              [(transient []) (transient [])]
              burn-vectors)))

(def ^:private directions [0.0 45.0 90.0 135.0 180.0 225.0 270.0 315.0])

;; 933ns (with all stub compute-* functions)
(defn- generate-burn-vectors!
  [inputs
   {:keys
    [max-spread-rate-matrix max-spread-direction-matrix eccentricity-matrix] :as matrices}
   ignited-cells clock]
  (persistent!
   (reduce (fn [acc [i j]]
             (compute-max-in-situ-values! inputs matrices clock i j)
             (let [max-spread-rate      (t/mget max-spread-rate-matrix i j)
                   max-spread-direction (t/mget max-spread-direction-matrix i j)
                   eccentricity         (t/mget eccentricity-matrix i j)]
               (reduce (fn [acc direction]
                         (let [spread-rate      (compute-spread-rate max-spread-rate
                                                                     max-spread-direction
                                                                     eccentricity
                                                                     direction)
                               terrain-distance (compute-terrain-distance inputs i j direction)]
                           (conj! acc (->BurnVector i j direction spread-rate terrain-distance 0.5 1.0))))
                       acc
                       directions)))
           (transient [])
           ignited-cells)))

(defn- run-loop
  [{:keys [cell-size ignition-start-time max-runtime] :as inputs} matrices ignited-cells]
  (let [cell-size           (double cell-size)
        max-runtime         (double max-runtime)
        ignition-start-time (double ignition-start-time)
        ignition-stop-time  (+ ignition-start-time max-runtime)]
    (loop [global-clock   ignition-start-time
           burn-vectors   (generate-burn-vectors! inputs matrices ignited-cells global-clock) ;TODO check only direction you can creat burn vectors in
           spot-ignitions {}
           spot-count     0]
      (if (and (< global-clock ignition-stop-time)
               (or (seq burn-vectors) (seq spot-ignitions)))
        (let [timestep                     (min (compute-dt cell-size burn-vectors)
                                                (- ignition-stop-time global-clock))
              [burn-vectors ignited-cells] (grow-burn-vectors! inputs matrices burn-vectors
                                                               global-clock timestep)
              [updated-burn-vectors
               spot-ignite-now-count
               spot-ignite-later]          (compute-spot-burn-vectors! inputs
                                                                       matrices
                                                                       burn-vectors
                                                                       spot-ignitions
                                                                       (persistent! ignited-cells)
                                                                       global-clock)
              updated-burn-vectors'        (persistent! updated-burn-vectors)]
          (recur (+ global-clock timestep)
                 updated-burn-vectors'
                 spot-ignite-later
                 (+ spot-count ^long spot-ignite-now-count)))
        {:exit-condition             (if (>= global-clock ignition-stop-time) :max-runtime-reached :no-burnable-fuels)
         :global-clock               global-clock
         :burn-time-matrix           (matrices :burn-time-matrix)
         :fire-line-intensity-matrix (matrices :fire-line-intensity-matrix)
         :fire-spread-matrix         (matrices :fire-spread-matrix)
         :fire-type-matrix           (matrices :fire-type-matrix)
         :flame-length-matrix        (matrices :flame-length-matrix)
         :spot-matrix                (matrices :spot-matrix)
         :spread-rate-matrix         (matrices :spread-rate-matrix)
         :crown-fire-count           0 ; FIXME: Calculate using tensor ops
         :spot-count                 spot-count})))) ; FIXME: Calculate using tensor ops or spot-ignitions

;;-----------------------------------------------------------------------------
;; Main Simulation Entry Point - Dispatches to Point/Perimeter Ignition
;;-----------------------------------------------------------------------------

;; FIXME: Move this multimethod check into run-simulations to avoid running it in every thread
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
     :travel-lines-matrix         (-> (t/new-tensor shape :datatype :short) (t/mset! i j 2r11111111))}))

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
  (let [shape               [num-rows num-cols]
        positive-burn-scar  initial-ignition-site
        negative-burn-scar  (d/clone (dfn/* -1.0 positive-burn-scar))
        burn-time-matrix    (add-ignited-cells! (d/clone negative-burn-scar)
                                                ignited-cells
                                                ignition-start-time)
        travel-lines-matrix (d/clone (d/emap (fn ^long [^double x] (if (pos? x) 2r11111111 2r00000000))
                                             :short
                                             positive-burn-scar))]
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
     :travel-lines-matrix         travel-lines-matrix}))

;; FIXME: Move this step into run-simulations to avoid running it in every thread
(defn- get-perimeter-cells
  [{:keys [num-rows num-cols initial-ignition-site fuel-model-matrix]}]
  (let [{:keys [row-idxs col-idxs]} (non-zero-indices initial-ignition-site)
        num-idxs                    (d/ecount row-idxs)]
    (loop [idx 0
           acc (transient [])]
      (if (< idx num-idxs)
        (let [i (row-idxs idx)
              j (col-idxs idx)]
          (if (burnable-neighbors? fuel-model-matrix
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
