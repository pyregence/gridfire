(ns gridfire.fire-spread-optimal
  (:require [clojure.string                :as s]
            [gridfire.common               :refer [burnable-cell?
                                                   burnable-cell?-partialed
                                                   burnable-fuel-model?
                                                   calc-fuel-moisture
                                                   compute-terrain-distance
                                                   in-bounds-optimal?
                                                   non-zero-indices]]
            [gridfire.conversion           :refer [mph->fpm hour->min min->hour]]
            [gridfire.crown-fire           :refer [crown-fire-eccentricity
                                                   crown-fire-line-intensity
                                                   cruz-crown-fire-spread
                                                   van-wagner-crown-fire-initiation?]]
            [gridfire.fuel-models-optimal  :refer [fuel-models-precomputed
                                                   moisturize]]
            [gridfire.grid-lookup          :as grid-lookup]
            [gridfire.spotting-optimal     :as spot-optimal]
            [gridfire.suppression          :as suppression]
            [gridfire.surface-fire-optimal :refer [rothermel-surface-fire-spread-no-wind-no-slope
                                                   rothermel-surface-fire-spread-max
                                                   compute-spread-rate
                                                   anderson-flame-depth
                                                   byram-fire-line-intensity
                                                   byram-flame-length
                                                   wind-adjustment-factor]]
            [tech.v3.datatype              :as d]
            [tech.v3.datatype.functional   :as dfn]
            [tech.v3.tensor                :as t])
  (:import (clojure.lang IHashEq)
           java.time.ZoneId
           (java.util ArrayList)
           java.util.Date
           (org.apache.commons.codec.digest MurmurHash3)))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(defrecord BurnVector
    [^long   i
     ^long   j
     ^double direction
     ^double fractional-distance
     ^double spread-rate
     ^double terrain-distance
     ^double burn-probability])

(defmacro fast->BurnVector
  [& args]
  `(BurnVector. ~@args))

;; Compared to keyword lookup, these accessors do less work:
;; in particular, they don't allocate a Number to box their return value,
;; and their call chain is more direct.
(defn- bv-i
  ^long [bv]
  (.-i ^BurnVector bv))

(defn- bv-j
  ^long [bv]
  (.-j ^BurnVector bv))

(defn- bv-burn-probability
  ^double [bv]
  (.-burn-probability ^BurnVector bv))

(defn- bv-direction
  ^double [bv]
  (.-direction ^BurnVector bv))

(defn- bv-fractional-distance
  ^double [bv]
  (.-fractional-distance ^BurnVector bv))

(defn- bv-spread-rate
  ^double [bv]
  (.-spread-rate ^BurnVector bv))

(defn- bv-terrain-distance
  ^double [bv]
  (.-terrain-distance ^BurnVector bv))

(comment
  ;; Keyword accessors re-allocate a boxed Number at every call:
  (identical? (:i burn-vector)
              (:i burn-vector))
  ;=> false
  *e)

(comment
  (require 'clj-java-decompiler.core)

  (clj-java-decompiler.core/decompile
   (defn- bv-burn-probability
     ^double [bv]
     (.-burn-probability ^BurnVector bv)))
  ;// Decompiling class: gridfire/fire_spread_optimal$bv_burn_probability__48406
  ;package gridfire;
  ;
  ;import clojure.lang.*;
  ;import gridfire.fire_spread_optimal.*;
  ;
  ;public final class fire_spread_optimal$bv_burn_probability__48406 extends AFunction implements OD
  ;{
  ;    public static double invokeStatic(final Object bv) {
  ;        return ((BurnVector)bv).burn_probability;
  ;    }
  ;
  ;    @Override
  ;    public Object invoke(final Object bv) {
  ;        return invokeStatic(bv);
  ;    }
  ;
  ;    @Override
  ;    public final double invokePrim(final Object bv) {
  ;        return invokeStatic(bv);
  ;    }
  ;}

  *e)


;;-----------------------------------------------------------------------------
;; Fire spread
;;-----------------------------------------------------------------------------

(defn- diagonal? [direction]
  (case direction
    0.0   false
    45.0  true
    90.0  false
    135.0 true
    180.0 false
    225.0 true
    270.0 false
    315.0 true))

(defn- find-max-spread-rate ^double
  [^double max-spread-rate burn-vector]
  (max max-spread-rate (bv-spread-rate burn-vector)))

(comment

  (clj-java-decompiler.core/decompile
   (defn- find-max-spread-rate ^double
     [^double max-spread-rate burn-vector]
     (max max-spread-rate ^double (bv-spread-rate burn-vector))))

  *e)

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
          ^double aspect     (grid-lookup/double-at get-aspect i j)
          ^double slope      (grid-lookup/double-at get-slope i j)
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

(deftype RothFWOArgs
  ;; Custom datatype representing the argument to rothermel-fast-wrapper-optimal,
  ;; yielding much more efficient memoization.
  [^double fuel-model-number
   ^double fuel-moisture-dead-1hr
   ^double fuel-moisture-dead-10hr
   ^double fuel-moisture-dead-100hr
   ^double fuel-moisture-dead-herbaceous
   ^double fuel-moisture-live-herbaceous
   ^double fuel-moisture-live-woody
   ^boolean grass-suppression?]
  Object
  (equals [_this rothFWOArgs2]
    (let [^RothFWOArgs rothFWOArgs2 rothFWOArgs2]
      (and (= fuel-model-number (.-fuel-model-number rothFWOArgs2))
           (= fuel-moisture-dead-1hr (.-fuel-moisture-dead-1hr rothFWOArgs2))
           (= fuel-moisture-dead-10hr (.-fuel-moisture-dead-10hr rothFWOArgs2))
           (= fuel-moisture-dead-100hr (.-fuel-moisture-dead-100hr rothFWOArgs2))
           (= fuel-moisture-dead-herbaceous (.-fuel-moisture-dead-herbaceous rothFWOArgs2))
           (= fuel-moisture-live-herbaceous (.-fuel-moisture-live-herbaceous rothFWOArgs2))
           (= fuel-moisture-live-woody (.-fuel-moisture-live-woody rothFWOArgs2))
           (= grass-suppression? (.-grass-suppression? rothFWOArgs2)))))
  (hashCode [this] (.hasheq ^IHashEq this))
  IHashEq
  (hasheq [_this]
   (-> (MurmurHash3/hash32 (Double/doubleToLongBits fuel-model-number))
       (long) (MurmurHash3/hash32 (Double/doubleToLongBits fuel-moisture-dead-1hr))
       (long) (MurmurHash3/hash32 (Double/doubleToLongBits fuel-moisture-dead-10hr))
       (long) (MurmurHash3/hash32 (Double/doubleToLongBits fuel-moisture-dead-100hr))
       (long) (MurmurHash3/hash32 (Double/doubleToLongBits fuel-moisture-dead-herbaceous))
       (long) (MurmurHash3/hash32 (Double/doubleToLongBits fuel-moisture-live-herbaceous))
       (long) (MurmurHash3/hash32 (Double/doubleToLongBits fuel-moisture-live-woody))
       (long) (MurmurHash3/hash32 (Double/doubleToLongBits (if grass-suppression? 1 0))))))

(comment

  (clj-java-decompiler.core/decompile
   (defrecord MyRecord [^long my-long ^double my-double]))

  *e)

(defn ^:redef rothermel-fast-wrapper-optimal2
  [^RothFWOArgs rothFWOArgs]
  (rothermel-fast-wrapper-optimal (.-fuel-model-number rothFWOArgs)
                                  [(.-fuel-moisture-dead-1hr rothFWOArgs)
                                   (.-fuel-moisture-dead-10hr rothFWOArgs)
                                   (.-fuel-moisture-dead-100hr rothFWOArgs)
                                   (.-fuel-moisture-dead-herbaceous rothFWOArgs)
                                   (.-fuel-moisture-live-herbaceous rothFWOArgs)
                                   (.-fuel-moisture-live-woody rothFWOArgs)]
                                  (.-grass-suppression? rothFWOArgs)))

(defn memoize-1arg
  "Like clojure.core/memoize, but optimized for 1-argument functions."
  [f]
  (let [mem       (atom {})
        not-found (Object.)]
    (fn mem-wrapper [arg]
      (let [v (get @mem arg not-found)]
        (if (identical? v not-found)
          (let [v (f arg)]
            (swap! mem assoc arg v)
            v)
          v)))))

(defn- store-if-max!
  [matrix i j ^double new-value]
  (let [^double old-value (t/mget matrix i j)]
    (when (> new-value old-value)
      (t/mset! matrix i j new-value))))

(defn- lookup-spread-rate-adjustment
  ^double [fuel-number->spread-rate-adjustment-array-lookup fuel-model]
  (aget (doubles fuel-number->spread-rate-adjustment-array-lookup) fuel-model))

(defn- compute-max-in-situ-values!
  [inputs matrices band i j]
  (let [compute-directional-values?                      (:compute-directional-values? inputs)
        get-slope                                        (:get-slope inputs)
        get-aspect                                       (:get-aspect inputs)
        get-canopy-cover                                 (:get-canopy-cover inputs)
        get-canopy-height                                (:get-canopy-height inputs)
        get-canopy-base-height                           (:get-canopy-base-height inputs)
        get-crown-bulk-density                           (:get-crown-bulk-density inputs)
        get-fuel-model                                   (:get-fuel-model inputs)
        get-temperature                                  (:get-temperature inputs)
        get-relative-humidity                            (:get-relative-humidity inputs)
        get-wind-speed-20ft                              (:get-wind-speed-20ft inputs)
        get-wind-from-direction                          (:get-wind-from-direction inputs)
        get-fuel-moisture-dead-1hr                       (:get-fuel-moisture-dead-1hr inputs)
        get-fuel-moisture-dead-10hr                      (:get-fuel-moisture-dead-10hr inputs)
        get-fuel-moisture-dead-100hr                     (:get-fuel-moisture-dead-100hr inputs)
        get-fuel-moisture-live-herbaceous                (:get-fuel-moisture-live-herbaceous inputs)
        get-fuel-moisture-live-woody                     (:get-fuel-moisture-live-woody inputs)
        get-foliar-moisture                              (:get-foliar-moisture inputs)
        fuel-number->spread-rate-adjustment-array-lookup (:fuel-number->spread-rate-adjustment-array-lookup inputs)
        crowning-disabled?                               (:crowning-disabled? inputs)
        ellipse-adjustment-factor                        (:ellipse-adjustment-factor inputs)
        grass-suppression?                               (:grass-suppression? inputs)
        max-spread-rate-matrix                           (:max-spread-rate-matrix matrices)
        max-spread-direction-matrix                      (:max-spread-direction-matrix matrices)
        spread-rate-matrix                               (:spread-rate-matrix matrices)
        flame-length-matrix                              (:flame-length-matrix matrices)
        fire-line-intensity-matrix                       (:fire-line-intensity-matrix matrices)
        fire-type-matrix                                 (:fire-type-matrix matrices)
        modified-time-matrix                             (:modified-time-matrix matrices)
        eccentricity-matrix                              (:eccentricity-matrix matrices)
        residence-time-matrix                            (:residence-time-matrix matrices)
        reaction-intensity-matrix                        (:reaction-intensity-matrix matrices)
        band                                             (long band)
        i                                                (long i)
        j                                                (long j)
        slope                                            (grid-lookup/double-at get-slope i j)
        aspect                                           (grid-lookup/double-at get-aspect i j)
        canopy-cover                                     (grid-lookup/double-at get-canopy-cover i j)
        canopy-height                                    (grid-lookup/double-at get-canopy-height i j)
        canopy-base-height                               (grid-lookup/double-at get-canopy-base-height i j)
        crown-bulk-density                               (grid-lookup/double-at get-crown-bulk-density i j)
        fuel-model                                       (grid-lookup/double-at get-fuel-model i j)
        temperature                                      (grid-lookup/double-at get-temperature band i j)
        relative-humidity                                (grid-lookup/double-at get-relative-humidity band i j)
        wind-speed-20ft                                  (grid-lookup/double-at get-wind-speed-20ft band i j)
        wind-from-direction                              (grid-lookup/double-at get-wind-from-direction band i j)
        fuel-moisture-dead-1hr                           (if get-fuel-moisture-dead-1hr
                                                           (grid-lookup/double-at get-fuel-moisture-dead-1hr band i j)
                                                           (calc-fuel-moisture relative-humidity temperature :dead :1hr))
        fuel-moisture-dead-10hr                          (if get-fuel-moisture-dead-10hr
                                                           (grid-lookup/double-at get-fuel-moisture-dead-10hr band i j)
                                                           (calc-fuel-moisture relative-humidity temperature :dead :10hr))
        fuel-moisture-dead-100hr                         (if get-fuel-moisture-dead-100hr
                                                           (grid-lookup/double-at get-fuel-moisture-dead-100hr band i j)
                                                           (calc-fuel-moisture relative-humidity temperature :dead :100hr))
        fuel-moisture-live-herbaceous                    (if get-fuel-moisture-live-herbaceous
                                                           (grid-lookup/double-at get-fuel-moisture-live-herbaceous band i j)
                                                           (calc-fuel-moisture relative-humidity temperature :live :herbaceous))
        fuel-moisture-live-woody                         (if get-fuel-moisture-live-woody
                                                           (grid-lookup/double-at get-fuel-moisture-live-woody band i j)
                                                           (calc-fuel-moisture relative-humidity temperature :live :woody))
        foliar-moisture                                  (grid-lookup/double-at get-foliar-moisture band i j)
        surface-fire-min                                 (rothermel-fast-wrapper-optimal2 (RothFWOArgs. fuel-model
                                                                                                        fuel-moisture-dead-1hr
                                                                                                        fuel-moisture-dead-10hr
                                                                                                        fuel-moisture-dead-100hr
                                                                                                        0.0 ; fuel-moisture-dead-herbaceous
                                                                                                        fuel-moisture-live-herbaceous
                                                                                                        fuel-moisture-live-woody
                                                                                                        grass-suppression?))
        midflame-wind-speed                              (mph->fpm
                                                          (* wind-speed-20ft
                                                             (wind-adjustment-factor ^double (:fuel-bed-depth surface-fire-min)
                                                                                     canopy-height
                                                                                     canopy-cover)))
        spread-rate-adjustment                           (some-> fuel-number->spread-rate-adjustment-array-lookup
                                                                 (lookup-spread-rate-adjustment fuel-model))
        surface-fire-max                                 (rothermel-surface-fire-spread-max surface-fire-min
                                                                                            midflame-wind-speed
                                                                                            wind-from-direction
                                                                                            slope
                                                                                            aspect
                                                                                            ellipse-adjustment-factor
                                                                                            spread-rate-adjustment)
        max-spread-rate                                  (:max-spread-rate surface-fire-max)
        max-spread-direction                             (:max-spread-direction surface-fire-max)
        eccentricity                                     (:eccentricity surface-fire-max)
        residence-time                                   (:residence-time surface-fire-min)
        reaction-intensity                               (:reaction-intensity surface-fire-min)
        max-surface-intensity                            (->> (anderson-flame-depth max-spread-rate ^double residence-time)
                                                              (byram-fire-line-intensity ^double reaction-intensity))]
    (if (and (not crowning-disabled?)
             (van-wagner-crown-fire-initiation? canopy-cover
                                                canopy-base-height
                                                foliar-moisture
                                                max-surface-intensity))
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
        (t/mset! max-spread-rate-matrix i j max-spread-rate)
        (t/mset! max-spread-direction-matrix i j max-spread-direction)
        (t/mset! eccentricity-matrix i j max-eccentricity)
        (t/mset! modified-time-matrix i j (inc band))
        (when compute-directional-values?
          (t/mset! residence-time-matrix i j residence-time)
          (t/mset! reaction-intensity-matrix i j reaction-intensity))
        (store-if-max! spread-rate-matrix i j max-spread-rate)
        (store-if-max! flame-length-matrix i j (byram-flame-length max-fire-line-intensity))
        (store-if-max! fire-line-intensity-matrix i j max-fire-line-intensity)
        (store-if-max! fire-type-matrix i j crown-type))
      (do
        (t/mset! max-spread-rate-matrix i j max-spread-rate)
        (t/mset! max-spread-direction-matrix i j max-spread-direction)
        (t/mset! eccentricity-matrix i j eccentricity)
        (t/mset! modified-time-matrix i j (inc band))
        (when compute-directional-values?
          (t/mset! residence-time-matrix i j residence-time)
          (t/mset! reaction-intensity-matrix i j reaction-intensity))
        (store-if-max! spread-rate-matrix i j max-spread-rate)
        (store-if-max! flame-length-matrix i j (byram-flame-length max-surface-intensity))
        (store-if-max! fire-line-intensity-matrix i j max-surface-intensity)
        (store-if-max! fire-type-matrix i j 1.0)))))

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

(defn make-burn-vector-constructor
  [num-rows num-cols burn-probability max-spread-rate-matrix
   max-spread-direction-matrix eccentricity-matrix fire-spread-matrix
   cell-size get-elevation i j]
  (let [i                    (long i)
        j                    (long j)
        burn-probability     (double burn-probability)
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
        (when (and (in-bounds-optimal? num-rows num-cols new-i new-j)
                   (> burn-probability ^double (t/mget fire-spread-matrix new-i new-j)))
          (let [spread-rate      (compute-spread-rate max-spread-rate
                                                      max-spread-direction
                                                      eccentricity
                                                      direction)
                terrain-distance (compute-terrain-distance cell-size get-elevation
                                                           num-rows num-cols i j new-i new-j)]
            (fast->BurnVector i j direction 0.5 spread-rate terrain-distance burn-probability)))))))

(def ^:private bits [0 1 2 3 4 5 6 7])

(defn- create-new-burn-vectors!
  [acc num-rows num-cols cell-size get-elevation travel-lines-matrix
   max-spread-rate-matrix max-spread-direction-matrix eccentricity-matrix
   fire-spread-matrix i j burn-probability]
  (let [travel-lines       (t/mget travel-lines-matrix i j)
        create-burn-vector (make-burn-vector-constructor num-rows num-cols burn-probability
                                                         max-spread-rate-matrix max-spread-direction-matrix eccentricity-matrix
                                                         fire-spread-matrix cell-size get-elevation i j)]
    (reduce (fn [acc ^long bit]
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
                                        (let [[i j]         cell
                                              [burn-time _] spot-info]
                                          (when (> band (dec ^long (t/mget modified-time-matrix i j)))
                                            (compute-max-in-situ-values! inputs matrices band i j))
                                          (t/mset! fire-spread-matrix i j 1.0) ;TODO parameterize burn-probability instead of 1.0
                                          ;; (t/mset! fire-spread-matrix i j burn-probability)
                                          (t/mset! burn-time-matrix i j burn-time)
                                          (create-new-burn-vectors! acc num-rows num-cols cell-size get-elevation
                                                                    travel-lines-matrix max-spread-rate-matrix max-spread-direction-matrix
                                                                    eccentricity-matrix fire-spread-matrix i j 1.0))) ; TODO parameterize burn-probability instead of 1.0
                                      (transient [])
                                      pruned-spot-ignite-now))]
    [spot-burn-vectors (count pruned-spot-ignite-now) pruned-spot-ignite-later (keys pruned-spot-ignite-now)]))

(defn- grow-burn-vectors!
  [matrices global-clock timestep burn-vectors]
  (let [fire-spread-matrix (:fire-spread-matrix matrices)
        fire-spread-getter (grid-lookup/tensor-cell-getter fire-spread-matrix)
        burn-time-matrix   (:burn-time-matrix matrices)
        burn-time-getter   (grid-lookup/tensor-cell-getter burn-time-matrix)
        global-clock       (double global-clock)
        timestep           (double timestep)
        ignited-cells-list (ArrayList.)]
    [(persistent!
      (reduce (fn [bvs-acc burn-vector]
                (let [i                         (bv-i burn-vector)
                      j                         (bv-j burn-vector)
                      direction                 (bv-direction burn-vector)
                      fractional-distance       (bv-fractional-distance burn-vector)
                      spread-rate               (bv-spread-rate burn-vector)
                      terrain-distance          (bv-terrain-distance burn-vector)
                      burn-probability          (bv-burn-probability burn-vector)
                      fractional-distance-delta (/ (* spread-rate timestep) terrain-distance)
                      new-fractional-distance   (+ fractional-distance fractional-distance-delta)
                      crossed-center?           (and (< fractional-distance 0.5)
                                                     (>= new-fractional-distance 0.5))
                      local-burn-time           (grid-lookup/double-at burn-time-getter i j)]
                  (when crossed-center?
                    (let [local-burn-probability (grid-lookup/double-at fire-spread-getter i j)]
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
                  (when (and crossed-center? (<= local-burn-time global-clock)) ; first to cross center of the cell this timestep
                    (.add ignited-cells-list [i j]))
                  (conj! bvs-acc (fast->BurnVector i j direction new-fractional-distance
                                                   spread-rate terrain-distance burn-probability))))
              (transient [])
              ;;      (if crossed-center? ; first to cross center of the cell this timestep
              ;;        (conj! ignited-cells [i j])
              ;;        ignited-cells)]))
              ;; [(transient []) (transient #{})]
              burn-vectors))
     (vec ignited-cells-list)]))

(defn- ignited-cells->burn-vectors
  [inputs matrices ignited-cells burn-vectors]
  (let [num-rows                    (:num-rows inputs)
        num-cols                    (:num-cols inputs)
        cell-size                   (:cell-size inputs)
        get-elevation               (:get-elevation inputs)
        fire-spread-matrix          (:fire-spread-matrix matrices)
        travel-lines-matrix         (:travel-lines-matrix matrices)
        max-spread-rate-matrix      (:max-spread-rate-matrix matrices)
        max-spread-direction-matrix (:max-spread-direction-matrix matrices)
        eccentricity-matrix         (:eccentricity-matrix matrices)]
    (persistent!
     (reduce
      (fn [acc [i j]]
        (create-new-burn-vectors! acc num-rows num-cols cell-size get-elevation travel-lines-matrix
                                  max-spread-rate-matrix max-spread-direction-matrix eccentricity-matrix
                                  fire-spread-matrix i j (t/mget fire-spread-matrix i j)))
      (transient burn-vectors)
      ignited-cells))))

(defn- update-directional-magnitude-values!
  [matrices direction spread-rate i j]
  (let [x-magnitude-sum-matrix (:x-magnitude-sum-matrix matrices)
        y-magnitude-sum-matrix (:y-magnitude-sum-matrix matrices)
        spread-rate-sum-matrix (:spread-rate-sum-matrix matrices)
        direction              (double direction)
        spread-rate            (double spread-rate)
        cur-x                  (double (t/mget x-magnitude-sum-matrix i j))
        cur-y                  (double (t/mget y-magnitude-sum-matrix i j))
        cur-spread-rate        (double (t/mget spread-rate-sum-matrix i j))]
    (t/mset! x-magnitude-sum-matrix i j (+ cur-x
                                           (-> direction
                                               (Math/toRadians)
                                               (Math/cos)
                                               (* spread-rate))))
    (t/mset! y-magnitude-sum-matrix i j  (+ cur-y
                                            (-> direction
                                                (Math/toRadians)
                                                (Math/sin)
                                                (* spread-rate))))
    (t/mset! spread-rate-sum-matrix i j (+ cur-spread-rate spread-rate))))

(defn- promote-burn-vectors
  [inputs matrices global-clock new-clock max-fractional-distance burn-vectors]
  (let [num-rows                    (:num-rows inputs)
        num-cols                    (:num-cols inputs)
        get-fuel-model              (:get-fuel-model inputs)
        compute-directional-values? (:compute-directional-values? inputs)
        fire-spread-matrix          (grid-lookup/tensor-cell-getter (:fire-spread-matrix matrices))
        burn-time-matrix            (grid-lookup/tensor-cell-getter (:burn-time-matrix matrices))
        travel-lines-matrix         (:travel-lines-matrix matrices)
        global-clock                (double global-clock)
        new-clock                   (double new-clock)
        max-fractional-distance     (double max-fractional-distance)
        cell-burnable?              (burnable-cell?-partialed get-fuel-model (:fire-spread-matrix matrices) num-rows num-cols)]
    ;(sc.api/brk)
    (comment
      (identical? (d/->double-array fire-spread-matrix)
                  (d/->double-array fire-spread-matrix))
      ;;true

      ;; Proof of concept of how primitive array lookup can be faster than t/mget:
      (require '[criterium.core :as bench])
      (let [{:keys [i j]} (first burn-vectors)
            ;i (long 792)
            ;j (long 800)
            ;i (long i)
            ;j (long j)
            mget-fire-spread-matrix     (let [^doubles arr (d/->double-array fire-spread-matrix)
                                              [n-rows n-cols] (d/shape fire-spread-matrix)
                                              n-rows (int n-rows)
                                              n-cols (int n-cols)]
                                          (fn ^double [^long i ^long j]
                                            (aget arr (unchecked-add-int (unchecked-multiply-int n-cols (int i))
                                                                         (int j)))))]
        (bench/quick-bench (dotimes [_ 1e3] (pos? (double (t/mget fire-spread-matrix i j)))))
        ;Evaluation count : 100194 in 6 samples of 16699 calls.
        ;             Execution time mean : 6.035277 µs
        ;    Execution time std-deviation : 129.746793 ns
        ;   Execution time lower quantile : 5.968026 µs ( 2.5%)
        ;   Execution time upper quantile : 6.259654 µs (97.5%)
        ;                   Overhead used : 8.814821 ns
        ;

        (bench/quick-bench (dotimes [_ 1e3] (pos? (.invokePrim ^clojure.lang.IFn$LLD mget-fire-spread-matrix i j)))))
      ;Evaluation count : 12266430 in 6 samples of 2044405 calls.
      ;             Execution time mean : 40.156598 ns
      ;    Execution time std-deviation : 0.401348 ns
      ;   Execution time lower quantile : 39.856165 ns ( 2.5%)
      ;   Execution time upper quantile : 40.619994 ns (97.5%)
      ;                   Overhead used : 8.814821 ns
      *e)
    (persistent!
     (reduce
      (fn [acc burn-vector]
        (let [i                      (bv-i burn-vector)
              j                      (bv-j burn-vector)
              burn-probability       (bv-burn-probability burn-vector)
              local-burn-probability (double (grid-lookup/double-at #_t/mget fire-spread-matrix i j))
              local-burn-time        (double (grid-lookup/double-at #_t/mget burn-time-matrix i j))]
          (if (and (> local-burn-time global-clock) ; cell was ignited this timestep
                   (<= burn-probability local-burn-probability))
            (let [direction   (bv-direction burn-vector)
                  spread-rate (bv-spread-rate burn-vector)
                  new-i       (case direction
                                0.0   (- i 1)
                                45.0  (- i 1)
                                90.0  i
                                135.0 (+ i 1)
                                180.0 (+ i 1)
                                225.0 (+ i 1)
                                270.0 i
                                315.0 (- i 1))
                  new-j       (case direction
                                0.0   j
                                45.0  (+ j 1)
                                90.0  (+ j 1)
                                135.0 (+ j 1)
                                180.0 j
                                225.0 (- j 1)
                                270.0 (- j 1)
                                315.0 (- j 1))]
              (when compute-directional-values?
                (update-directional-magnitude-values! matrices direction spread-rate i j))
              (if (and (.invokePrim ^clojure.lang.IFn$LLDO cell-burnable? new-i new-j burn-probability)
                       (or (not (diagonal? direction))
                           (burnable-fuel-model? (grid-lookup/double-at get-fuel-model new-i j))
                           (burnable-fuel-model? (grid-lookup/double-at get-fuel-model i new-j))))
                (let [spread-rate             (double (bv-spread-rate burn-vector))
                      terrain-distance        (double (bv-terrain-distance burn-vector))
                      dt-after-ignition       (- new-clock local-burn-time)
                      new-fractional-distance (min max-fractional-distance
                                                   (+ 0.5 (/ (* spread-rate dt-after-ignition) terrain-distance)))
                      new-spread-rate         (if (< new-fractional-distance max-fractional-distance)
                                                spread-rate
                                                (/ (* (- new-fractional-distance 0.5) terrain-distance) dt-after-ignition))]
                  (conj! acc (fast->BurnVector i j direction new-fractional-distance new-spread-rate terrain-distance local-burn-probability)))
                (do
                  (t/mset! travel-lines-matrix i j ; TODO make into function
                           (bit-clear (long (t/mget travel-lines-matrix i j))
                                      (long (case direction
                                              0.0 0
                                              45.0 1
                                              90.0 2
                                              135.0 3
                                              180.0 4
                                              225.0 5
                                              270.0 6
                                              315.0 7))))
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
        fire-spread-getter          (grid-lookup/tensor-cell-getter fire-spread-matrix)
        modified-time-matrix        (:modified-time-matrix matrices)
        modified-time-getter        (grid-lookup/tensor-cell-getter modified-time-matrix)
        max-spread-rate-matrix      (:max-spread-rate-matrix matrices)
        max-spread-rate-getter      (grid-lookup/tensor-cell-getter max-spread-rate-matrix)
        max-spread-direction-matrix (:max-spread-direction-matrix matrices)
        max-spread-direction-getter (grid-lookup/tensor-cell-getter max-spread-direction-matrix)
        eccentricity-matrix         (:eccentricity-matrix matrices)
        eccentricity-getter         (grid-lookup/tensor-cell-getter eccentricity-matrix)
        burn-time-matrix            (:burn-time-matrix matrices)
        burn-time-getter            (grid-lookup/tensor-cell-getter burn-time-matrix)
        band                        (long band)
        global-clock                (double global-clock)
        new-clock                   (double new-clock)
        max-fractional-distance     (double max-fractional-distance)
        ignited-cells-list          (ArrayList.)]
    [(persistent!
      (reduce
       (fn [burn-vectors burn-vector]
         (let [fractional-distance (double (:fractional-distance burn-vector))]
           (if (< fractional-distance 1.0)
             (conj! burn-vectors burn-vector)
             (let [i                (long (:i burn-vector))
                   j                (long (:j burn-vector))
                   direction        (double (:direction burn-vector))
                   burn-probability (double (:burn-probability burn-vector))
                   direction-bit    (case direction
                                      0.0 0
                                      45.0 1
                                      90.0 2
                                      135.0 3
                                      180.0 4
                                      225.0 5
                                      270.0 6
                                      315.0 7)]
               ;; TODO make into function
               (as-> (t/mget travel-lines-matrix i j) $
                     (bit-clear $ direction-bit)
                     (t/mset! travel-lines-matrix i j $))
               (let [new-i (long (case direction
                                   0.0 (- i 1)
                                   45.0 (- i 1)
                                   90.0 i
                                   135.0 (+ i 1)
                                   180.0 (+ i 1)
                                   225.0 (+ i 1)
                                   270.0 i
                                   315.0 (- i 1)))
                     new-j (long (case direction
                                   0.0 j
                                   45.0 (+ j 1)
                                   90.0 (+ j 1)
                                   135.0 (+ j 1)
                                   180.0 j
                                   225.0 (- j 1)
                                   270.0 (- j 1)
                                   315.0 (- j 1)))]
                 (if (and (burnable-cell? get-fuel-model fire-spread-matrix burn-probability
                                          num-rows num-cols new-i new-j)
                          (or (not (diagonal? direction))
                              (burnable-fuel-model? (grid-lookup/double-at get-fuel-model new-i j))
                              (burnable-fuel-model? (grid-lookup/double-at get-fuel-model i new-j))))
                   (do
                     (when (> band (dec (long (grid-lookup/double-at modified-time-getter new-i new-j)) #_(t/mget modified-time-matrix new-i new-j)))
                       ;; vector is first in this timestep to compute
                       (compute-max-in-situ-values! inputs matrices band new-i new-j))
                     ;; TODO move to function
                     (as-> (t/mget travel-lines-matrix new-i new-j) $
                           (bit-set $ direction-bit)
                           (t/mset! travel-lines-matrix new-i new-j $))
                     (let [spread-rate             (bv-spread-rate burn-vector)
                           terrain-distance        (bv-terrain-distance burn-vector)
                           max-spread-rate         (grid-lookup/double-at max-spread-rate-getter new-i new-j) #_(double (t/mget max-spread-rate-matrix new-i new-j))
                           max-spread-direction    (grid-lookup/double-at max-spread-direction-getter new-i new-j) #_(double (t/mget max-spread-direction-matrix new-i new-j))
                           eccentricity            (grid-lookup/double-at eccentricity-getter new-i new-j)#_(double (t/mget eccentricity-matrix new-i new-j))
                           new-spread-rate         (compute-spread-rate max-spread-rate
                                                                        max-spread-direction
                                                                        eccentricity
                                                                        direction)
                           ;; TODO move case form to functions
                           new-terrain-distance    (double (compute-terrain-distance cell-size get-elevation num-rows num-cols new-i new-j
                                                                                     (case direction
                                                                                       0.0 (- new-i 1)
                                                                                       45.0 (- new-i 1)
                                                                                       90.0 new-i
                                                                                       135.0 (+ new-i 1)
                                                                                       180.0 (+ new-i 1)
                                                                                       225.0 (+ new-i 1)
                                                                                       270.0 new-i
                                                                                       315.0 (- new-i 1))
                                                                                     (case direction
                                                                                       0.0 new-j
                                                                                       45.0 (+ new-j 1)
                                                                                       90.0 (+ new-j 1)
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
                           new-burn-vectors        (conj! burn-vectors
                                                          (fast->BurnVector new-i new-j direction new-fractional-distance new-spread-rate
                                                                        new-terrain-distance burn-probability))]
                       (if (< new-fractional-distance 0.5)
                         new-burn-vectors
                         (let [local-burn-probability (grid-lookup/double-at fire-spread-getter new-i new-j) #_(t/mget fire-spread-matrix new-i new-j)]
                           (if (< burn-probability local-burn-probability)
                             new-burn-vectors
                             (let [relative-burn-time      (-> (/ dt-in-neighbor new-fractional-distance)
                                                               (* 0.5))
                                   burn-time               (-> new-clock
                                                               (- dt-in-neighbor)
                                                               (+ relative-burn-time))
                                   local-burn-time (grid-lookup/double-at burn-time-getter new-i new-j) #_(t/mget burn-time-matrix new-i new-j)]
                               (if (> burn-probability local-burn-probability)
                                 (do
                                   (t/mset! fire-spread-matrix new-i new-j burn-probability)
                                   (t/mset! burn-time-matrix new-i new-j burn-time)
                                   (when (<= local-burn-time global-clock)
                                     (.add ignited-cells-list [new-i new-j]))
                                   new-burn-vectors)
                                 ;; [new-burn-vectors (conj! ignited-cells [new-i new-j])]) ; TODO check set vs vectors
                                 (do
                                   (when (< burn-time local-burn-time)
                                     (t/mset! burn-time-matrix new-i new-j burn-time))
                                   new-burn-vectors))))))))
                   burn-vectors))))))
       (transient [])
       ;; [(transient []) (transient #{})]
       burn-vectors))
     ;; NOTE ArrayList -> vec is very fast because the underlying array typically is not copied.
     (vec ignited-cells-list)]))

(defn- parse-burn-period
  "Return the number of minutes into the day given HH:MM"
  ^double
  [burn-period]
  (let [[hour minute] (mapv #(Integer/parseInt %) (s/split burn-period #":"))]
    (+ (hour->min hour) ^double minute)))

(defn- recompute-burn-vectors
  [inputs matrices ^long band burn-vectors]
  (let [modified-time-matrix        (:modified-time-matrix matrices)
        max-spread-rate-matrix      (:max-spread-rate-matrix matrices)
        max-spread-direction-matrix (:max-spread-direction-matrix matrices)
        eccentricity-matrix         (:eccentricity-matrix matrices)]
    (mapv (fn [burn-vector]
            (let [i (:i burn-vector)
                  j (:j burn-vector)]
              (when (> band (dec ^long (t/mget modified-time-matrix i j)))
                (compute-max-in-situ-values! inputs matrices band i j))
              (let [direction            (double (:direction burn-vector))
                    max-spread-rate      (double (t/mget max-spread-rate-matrix i j))
                    max-spread-direction (double (t/mget max-spread-direction-matrix i j))
                    eccentricity         (double (t/mget eccentricity-matrix i j))
                    new-spread-rate      (compute-spread-rate max-spread-rate
                                                              max-spread-direction
                                                              eccentricity
                                                              direction)]
                (assoc burn-vector :spread-rate new-spread-rate))))
          burn-vectors)))

(defn- compute-fire-front-direction!
  [matrices i j]
  (let [x-magnitude-sum-matrix (:x-magnitude-sum-matrix matrices)
        y-magnitude-sum-matrix (:y-magnitude-sum-matrix matrices)
        spread-rate-sum-matrix (:spread-rate-sum-matrix matrices)
        x-magnitude-sum        (double (t/mget x-magnitude-sum-matrix i j))
        y-magnitude-sum        (double (t/mget y-magnitude-sum-matrix i j))
        spread-rate-sum        (double (t/mget spread-rate-sum-matrix i j))]
    (t/mset! x-magnitude-sum-matrix i j 0.0)
    (t/mset! y-magnitude-sum-matrix i j 0.0)
    (t/mset! spread-rate-sum-matrix i j 0.0)
    (-> (Math/atan2 (/ y-magnitude-sum spread-rate-sum)
                    (/ x-magnitude-sum spread-rate-sum))
        (Math/toDegrees)
        (mod 360))))

(defn- compute-directional-in-situ-values!
  [matrices ignited-cells]
  (when (seq ignited-cells)
    (let [max-spread-rate-matrix          (:max-spread-rate-matrix matrices)
          max-spread-direction-matrix     (:max-spread-direction-matrix matrices)
          eccentricity-matrix             (:eccentricity-matrix matrices)
          residence-time-matrix           (:residence-time-matrix matrices)
          reaction-intensity-matrix       (:reaction-intensity-matrix matrices)
          directional-flame-length-matrix (:directional-flame-length-matrix matrices)]
      (doseq [[i j] ignited-cells]
        (let [direction            (compute-fire-front-direction! matrices i j)
              max-spread-rate      (t/mget max-spread-rate-matrix i j)
              max-spread-direction (t/mget max-spread-direction-matrix i j)
              eccentricity         (t/mget eccentricity-matrix i j)
              spread-rate          (compute-spread-rate max-spread-rate
                                                        max-spread-direction
                                                        eccentricity
                                                        direction)
              residence-time       (t/mget residence-time-matrix i j)
              reaction-intensity   (t/mget reaction-intensity-matrix i j)
              fire-line-intensity  (->> (anderson-flame-depth spread-rate residence-time)
                                        (byram-fire-line-intensity reaction-intensity))
              flame-length         (byram-flame-length fire-line-intensity)]
          (t/mset! directional-flame-length-matrix i j flame-length))))))

(defn- initialize-fire-in-situ-values!
  [inputs matrices band ignited-cells]
  (let [compute-directional-values?     (:compute-directional-values? inputs)
        flame-length-matrix             (:flame-length-matrix matrices)
        directional-flame-length-matrix (:directional-flame-length-matrix matrices)]
    (doseq [[i j] ignited-cells]
      (compute-max-in-situ-values! inputs matrices band i j)
      (when compute-directional-values?
        (t/mset! directional-flame-length-matrix i j (t/mget flame-length-matrix i j))))))

(def ^:const ^:private minutes-per-24h 1440.0)

;; FIXME Update target spread rate on burn-vectors if new band > band
(defn- run-loop
  [inputs matrices ignited-cells]
  (let [compute-directional-values?      (:compute-directional-values? inputs)
        cell-size                        (double (:cell-size inputs))
        max-runtime                      (double (:max-runtime inputs))
        ignition-start-time              (double (:ignition-start-time inputs))
        ignition-stop-time               (+ ignition-start-time max-runtime)
        ignition-start-timestamp         (-> ^Date (:ignition-start-timestamp inputs)
                                             (.toInstant)
                                             (.atZone (ZoneId/of "UTC")))
        ignition-start-time-min-into-day (+ (hour->min (.getHour ignition-start-timestamp))
                                            (double (.getMinute ignition-start-timestamp)))
        burn-period-start                (parse-burn-period (:burn-period-start inputs))
        burn-period-end                  (let [bp-end (parse-burn-period (:burn-period-end inputs))]
                                           (if (< bp-end burn-period-start)
                                             (+ bp-end minutes-per-24h)
                                             bp-end))
        burn-period-dt                   (- burn-period-end burn-period-start)
        non-burn-period-dt               (- minutes-per-24h burn-period-dt)
        burn-period-clock                (+ ignition-start-time
                                            (double
                                             ;; This correction ensures that the ignition occurs at the start of the Burn Period.
                                             (cond
                                               ;; If too early in the day:
                                               ;; skipping ahead to the beginning of today's Burn Period
                                               (< ignition-start-time-min-into-day burn-period-start)
                                               (- burn-period-start ignition-start-time-min-into-day)

                                               ;; If too late in the day:
                                               ;; skipping ahead to the beginning of tomorrow's Burn Period
                                               (> ignition-start-time-min-into-day burn-period-end)
                                               (+ (- minutes-per-24h ignition-start-time-min-into-day) burn-period-start)

                                               :else
                                               (- burn-period-start ignition-start-time-min-into-day))))
        non-burn-period-clock            (+ burn-period-clock burn-period-dt)
        ignition-start-time              (max ignition-start-time burn-period-clock)
        band                             (min->hour ignition-start-time)
        suppression-dt                   (double (or (:suppression-dt inputs) Double/NaN))
        suppression-dt?                  (not (Double/isNaN suppression-dt))]
    (initialize-fire-in-situ-values! inputs matrices band ignited-cells)
    (loop [global-clock                         ignition-start-time
           band                                 band
           non-burn-period-clock                non-burn-period-clock
           suppression-clock                    (double (if suppression-dt? (+ ignition-start-time suppression-dt) max-runtime))
           burn-vectors                         (ignited-cells->burn-vectors inputs matrices ignited-cells [])
           spot-ignitions                       {}
           spot-count                           0
           total-cells-suppressed               0
           previous-num-perimeter-cells         0
           ignited-cells-since-last-suppression []
           fraction-contained                   0.0]
      (if (and (< global-clock ignition-stop-time)
               (or (seq burn-vectors) (seq spot-ignitions)))
        (let [dt-until-max-runtime               (- ignition-stop-time global-clock)]
          (cond
            (and suppression-dt? (= global-clock suppression-clock))
            (let [max-runtime-fraction (/ (- global-clock ignition-start-time) max-runtime)
                  [bvs-to-process-next
                   total-cells-suppressed
                   previous-num-perimeter-cells
                   fraction-contained] (suppression/suppress-burn-vectors inputs
                                                                          max-runtime-fraction
                                                                          previous-num-perimeter-cells
                                                                          total-cells-suppressed
                                                                          burn-vectors
                                                                          ignited-cells-since-last-suppression
                                                                          fraction-contained)]
              (recur global-clock
                     band
                     non-burn-period-clock
                     (+ global-clock suppression-dt)
                     bvs-to-process-next
                     spot-ignitions
                     spot-count
                     (long total-cells-suppressed)
                     (long previous-num-perimeter-cells)
                     []
                     (double fraction-contained)))

            (= global-clock non-burn-period-clock)
            (let [timestep  (double (min non-burn-period-dt dt-until-max-runtime))
                  new-clock (+ global-clock timestep)
                  new-band  (min->hour new-clock)]
              (if (and suppression-dt? (<= suppression-clock new-clock))
                (let [suppression-clocks     (iterate #(+ (double %) suppression-dt) suppression-clock)
                      last-suppression-clock (double (last (take-while #(<= (double %) new-clock) suppression-clocks)))
                      [bvs-to-process-next
                       total-cells-suppressed
                       previous-num-perimeter-cells
                       fraction-contained]   (suppression/suppress-burn-vectors inputs
                                                                                (/ (- last-suppression-clock ignition-start-time) max-runtime)
                                                                                previous-num-perimeter-cells
                                                                                total-cells-suppressed
                                                                                burn-vectors
                                                                                ignited-cells-since-last-suppression
                                                                                fraction-contained)]
                  (recur new-clock
                         new-band
                         (+ new-clock burn-period-dt)
                         (+ last-suppression-clock suppression-dt)
                         bvs-to-process-next
                         (if (zero? non-burn-period-dt)
                           spot-ignitions
                           {})
                         spot-count
                         (long total-cells-suppressed)
                         (long previous-num-perimeter-cells)
                         []
                         (double fraction-contained)))
                (recur new-clock
                       new-band
                       (+ new-clock burn-period-dt)
                       suppression-clock
                       burn-vectors
                       (if (zero? non-burn-period-dt)
                         spot-ignitions
                         {})
                       spot-count
                       total-cells-suppressed
                       previous-num-perimeter-cells
                       ignited-cells-since-last-suppression
                       fraction-contained)))

            :else
            (let [dt-until-new-hour              (- 60.0 (rem global-clock 60.0))
                  dt-until-non-burn-period-clock (- non-burn-period-clock global-clock)
                  bvs                            (if (and (> global-clock ignition-start-time)
                                                          (or (= dt-until-new-hour 60.0)
                                                              (= dt-until-non-burn-period-clock burn-period-dt)))
                                                   (recompute-burn-vectors inputs matrices band burn-vectors)
                                                   burn-vectors)
                  timestep                       (double
                                                  (cond-> (compute-dt cell-size bvs)
                                                    dt-until-new-hour              (min dt-until-new-hour)
                                                    dt-until-max-runtime           (min dt-until-max-runtime)
                                                    dt-until-non-burn-period-clock (min dt-until-non-burn-period-clock)
                                                    suppression-dt?                (min (let [dt-until-suppression-clock (- suppression-clock global-clock)]
                                                                                          dt-until-suppression-clock))))
                  new-clock                      (+ global-clock timestep)
                  [grown-bvs
                   ignited-cells]                (grow-burn-vectors! matrices global-clock timestep bvs)
                  [transitioned-bvs
                   transition-ignited-cells]     (->> grown-bvs
                                                      (ignited-cells->burn-vectors inputs matrices ignited-cells)
                                                      (promote-burn-vectors inputs matrices global-clock new-clock 1.99)
                                                      (transition-burn-vectors inputs matrices band global-clock new-clock 0.99))
                  promoted-transitioned-bvs      (->> transitioned-bvs
                                                      (ignited-cells->burn-vectors inputs matrices transition-ignited-cells)
                                                      (promote-burn-vectors inputs matrices global-clock new-clock 0.99)) ;TODO optimize, promoting twice
                  [spot-bvs
                   spot-ignite-now-count
                   spot-ignite-later
                   spot-ignited-cells]           (compute-spot-burn-vectors! inputs
                                                                             matrices
                                                                             spot-ignitions
                                                                             (into ignited-cells transition-ignited-cells)
                                                                             band
                                                                             new-clock)
                  promoted-spot-bvs              (->> (into promoted-transitioned-bvs spot-bvs)
                                                      (promote-burn-vectors inputs matrices global-clock new-clock 1.49)) ;TODO optimize, promoting thrice
                  [transition-promoted-spot-bvs
                   _]                            (transition-burn-vectors inputs matrices band global-clock new-clock 0.49 promoted-spot-bvs)
                  all-ignited-cells              (-> ignited-cells
                                                     (into transition-ignited-cells)
                                                     (into spot-ignited-cells))]
              ;; TODO if spot ignitions is updated to have varying burn probability make sure there are no duplicates
              ;; of ignited cells in this list of ignited cells passed to compute-directional-in-stiu-values!
              (when compute-directional-values?
                (compute-directional-in-situ-values! matrices all-ignited-cells))
              (recur new-clock
                     (min->hour new-clock)
                     non-burn-period-clock
                     suppression-clock
                     transition-promoted-spot-bvs
                     spot-ignite-later
                     (+ spot-count ^long spot-ignite-now-count)
                     total-cells-suppressed
                     previous-num-perimeter-cells
                     (into ignited-cells-since-last-suppression all-ignited-cells)
                     fraction-contained))))
        (let [fire-type-matrix (:fire-type-matrix matrices)]
          {:exit-condition                  (if (>= global-clock ignition-stop-time) :max-runtime-reached :no-burnable-fuels)
           :global-clock                    global-clock
           :burn-time-matrix                (:burn-time-matrix matrices)
           :fire-line-intensity-matrix      (:fire-line-intensity-matrix matrices)
           :fire-spread-matrix              (:fire-spread-matrix matrices)
           :fire-type-matrix                fire-type-matrix
           :flame-length-matrix             (:flame-length-matrix matrices)
           :directional-flame-length-matrix (:directional-flame-length-matrix matrices)
           :spot-matrix                     (:spot-matrix matrices)
           :spread-rate-matrix              (:spread-rate-matrix matrices)
           :surface-fire-count              (->> fire-type-matrix
                                                 (d/emap #(if (= ^double % 1.0) 1 0) :int64)
                                                 (dfn/sum))
           :crown-fire-count                (->> fire-type-matrix
                                                 (d/emap #(if (>= ^double % 2.0) 1 0) :int64)
                                                 (dfn/sum))
           :spot-count                      spot-count})))))

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
     directional-flame-length-matrix
     max-spread-direction-matrix
     max-spread-rate-matrix
     modified-time-matrix
     residence-time-matrix
     reaction-intensity-matrix
     spot-matrix
     spread-rate-matrix
     spread-rate-sum-matrix
     travel-lines-matrix
     x-magnitude-sum-matrix
     y-magnitude-sum-matrix])

(defn simulation-matrices-record
  [m]
  (map->SimulationMatrices
   (reduce-kv (fn [ret k v]
                (cond-> ret
                        (t/tensor? v)
                        (update k grid-lookup/add-double-getter)))
              m
              m)))

;;-----------------------------------------------------------------------------
;; Main Simulation Entry Point - Dispatches to Point/Perimeter Ignition
;;-----------------------------------------------------------------------------

;; TODO: Move this multimethod check into run-simulations to avoid running it in every thread
(defmulti run-fire-spread
  "Runs the raster-based fire spread model with a SimulationInputs record containing these fields:
  |---------------------------------------------------+--------------------+-----------------------------------------------------------|
  | Key                                               | Value Type         | Value Units                                               |
  |---------------------------------------------------+--------------------+-----------------------------------------------------------|
  | :num-rows                                         | long               | column count of fuel-model-matrix                         |
  | :num-cols                                         | long               | row count of fuel-model-matrix                            |
  | :cell-size                                        | double             | feet                                                      |
  |---------------------------------------------------+--------------------+-----------------------------------------------------------|
  | :ignition-start-time                              | double             | minutes                                                   |
  | :max-runtime                                      | double             | minutes                                                   |
  |---------------------------------------------------+--------------------+-----------------------------------------------------------|
  | :initial-ignition-site                            | [i,j] or 2D tensor | [y,x] coordinate or categories 0-2 in tensor              |
  |---------------------------------------------------+--------------------+-----------------------------------------------------------|
  | :crowning-disabled?                               | boolean            | true or false                                             |
  | :ellipse-adjustment-factor                        | double             | < 1.0 = more circular, > 1.0 = more elliptical            |
  | :grass-suppression?                               | boolean            | true or false                                             |
  |---------------------------------------------------+--------------------+-----------------------------------------------------------|
  | :rand-gen                                         | java.util.Random   | uniform sample [0-1]                                      |
  |---------------------------------------------------+--------------------+-----------------------------------------------------------|
  | :get-elevation                                    | (i,j) -> v         | feet                                                      |
  | :get-slope                                        | (i,j) -> v         | vertical feet/horizontal feet                             |
  | :get-aspect                                       | (i,j) -> v         | degrees clockwise from north [0-360)                      |
  |---------------------------------------------------+--------------------+-----------------------------------------------------------|
  | :get-canopy-cover                                 | (i,j) -> v         | percent [0-100]                                           |
  | :get-canopy-height                                | (i,j) -> v         | feet                                                      |
  | :get-canopy-base-height                           | (i,j) -> v         | feet                                                      |
  | :get-crown-bulk-density                           | (i,j) -> v         | lb/ft^3                                                   |
  |---------------------------------------------------+--------------------+-----------------------------------------------------------|
  | :get-fuel-model                                   | (i,j) -> v         | fuel model numbers [1-256]                                |
  |---------------------------------------------------+--------------------+-----------------------------------------------------------|
  | :get-temperature                                  | (b,i,j) -> v       | degrees Fahrenheit                                        |
  | :get-relative-humidity                            | (b,i,j) -> v       | percent [0-100]                                           |
  | :get-wind-speed-20ft                              | (b,i,j) -> v       | miles/hour                                                |
  | :get-wind-from-direction                          | (b,i,j) -> v       | degrees clockwise from north                              |
  |---------------------------------------------------+--------------------+-----------------------------------------------------------|
  | :get-fuel-moisture-dead-1hr                       | (b,i,j) -> v       | ratio [0-1]                                               |
  | :get-fuel-moisture-dead-10hr                      | (b,i,j) -> v       | ratio [0-1]                                               |
  | :get-fuel-moisture-dead-100hr                     | (b,i,j) -> v       | ratio [0-1]                                               |
  | :get-fuel-moisture-live-herbaceous                | (b,i,j) -> v       | ratio [0-1]                                               |
  | :get-fuel-moisture-live-woody                     | (b,i,j) -> v       | ratio [0-1]                                               |
  | :get-foliar-moisture                              | (b,i,j) -> v       | ratio [0-1]                                               |
  |---------------------------------------------------+--------------------+-----------------------------------------------------------|
  | :spotting                                         | map                | :decay-constant -> double                                 |
  |                                                   |                    | :num-firebrands -> long                                   |
  |                                                   |                    | :surface-fire-spotting -> map                             |
  |                                                   |                    | :crown-fire-spotting-percent -> double or [double double] |
  |---------------------------------------------------+--------------------+-----------------------------------------------------------|
  | :fuel-number->spread-rate-adjustment-array-lookup | array of doubles   | unitless                                                  |
  |---------------------------------------------------+--------------------+-----------------------------------------------------------|
  | :suppression-dt                                   | double             | minutes                                                   |
  | :suppression-coefficient                          | double             | none                                                      |
  | :sdi-containment-overwhelming-area-growth-rate    | double             | Acres/day                                                 |
  | :sdi-reference-suppression-speed                  | double             | percent/day                                               |
  | :sdi-sensitivity-to-difficulty                    | double             | none                                                      |
  |---------------------------------------------------+--------------------+-----------------------------------------------------------|"
  (fn [inputs]
    (if (vector? (:initial-ignition-site inputs))
      :ignition-point
      :ignition-perimeter)))

;;-----------------------------------------------------------------------------
;; Point Ignition
;;-----------------------------------------------------------------------------

(defn- initialize-point-ignition-matrices
  [inputs]
  (let [num-rows                    (long (:num-rows inputs))
        num-cols                    (long (:num-cols inputs))
        [i j]                       (:initial-ignition-site inputs)
        ignition-start-time         (:ignition-start-time inputs)
        spotting                    (:spotting inputs)
        compute-directional-values? (:compute-directional-values? inputs)
        shape                       [num-rows num-cols]
        burn-time-matrix            (-> (* num-rows num-cols)
                                        (double-array -1.0)
                                        (t/ensure-tensor)
                                        (t/reshape shape)
                                        (t/mset! i j ignition-start-time))]
    (simulation-matrices-record
     {:burn-time-matrix                burn-time-matrix
      :eccentricity-matrix             (t/new-tensor shape)
      :fire-line-intensity-matrix      (t/new-tensor shape)
      :fire-spread-matrix              (-> (t/new-tensor shape) (t/mset! i j 1.0))
      :fire-type-matrix                (t/new-tensor shape)
      :firebrand-count-matrix          (when spotting (t/new-tensor shape :datatype :int32))
      :flame-length-matrix             (t/new-tensor shape)
      :directional-flame-length-matrix (when compute-directional-values? (t/new-tensor shape))
      :max-spread-direction-matrix     (t/new-tensor shape)
      :max-spread-rate-matrix          (t/new-tensor shape)
      :modified-time-matrix            (t/new-tensor shape :datatype :int32)
      :residence-time-matrix           (when compute-directional-values? (t/new-tensor shape))
      :reaction-intensity-matrix       (when compute-directional-values? (t/new-tensor shape))
      :spot-matrix                     (when spotting (t/new-tensor shape))
      :spread-rate-matrix              (t/new-tensor shape)
      :spread-rate-sum-matrix          (when compute-directional-values? (t/new-tensor shape))
      :travel-lines-matrix             (t/new-tensor shape :datatype :short)
      :x-magnitude-sum-matrix          (when compute-directional-values? (t/new-tensor shape))
      :y-magnitude-sum-matrix          (when compute-directional-values? (t/new-tensor shape))})))

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
  (let [num-rows                    (long (:num-rows inputs))
        num-cols                    (long (:num-cols inputs))
        positive-burn-scar          (:initial-ignition-site inputs)
        ignition-start-time         (:ignition-start-time inputs)
        spotting                    (:spotting inputs)
        compute-directional-values? (:compute-directional-values? inputs)
        shape                       [num-rows num-cols]
        negative-burn-scar          (d/clone (dfn/* -1.0 positive-burn-scar))
        burn-time-matrix            (-> (* num-rows num-cols)
                                        (double-array -1.0)
                                        (t/ensure-tensor)
                                        (t/reshape shape)
                                        (add-ignited-cells! ignited-cells ignition-start-time))]
    (simulation-matrices-record
     {:burn-time-matrix                burn-time-matrix
      :eccentricity-matrix             (d/clone negative-burn-scar)
      :fire-line-intensity-matrix      negative-burn-scar
      :fire-spread-matrix              (d/clone positive-burn-scar)
      :fire-type-matrix                (d/clone negative-burn-scar)
      :firebrand-count-matrix          (when spotting (t/new-tensor shape :datatype :int32))
      :flame-length-matrix             (d/clone negative-burn-scar)
      :directional-flame-length-matrix (when compute-directional-values? (d/clone negative-burn-scar))
      :max-spread-direction-matrix     (d/clone negative-burn-scar)
      :max-spread-rate-matrix          (d/clone negative-burn-scar)
      :modified-time-matrix            (t/new-tensor shape :datatype :int32)
      :residence-time-matrix           (when compute-directional-values? (d/clone negative-burn-scar))
      :reaction-intensity-matrix       (when compute-directional-values? (d/clone negative-burn-scar))
      :spot-matrix                     (when spotting (t/new-tensor shape))
      :spread-rate-matrix              (d/clone negative-burn-scar)
      :spread-rate-sum-matrix          (when compute-directional-values? (t/new-tensor shape))
      :travel-lines-matrix             (t/new-tensor shape :datatype :short)
      :x-magnitude-sum-matrix          (when compute-directional-values? (t/new-tensor shape))
      :y-magnitude-sum-matrix          (when compute-directional-values? (t/new-tensor shape))})))

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
