;; [[file:../../org/GridFire.org::gridfire.fire-spread-beginning][gridfire.fire-spread-beginning]]
(ns gridfire.fire-spread
  (:require [clojure.string                :as s]
            [gridfire.common               :refer [burnable-cell?
                                                   burnable-fuel-model?
                                                   calc-fuel-moisture
                                                   in-bounds-optimal?
                                                   non-zero-indices
                                                   overtakes-lower-probability-fire?
                                                   terrain-distance-fn
                                                   terrain-distance-from-cell-getter
                                                   terrain-distance-invoke]]
            [gridfire.conversion           :refer [deg->rad mph->fpm hour->min min->hour]]
            [gridfire.crown-fire           :refer [crown-fire-eccentricity
                                                   crown-fire-line-intensity
                                                   cruz-crown-fire-spread
                                                   van-wagner-crown-fire-initiation?]]
            [gridfire.elliptical           :as ellip]
            [gridfire.fuel-models          :refer [fuel-models-precomputed
                                                   moisturize]]
            [gridfire.grid-lookup          :as grid-lookup]
            [gridfire.spotting             :as spotting]
            [gridfire.structs.burn-vector  :as burn-vec]
            [gridfire.structs.rfwo         :as rfwo-struct]
            [gridfire.suppression          :as suppression]
            [gridfire.surface-fire         :refer [rothermel-surface-fire-spread-no-wind-no-slope
                                                   rothermel-surface-fire-spread-max
                                                   compute-spread-rate
                                                   anderson-flame-depth
                                                   byram-fire-line-intensity
                                                   byram-flame-length
                                                   wind-adjustment-factor]]
            [gridfire.utils.flow           :refer [case-double]]
            [gridfire.utils.gradient       :as gradient]
            [tech.v3.datatype              :as d]
            [tech.v3.datatype.functional   :as dfn]
            [tech.v3.tensor                :as t])
  (:import (clojure.lang IFn$OLLDO)
           java.time.ZoneId
           (java.util ArrayList Date)))

(set! *unchecked-math* :warn-on-boxed)
;; fire-spread-beginning ends here

;; [[file:../../org/GridFire.org::fire-spread-directional-utils][fire-spread-directional-utils]]
(defn- direction-bit->angle
  ^double [^long dir-bit]
  (case dir-bit
    0 0.0
    1 45.0
    2 90.0
    3 135.0
    4 180.0
    5 225.0
    6 270.0
    7 315.0))

(defn direction-angle->bit
  ^long [^double dir-angle]
  (case-double dir-angle
    0.0   0
    45.0  1
    90.0  2
    135.0 3
    180.0 4
    225.0 5
    270.0 6
    315.0 7))

(defn direction-angle->i-incr
  ^long [^double dir-angle]
  (case-double dir-angle
    (0.0 45.0 315.0)   -1
    (135.0 180.0 225.0) 1
    (90.0 270.0)        0))

(defn direction-angle->j-incr
  ^long [^double dir-angle]
  (case-double dir-angle
    (45.0 90.0 135.0)   1
    (0.0 180.0)         0
    (225.0 270.0 315.0) -1))

(defn diagonal? [^double direction-angle]
  (case-double direction-angle
    (0.0  90.0  180.0 270.0) false
    (45.0 135.0 225.0 315.0) true))
;; fire-spread-directional-utils ends here

;; [[file:../../org/GridFire.org::fire-spread-estimate-incidence][fire-spread-estimate-incidence]]
(def ^:const default-incidence-cosine
  "The default guess for the cosine of the fireline-incidence of maximum spread.

  When lacking information to estimate the fireline-normal direction,
  we conservatively assume that it is orthogonal to the direction of maximum spread (flanking fire),
  and therefore the cosine is 0."
  0.0)

(defn estimate-fireline-incidence-cosine
  "Estimates the fireline-normal direction based on Time-of-Arrival data; more precisely, returns the cosine of
  the 'incidence' angle between the direction of advancing fire front and
  some direction supplied as `dir-angle` (in degrees from North).

  Equivalently, returns (a guess of) the dot-product <duv|fln>, in which
  |duv> is the unit vector in the supplied direction and
  |fln> is the fireline-normal unit vector.

  The arguments are:
  - `burn-time-getter`: a getter for the Time of Arrival of the fire,
    as returned by `(grid-lookup/mgetter-double burn-time-matrix)`
  - `incoming-burnvec` (optional): the Burn Vector where we want to estimate the fireline-normal spread rate,
     or nil if not available;
  - `dir-angle`: angle to the North direction in degrees, representing the direction of interest
     (typically :max-spread-direction);
  - `fallback-guess`: a default cosine to return when estimation fails.

  Returns `fallback-guess` when there is not enough information to make a gradient estimate,
  which happens in particular when `incoming-burnvec` is nil."
  ;; NOTE why return the cosine to some other direction, (Val, 30 Nov 2022)
  ;; rather than an angle or vector representing the fireline-normal direction? Several reasons:
  ;; 1. When the fireline-normal direction cannot be estimated,
  ;;    it can be difficult to find a sensible "default value" for a direction.
  ;;    Less so for a cosine.
  ;; 2. The cosine is what we need downstream, and is easy and fast to compute.
  ;;    Turning that into an angle would be a useless and costly computation.
  ^double [burn-time-getter incoming-burnvec ^double dir-angle ^double fallback-guess]
  (if (nil? incoming-burnvec)
    fallback-guess
    ;; NOTE what with gradient geometry and geospatial encoding conventions,
    ;; this function combines so many confusing part of the code
    ;; that I felt the need for abundant commenting and naming. (Val, 30 Nov 2022)
    ;; NOTATION this code will be using the <bra|ket> notation for vectors and dot-products:
    ;; <a|b> represents the dot-product between vectors |a> and |b>.
    ;; |i> and |j> represent the unit vectors which respectively increment i and j by 1,
    ;; such that the i-coordinate of |a> is <i|a> (or equivalently <a|i>),
    ;; and <a|b> = <a|i><i|b> + <a|j><j|b>.
    ;; Our dot-product of choice <.|.> is thus defined by #{|i> |j>} forming an orthonormal basis.
    ;; By definition of the gradient, <grad-F|i> = ∂F/∂i and <grad-F|j> = ∂F/∂j.
    (let [ignited-this-cell? (>= (burn-vec/get-fractional-distance incoming-burnvec)
                                 0.5)
          bv-dir-angle       (burn-vec/get-direction incoming-burnvec)
          ;; The fireline-normal direction should be given by the gradient of the Time of Arrival (ToA).
          ;; The problem is that the gradient is not easy to estimate at the fire perimeter,
          ;; because we might not yet have a gradient for all neighboring cells.
          ;; For estimating the gradient, we choose the cell of origin of the Burn Vector,
          ;; because we want this cell to have enough neighbors which can contribute a ToA
          ;; to the gradient computation.
          grad-cell-i        (- (burn-vec/get-i incoming-burnvec)
                                (if ignited-this-cell?
                                  0
                                  (direction-angle->i-incr bv-dir-angle)))
          grad-cell-j        (- (burn-vec/get-j incoming-burnvec)
                                (if ignited-this-cell?
                                  0
                                  (direction-angle->j-incr bv-dir-angle)))
          ;; Having chosen a nearby cell, we now estimate the ToA-gradient at that cell:
          toa-ij             (grid-lookup/double-at burn-time-getter grad-cell-i grad-cell-j)
          ;; IMPROVEMENT it might be interesting to factor out the gradient computation to a function.
          ;; The problem is avoiding performance regressions: packing 2 primitive values into an object might be significantly less efficient.
          ;; One (hacky) strategy might be to encode 2 32-bit floats into 1 primitive long.
          <grad-ToA|i>       (let [toa-i   toa-ij
                                   toa-i+1 (grid-lookup/double-at burn-time-getter (unchecked-inc grad-cell-i) grad-cell-j)
                                   toa-i-1 (grid-lookup/double-at burn-time-getter (unchecked-dec grad-cell-i) grad-cell-j)]
                               (gradient/estimate-dF -1.0 toa-i-1 toa-i toa-i+1))
          <grad-ToA|j>       (let [toa-j   toa-ij
                                   toa-j+1 (grid-lookup/double-at burn-time-getter grad-cell-i (unchecked-inc grad-cell-j))
                                   toa-j-1 (grid-lookup/double-at burn-time-getter grad-cell-i (unchecked-dec grad-cell-j))]
                               (gradient/estimate-dF -1.0 toa-j-1 toa-j toa-j+1))
          grad-ToA-norm      (Math/sqrt (+ (* <grad-ToA|i> <grad-ToA|i>)
                                           (* <grad-ToA|j> <grad-ToA|j>)))]
      ;; A zero gradient happens in particular when gradient estimation has failed on both axes, for lack of ToA data.
      ;; IMPROVEMENT: when that happens, we might want to retry with diagonal axes.
      ;; IMPROVEMENT we might also want to discard a near-zero gradient - probably too chaotic.
      ;; The problem is that we'd probably need to know the cell size to determine a sensible threshold for 'near-zero'.
      (if (zero? grad-ToA-norm)
        fallback-guess
        (let [;; The fireline-normal unit vector |fln> is estimated by scaling the ToA gradient to unit length:
              <fln|i>   (/ <grad-ToA|i> grad-ToA-norm)
              <fln|j>   (/ <grad-ToA|j> grad-ToA-norm)
              ;; We will now resolve the unit vector |duv> corresponding to the supplied `dir-angle`:
              dir-rad   (deg->rad dir-angle)
              ;; These 2 lines reflect our convention for what how we define "angle from North" and the x,y axes:
              <duv|y>   (Math/cos dir-rad)
              <duv|x>   (Math/sin dir-rad)
              ;; These 2 lines reflect our convention relating matrix coordinates to spatial coordinates: |i> = -|y> and |j> = |x>,
              ;; where |x> is the unit vector in the direction of increasing y, and likewise of |y>.
              <duv|i>   (- <duv|y>)
              <duv|j>   <duv|x>
              ;; Finally computing our dot-product, by applying the coordinates-based formula: <a|b> = <a|i><i|b> + <a|j><j|b>.
              ;; Note: if you want to reconcile the angular-directions and unit-vectors perspectives,
              ;; it can be insightful to view the following formula as an application of
              ;; the trigonometric identity: cos(a-b) = cos(a)*cos(b) + sin(a)*sin(b)
              <duv|fln> (+ (* <duv|i> <fln|i>)
                           (* <duv|j> <fln|j>))]
          <duv|fln>)))))
;; fire-spread-estimate-incidence ends here

;; [[file:../../org/GridFire.org::fire-spread-core-algorithm][fire-spread-core-algorithm]]
;;-----------------------------------------------------------------------------
;; Fire spread
;;-----------------------------------------------------------------------------

(defn- find-max-spread-rate ^double
  [^double max-spread-rate burn-vector]
  (max max-spread-rate (burn-vec/get-spread-rate burn-vector)))

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

(defn rothermel-surface-fire-wrapped
  [^double fuel-model-number fuel-moisture grass-suppression?]
  (-> (long fuel-model-number)
      (fuel-models-precomputed)
      (moisturize fuel-moisture)
      (rothermel-surface-fire-spread-no-wind-no-slope grass-suppression?)))

(defn ^:dynamic ^:redef rothermel-fast-wrapper-optimal
  [rfwo-args]
  (rothermel-surface-fire-wrapped (rfwo-struct/get-fuel-model-number rfwo-args)
                                  (rfwo-struct/get-fuel-moisture-vec rfwo-args)
                                  (rfwo-struct/get-grass-suppression? rfwo-args)))

(defn- memoize-1arg
  "Like clojure.core/memoize, but optimized for 1-argument functions,
  avoiding varargs overhead."
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

(defn memoize-rfwo
  "Memoization function specialized for rothermel-fast-wrapper-optimal."
  [f]
  ;; NOTE currently implemented using clojure.core/memoize, but that will change.
  (memoize-1arg f))

(defn- store-if-max!
  [matrix ^long i ^long j ^double new-value]
  (let [old-value (grid-lookup/mget-double-at matrix i j)]
    (when (> new-value old-value)
      (t/mset! matrix i j new-value))))

(defn- lookup-spread-rate-adjustment
  ^double [fuel-number->spread-rate-adjustment-array-lookup fuel-model]
  (aget (doubles fuel-number->spread-rate-adjustment-array-lookup) fuel-model))

(defn- compute-max-in-situ-values!
  "Computes and saves fire-spread behavior quantities for a cell-band.
  This function must be called the first time a burn-vector appears in the given cell
  during the given hourly band, be because it travelled (transitioned) into it,
  or because the cell got ignited in some other way (e.g. spotting or initial ignition);
  in the first case, `incoming-burnvec` must be non-nil, so that it can be used
  to estimate the fireline-normal direction."
  [inputs matrices band i j incoming-burnvec]
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
        i                                                (long i)
        j                                                (long j)
        band                                             (long band)
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
        surface-fire-min                                 (rothermel-fast-wrapper-optimal (rfwo-struct/make-RothFWOArgs fuel-model
                                                                                                                       fuel-moisture-dead-1hr
                                                                                                                       fuel-moisture-dead-10hr
                                                                                                                       fuel-moisture-dead-100hr
                                                                                                                       0.0 ; fuel-moisture-dead-herbaceous
                                                                                                                       fuel-moisture-live-herbaceous
                                                                                                                       fuel-moisture-live-woody
                                                                                                                       (boolean grass-suppression?)))
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
        max-spread-direction                             (:max-spread-direction surface-fire-max) ; IMPROVEMENT primitive lookup, for performance.
        eccentricity                                     (:eccentricity surface-fire-max)
        residence-time                                   (:residence-time surface-fire-min)
        reaction-intensity                               (:reaction-intensity surface-fire-min)
        burn-time-getter                                 (grid-lookup/mgetter-double (:burn-time-matrix matrices))
        fln-incidence-cos                                (estimate-fireline-incidence-cosine burn-time-getter
                                                                                             incoming-burnvec
                                                                                             (double max-spread-direction)
                                                                                             default-incidence-cosine)
        fln-spread-rate-scalar                           (ellip/fireline-normal-spread-rate-scalar (double eccentricity) fln-incidence-cos)
        fireline-normal-spread-rate                      (* (double max-spread-rate) fln-spread-rate-scalar)
        surface-intensity                                (->> (anderson-flame-depth fireline-normal-spread-rate ^double residence-time)
                                                              (byram-fire-line-intensity ^double reaction-intensity))]
    (if (and (not crowning-disabled?)
             (van-wagner-crown-fire-initiation? canopy-cover
                                                canopy-base-height
                                                foliar-moisture
                                                surface-intensity))
      (let [crown-spread-max        (cruz-crown-fire-spread wind-speed-20ft
                                                            crown-bulk-density
                                                            fuel-moisture-dead-1hr)
            crown-type              (if (neg? crown-spread-max) 2.0 3.0) ; 2=passive, 3=active
            crown-spread-max        (Math/abs crown-spread-max)
            crown-eccentricity      (if (> (double max-spread-rate) crown-spread-max)
                                      (double eccentricity)
                                      (crown-fire-eccentricity wind-speed-20ft ellipse-adjustment-factor))
            crown-fln-spread-rate   (* crown-spread-max
                                       (ellip/fireline-normal-spread-rate-scalar crown-eccentricity fln-incidence-cos))
            crown-intensity         (crown-fire-line-intensity crown-fln-spread-rate
                                                               crown-bulk-density
                                                               (- canopy-height canopy-base-height)
                                                               (:heat-of-combustion surface-fire-min))
            tot-fire-line-intensity (+ surface-intensity crown-intensity)
            max-spread-rate         (max ^double max-spread-rate crown-spread-max)]
        (t/mset! max-spread-rate-matrix i j max-spread-rate)
        (t/mset! max-spread-direction-matrix i j max-spread-direction)
        (t/mset! eccentricity-matrix i j crown-eccentricity)
        (t/mset! modified-time-matrix i j (inc band))
        (when compute-directional-values?
          (t/mset! residence-time-matrix i j residence-time)
          (t/mset! reaction-intensity-matrix i j reaction-intensity))
        (store-if-max! spread-rate-matrix i j crown-fln-spread-rate)
        (store-if-max! flame-length-matrix i j (byram-flame-length tot-fire-line-intensity))
        (store-if-max! fire-line-intensity-matrix i j tot-fire-line-intensity)
        (store-if-max! fire-type-matrix i j crown-type))
      (do
        (t/mset! max-spread-rate-matrix i j max-spread-rate)
        (t/mset! max-spread-direction-matrix i j max-spread-direction)
        (t/mset! eccentricity-matrix i j eccentricity)
        (t/mset! modified-time-matrix i j (inc band))
        (when compute-directional-values?
          (t/mset! residence-time-matrix i j residence-time)
          (t/mset! reaction-intensity-matrix i j reaction-intensity))
        (store-if-max! spread-rate-matrix i j fireline-normal-spread-rate)
        (store-if-max! flame-length-matrix i j (byram-flame-length surface-intensity))
        (store-if-max! fire-line-intensity-matrix i j surface-intensity)
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

(defmacro dir-bits-reduce
  "Macro for efficiently reducing over the 8 direction bits, similarly to clojure.core/areduce.

  `dir-bit-sym` will be bound to a primitive long,
  and `ret-sym` to the reduction accumulator,
  which will be initialized by evaluating `init`
  and updated by evaluating `expr`."
  [[dir-bit-sym ret-sym] init expr]
  {:pre [(symbol? dir-bit-sym)
         (symbol? ret-sym)]}
  `(loop [~dir-bit-sym (long 0)
          ~ret-sym     ~init]
     (if (= ~dir-bit-sym 8)
       ~ret-sym
       (recur (unchecked-inc ~dir-bit-sym)
              ~expr))))

;; NAMING CONVENTION the '-pfn' suffix stands for 'Prepared FuNction' or 'Partial'd FuNction'.
(defn create-new-burn-vectors!-pfn
  "Prepares a partialed primitive-signature function to be called via `create-new-burn-vectors!-invoke`."
  [num-rows num-cols cell-size get-elevation
   travel-lines-matrix fire-spread-matrix
   max-spread-rate-matrix max-spread-direction-matrix eccentricity-matrix]
  (let [num-rows                    (long num-rows)
        num-cols                    (long num-cols)
        max-spread-rate-getter      (grid-lookup/mgetter-double max-spread-rate-matrix)
        max-spread-direction-getter (grid-lookup/mgetter-double max-spread-direction-matrix)
        eccentricity-getter         (grid-lookup/mgetter-double eccentricity-matrix)
        fire-spread-getter          (grid-lookup/mgetter-double fire-spread-matrix)]
    (fn launch-from! [bvs-acc ^long i ^long j ^double burn-probability]
      (let [max-spread-rate      (grid-lookup/double-at max-spread-rate-getter i j)
            max-spread-direction (grid-lookup/double-at max-spread-direction-getter i j)
            eccentricity         (grid-lookup/double-at eccentricity-getter i j)
            travel-lines         (long (t/mget travel-lines-matrix i j)) ;; IMPROVEMENT mget-long-at, or something like it.
            get-ij-terrain-dist  (terrain-distance-from-cell-getter get-elevation num-rows num-cols cell-size i j)]
        (dir-bits-reduce [dir-bit bvs-acc]
          bvs-acc
          (if (bit-test travel-lines dir-bit)
            ;; INVARIANT: at most 1 Burn Vector at a time per travel line.
            bvs-acc
            (let [direction       (direction-bit->angle dir-bit)
                  new-i           (+ i (direction-angle->i-incr direction))
                  new-j           (+ j (direction-angle->j-incr direction))
                  new-burn-vector (when (and (in-bounds-optimal? num-rows num-cols new-i new-j)
                                             (overtakes-lower-probability-fire? burn-probability
                                                                                (grid-lookup/double-at fire-spread-getter new-i new-j)))
                                    (let [spread-rate      (compute-spread-rate max-spread-rate
                                                                                max-spread-direction
                                                                                eccentricity
                                                                                direction)
                                          terrain-distance (grid-lookup/double-at get-ij-terrain-dist new-i new-j)]
                                      ;; NOTE we start at fractional-distance = 0.5, which represents the center of the cell. (Val, 16 Nov 2022)
                                      (burn-vec/make-burn-vector i j direction 0.5 spread-rate terrain-distance burn-probability)))]
              (if new-burn-vector
                (do
                  ;; TODO move into function
                  (as-> (t/mget travel-lines-matrix i j) $
                        (bit-set $ dir-bit)
                        (t/mset! travel-lines-matrix i j $))
                  (conj! bvs-acc new-burn-vector))
                bvs-acc))))))))

(defmacro create-new-burn-vectors!-invoke
  "Adds the relevant new burn vectors to `bvs-acc` starting from cell `[i j]` with the given `burn-probability`.

  `create-new-burn-vectors!` must be the function prepared by `#'create-new-burn-vectors!-pfn`;
  the point of this macro is to wrap the Java interop which invokes it efficiently,
  hiding it from callers."
  [create-new-burn-vectors! bvs-acc i j burn-probability]
  (let [cnbv-sym (-> (gensym 'cvbv)
                     (vary-meta assoc :tag `IFn$OLLDO))]
    `(let [~cnbv-sym ~create-new-burn-vectors!]
       (.invokePrim ~cnbv-sym ~bvs-acc ~i ~j ~burn-probability))))

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
               (merge-spot-ignitions acc (spotting/spread-firebrands inputs matrices i j)))
             (transient {})
             ignited-cells))))

(defn- compute-spot-burn-vectors!
  [inputs matrices spot-ignitions ignited-cells band new-clock]
  (let [num-rows                    (:num-rows inputs)
        num-cols                    (:num-cols inputs)
        cell-size                   (:cell-size inputs)
        get-elevation               (:get-elevation inputs)
        fire-spread-matrix          (:fire-spread-matrix matrices)
        fire-spread-getter          (grid-lookup/mgetter-double fire-spread-matrix)
        burn-time-matrix            (:burn-time-matrix matrices)
        travel-lines-matrix         (:travel-lines-matrix matrices)
        max-spread-rate-matrix      (:max-spread-rate-matrix matrices)
        max-spread-direction-matrix (:max-spread-direction-matrix matrices)
        eccentricity-matrix         (:eccentricity-matrix matrices)
        modified-time-getter        (grid-lookup/mgetter-double (:modified-time-matrix matrices))
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
                                        (>= (grid-lookup/double-at fire-spread-getter (long i) (long j)) (double p))))
        pruned-spot-ignite-later    (into {} (remove ignited? spot-ignite-later)) ; TODO move to identify-spot-ignition
        pruned-spot-ignite-now      (filterv #(not (ignited? %)) spot-ignite-now) ; TODO move to identify-spot-ignition
        create-new-bvs!             (create-new-burn-vectors!-pfn num-rows num-cols cell-size get-elevation
                                                                  travel-lines-matrix fire-spread-matrix
                                                                  max-spread-rate-matrix max-spread-direction-matrix eccentricity-matrix)
        spot-burn-vectors           (persistent!
                                     (reduce
                                      (fn [bvs-acc [cell spot-info]]
                                        (let [[i j]         cell
                                              [burn-time _] spot-info]
                                          (when (> band (dec (long
                                                              ;; FIXME check if double-typed
                                                              (grid-lookup/double-at modified-time-getter i j))))
                                            (compute-max-in-situ-values! inputs matrices band i j nil))
                                          (t/mset! fire-spread-matrix i j 1.0) ;TODO parameterize burn-probability instead of 1.0
                                          ;; (t/mset! fire-spread-matrix i j burn-probability)
                                          (t/mset! burn-time-matrix i j burn-time)
                                          (create-new-burn-vectors!-invoke create-new-bvs! bvs-acc i j 1.0))) ; TODO parameterize burn-probability instead of 1.0
                                      (transient [])
                                      pruned-spot-ignite-now))]
    [spot-burn-vectors (count pruned-spot-ignite-now) pruned-spot-ignite-later (keys pruned-spot-ignite-now)]))

(defn- ignited-in-this-timestep?
  [^double global-clock-before-timestep ^double burn-time-matrix-ij]
  (< global-clock-before-timestep burn-time-matrix-ij))

(defn- grow-burn-vectors!
  "Updates the progress of the Burn Vectors along their travel lines, detecting which cells get ignited.

  Returns a [new-burn-vectors ignited-cells] tuples,
  and mutates :burn-time-matrix and :fire-spread-matrix."
  [matrices global-clock timestep burn-vectors]
  (let [fire-spread-matrix (:fire-spread-matrix matrices)
        fire-spread-getter (grid-lookup/mgetter-double fire-spread-matrix)
        burn-time-matrix   (:burn-time-matrix matrices)
        burn-time-getter   (grid-lookup/mgetter-double burn-time-matrix)
        global-clock       (double global-clock)
        timestep           (double timestep)
        ignited-cells-list (ArrayList.)]
    [(persistent!
      (reduce (fn [bvs-acc burn-vector]
                (let [i                         (burn-vec/get-i burn-vector)
                      j                         (burn-vec/get-j burn-vector)
                      direction                 (burn-vec/get-direction burn-vector)
                      fractional-distance       (burn-vec/get-fractional-distance burn-vector)
                      spread-rate               (burn-vec/get-spread-rate burn-vector)
                      terrain-distance          (burn-vec/get-terrain-distance burn-vector)
                      burn-probability          (burn-vec/get-burn-probability burn-vector)
                      fractional-distance-delta (/ (* spread-rate timestep) terrain-distance)
                      new-fractional-distance   (+ fractional-distance fractional-distance-delta)
                      crossed-center?           (and (< fractional-distance 0.5)
                                                     (>= new-fractional-distance 0.5))
                      local-burn-time           (grid-lookup/double-at burn-time-getter i j)]
                  (when crossed-center?
                    ;; INVARIANT a cell is ignited when a burn vector traverses its center,
                    ;; which corresponds to a :fractional-distance of 0.5.
                    (let [local-burn-probability (grid-lookup/double-at fire-spread-getter i j)]
                      (when (>= burn-probability local-burn-probability)
                        (let [burn-time (-> 0.5
                                            (- fractional-distance)
                                            (/ fractional-distance-delta)
                                            (* timestep)
                                            (+ global-clock))]
                          (if (> burn-probability local-burn-probability)
                            ;; INVARIANT :burn-time-matrix and :fire-spread-matrix record the fire of highest :burn-probability.
                            (do
                              (t/mset! fire-spread-matrix i j burn-probability)
                              (t/mset! burn-time-matrix i j burn-time))
                            (when (< burn-time local-burn-time)
                              (comment (assert (= burn-probability local-burn-probability)))
                              ;; Invariant: burn-time-matrix records the time of the earliest event causing the cell to burn.
                              (t/mset! burn-time-matrix i j burn-time)))))))
                  (when (and crossed-center? (not (ignited-in-this-timestep? global-clock local-burn-time))) ; first to cross center of the cell this timestep
                    (.add ignited-cells-list [i j]))
                  (conj! bvs-acc (burn-vec/make-burn-vector i j direction new-fractional-distance
                                                            spread-rate terrain-distance burn-probability))))
              (transient [])
              ;;      (if crossed-center? ; first to cross center of the cell this timestep
              ;;        (conj! ignited-cells [i j])
              ;;        ignited-cells)]))
              ;; [(transient []) (transient #{})]
              burn-vectors))
     (vec ignited-cells-list)]))

(defn- ignited-cells->burn-vectors
  "Adds new Burn Vectors from the list of newly ignited cells.

  Returns a bigger burn-vectors vector,
  and mutates :travel-lines-matrix.

  IMPORTANT: this function must be followed by #'promote-burn-vectors."
  [inputs matrices ignited-cells burn-vectors]
  (let [num-rows                    (:num-rows inputs)
        num-cols                    (:num-cols inputs)
        cell-size                   (:cell-size inputs)
        get-elevation               (:get-elevation inputs)
        fire-spread-matrix          (:fire-spread-matrix matrices)
        fire-spread-getter          (grid-lookup/mgetter-double fire-spread-matrix)
        travel-lines-matrix         (:travel-lines-matrix matrices)
        max-spread-rate-matrix      (:max-spread-rate-matrix matrices)
        max-spread-direction-matrix (:max-spread-direction-matrix matrices)
        eccentricity-matrix         (:eccentricity-matrix matrices)
        create-new-bvs!             (create-new-burn-vectors!-pfn num-rows num-cols cell-size get-elevation
                                                                  travel-lines-matrix fire-spread-matrix
                                                                  max-spread-rate-matrix max-spread-direction-matrix eccentricity-matrix)]
    (persistent!
     (reduce
      (fn [bvs-acc [i j]]
        (let [i                (long i)
              j                (long j)
              burn-probability (grid-lookup/double-at fire-spread-getter i j)]
          (create-new-burn-vectors!-invoke create-new-bvs! bvs-acc i j burn-probability)))
      (transient burn-vectors)
      ignited-cells))))

(defn- update-directional-magnitude-values!
  [matrices direction spread-rate i j]
  (let [x-magnitude-sum-matrix (:x-magnitude-sum-matrix matrices)
        y-magnitude-sum-matrix (:y-magnitude-sum-matrix matrices)
        spread-rate-sum-matrix (:spread-rate-sum-matrix matrices)
        direction              (double direction)
        spread-rate            (double spread-rate)
        cur-x                  (grid-lookup/mget-double-at x-magnitude-sum-matrix i j)
        cur-y                  (grid-lookup/mget-double-at y-magnitude-sum-matrix i j)
        cur-spread-rate        (grid-lookup/mget-double-at spread-rate-sum-matrix i j)]
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

(defn- bv-can-spread-fire-to-target-cell?
  "Whether this burn vector still has potential to ignite its target cell."
  [get-fuel-model fire-spread-matrix num-rows num-cols
   i j direction burn-probability]
  ;; HACK relying on an implementation detail of Clojure for performance; we could also make a custom Java interface.
  (let [i         (long i)
        j         (long j)
        direction (double direction)
        new-i     (+ i (direction-angle->i-incr direction))
        new-j     (+ j (direction-angle->j-incr direction))]
    ;; IMPROVEMENT currify burnable-cell? for performance. (Val, 16 Nov 2022)
    (and (burnable-cell? get-fuel-model fire-spread-matrix burn-probability
                         num-rows num-cols new-i new-j)
         (not (and (diagonal? direction)
                   ;; NOTE we use a named local for clarity; we avoid using a function for performance. (Val, 22 Nov 2022)
                   (let [crossing-diagonal-barrier? (and (not (burnable-fuel-model? (grid-lookup/double-at get-fuel-model new-i j)))
                                                         (not (burnable-fuel-model? (grid-lookup/double-at get-fuel-model i new-j))))]
                     crossing-diagonal-barrier?))))))

(defn- promote-burn-vectors
  "Replaces or removes the burn vector which have become obsolete because of an ignition.

  In particular, if the cell just got ignited at :burn-probability p,
  the burn vectors with :burn-probability < p get replaced by a Burn Vector
  starting from the center of the cell."
  [inputs matrices global-clock new-clock max-fractional-distance burn-vectors]
  (let [num-rows                    (:num-rows inputs)
        num-cols                    (:num-cols inputs)
        get-fuel-model              (:get-fuel-model inputs)
        compute-directional-values? (:compute-directional-values? inputs)
        fire-spread-matrix          (:fire-spread-matrix matrices)
        fire-spread-getter          (grid-lookup/mgetter-double fire-spread-matrix)
        burn-time-matrix            (:burn-time-matrix matrices)
        burn-time-getter            (grid-lookup/mgetter-double burn-time-matrix)
        travel-lines-matrix         (:travel-lines-matrix matrices)
        global-clock                (double global-clock)
        new-clock                   (double new-clock)
        max-fractional-distance     (double max-fractional-distance)]
    (persistent!
     (reduce
      (fn [bvs-acc burn-vector]
        (let [i                      (burn-vec/get-i burn-vector)
              j                      (burn-vec/get-j burn-vector)
              burn-probability       (burn-vec/get-burn-probability burn-vector)
              local-burn-probability (grid-lookup/double-at fire-spread-getter i j)
              local-burn-time        (grid-lookup/double-at burn-time-getter i j)]
          (if-not (and (ignited-in-this-timestep? global-clock local-burn-time)
                       ;; A burn vector of higher :burn-probability than the ignition remains unaffected by that ignition.
                       ;; WARNING: when (> 0.5 (:fractional-distance burn-vector)),
                       ;; this causes the low-probability spread in that direction to be delayed until the higher-probability
                       ;; burn vector reaches the center. This is something of a modeling inconsistency.
                       (not (overtakes-lower-probability-fire? burn-probability local-burn-probability)))
            (conj! bvs-acc burn-vector) ; the burn vector remains as is.
            (let [direction   (burn-vec/get-direction burn-vector)
                  spread-rate (burn-vec/get-spread-rate burn-vector)]
              (when compute-directional-values?
                (update-directional-magnitude-values! matrices direction spread-rate i j))
              (if (bv-can-spread-fire-to-target-cell? get-fuel-model fire-spread-matrix num-rows num-cols
                                                      i j direction burn-probability)
                (let [spread-rate             (burn-vec/get-spread-rate burn-vector)
                      terrain-distance        (burn-vec/get-terrain-distance burn-vector)
                      dt-after-ignition       (- new-clock local-burn-time)
                      new-fractional-distance (min max-fractional-distance
                                                   (+ 0.5 (/ (* spread-rate dt-after-ignition) terrain-distance)))
                      new-spread-rate         (if (< new-fractional-distance max-fractional-distance)
                                                spread-rate
                                                ;; Re-computing an empirical spread rate,
                                                ;; because we have artificially reduced the spread rate of the burn vector
                                                ;; using max-fractional-distance.
                                                ;; NOTE that we'll have (<= new-spread-rate spread-rate).
                                                (/ (* (- new-fractional-distance 0.5) terrain-distance) dt-after-ignition))
                      ;; The new BV's :burn-probability is inherited from the ignition.
                      new-burn-probability    local-burn-probability]
                  (conj! bvs-acc
                         ;; This burn vector got its progress updated.
                         (burn-vec/make-burn-vector i j direction new-fractional-distance new-spread-rate terrain-distance new-burn-probability)))
                (do
                  ;; This travel line gets cleared of its Burn Vector because there is no longer a target cell to burn.
                  (t/mset! travel-lines-matrix i j ; TODO make into function
                           (bit-clear (t/mget travel-lines-matrix i j)
                                      (direction-angle->bit direction)))
                  bvs-acc))))))
      (transient [])
      burn-vectors))))

#_(defn- fd->dt
    [fractional-distance terrain-distance spread-rate]
    (-> fractional-distance
        (* terrain-distance)
        (/ spread-rate)))

(defn- transition-burn-vectors
  "Detects when burn vector cross from one cell to another and updates accordingly.

  In addition to updating matrices (side-effect!),
  returns a [new-burn-vectors newly-ignited-cells] tuple,
  in which newly-ignited-cells is an [[i j] ...] sequence."
  [inputs matrices band global-clock new-clock max-fractional-distance burn-vectors]
  ;; IMPROVEMENT for performance (fewer object creations and flatter memory layout), we might want to return
  ;; the newly-ignited-cells not as [i j] tuples, but as BurnVectors or something like it.
  (let [cell-size                   (:cell-size inputs)
        get-elevation               (:get-elevation inputs)
        num-rows                    (:num-rows inputs)
        num-cols                    (:num-cols inputs)
        get-fuel-model              (:get-fuel-model inputs)
        travel-lines-matrix         (:travel-lines-matrix matrices)
        fire-spread-matrix          (:fire-spread-matrix matrices)
        fire-spread-getter          (grid-lookup/mgetter-double fire-spread-matrix)
        modified-time-matrix        (:modified-time-matrix matrices)
        modified-time-getter        (grid-lookup/mgetter-double modified-time-matrix)
        max-spread-rate-matrix      (:max-spread-rate-matrix matrices)
        max-spread-rate-getter      (grid-lookup/mgetter-double max-spread-rate-matrix)
        max-spread-direction-matrix (:max-spread-direction-matrix matrices)
        max-spread-direction-getter (grid-lookup/mgetter-double max-spread-direction-matrix)
        eccentricity-matrix         (:eccentricity-matrix matrices)
        eccentricity-getter         (grid-lookup/mgetter-double eccentricity-matrix)
        burn-time-matrix            (:burn-time-matrix matrices)
        burn-time-getter            (grid-lookup/mgetter-double burn-time-matrix)
        band                        (long band)
        global-clock                (double global-clock)
        new-clock                   (double new-clock)
        max-fractional-distance     (double max-fractional-distance)
        ignited-cells-list          (ArrayList.)
        terrain-dist-fn             (terrain-distance-fn get-elevation num-rows num-cols cell-size)]
    [(persistent!
      (reduce
       (fn [new-bvs-acc burn-vector]
         (let [fractional-distance        (burn-vec/get-fractional-distance burn-vector)
               hasnt-entered-target-cell? (< fractional-distance 1.0)]
           (if hasnt-entered-target-cell?
             (conj! new-bvs-acc burn-vector)
             ;; The burn vector has entered the target cell.
             (let [i                (burn-vec/get-i burn-vector)
                   j                (burn-vec/get-j burn-vector)
                   direction        (burn-vec/get-direction burn-vector)
                   burn-probability (burn-vec/get-burn-probability burn-vector)
                   direction-bit    (direction-angle->bit direction)]
               ;; TODO make into function
               ;; Clearing the corresponding travel line in the origin cell. (Val, 16 Nov 2022)
               (as-> (t/mget travel-lines-matrix i j) $
                     (bit-clear $ direction-bit)
                     (t/mset! travel-lines-matrix i j $))
               (let [new-i (+ i (direction-angle->i-incr direction))
                     new-j (+ j (direction-angle->j-incr direction))]
                 (if-not (bv-can-spread-fire-to-target-cell? get-fuel-model fire-spread-matrix num-rows num-cols
                                                             i j direction burn-probability)
                   ;; Dropping this burn vector - more exactly, not creating a new BV in the target cell.
                   ;; Note that we cleared the original BV's travel line above.
                   new-bvs-acc
                   (do
                     (when (> band (dec (long (grid-lookup/double-at modified-time-getter new-i new-j))))
                       ;; vector is first in this timestep to compute
                       ;; NOTE using the old burn-vector here is helpful:
                       ;; it increases the chance that we'll use the cell of origin
                       ;; for ToA gradient estimation.
                       (compute-max-in-situ-values! inputs matrices band new-i new-j burn-vector))
                     ;; TODO move to function
                     (as-> (t/mget travel-lines-matrix new-i new-j) $
                           (bit-set $ direction-bit)
                           (t/mset! travel-lines-matrix new-i new-j $))
                     (let [spread-rate             (burn-vec/get-spread-rate burn-vector)
                           terrain-distance        (burn-vec/get-terrain-distance burn-vector)
                           max-spread-rate         (grid-lookup/double-at max-spread-rate-getter new-i new-j)
                           max-spread-direction    (grid-lookup/double-at max-spread-direction-getter new-i new-j)
                           eccentricity            (grid-lookup/double-at eccentricity-getter new-i new-j)
                           new-spread-rate         (compute-spread-rate max-spread-rate
                                                                        max-spread-direction
                                                                        eccentricity
                                                                        direction)
                           new-terrain-distance    (terrain-distance-invoke terrain-dist-fn
                                                                            new-i
                                                                            new-j
                                                                            (+ new-i (direction-angle->i-incr direction))
                                                                            (+ new-j (direction-angle->j-incr direction)))
                           ;; time since entering new cell
                           dt-in-neighbor          (-> fractional-distance (- 1.0)
                                                       (* terrain-distance) ; the length spent in the new cell
                                                       (/ spread-rate))
                           new-fractional-distance (min max-fractional-distance
                                                        (/ (* new-spread-rate dt-in-neighbor) new-terrain-distance))
                           new-spread-rate         (if (< new-fractional-distance max-fractional-distance)
                                                     new-spread-rate
                                                     (/ (* new-fractional-distance new-terrain-distance) dt-in-neighbor))
                           new-bvs-acc+1           (conj! new-bvs-acc
                                                          ;; This new Burn Vector is the result of moving ('transitioning') the old Burn Vector to the new cell.
                                                          (burn-vec/make-burn-vector new-i new-j direction new-fractional-distance new-spread-rate
                                                                                     new-terrain-distance burn-probability))
                           bv-has-reached-center?  (>= new-fractional-distance 0.5)]
                       (when bv-has-reached-center?
                         (let [local-burn-probability     (grid-lookup/double-at fire-spread-getter new-i new-j)
                               failed-to-ignite-new-cell? (< burn-probability local-burn-probability)]
                           (when-not failed-to-ignite-new-cell?
                             (let [relative-burn-time (-> (/ dt-in-neighbor new-fractional-distance)
                                                          (* 0.5))
                                   burn-time          (-> new-clock
                                                          (- dt-in-neighbor)
                                                          (+ relative-burn-time))
                                   recorded-burn-time (grid-lookup/double-at burn-time-getter new-i new-j)]
                               (if (overtakes-lower-probability-fire? burn-probability local-burn-probability)
                                 (do
                                   (t/mset! fire-spread-matrix new-i new-j burn-probability) ;; logically a max update
                                   (t/mset! burn-time-matrix new-i new-j burn-time)
                                   ;; For performance, avoiding redundant additions to ignited-cells.
                                   (when-not (ignited-in-this-timestep? global-clock recorded-burn-time)
                                     (.add ignited-cells-list [new-i new-j])))
                                 ;; This conditional branch corresponds to: (= burn-probability local-burn-probability)
                                 ;; this is why we don't add to ignited cells (it's already ignited).
                                 (when (< burn-time recorded-burn-time)
                                   ;; We have just realized that the cell actually ignited earlier than recorded until now,
                                   ;; so we correct the recorded-burn-time.
                                   ;; Invariant: burn-time-matrix holds the earliest time a burn vector ignited it.
                                   (t/mset! burn-time-matrix new-i new-j burn-time)))))))
                       new-bvs-acc+1))))))))
       (transient [])
       burn-vectors))
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
        modified-time-getter        (grid-lookup/mgetter-double modified-time-matrix)
        max-spread-rate-matrix      (:max-spread-rate-matrix matrices)
        max-spread-rate-getter      (grid-lookup/mgetter-double max-spread-rate-matrix)
        max-spread-direction-matrix (:max-spread-direction-matrix matrices)
        max-spread-direction-getter (grid-lookup/mgetter-double max-spread-direction-matrix)
        eccentricity-matrix         (:eccentricity-matrix matrices)
        eccentricity-getter         (grid-lookup/mgetter-double eccentricity-matrix)]
    (mapv (fn [burn-vector]
            (let [i (burn-vec/get-i burn-vector)
                  j (burn-vec/get-j burn-vector)]
              (when (> band (dec (long (grid-lookup/double-at modified-time-getter i j))))
                (compute-max-in-situ-values! inputs matrices band i j burn-vector))
              (let [direction            (burn-vec/get-direction burn-vector)
                    max-spread-rate      (grid-lookup/double-at max-spread-rate-getter i j)
                    max-spread-direction (grid-lookup/double-at max-spread-direction-getter i j)
                    eccentricity         (grid-lookup/double-at eccentricity-getter i j)
                    new-spread-rate      (compute-spread-rate max-spread-rate
                                                              max-spread-direction
                                                              eccentricity
                                                              direction)]
                (assoc burn-vector :spread-rate new-spread-rate))))
          burn-vectors)))

(defn- compute-fire-front-direction!
  [matrices ^long i ^long j]
  (let [x-magnitude-sum-matrix (:x-magnitude-sum-matrix matrices)
        y-magnitude-sum-matrix (:y-magnitude-sum-matrix matrices)
        spread-rate-sum-matrix (:spread-rate-sum-matrix matrices)
        x-magnitude-sum        (grid-lookup/mget-double-at x-magnitude-sum-matrix i j)
        y-magnitude-sum        (grid-lookup/mget-double-at y-magnitude-sum-matrix i j)
        spread-rate-sum        (grid-lookup/mget-double-at spread-rate-sum-matrix i j)]
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
          max-spread-rate-getter          (grid-lookup/mgetter-double max-spread-rate-matrix)
          max-spread-direction-matrix     (:max-spread-direction-matrix matrices)
          max-spread-direction-getter     (grid-lookup/mgetter-double max-spread-direction-matrix)
          eccentricity-matrix             (:eccentricity-matrix matrices)
          eccentricity-getter             (grid-lookup/mgetter-double eccentricity-matrix)
          residence-time-matrix           (:residence-time-matrix matrices)
          residence-time-getter           (grid-lookup/mgetter-double residence-time-matrix)
          reaction-intensity-matrix       (:reaction-intensity-matrix matrices)
          reaction-intensity-getter       (grid-lookup/mgetter-double reaction-intensity-matrix)
          directional-flame-length-matrix (:directional-flame-length-matrix matrices)]
      (doseq [[i j] ignited-cells]
        (let [i                    (long i)
              j                    (long j)
              direction            (compute-fire-front-direction! matrices i j)
              max-spread-rate      (grid-lookup/double-at max-spread-rate-getter i j)
              max-spread-direction (grid-lookup/double-at max-spread-direction-getter i j)
              eccentricity         (grid-lookup/double-at eccentricity-getter i j)
              spread-rate          (compute-spread-rate max-spread-rate
                                                        max-spread-direction
                                                        eccentricity
                                                        direction)
              residence-time       (grid-lookup/double-at residence-time-getter i j)
              reaction-intensity   (grid-lookup/double-at reaction-intensity-getter i j)
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
      (compute-max-in-situ-values! inputs matrices band i j nil)
      (when compute-directional-values?
        (t/mset! directional-flame-length-matrix i j (grid-lookup/mget-double-at flame-length-matrix i j))))))

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
                                                      ;; Why this 1.99 max-fractional-distance values? To enforce the
                                                      ;; INVARIANT that any Burn Vector in the current timestep cannot travel more than a single cell-size
                                                      ;; (does not account elevation just horizontal). See compute-dt. There is one
                                                      ;; EDGE CASE in which bvs can burn pass one cell size given the spread rate and
                                                      ;; the computed timestep. It is when a bv gets created from ignited-cells->burn-vectors phase.
                                                      ;; The spread rate of this bv was not included in the compute-dt step and so if it has a
                                                      ;; higher spread rate than any other bv used in compute-dt then it will burn more than a
                                                      ;; single cell size.
                                                      ;; An alternative to capping the fractional-distance would be to adjust the time steps
                                                      ;; to be an upper bound of the time required for the burn vectors to cross.
                                                      ;; We tried that based on max-spread-rate calculations, but found it to be extremely
                                                      ;; inefficient because it made the time steps unreasonably small.
                                                      ;; Capping the fractional distance caused only small changes in prediction accuracy,
                                                      ;; so we went with that.
                                                      ;; More background on this approach and how it relates to the Morais 2001 paper can be found here:
                                                      ;; https://github.com/pyregence/gridfire/pull/125#issuecomment-1322861034
                                                      (promote-burn-vectors inputs matrices global-clock new-clock 1.99)
                                                      ;; (= 0.99 max-fractional-distance) to make sure that
                                                      ;; transitioning Burn Vectors will not double-transition.
                                                      (transition-burn-vectors inputs matrices band global-clock new-clock 0.99))
                  promoted-transitioned-bvs      (->> transitioned-bvs
                                                      (ignited-cells->burn-vectors inputs matrices transition-ignited-cells)
                                                      (promote-burn-vectors inputs matrices global-clock new-clock 0.99)) ;TODO optimize, promoting twice
                  ;; NOTE why are we promoting exactly twice in the above? Because there are 2 ignition-causing phases: grow-burn-vectors! and transition-burn-vectors.
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
              ;; of ignited cells in this list of ignited cells passed to compute-directional-in-situ-values!
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
;; fire-spread-core-algorithm ends here

;; [[file:../../org/GridFire.org::fire-spread-matrices][fire-spread-matrices]]
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
     travel-lines-matrix                   ; INTRO the set of burn vector directions currently blocked in each cell, encoded as 8 bit flags.
     x-magnitude-sum-matrix
     y-magnitude-sum-matrix])

(def matrix-k->out-of-bounds-value
  {:burn-time-matrix -1.0})

(defn make-simulation-matrices
  [m]
  (map->SimulationMatrices (->> m
                                (map (fn [[k m]]
                                       [k (when (t/tensor? m)
                                            (grid-lookup/add-double-getter m
                                                                           (get matrix-k->out-of-bounds-value k)))]))
                                (into {}))))

(comment

  :fire-spread-matrix ; INTRO keeps track of which cells have burned, as a probability between 0 and 1.
  :burn-probability   ; INTRO holds the same probability values in Burn Vectors.
  ;; The idea is that the simulation simulates not a single fire line,
  ;; but a superposition of fire lines corresponding to a spectrum of 'parallel worlds' with different fire spread.
  ;; A burn probability of 0.84 means that this cell burns in 84% of these parallel worlds.
  ;; INVARIANT: a burn vector with a :burn-probability of P can only ignite cells that have not been reached
  ;; by BVs of :burn-probability higher than P, and will then update :fire-spread-matrix to P.
  ;; If the generated :burn-probability values are only allowed to be 0 or 1,
  ;; we retrieve the more classical and less sophisticated model of a single fire spread.
  ;; Why probabilistic values? The motivation comes from the random nature of spotting.
  ;; WARNING: it is tricky to reason about the probability model underlying this logic.
  ;; In particular, it is NOT equivalent to drawing i.i.d spot ignitions, since burn vectors
  ;; of independent origins can block one another.
  ;; It is an error to think of this probabilistic-updating logic as one would think of, say,
  ;; the diffusion equation associated with a random walk.
  ;; One way to think about this is that there is a 1D latent random variable,
  ;; the 'luck level' of the fire evolution, ranging from 0 to 1;
  ;; A burn vector with :burn-probability 0.23 is present in 23% of the parallel universes,
  ;; those with luck level no greater than 0.23.
  ;; TODO turn this into a proper analysis in the Org article.

  ;; Here's a summary of this approach by Gary Johnson (https://mattermost.sig-gis.com/cec-project/pl/8go9hbcdciyp5bz6rmdu7sw59o):
  ;; The logic is fairly simple:
  ;; 1. The initial ignition site (or perimeter) is assigned a burn-probability of 1.0 at simulation start time since the fact that these cells are ignited is one of our model's necessary initial conditions.
  ;; 2. All burn vectors originating by means of a surface or crown fire from these cells are considered part of the "main fire" and are all given the same 1.0 burn-probability value. We come to this by noting that surface and crown fires spread deterministically in GridFire. (While perturbations to the underlying datasets may be calculated at model runtime, they are only applied once per input per b,i,j coordinate and are considered to not represent randomness in the model so much as uncertainty in the input data. Since they could have been computed in a batch operation prior to starting each simulation, we are considering these perturbed datasets to be equivalent to unperturbed datasets in terms of describing one possible world.)
  ;; 3. If a burned cell's fire line intensity is high enough to cause either surface or crown spotting, then we cast a number of embers out in front of the main fire by means of a spatial random sampling algorithm.
  ;; 4. Any unburned cells which receive an ember are assigned an ignition probability based on formulas from the academic literature. As more embers accumulate in any unburned cell, its ignition probability will increase.
  ;; 5. Each timestep, we use a random sampling process to determine which ember-containing cells may ignite and become burned. If any such cell does ignite, we assign its precomputed ignition probability as the burn-probability value on all outgoing burn vectors. In this way, all of the burn vectors within each spot fire will have the same burn-probability but this burn-probability value will vary between spot fires.
  ;; 6. Whenever burn vectors from different fires (either any combination of the main fire with spot fires or just spot fires with other spot fires) both enter the same cell, we use a collision resolution algorithm that allows higher probability fires to burn through cells that have already been burned by a lower probability fire. At the same time, lower probability fires will be extinguished when running into higher probability fires.
  ;; 7. At the end of a simulation, the burn-probability value in each cell (stored in the unfortunately named fire-spread-matrix) should be a positive number indicating the highest probability fire that burned through it. Note again that this probability is simply tracking the probability that an ignition would have occurred at the initial ignition site for each fire (main or spot). The logic goes like this: If the main fire (1.0) reached you in the simulation time, then by the deterministic rules of our spread algorithm, this cell would definitely burn. If not, then the next highest probability spot fire would lay claim to burning this cell. Since those spot fires may have never come into existence in the first place, we don't have a deterministic guarantee that this cell would have burned. However, if a particular spot fire did, in fact, ignite (with probability p), then its deterministic surface/crown spread algorithm would guarantee that this cell would be burned.
  ;; 8. Since spot fires can also cast embers with a high enough fire line intensity, we calculate the burn-probability of the child spot fire as the product of its ignition probability with its parent's ignition probability. So if the parent ignited with probability p0=0.6 and the child ignited with probability p1=0.5, then we set the child's burn vectors to have a burn-probability of p0*p1=0.3. Obviously, this includes a baked in assumption that these chains of spot ignition events are distributed i.i.d, which may be overly naive. We might consider given these a more thorough treatment as a Markov process instead, but this served as a simple first pass approximation.
  ;; The bulk of the code related to burn-probability is, of course, in the collision detection and resolution logic. If we want to change how the probabilities propagate from one spot fire to another, that should be isolated to a quite small part of the code currently without needing to change the rest of the logic significantly.

  *e)
;; fire-spread-matrices ends here

;; [[file:../../org/GridFire.org::fire-spread-matrices][fire-spread-matrices]]
;;-----------------------------------------------------------------------------
;; Main Simulation Entry Point - Dispatches to Point/Perimeter Ignition
;;-----------------------------------------------------------------------------

;; TODO: Move this multimethod check into run-simulations to avoid running it in every thread
(defmulti ^:private run-fire-spread*
  (fn [inputs]
    (if (vector? (:initial-ignition-site inputs))
      :ignition-point
      :ignition-perimeter)))

(defn run-fire-spread
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
  [inputs]
  (let [sfmin-memoization (get-in inputs [:memoization :surface-fire-min])]
    (binding [rothermel-fast-wrapper-optimal (if (= sfmin-memoization :within-sims) ; NOTE :within-sims to avoid the memory leaks caused by :across-sims.
                                               (memoize-rfwo rothermel-fast-wrapper-optimal)
                                               rothermel-fast-wrapper-optimal)]
      (run-fire-spread* inputs))))

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
                                        (float-array -1.0)
                                        (t/ensure-tensor)
                                        (t/reshape shape)
                                        (t/mset! i j ignition-start-time))]
    (make-simulation-matrices
     {:burn-time-matrix                burn-time-matrix
      :eccentricity-matrix             (t/new-tensor shape :datatype :float32)
      :fire-line-intensity-matrix      (t/new-tensor shape :datatype :float32)
      :fire-spread-matrix              (-> (t/new-tensor shape :datatype :float32) (t/mset! i j 1.0))
      :fire-type-matrix                (t/new-tensor shape :datatype :float32)
      :firebrand-count-matrix          (when spotting (t/new-tensor shape :datatype :int32))
      :flame-length-matrix             (t/new-tensor shape :datatype :float32)
      :directional-flame-length-matrix (when compute-directional-values? (t/new-tensor shape :datatype :float32))
      :max-spread-direction-matrix     (t/new-tensor shape :datatype :float32)
      :max-spread-rate-matrix          (t/new-tensor shape :datatype :float32)
      :modified-time-matrix            (t/new-tensor shape :datatype :int32)
      :residence-time-matrix           (when compute-directional-values? (t/new-tensor shape :datatype :float32))
      :reaction-intensity-matrix       (when compute-directional-values? (t/new-tensor shape :datatype :float32))
      :spot-matrix                     (when spotting (t/new-tensor shape :datatype :float32))
      :spread-rate-matrix              (t/new-tensor shape :datatype :float32)
      :spread-rate-sum-matrix          (when compute-directional-values? (t/new-tensor shape :datatype :float32))
      :travel-lines-matrix             (t/new-tensor shape :datatype :int16)
      :x-magnitude-sum-matrix          (when compute-directional-values? (t/new-tensor shape :datatype :float32))
      :y-magnitude-sum-matrix          (when compute-directional-values? (t/new-tensor shape :datatype :float32))})))

(defmethod run-fire-spread* :ignition-point
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
        positive-burn-scar          (t/clone (:initial-ignition-site inputs) :datatype :float32)
        ignition-start-time         (:ignition-start-time inputs)
        spotting                    (:spotting inputs)
        compute-directional-values? (:compute-directional-values? inputs)
        shape                       [num-rows num-cols]
        negative-burn-scar          (d/clone (dfn/* -1.0 positive-burn-scar))
        burn-time-matrix            (-> (* num-rows num-cols)
                                        (float-array -1.0)
                                        (t/ensure-tensor)
                                        (t/reshape shape)
                                        (add-ignited-cells! ignited-cells ignition-start-time))]
    (make-simulation-matrices
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
      :spot-matrix                     (when spotting (t/new-tensor shape :datatype :float32))
      :spread-rate-matrix              (d/clone negative-burn-scar)
      :spread-rate-sum-matrix          (when compute-directional-values? (t/new-tensor shape :datatype :float32))
      :travel-lines-matrix             (t/new-tensor shape :datatype :int16)
      :x-magnitude-sum-matrix          (when compute-directional-values? (t/new-tensor shape :datatype :float32))
      :y-magnitude-sum-matrix          (when compute-directional-values? (t/new-tensor shape :datatype :float32))})))

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
                                   (grid-lookup/add-double-getter initial-ignition-site)
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

(defmethod run-fire-spread* :ignition-perimeter
  [inputs]
  (let [ignited-cells (get-perimeter-cells inputs)]
    (when (seq ignited-cells)
      (run-loop inputs
                (initialize-perimeter-ignition-matrices inputs ignited-cells)
                ignited-cells))))
;; fire-spread-entry-point ends here
