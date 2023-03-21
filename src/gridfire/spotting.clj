;; [[file:../../org/GridFire.org::sardoy-firebrand-dispersal][sardoy-firebrand-dispersal]]
(ns gridfire.spotting
  (:require [gridfire.common              :refer [burnable-cell?
                                                  burnable-fuel-model?
                                                  calc-fuel-moisture
                                                  in-bounds-optimal?
                                                  terrain-distance-fn
                                                  terrain-distance-invoke]]
            [gridfire.conversion          :as convert]
            [gridfire.grid-lookup         :as grid-lookup]
            [gridfire.spotting.elmfire    :as spotting-elm]
            [gridfire.utils.random        :refer [my-rand-range]]
            [tech.v3.tensor               :as t]
            [vvvvalvalval.supdate.api     :as supd])
  (:import (java.util Random)
           (org.apache.commons.math3.distribution PoissonDistribution)
           (org.apache.commons.math3.random RandomGenerator JDKRandomGenerator)
           (org.apache.commons.math3.util FastMath)))
(set! *unchecked-math* :warn-on-boxed)

;;-----------------------------------------------------------------------------
;; Formulas
;;-----------------------------------------------------------------------------

(defn- sample-normal
  "Returns sample from normal/gaussian distribution given mu and sd."
  ^double
  [^Random rand-gen ^double mu ^double sd]
  (+ mu (* sd (.nextGaussian rand-gen))))

(defn- sample-lognormal
  "Returns sample from log-normal distribution given mu and sd."
  ^double
  [^Random rand-gen ^double mu ^double sd]
  (FastMath/exp (sample-normal rand-gen mu sd)))

(defn deltax-expected-value
  ^double [^double mu-x ^double sigma-x]
  (convert/m->ft (FastMath/exp (+ mu-x
                                  (/ (FastMath/pow sigma-x (int 2))
                                     2.0)))))

(defn deltax-coefficient-of-variation
  ^double [^double sigma-x]
  (FastMath/sqrt (- (FastMath/exp (FastMath/pow sigma-x (int 2)))
                    1)))

;; NOTE might be turned into a multimethod.
(defn resolve-lognormal-params
  [spotting-config ^double fire-line-intensity ^double  wind-speed-20ft]
  (spotting-elm/resolve-lognormal-params spotting-config fire-line-intensity wind-speed-20ft))

(defn delta-x-sampler
  "Returns a function for randomly sampling ΔX, the spotting jump along the wind direction (in ft)."
  [spotting-config ^double fire-line-intensity ^double wind-speed-20ft]
  (let [ln-params (resolve-lognormal-params spotting-config fire-line-intensity wind-speed-20ft)
        mux       (:prob.lognormal/mu ln-params)
        sigmax    (:prob.lognormal/sigma ln-params)]
    (fn ^double draw-deltax-ft [rand-gen]
      (-> (sample-lognormal rand-gen mux sigmax)
          (convert/m->ft)))))

(def ^:private sigma-y-scalar-ft
  (* (convert/m->ft 1.0)
     0.92
     (/ 0.47
        (FastMath/pow 0.88 2))))

(defn himoto-resolve-default-sigma-y-from-lognormal-params
  ^double [^double mu-x ^double sigma-x]
  (let [es2h         (FastMath/exp (/ (FastMath/pow sigma-x 2)
                                      2))
        avg-deltax-m (* (FastMath/exp mu-x) es2h)]
    (* (double sigma-y-scalar-ft)
       avg-deltax-m
       (+ es2h 1.0)
       (- es2h 1.0))))

(comment
  ;; When will we have the default sigma_Y > E[ΔX]?
  ;; It can be seen that this nonsensical situation
  ;; happens iff sigma_X exceeds the following number:
  (Math/sqrt
   (Math/log (+ 1.0
                (/ (Math/pow 0.88 2)
                   (* 0.92 0.47)))))
  ;; =>
  1.0131023746492023

  *e)

(defn himoto-resolve-default-sigma-y
  ^double [spotting-config ^double fire-line-intensity ^double wind-speed-20ft]
  (let [ln-params (resolve-lognormal-params spotting-config fire-line-intensity wind-speed-20ft)
        mux       (:prob.lognormal/mu ln-params)
        sigmax    (:prob.lognormal/sigma ln-params)]
    (himoto-resolve-default-sigma-y-from-lognormal-params mux sigmax)))

(defn resolve-delta-y-sigma
  ^double [spotting-config ^double fire-line-intensity ^double wind-speed-20ft]
  (or (some-> (:delta-y-sigma spotting-config) (double))
      (himoto-resolve-default-sigma-y spotting-config fire-line-intensity wind-speed-20ft)))

(defn delta-y-sampler
  "Returns a function for randomly sampling ΔY, the spotting jump perpendicular to the wind direction (in ft)."
  [spotting-config ^double fire-line-intensity ^double wind-speed-20ft]
  (let [sigma-y (resolve-delta-y-sigma spotting-config fire-line-intensity wind-speed-20ft)]
    (fn draw-deltay-ft ^double [rand-gen]
      (convert/m->ft (sample-normal rand-gen 0 sigma-y)))))

(defn- sample-wind-dir-deltas
  "Draws a random sequence of [ΔX ΔY] pairs of signed distances (in ft) from the supplied cell,
  representing the coordinates of the spotting jump
  in the directions parallel and perpendicular to the wind.
  ΔX will typically be positive (downwind),
  and positive ΔY means to the right of the downwind direction."
  [inputs fire-line-intensity wind-speed-20ft num-firebrands]
  (let [spotting-config   (:spotting inputs)
        rand-gen          (:rand-gen inputs)
        fl-intensity      (convert/Btu-ft-s->kW-m fire-line-intensity)
        ws20ft            (convert/mph->mps wind-speed-20ft)
        sample-delta-x-fn (delta-x-sampler spotting-config fl-intensity ws20ft)
        sample-delta-y-fn (delta-y-sampler spotting-config fl-intensity ws20ft)]
    (->> (range num-firebrands)
         ;; NOTE it turns out that clojure.core/repeatedly is slow (lazy seq overhead), (Val, 19 Mar 2023)
         ;; so we're re-implementing a fast vector-returning version of it.
         (mapv (fn sample-delta-tuple [_i]
                 [(sample-delta-x-fn rand-gen)
                  (sample-delta-y-fn rand-gen)])))))
;; sardoy-firebrand-dispersal ends here
;; [[file:../../org/GridFire.org::convert-deltas][convert-deltas]]
(defn delta->grid-dx
  "Computes the grid-aligned x coordinate of the delta vector, given the wind-aligned [ΔX ΔY] coordinates."
  ^double [^double cos-w ^double sin-w ^double delta-x ^double delta-y]
  ;; NOTATION: |w>, |wp>, |gx>, |gy> represent unit vectors,
  ;; respectively in the downwind, wind-perpendicular, grid x, grid y directions.
  ;; Note that the firebrand trajectory vector is |Δ> = (ΔX.|w> + ΔY.|wp>),
  ;; such that <gx|Δ> = (ΔX.<gx|w> + ΔY.<gx|wp>)
  (let [<gx|w>  sin-w
        <gx|wp> cos-w]
    (+ (* delta-x <gx|w>)
       (* delta-y <gx|wp>))))

(defn delta->grid-dy
  "Computes the grid-aligned y coordinate of the delta vector, given the wind-aligned [ΔX ΔY] coordinates.

  Returns a signed distance (same unit as ΔX and ΔY)."
  ^double [^double cos-w ^double sin-w ^double delta-x ^double delta-y]
  ;; Note that the firebrand trajectory vector is |Δ> = (ΔX.|w> + ΔY.|wp>),
  ;; such that <gy|Δ> = (ΔX.<gy|w> + ΔY.<gy|wp>)
  (let [<gy|w>  (- cos-w)
        <gy|wp> sin-w]
    (+ (* delta-x <gy|w>)
       (* delta-y <gy|wp>))))

(defn distance->n-cells
  "Converts a delta expressed as a signed distance to one expressed as a number of grid cells."
  ^long [^double cell-size ^double d]
  (FastMath/round (/ d cell-size)))
;; convert-deltas ends here
;; [[file:../../org/GridFire.org::firebrand-ignition-probability][firebrand-ignition-probability]]
(defn heat-of-preignition
  "Returns heat of preignition given:
   - Temperature: (Celsius)
   - Fine fuel moisture (0-1 ratio)

   Q_ig = 144.512 - 0.266*T_o - 0.00058 * (T_o)^2 - T_o * M + 18.54 * (1 - exp ( -15.1 * M ) ) + 640 * M  (eq. 10)"
  ^double
  [^double temperature ^double fine-fuel-moisture]
  (let [T_o temperature
        M   fine-fuel-moisture

        ;; heat required to reach ignition temperature
        Q_a (+ 144.512 (* -0.266 T_o) (* -0.00058 (FastMath/pow T_o 2)))

        ;; heat required to raise moisture to reach boiling point
        Q_b (* -1.0 T_o M)

        ;; Heat of desorption
        Q_c (* 18.54 (- 1.0 (FastMath/exp (* -15.1 M))))

        ;; Heat required to vaporize moisture
        Q_d (* 640.0 M)]
    (+ Q_a Q_b Q_c Q_d)))

(defn schroeder-ign-prob
  "Returns the probability of ignition as described in Shroeder (1969) given:
   - Temperature: (Celsius)
   - Fine fuel moisture (0-1 ratio)

   X = (400 - Q_ig) / 10
   P(I) = (0.000048 * X^4.3) / 50    (pg. 15)"
  ^double
  [^double temperature ^double fine-fuel-moisture]
  (let [Q_ig (heat-of-preignition temperature fine-fuel-moisture)
        X    (/ (- 400.0 Q_ig) 10.0)]
    (-> X
        (FastMath/pow 4.3)
        (* 0.000048)
        (/ 50.0)
        (FastMath/min 1.0)
        (FastMath/max 0.0))))

(defn- one-minus ^double [^double x] (- 1.0 x))

(defmacro spot-ignition-probability*
  "Returns the probability of spot fire ignition (Perryman 2012) given:
   - Schroeder's probability of ignition [P(I)] (0-1)
   - Decay constant [lambda] (m^-1)
   - Distance from the torched cell [d] (meters)

   P(Spot Ignition) = P(I) * exp(-lambda * d).

   For performance, syntactic abstraction is used to allow early termination of the calculation,
   through the use of upper bounds on the result probability. Concretely, this is done
   by injecting additional arguments into `yield-expr`, which must be a list.
   `yield-expr` will receive either 1 argument-the final probability-
   or it will receive 2 arguments-an upper bound and an expression for continuing the computation."
  [yield-expr decay-constant spotting-distance ignition-probability]
  `(let [distance-penalty# (-> ~decay-constant
                               (* -1.0)
                               (* ~spotting-distance)
                               (FastMath/exp))]
     (~@yield-expr distance-penalty#
      (let [pi# ~ignition-probability]
        (~@yield-expr (* pi# distance-penalty#))))))


(defmacro yield-lazy-sequence
  [& args]
  (case (count args)
    1 (let [[arg0] args]
        `(lazy-seq [~arg0]))
    2 (let [[upper-bound next-steps] args]
        `(cons ~upper-bound (lazy-seq ~next-steps)))))

(defmacro yield-successful-prob
  "(successful-prob rand01) is to be used as the `yield-expr` in `spot-ignition-probability*`
  when you want to obtain the probability only if it is > rand01, and return nil otherwise."
  [rand01 & args]
  (case (count args)
    1 (let [[arg0] args]
        `(let [p# ~arg0]
           (when (< ~rand01 p#)
             p#)))
    2 (let [[upper-bound next-steps] args]
        `(when (< ~rand01 ~upper-bound)
           ~next-steps))))

(defmacro yield-final-result
  [& args]
  (case (count args)
    1 (let [[arg0] args]
        arg0)
    2 (let [[_upper-bound next-steps] args]
        next-steps)))

(defn spot-ignition-probability
  ^double
  [^double ignition-probability ^double decay-constant ^double spotting-distance]
  (spot-ignition-probability* (yield-final-result) decay-constant spotting-distance ignition-probability))
;; firebrand-ignition-probability ends here
;; [[file:../../org/GridFire.org::firebrands-time-of-ignition][firebrands-time-of-ignition]]
(defn spot-ignition?
  [rand-gen ^double spot-ignition-probability]
  (let [random-number (my-rand-range rand-gen 0 1)]
    (>= spot-ignition-probability random-number)))

(defn albini-t-max
  "Returns the time of spot ignition using (Albini 1979) in minutes given:
   - Flame length: (m) [z_F]

   a = 5.963                                                     (D33)
   b = a - 1.4                                                   (D34)
   D = 0.003
   t_c = 1
   w_F = 2.3 * (z_F)^0.5                                         (A58)
   t_o = t_c / (2 * z_F / w_F)
   z =  0.39 * D * 10^5
   t_T = t_o + 1.2 + (a / 3) * ( ( (b + (z/z_F) )/a )^3/2 - 1 )  (D43)"
  ^double
  [^double flame-length]
  (let [a     5.963                                ; constant from (D33)
        b     4.563                                ; constant from (D34)
        z-max 117.0                                ; max height given particle diameter of 0.003m
        w_F   (* 2.3 (FastMath/sqrt flame-length)) ; upward axial velocity at flame tip
        t_0   (/ w_F (* 2.0 flame-length))]        ; period of steady burning of tree crowns (t_c, min) normalized by 2*z_F / w_F
    (-> z-max
        (/ flame-length)
        (+ b)
        (/ a)
        (FastMath/pow 1.5)
        (- 1.0)
        (* (/ a 3.0))
        (+ 1.2)
        (+ t_0))))

(defn spot-ignition-time
  "Returns the time of spot ignition using (Albini 1979) and (Perryman 2012) in minutes given:
   - Global clock: (min)
   - Flame length: (m)

   t_spot = clock + (2 * t_max) + t_ss"
  ^double
  [^double burn-time ^double flame-length]
  (let [t-steady-state 20.0] ; period of building up to steady state from ignition (min)
    (-> (albini-t-max flame-length)
        (* 2.0)
        (+ burn-time)
        (+ t-steady-state))))
;; firebrands-time-of-ignition ends here
;; [[file:../../org/GridFire.org::spotting-fire-probability][spotting-fire-probability]]
(defn- in-range?
  [[min max] fuel-model-number]
  (<= min fuel-model-number max))

(defn- intranges-mapping-lookup
  "Looks up a value in a mapping from fuel number to anything,
  encoded as either a single value v (constant mapping),
  or as a vector of [[min-fuel-number max-fuel-number] v] pairs,
  such as:
  [[[1 140]   0.0]
   [[141 149] 1.0]
   [[150 256] 1.0]]"
  [intranges-mapping fuel-model-number]
  (if (vector? intranges-mapping)
    ;; IMPROVEMENT for performance, we could do a non-sequential lookup, (Val, 02 Nov 2022)
    ;; e.g. a dichotomic search,
    ;; or even better just (aget) an array into which we have indexed the decompressed mapping.
    (loop [irm-entries intranges-mapping]
      (when-let [[fuel-range v] (first irm-entries)]
        (if (in-range? fuel-range fuel-model-number)
          v
          (recur (rest irm-entries)))))
    intranges-mapping))

(defn surface-fire-spot-fire?
  "Expects surface-fire-spotting config to be a sequence of tuples of
  ranges [lo hi] and spotting probability. The range represents the range (inclusive)
  of fuel model numbers that the spotting probability is set to.
  [[[1 140] 0.0]
  [[141 149] 1.0]
  [[150 256] 1.0]]"
  [inputs [i j] ^double fire-line-intensity]
  (let [rand-gen                     (:rand-gen inputs)
        get-fuel-model               (:get-fuel-model inputs)
        fuel-model-number            (long (grid-lookup/double-at get-fuel-model i j))
        surface-fire-spotting        (:surface-fire-spotting (:spotting inputs))
        critical-fire-line-intensity (-> (:critical-fire-line-intensity surface-fire-spotting)
                                         (intranges-mapping-lookup fuel-model-number)
                                         (or 0.0)
                                         (double))]
    (when (and surface-fire-spotting
               (> fire-line-intensity critical-fire-line-intensity))
      (let [spot-percent (-> (:spotting-percent surface-fire-spotting)
                             (intranges-mapping-lookup fuel-model-number)
                             (or 0.0)
                             (double))]
        (>= spot-percent (my-rand-range rand-gen 0.0 1.0))))))

(defn crown-spot-fire?
  "Determine whether crowning causes spot fires. Config key `:spotting` should
   take either a vector of probabilities (0-1) or a single spotting probability."
  [inputs]
  (when-some [spot-percent (:crown-fire-spotting-percent (:spotting inputs))] ; WARNING 'percent' is misleading. (Val, 17 Mar 2023)
    (let [rand-gen (:rand-gen inputs)
          p        (double spot-percent)]
      (>= p (my-rand-range rand-gen 0.0 1.0)))))

(defn- spot-fire? [inputs crown-fire? here fire-line-intensity]
  (if crown-fire?
    (crown-spot-fire? inputs)
    (surface-fire-spot-fire? inputs here fire-line-intensity)))

(defn- sample-poisson
  ^long [^double mean ^RandomGenerator rng]
  (-> (PoissonDistribution. rng
                            mean
                            PoissonDistribution/DEFAULT_EPSILON
                            PoissonDistribution/DEFAULT_MAX_ITERATIONS)
      (.sample)
      (long)))

(defn sample-number-of-firebrands
  ^long [spotting-config ^RandomGenerator rng]
  (sample-poisson (:num-firebrands spotting-config) rng))
;; spotting-fire-probability ends here
;; [[file:../../org/GridFire.org::spread-firebrands][spread-firebrands]]

(defn- update-firebrand-count!-fn
  [inputs firebrand-count-matrix fire-spread-matrix i0 j0]
  (let [num-rows                (:num-rows inputs)
        num-cols                (:num-cols inputs)
        get-fuel-model          (:get-fuel-model inputs)
        source-burn-probability (grid-lookup/mget-double-at fire-spread-matrix (long i0) (long j0))]
    (fn inc-target-cell! [i1 j1]
      ;; FIXME REVIEW Why this check? Why would we not count firebrands in non-burnable cells, e.g. urban areas?
      (when (burnable-cell? get-fuel-model
                            fire-spread-matrix
                            source-burn-probability
                            num-rows
                            num-cols
                            i1
                            j1)
        (->> (grid-lookup/mget-double-at firebrand-count-matrix i1 j1)
             (long)
             (inc)
             (t/mset! firebrand-count-matrix i1 j1))))))

;; FIXME: Drop cell = [i j]
(defn spread-firebrands
  "Returns a sequence of [[i1 j1] [t p]] key value pairs where
  key: [i1 j1] grid coordinates of the target cell
  val: [t p] where:
  t: time to ignition
  p: ignition-probability.

  Also mutates :firebrand-count-matrix to update the counts."
  [inputs matrices i j]
  (let [cell                       [i j]
        i                          (long i)
        j                          (long j)
        fire-line-intensity-matrix (:fire-line-intensity-matrix matrices)
        fire-type-matrix           (:fire-type-matrix matrices)
        fire-line-intensity        (grid-lookup/mget-double-at fire-line-intensity-matrix i j)
        crown-fire?                (-> fire-type-matrix (grid-lookup/mget-double-at i j) (double) (> 1.0))]
    (when (spot-fire? inputs crown-fire? cell fire-line-intensity)
      (let [rand-gen                (:rand-gen inputs)
            spotting                (:spotting inputs)
            rng                     (JDKRandomGenerator. (.nextInt ^Random rand-gen))
            num-fbs                 (sample-number-of-firebrands spotting rng)]
        (when-not (zero? num-fbs)
          (->> (range num-fbs)
               ;; NOTE it turns out that clojure.core/repeatedly is slow (lazy seq overhead), (Val, 19 Mar 2023)
               ;; so we're re-implementing a fast vector-returning version of it.
               (into []
                     (keep (let [burn-time-matrix           (:burn-time-matrix matrices)
                                 burn-time                  (grid-lookup/mget-double-at burn-time-matrix i j)
                                 band                       (long (/ burn-time 60.0))
                                 cell-size                  (:cell-size inputs)
                                 get-wind-speed-20ft        (:get-wind-speed-20ft inputs)
                                 get-wind-from-direction    (:get-wind-from-direction inputs)
                                 firebrand-count-matrix     (:firebrand-count-matrix matrices)
                                 fire-spread-matrix         (:fire-spread-matrix matrices)
                                 rand-gen                   (:rand-gen inputs)
                                 fl-intensity               (convert/Btu-ft-s->kW-m fire-line-intensity)
                                 wd                         (grid-lookup/double-at get-wind-from-direction band i j)
                                 wind-to-direction          (convert/deg->rad (mod (+ 180 wd) 360))
                                 cos-w                      (FastMath/cos wind-to-direction)
                                 sin-w                      (FastMath/sin wind-to-direction)
                                 ws                         (grid-lookup/double-at get-wind-speed-20ft band i j)
                                 ws20ft                     (convert/mph->mps ws)
                                 sample-delta-x-fn          (delta-x-sampler spotting fl-intensity ws20ft)
                                 sample-delta-y-fn          (delta-y-sampler spotting fl-intensity ws20ft)
                                 decay-constant             (double (:decay-constant spotting))
                                 num-rows                   (:num-rows inputs)
                                 num-cols                   (:num-cols inputs)
                                 get-fuel-model             (:get-fuel-model inputs)
                                 get-temperature            (:get-temperature inputs)
                                 get-relative-humidity      (:get-relative-humidity inputs)
                                 get-fuel-moisture-dead-1hr (:get-fuel-moisture-dead-1hr inputs)
                                 source-burn-probability    (grid-lookup/mget-double-at fire-spread-matrix i j)
                                 flame-length-matrix        (:flame-length-matrix matrices)
                                 origin-flame-length        (convert/ft->m (grid-lookup/mget-double-at flame-length-matrix i j))
                                 update-firebrand-count!    (update-firebrand-count!-fn inputs firebrand-count-matrix fire-spread-matrix i j)]
                             (fn resolve-spotting-ignition! [_k]
                               (let [delta-x (sample-delta-x-fn rand-gen)
                                     delta-y (sample-delta-y-fn rand-gen)
                                     grid-dx (delta->grid-dx cos-w sin-w delta-x delta-y)
                                     grid-dy (delta->grid-dy cos-w sin-w delta-x delta-y)
                                     i1      (+ i (distance->n-cells cell-size grid-dy))
                                     j1      (+ j (distance->n-cells cell-size grid-dx))]
                                 (update-firebrand-count! i1 j1)
                                 (when (and
                                        (in-bounds-optimal? num-rows num-cols i1 j1)
                                        (burnable-fuel-model? (grid-lookup/double-at get-fuel-model i1 j1)))
                                   (let [spotting-distance    (convert/ft->m (FastMath/hypot grid-dx grid-dy))
                                         rand01               (my-rand-range rand-gen 0 1)]
                                     (when-some [spot-ignition-p (spot-ignition-probability* (yield-successful-prob rand01)
                                                                                             decay-constant
                                                                                             spotting-distance
                                                                                             ;; NOTE that the following (costly) expression will only be evaluated a small fraction of the time. (Val, 21 Mar 2023)
                                                                                             (let [temperature        (grid-lookup/double-at get-temperature band i1 j1)
                                                                                                   fine-fuel-moisture (if get-fuel-moisture-dead-1hr
                                                                                                                        (grid-lookup/double-at get-fuel-moisture-dead-1hr band i1 j1)
                                                                                                                        (calc-fuel-moisture
                                                                                                                         ;; FIXME must be in target cell: i j -> i1 j1
                                                                                                                         (grid-lookup/double-at get-relative-humidity band i j)
                                                                                                                         temperature :dead :1hr))]
                                                                                               (schroeder-ign-prob (convert/F->C (double temperature)) fine-fuel-moisture)))]
                                       (let [burn-probability (* (double spot-ignition-p) source-burn-probability)]
                                         (when (and (>= burn-probability 0.1) ; TODO parametrize 0.1 in gridfire.edn
                                                    (> (double burn-probability) (grid-lookup/mget-double-at fire-spread-matrix i1 j1)))
                                           (let [t (spot-ignition-time burn-time origin-flame-length)]
                                             [[i1 j1] [t burn-probability]])))))))))))))))))
;; spread-firebrands ends here

;; [[file:../../org/GridFire.org::spotting-firebrands-params-sampling][spotting-firebrands-params-sampling]]
(defn- sample-spotting-param
  ^double
  [param rand-gen]
  (if (and (map? param) (contains? param :lo) (contains? param :hi))
    (let [{:keys [lo hi]} param
          ;; FIXME REVIEW Why on Earth are we sampling from a range with random bounds? (Val, 17 Mar 2023)
          l               (if (vector? lo) (my-rand-range rand-gen (lo 0) (lo 1)) lo)
          h               (if (vector? hi) (my-rand-range rand-gen (hi 0) (hi 1)) hi)]
      (my-rand-range rand-gen l h))
    param))

(defn- sample-from-uniform
  "Draws a random number from a Uniform Distribution,
  encoded as either a single number (no randomness, a.k.a. Constant Distribution)
  or a [min max] range."
  ^double [values-range rand-gen]
  (cond
    (number? values-range) (double values-range)
    (vector? values-range) (let [[min max] values-range]
                             (my-rand-range rand-gen min max))))

(defn- sample-intranges-mapping-values
  [pairs rand-gen]
  (if (sequential? pairs)
    (supd/supdate pairs [{1 #(sample-from-uniform % rand-gen)}])
    pairs))

(defn sample-spotting-params
  "Resolves values for the spotting parameters which are configured as a range to draw from.

  Given a GridFire spotting configuration map,
  replaces those parameters which are configured as a range
  with randomly drawn values from that range,
  returning a new map."
  [spotting-config rand-gen]
  (-> spotting-config
      ;; Yes, it' a mess, that's what we get for having made the configuration so irregular. (Val, 17 Mar 2023)
      (as-> sp-params
            (reduce (fn [sp k]
                      (supd/supdate sp {k #(sample-spotting-param % rand-gen)}))
                    sp-params
                    [:num-firebrands
                     :decay-constant
                     :mean-distance
                     :normalized-distance-variance
                     :flin-exp
                     :ws-exp
                     :delta-y-sigma])
            (supd/supdate sp-params
                          {:crown-fire-spotting-percent #(some-> % (sample-from-uniform rand-gen))
                           :surface-fire-spotting       {:critical-fire-line-intensity #(sample-intranges-mapping-values % rand-gen)
                                                         :spotting-percent             #(sample-intranges-mapping-values % rand-gen)}}))))
;; spotting-firebrands-params-sampling ends here
