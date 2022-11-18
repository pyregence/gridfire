(ns gridfire.spotting-optimal
  (:require [gridfire.common              :refer [burnable-cell?
                                                  burnable-fuel-model?
                                                  calc-fuel-moisture
                                                  compute-terrain-distance
                                                  in-bounds-optimal?]]
            [gridfire.conversion          :as convert]
            [gridfire.grid-lookup         :as grid-lookup]
            [gridfire.utils.random        :refer [my-rand-range]]
            [tech.v3.tensor               :as t])
  (:import java.util.Random))

(set! *unchecked-math* :warn-on-boxed)

;;-----------------------------------------------------------------------------
;; Formulas
;;-----------------------------------------------------------------------------

(defn- sample-spotting-params
  ^double
  [param rand-gen]
  (if (map? param)
    (let [{:keys [lo hi]} param
          l               (if (vector? lo) (my-rand-range rand-gen (lo 0) (lo 1)) lo)
          h               (if (vector? hi) (my-rand-range rand-gen (hi 0) (hi 1)) hi)]
      (my-rand-range rand-gen l h))
    param))

(defn- mean-variance
  "Returns mean spotting distance and it's variance given:
  fire-line-intensity: (kWm^-1)
  wind-speed-20ft: (ms^-1)"
  [{:keys [^double mean-distance ^double flin-exp ^double ws-exp ^double normalized-distance-variance]}
   rand-gen ^double fire-line-intensity ^double wind-speed-20ft]
  (let [a (sample-spotting-params mean-distance rand-gen)
        b (sample-spotting-params flin-exp rand-gen)
        c (sample-spotting-params ws-exp rand-gen)
        m (* a (Math/pow fire-line-intensity b) (Math/pow wind-speed-20ft c))]
    {:mean m :variance (* m (sample-spotting-params normalized-distance-variance rand-gen))}))

(defn- standard-deviation
  "Returns standard deviation for the lognormal distribution given:
  mean spotting distance and it's variance"
  ^double
  [^double m ^double v]
  (Math/sqrt (Math/log (+ 1 (/ v (Math/pow m 2))))))

(defn- normalized-mean
  "Returns normalized mean for the lognormal distribution given:
  mean spotting distance and it's variance"
  ^double
  [^double m ^double v]
  (Math/log (/ (Math/pow m 2)
               (Math/sqrt (+ v (Math/pow m 2))))))

(defn- sample-normal
  "Returns sample from normal/gaussian distribution given mu and sd."
  ^double
  [^Random rand-gen ^double mu ^double sd]
  (+ mu (* sd (.nextGaussian rand-gen))))

(defn- sample-lognormal
  "Returns sample from log-normal distribution given mu and sd."
  ^double
  [^Random rand-gen ^double mu ^double sd]
  (Math/exp (sample-normal rand-gen mu sd)))

(defn- sample-wind-dir-deltas
  "Returns a sequence of [x y] distances (meters) that firebrands land away
  from a torched cell at i j where:
  x: parallel to the wind
  y: perpendicular to the wind (positive values are to the right of wind direction)"
  [inputs fire-line-intensity-matrix wind-speed-20ft [i j]]
  (let [spotting                (:spotting inputs)
        rand-gen                (:rand-gen inputs)
        num-firebrands          (long (sample-spotting-params (:num-firebrands spotting) rand-gen))
        intensity               (convert/Btu-ft-s->kW-m (t/mget fire-line-intensity-matrix i j))
        {:keys [mean variance]} (mean-variance spotting rand-gen intensity wind-speed-20ft)
        mu                      (normalized-mean mean variance)
        sd                      (standard-deviation mean variance)
        parallel-values         (repeatedly num-firebrands #(sample-lognormal rand-gen mu sd))
        perpendicular-values    (repeatedly num-firebrands #(sample-normal rand-gen 0.0 0.92))]
    (mapv (fn [x y] [(convert/m->ft x) (convert/m->ft y)])
          parallel-values
          perpendicular-values)))
;; sardoy-firebrand-dispersal ends here
;; [[file:../../org/GridFire.org::convert-deltas][convert-deltas]]
(defn hypotenuse ^double
  [x y]
  (Math/sqrt (+ (Math/pow x 2) (Math/pow y 2))))

(defn deltas-wind->coord
  "Converts deltas from the torched tree in the wind direction to deltas
  in the coordinate plane"
  [deltas ^double wind-direction]
  (mapv (fn [[d-paral d-perp]]
          (let [d-paral (double d-paral)
                d-perp  (double d-perp)
                H       (hypotenuse d-paral d-perp)
                t1      wind-direction
                t2      (convert/rad->deg (Math/atan (/ d-perp d-paral)))
                t3      (+ t1 t2)]
            [(* H (Math/sin (convert/deg->rad t3)))
             (* -1 H (Math/cos (convert/deg->rad t3)))]))
        deltas))

(defn firebrands
  "Returns a sequence of cells [i,j] that firebrands land in.
   Note: matrix index [i,j] refers to [row, column]. Therefore, we need to flip
   [row,column] to get to [x,y] coordinates."
  [deltas wind-towards-direction cell ^double cell-size]
  (let [step         (/ cell-size 2)
        [y x]        (mapv #(+ step (* ^double % cell-size)) cell)
        x            (double x)
        y            (double y)
        coord-deltas (deltas-wind->coord deltas wind-towards-direction)]
    (mapv (fn [[dx dy]]
            (let [dx (double dx)
                  dy (double dy)]
              [(long (Math/floor (/ (+ dy y) cell-size)))
               (long (Math/floor (/ (+ dx x) cell-size)))]))
          coord-deltas)))
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
        Q_a (+ 144.512 (* -0.266 T_o) (* -0.00058 (Math/pow T_o 2.0)))

        ;; heat required to raise moisture to reach boiling point
        Q_b (* -1.0 T_o M)

        ;; Heat of desorption
        Q_c (* 18.54 (- 1.0 (Math/exp (* -15.1 M))))

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
        (Math/pow 4.3)
        (* 0.000048)
        (/ 50.0)
        (Math/min 1.0)
        (Math/max 0.0))))

(defn- one-minus ^double [^double x] (- 1.0 x))

(defn spot-ignition-probability
  "Returns the probability of spot fire ignition (Perryman 2012) given:
   - Schroeder's probability of ignition [P(I)] (0-1)
   - Decay constant [lambda] (0.005)
   - Distance from the torched cell [d] (meters)
   - Number of firebrands accumulated in the cell [b]

   P(Spot Ignition) = 1 - (1 - (P(I) * exp(-lambda * d)))^b"
  ^double
  [^double ignition-probability ^double decay-constant ^double spotting-distance ^double firebrand-count]
  (-> decay-constant
      (* -1.0)
      (* spotting-distance)
      (Math/exp)
      (* ignition-probability)
      (one-minus)
      (Math/pow firebrand-count)
      (one-minus)))
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
  (let [a     5.963                            ; constant from (D33)
        b     4.563                            ; constant from (D34)
        z-max 117.0                            ; max height given particle diameter of 0.003m
        w_F   (* 2.3 (Math/sqrt flame-length)) ; upward axial velocity at flame tip
        t_0   (/ w_F (* 2.0 flame-length))]    ; period of steady burning of tree crowns (t_c, min) normalized by 2*z_F / w_F
    (-> z-max
        (/ flame-length)
        (+ b)
        (/ a)
        (Math/pow 1.5)
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
;; [[file:../../org/GridFire.org::spread-firebrands][spread-firebrands]]
(defn- update-firebrand-counts!
  [inputs firebrand-count-matrix fire-spread-matrix source firebrands]
  (let [num-rows                (:num-rows inputs)
        num-cols                (:num-cols inputs)
        get-fuel-model          (:get-fuel-model inputs)
        [i j]                   source
        source-burn-probability (t/mget fire-spread-matrix i j)]
    (doseq [[y x] firebrands]
      (when (burnable-cell? get-fuel-model
                            fire-spread-matrix
                            source-burn-probability
                            num-rows
                            num-cols
                            y
                            x)
        (->> (t/mget firebrand-count-matrix y x)
             (long)
             (inc)
             (t/mset! firebrand-count-matrix y x))))))

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

(defn- sample-from-uniform
  "Draws a random number from a Uniform Distribution,
  encoded as either a single number (no randomness, a.k.a. Constant Distribution)
  or a [min max] range."
  ^double [values-range rand-gen]
  (cond
    (number? values-range) (double values-range)
    (vector? values-range) (let [[min max] values-range]
                             (my-rand-range rand-gen min max))))

(defn surface-fire-spot-fire?
  "Expects surface-fire-spotting config to be a sequence of tuples of
  ranges [lo hi] and spotting probability. The range represents the range (inclusive)
  of fuel model numbers that the spotting probability is set to.
  [[[1 140] 0.0]
  [[141 149] 1.0]
  [[150 256] 1.0]]"
  [inputs [i j] ^double fire-line-intensity]
  (let [i                            (long i)
        j                            (long j)
        rand-gen                     (:rand-gen inputs)
        get-fuel-model               (:get-fuel-model inputs)
        fuel-model-number            (long (grid-lookup/double-at get-fuel-model i j))
        surface-fire-spotting        (:surface-fire-spotting (:spotting inputs))
        critical-fire-line-intensity (-> (:critical-fire-line-intensity surface-fire-spotting)
                                         (intranges-mapping-lookup fuel-model-number)
                                         (or 0.0)
                                         (sample-from-uniform rand-gen))]
    (when (and surface-fire-spotting
               (> fire-line-intensity critical-fire-line-intensity))
      (let [spot-percent (-> (:spotting-percent surface-fire-spotting)
                             (intranges-mapping-lookup fuel-model-number)
                             (or 0.0)
                             (sample-from-uniform rand-gen))]
        (>= spot-percent (my-rand-range rand-gen 0.0 1.0))))))

(defn crown-spot-fire?
  "Determine whether crowning causes spot fires. Config key `:spotting` should
   take either a vector of probabilities (0-1) or a single spotting probability."
  [inputs]
  (when-let [spot-percent (:crown-fire-spotting-percent (:spotting inputs))]
    (let [rand-gen  (:rand-gen inputs)
          ^double p (if (vector? spot-percent)
                      (let [[lo hi] spot-percent]
                        (my-rand-range rand-gen lo hi)) ; TODO should this be calculated once at input phase?
                      spot-percent)]
      (>= p (my-rand-range rand-gen 0.0 1.0)))))

(defn- spot-fire? [inputs crown-fire? here fire-line-intensity]
  (if crown-fire?
    (crown-spot-fire? inputs)
    (surface-fire-spot-fire? inputs here fire-line-intensity)))

;; FIXME: Drop cell = [i j]
(defn spread-firebrands
  "Returns a sequence of key value pairs where
  key: [x y] locations of the cell
  val: [t p] where:
  t: time of ignition
  p: ignition-probability"
  [inputs matrices i j]
  (let [num-rows                   (:num-rows inputs)
        num-cols                   (:num-cols inputs)
        cell-size                  (:cell-size inputs)
        rand-gen                   (:rand-gen inputs)
        spotting                   (:spotting inputs)
        get-elevation              (:get-elevation inputs)
        get-fuel-model             (:get-fuel-model inputs)
        get-temperature            (:get-temperature inputs)
        get-relative-humidity      (:get-relative-humidity inputs)
        get-wind-speed-20ft        (:get-wind-speed-20ft inputs)
        get-wind-from-direction    (:get-wind-from-direction inputs)
        get-fuel-moisture-dead-1hr (:get-fuel-moisture-dead-1hr inputs)
        firebrand-count-matrix     (:firebrand-count-matrix matrices)
        fire-spread-matrix         (:fire-spread-matrix matrices)
        fire-line-intensity-matrix (:fire-line-intensity-matrix matrices)
        flame-length-matrix        (:flame-length-matrix matrices)
        fire-type-matrix           (:fire-type-matrix matrices)
        burn-time-matrix           (:burn-time-matrix matrices)
        burn-time                  (double (t/mget burn-time-matrix i j))
        cell                       [i j]
        fire-line-intensity        (t/mget fire-line-intensity-matrix i j)
        crown-fire?                (-> fire-type-matrix (t/mget i j) (double) (> 1.0))]
    (when (spot-fire? inputs crown-fire? cell fire-line-intensity)
      (let [band                    (long (/ burn-time 60.0))
            ws                      (grid-lookup/double-at get-wind-speed-20ft band i j)
            wd                      (grid-lookup/double-at get-wind-from-direction band i j)
            deltas                  (sample-wind-dir-deltas inputs
                                                            fire-line-intensity-matrix
                                                            (convert/mph->mps ws)
                                                            cell)
            wind-to-direction       (mod (+ 180 wd) 360)
            firebrands              (firebrands deltas wind-to-direction cell cell-size)
            source-burn-probability (double (t/mget fire-spread-matrix i j))]
        (update-firebrand-counts! inputs firebrand-count-matrix fire-spread-matrix cell firebrands)
        (->> (for [[x y] firebrands]
               (let [x (long x)
                     y (long y)]
                 (when (and
                        (in-bounds-optimal? num-rows num-cols x y)
                        (burnable-fuel-model? (grid-lookup/double-at get-fuel-model x y)))
                   (let [temperature          (grid-lookup/double-at get-temperature band x y)
                         fine-fuel-moisture   (if get-fuel-moisture-dead-1hr
                                                (grid-lookup/double-at get-fuel-moisture-dead-1hr band x y)
                                                (calc-fuel-moisture
                                                 (grid-lookup/double-at get-relative-humidity band i j)
                                                 temperature :dead :1hr))
                         ignition-probability (schroeder-ign-prob (convert/F->C (double temperature)) fine-fuel-moisture)
                         decay-constant       (double (:decay-constant spotting))
                         spotting-distance    (convert/ft->m
                                               (compute-terrain-distance cell-size
                                                                         get-elevation
                                                                         num-rows
                                                                         num-cols
                                                                         i
                                                                         j
                                                                         x
                                                                         y))
                         firebrand-count      (t/mget firebrand-count-matrix x y)
                         spot-ignition-p      (spot-ignition-probability ignition-probability
                                                                         decay-constant
                                                                         spotting-distance
                                                                         firebrand-count)
                         burn-probability     (* spot-ignition-p source-burn-probability)]
                     (when (and (>= burn-probability 0.1)   ; TODO parametrize 0.1 in gridfire.edn
                                (> (double burn-probability) ^double (t/mget fire-spread-matrix x y))
                                (spot-ignition? rand-gen spot-ignition-p))
                       (let [t (spot-ignition-time burn-time
                                                   (convert/ft->m (t/mget flame-length-matrix i j)))]
                         [[x y] [t burn-probability]]))))))
             (remove nil?))))))
