;; [[file:../../org/GridFire.org::rothermel-surface-fire-spread-no-wind-no-slope][rothermel-surface-fire-spread-no-wind-no-slope]]
(ns gridfire.surface-fire
  (:require [gridfire.fuel-models :refer [map-category map-size-class
                                          category-sum size-class-sum]]))

(defn grass-fuel-model?
  [^long number]
  (and (> number 100) (< number 110)))

(defn rothermel-surface-fire-spread-no-wind-no-slope
  "Returns the rate of surface fire spread in ft/min and the reaction
   intensity (i.e., amount of heat output) of a fire in Btu/ft^2*min
   given a map containing these keys:
   - number [fuel model number]
   - delta [fuel depth (ft)]
   - w_o [ovendry fuel loading (lb/ft^2)]
   - sigma [fuel particle surface-area-to-volume ratio (ft^2/ft^3)]
   - h [fuel particle low heat content (Btu/lb)]
   - rho_p [ovendry particle density (lb/ft^3)]
   - S_T [fuel particle total mineral content (lb minerals/lb ovendry wood)]
   - S_e [fuel particle effective mineral content (lb silica-free minerals/lb ovendry wood)]
   - M_x [moisture content of extinction (lb moisture/lb ovendry wood)]
   - M_f [fuel particle moisture content (lb moisture/lb ovendry wood)]
   - f_ij [percent of load per size class (%)]
   - f_i [percent of load per category (%)]
   - g_ij [percent of load per size class from Albini_1976_FIREMOD, page 20]"
  [{:keys [number delta w_o sigma  h rho_p S_T S_e M_x  M_f f_ij f_i g_ij]}]
  (let [number     (long number)
        delta      (double delta)
        S_e_i      (size-class-sum (fn [i j] (* (-> f_ij i ^double (j)) (-> S_e i ^double (j)))))

        ;; Mineral damping coefficient
        eta_S_i    (map-category (fn [i] (let [^double S_e_i (-> S_e_i i)]
                                           (if (pos? S_e_i)
                                             (/ 0.174 (Math/pow S_e_i 0.19))
                                             1.0))))

        M_f_i      (size-class-sum (fn [i j] (* (-> f_ij i ^double (j)) (-> M_f i ^double (j)))))

        M_x_i      (size-class-sum (fn [i j] (* (-> f_ij i ^double (j)) (-> M_x i ^double (j)))))

        r_M_i      (map-category (fn [i] (let [^double M_f (-> M_f_i i)
                                               ^double M_x (-> M_x_i i)]
                                           (if (pos? M_x)
                                             (min 1.0 (/ M_f M_x))
                                             1.0))))

        ;; Moisture damping coefficient
        eta_M_i    (map-category (fn [i] (+ 1.0
                                            (* -2.59 (-> r_M_i ^double (i)))
                                            (* 5.11 (Math/pow (-> r_M_i i) 2))
                                            (* -3.52 (Math/pow (-> r_M_i i) 3)))))

        h_i        (size-class-sum (fn [i j] (* (-> f_ij i ^double (j)) (-> h i ^double (j)))))

        ;; Net fuel loading (lb/ft^2)
        W_n_i      (size-class-sum (fn [i j] (* (-> g_ij i ^double (j))
                                                (-> w_o i ^double (j))
                                                (- 1.0 (-> S_T i ^double (j))))))

        beta_i     (size-class-sum (fn [i j] (/ (-> w_o i ^double (j)) (-> rho_p i ^double (j)))))

        ;; Packing ratio
        beta       (if (pos? delta)
                     (/ (category-sum (fn [i] (-> beta_i ^double (i)))) delta)
                     0.0)

        sigma'_i   (size-class-sum (fn [i j] (* (-> f_ij i ^double (j)) (-> sigma i ^double (j)))))

        sigma'     (category-sum (fn [i] (* (-> f_i ^double (i)) (-> sigma'_i ^double (i)))))

        ;; Optimum packing ratio
        beta_op    (if (pos? sigma')
                     (/ 3.348 (Math/pow sigma' 0.8189))
                     1.0)

        ;; Albini 1976 replaces (/ 1 (- (* 4.774 (Math/pow sigma' 0.1)) 7.27))
        A          (if (pos? sigma')
                     (/ 133.0 (Math/pow sigma' 0.7913))
                     0.0)

        ;; Maximum reaction velocity (1/min)
        Gamma'_max (/ (Math/pow sigma' 1.5)
                      (+ 495.0 (* 0.0594 (Math/pow sigma' 1.5))))

        ;; Optimum reaction velocity (1/min)
        Gamma'     (* Gamma'_max
                      (Math/pow (/ beta beta_op) A)
                      (Math/exp (* A (- 1.0 (/ beta beta_op)))))

        ;; Reaction intensity (Btu/ft^2*min)
        I_R        (* Gamma' (category-sum (fn [i] (* ^double (W_n_i i) ^double (h_i i)
                                                      ^double (eta_M_i i) ^double (eta_S_i i)))))

        ;; Propagating flux ratio
        xi         (/ (Math/exp (* (+ 0.792 (* 0.681 (Math/pow sigma' 0.5)))
                                   (+ beta 0.1)))
                      (+ 192.0 (* 0.2595 sigma')))

        E          (* 0.715 (Math/exp (* -3.59 (/ sigma' 10000.0))))

        B          (* 0.02526 (Math/pow sigma' 0.54))

        C          (* 7.47 (Math/exp (* -0.133 (Math/pow sigma' 0.55))))

        ;; Derive wind factor
        get-phi_W  (fn ^double [^double midflame-wind-speed]
                     (if (and (pos? beta) (pos? midflame-wind-speed))
                       (-> midflame-wind-speed
                           (Math/pow B)
                           (* C)
                           (/ (Math/pow (/ beta beta_op) E)))
                       0.0))

        ;; Derive wind speed from wind factor
        get-wind-speed (fn [^double phi_W]
                         (-> phi_W
                             (* (Math/pow (/ beta beta_op) E))
                             ^double (/ C)
                             (Math/pow (/ 1.0 B))))

        ;; Derive slope factor
        get-phi_S  (fn [^double slope]
                     (if (and (pos? beta) (pos? slope))
                       (* 5.275 (Math/pow beta -0.3) (Math/pow slope 2.0))
                       0.0))

        ;; Heat of preignition (Btu/lb)
        Q_ig       (map-size-class (fn [i j] (+ 250.0 (* 1116.0 (-> M_f i ^double (j))))))

        foo_i      (size-class-sum (fn [i j] (let [^double sigma_ij (-> sigma i j)
                                                   ^double Q_ig_ij  (-> Q_ig  i j)]
                                               (if (pos? sigma_ij)
                                                 (* (-> f_ij i ^double (j))
                                                    (Math/exp (/ -138 sigma_ij))
                                                    Q_ig_ij)
                                                 0.0))))

        rho_b_i    (size-class-sum (fn [i j] (-> w_o i j)))

        ;; Ovendry bulk density (lb/ft^3)
        rho_b      (if (pos? delta)
                     (/ (category-sum (fn [i] (-> rho_b_i i))) delta)
                     0.0)

        rho_b-epsilon-Q_ig (* rho_b (category-sum (fn [i] (* (-> f_i ^double (i)) (-> foo_i ^double (i))))))

        ;; Surface fire spread rate (ft/min)
        R          (if (pos? rho_b-epsilon-Q_ig)
                     (/ (* I_R xi) rho_b-epsilon-Q_ig)
                     0.0)

        ;; Addition proposed by Chris Lautenberger (REAX 2015)
        spread-rate-multiplier (if (grass-fuel-model? number) 0.5 1.0)]

    {:spread-rate        (* R spread-rate-multiplier)
     :reaction-intensity I_R
     :residence-time     (/ 384.0 sigma')
     :get-phi_W          get-phi_W
     :get-phi_S          get-phi_S
     :get-wind-speed     get-wind-speed}))
;; rothermel-surface-fire-spread-no-wind-no-slope ends here

;; [[file:../../org/GridFire.org::wind-adjustment-factor][wind-adjustment-factor]]
(defn wind-adjustment-factor
  "ft ft 0-100"
  ^double
  [^double fuel-bed-depth ^double canopy-height ^double canopy-cover]
  (cond
    ;; sheltered: equation 2 based on CC and CH, CR=1 (Andrews 2012)
    (and (pos? canopy-cover)
         (pos? canopy-height))
    (/ 0.555 (* (Math/sqrt (* (/ canopy-cover 300.0) canopy-height))
                (Math/log (/ (+ 20.0 (* 0.36 canopy-height)) (* 0.13 canopy-height)))))

    ;; unsheltered: equation 6 H_F = H (Andrews 2012)
    (pos? fuel-bed-depth)
    (/ 1.83 (Math/log (/ (+ 20.0 (* 0.36 fuel-bed-depth)) (* 0.13 fuel-bed-depth))))

    ;; non-burnable fuel model
    :otherwise
    0.0))

(defn wind-adjustment-factor-elmfire
  "ft m 0-1"
  ^double
  [^double fuel-bed-depth ^double canopy-height ^double canopy-cover]
  (cond
    ;; sheltered WAF
    (and (pos? canopy-cover)
         (pos? canopy-height))
    (* (/ 1.0 (Math/log (/ (+ 20.0 (* 0.36 (/ canopy-height 0.3048)))
                           (* 0.13 (/ canopy-height 0.3048)))))
       (/ 0.555 (Math/sqrt (* (/ canopy-cover 3.0) (/ canopy-height 0.3048)))))

    ;; unsheltered WAF
    (pos? fuel-bed-depth)
    (* (/ (+ 1.0 (/ 0.36 1.0))
          (Math/log (/ (+ 20.0 (* 0.36 fuel-bed-depth))
                       (* 0.13 fuel-bed-depth))))
       (- (Math/log (/ (+ 1.0 0.36) 0.13)) 1.0))

    ;; non-burnable fuel model
    :otherwise
    0.0))
;; wind-adjustment-factor ends here

;; [[file:../../org/GridFire.org::rothermel-surface-fire-spread-max-and-any][rothermel-surface-fire-spread-max-and-any]]
(defn almost-zero? [^double x]
  (< (Math/abs x) 0.000001))

(defn degrees-to-radians ^double
  [^double degrees]
  (/ (* degrees Math/PI) 180.0))

(defn radians-to-degrees
  ^double
  [^double radians]
  (/ (* radians 180.0) Math/PI))

(defn scale-spread-to-max-wind-speed
  [{:keys [effective-wind-speed max-spread-direction] :as spread-properties}
   ^double spread-rate max-wind-speed  ^double phi-max]
  (let [effective-wind-speed (double effective-wind-speed)
        max-spread-direction (double max-spread-direction)]
    (if (> effective-wind-speed ^double max-wind-speed)
      {:max-spread-rate      (* spread-rate (+ 1.0 phi-max))
       :max-spread-direction max-spread-direction
       :effective-wind-speed max-wind-speed}
      spread-properties)))

(defn add-eccentricity
  [{:keys [effective-wind-speed] :as spread-properties} ellipse-adjustment-factor]
  (let [effective-wind-speed (double effective-wind-speed)
        length-width-ratio (+ 1.0 (* 0.002840909
                                     effective-wind-speed
                                     ^double ellipse-adjustment-factor))
        eccentricity       (/ (Math/sqrt (- (Math/pow length-width-ratio 2.0) 1.0))
                              length-width-ratio)]
    (assoc spread-properties :eccentricity eccentricity)))

(defn smallest-angle-between ^double
  [^double theta1 ^double theta2]
  (let [angle (Math/abs (- theta1 theta2))]
    (if (> angle 180.0)
      (- 360.0 angle)
      angle)))

(defn rothermel-surface-fire-spread-max
  "Note: fire ellipse adjustment factor, < 1.0 = more circular, > 1.0 = more elliptical"
  [{:keys [spread-rate reaction-intensity get-phi_W get-phi_S get-wind-speed]}
   midflame-wind-speed wind-from-direction slope aspect ellipse-adjustment-factor]
  (let [^double phi_W             (get-phi_W midflame-wind-speed)
        ^double phi_S             (get-phi_S slope)
        ^double slope-direction   (mod (+ ^double aspect 180.0) 360.0)
        ^double wind-to-direction (mod (+ ^double wind-from-direction 180.0) 360.0)
        max-wind-speed            (* 0.9 ^double reaction-intensity)
        ^double phi-max           (get-phi_W max-wind-speed)
        spread-rate               (double spread-rate)]
    (->
     (cond (and (almost-zero? midflame-wind-speed) (almost-zero? slope))
           ;; no wind, no slope
           {:max-spread-rate      spread-rate
            :max-spread-direction 0.0
            :effective-wind-speed 0.0}

           (almost-zero? slope)
           ;; wind only
           {:max-spread-rate      (* spread-rate (+ 1.0 phi_W))
            :max-spread-direction wind-to-direction
            :effective-wind-speed midflame-wind-speed}

           (almost-zero? midflame-wind-speed)
           ;; slope only
           {:max-spread-rate      (* spread-rate (+ 1.0 phi_S))
            :max-spread-direction slope-direction
            :effective-wind-speed (get-wind-speed phi_S)}

           (< (smallest-angle-between wind-to-direction slope-direction) 15.0)
           ;; wind blows (within 15 degrees of) upslope
           {:max-spread-rate      (* spread-rate (+ 1.0 phi_W phi_S))
            :max-spread-direction slope-direction
            :effective-wind-speed (get-wind-speed (+ phi_W phi_S))}

           :else
           ;; wind blows across slope
           (let [slope-magnitude    (* spread-rate phi_S)
                 wind-magnitude     (* spread-rate phi_W)
                 difference-angle   (degrees-to-radians
                                     (mod (- wind-to-direction slope-direction) 360.0))
                 x                  (+ slope-magnitude
                                       (* wind-magnitude (Math/cos difference-angle)))
                 y                  (* wind-magnitude (Math/sin difference-angle))
                 combined-magnitude (Math/sqrt (+ (* x x) (* y y)))]
             (if (almost-zero? combined-magnitude)
               {:max-spread-rate      spread-rate
                :max-spread-direction 0.0
                :effective-wind-speed 0.0}
               (let [max-spread-rate      (+ spread-rate combined-magnitude)
                     phi-combined         (- (/ max-spread-rate spread-rate) 1.0)
                     offset               (radians-to-degrees
                                           (Math/asin (/ (Math/abs y) combined-magnitude)))
                     offset'              (if (>= x 0.0)
                                            (if (>= y 0.0)
                                              offset
                                              (- 360.0 offset))
                                            (if (>= y 0.0)
                                              (- 180.0 offset)
                                              (+ 180.0 offset)))
                     max-spread-direction (mod (+ slope-direction offset') 360.0)
                     effective-wind-speed (get-wind-speed phi-combined)]
                 {:max-spread-rate      max-spread-rate
                  :max-spread-direction max-spread-direction
                  :effective-wind-speed effective-wind-speed}))))
     (scale-spread-to-max-wind-speed spread-rate max-wind-speed phi-max)
     (add-eccentricity ellipse-adjustment-factor))))

(defn rothermel-surface-fire-spread-any-optimal ^double
  [max-spread-rate max-spread-direction eccentricity spread-direction]
  (let [max-spread-rate      (double max-spread-rate)
        max-spread-direction (double max-spread-direction)
        eccentricity         (double eccentricity)
        theta                (smallest-angle-between max-spread-direction spread-direction)]
    (if (or (almost-zero? eccentricity) (almost-zero? theta))
      max-spread-rate
      (* max-spread-rate (/ (- 1.0 eccentricity)
                            (- 1.0 (* eccentricity
                                      (Math/cos (degrees-to-radians theta)))))))))

(defn rothermel-surface-fire-spread-any ^double
  [{:keys [max-spread-rate max-spread-direction eccentricity]} spread-direction]
  (let [max-spread-rate      (double max-spread-rate)
        max-spread-direction (double max-spread-direction)
        eccentricity         (double eccentricity)
        theta                (smallest-angle-between max-spread-direction spread-direction)]
    (if (or (almost-zero? eccentricity) (almost-zero? theta))
      max-spread-rate
      (* max-spread-rate (/ (- 1.0 eccentricity)
                            (- 1.0 (* eccentricity
                                      (Math/cos (degrees-to-radians theta)))))))))
;; rothermel-surface-fire-spread-max-and-any ends here

;; [[file:../../org/GridFire.org::surface-fire-intensity-formulas][surface-fire-intensity-formulas]]
(defn anderson-flame-depth
  "Returns the depth, or front-to-back distance, of the actively flaming zone
   of a free-spreading fire in ft given:
   - spread-rate (ft/min)
   - residence-time (min)"
  ^double
  [^double spread-rate ^double residence-time]
  (* spread-rate residence-time))

(defn byram-fire-line-intensity
  "Returns the rate of heat release per unit of fire edge in Btu/ft*s given:
   - reaction-intensity (Btu/ft^2*min)
   - flame-depth (ft)"
  ^double
  [^double reaction-intensity ^double flame-depth]
  (/ (* reaction-intensity flame-depth) 60.0))

(defn byram-flame-length
  "Returns the average flame length in ft given:
   - fire-line-intensity (Btu/ft*s)"
  ^double
  [^double fire-line-intensity]
  (* 0.45 (Math/pow fire-line-intensity 0.46)))
;; surface-fire-intensity-formulas ends here
