;; [[file:../../org/GridFire.org::gridfire.surface-fire][gridfire.surface-fire]]
(ns gridfire.surface-fire
  (:require [gridfire.conversion  :refer [deg->rad rad->deg]]
            [gridfire.fuel-models :refer [is-dynamic-grass-fuel-model-number? map-category map-size-class category-sum size-class-sum]]))

(set! *unchecked-math* :warn-on-boxed)

(defn calc-mineral-damping-coefficients [f_ij S_e]
  (let [S_e_i (size-class-sum (fn ^double [i] (* ^double (f_ij i) ^double (S_e i))))]
    (map-category (fn ^double [i]
                    (let [^double S_e_i (S_e_i i)]
                      (if (pos? S_e_i)
                        (/ 0.174 (Math/pow S_e_i 0.19))
                        1.0))))))

(defn calc-moisture-damping-coefficients [f_ij M_f M_x]
  (let [M_f_i (size-class-sum (fn ^double [i] (* ^double (f_ij i) ^double (M_f i))))
        M_x_i (size-class-sum (fn ^double [i] (* ^double (f_ij i) ^double (M_x i))))]
    (map-category (fn ^double [i]
                    (let [^double M_f (M_f_i i)
                          ^double M_x (M_x_i i)
                          r_M (if (pos? M_x)
                                (min 1.0 (/ M_f M_x))
                                1.0)]
                      (-> 1.0
                          (+ (* -2.59 r_M))
                          (+ (* 5.11 (Math/pow r_M 2.0)))
                          (+ (* -3.52 (Math/pow r_M 3.0)))))))))

(defn calc-low-heat-content [f_ij h]
  (size-class-sum (fn ^double [i] (* ^double (f_ij i) ^double (h i)))))

(defn calc-net-fuel-loading [g_ij w_o S_T]
  (size-class-sum (fn ^double [i]
                    (let [^double g_ij (g_ij i)
                          ^double w_o  (w_o i)
                          ^double S_T  (S_T i)]
                      (-> g_ij
                          (* w_o)
                          (* (- 1.0 S_T)))))))

(defn calc-packing-ratio ^double [w_o rho_p ^double delta]
  (let [beta_i (size-class-sum (fn ^double [i] (/ ^double (w_o i) ^double (rho_p i))))]
    (if (pos? delta)
      (/ (category-sum (fn ^double [i] ^double (beta_i i))) delta)
      0.0)))

(defn calc-surface-area-to-volume-ratio ^double [f_i f_ij sigma]
  (let [sigma'_i (size-class-sum (fn ^double [i] (* ^double (f_ij i) ^double (sigma i))))]
    (category-sum (fn ^double [i] (* ^double (f_i i) ^double (sigma'_i i))))))

(defn calc-optimum-packing-ratio ^double [^double sigma']
  (if (pos? sigma')
    (/ 3.348 (Math/pow sigma' 0.8189))
    1.0))

(defn calc-optimum-reaction-velocity ^double [^double beta ^double sigma' ^double beta_op]
  (let [;; Albini 1976 replaces (/ 1 (- (* 4.774 (Math/pow sigma' 0.1)) 7.27))
        A          (if (pos? sigma')
                     (/ 133.0 (Math/pow sigma' 0.7913))
                     0.0)
        B          (Math/pow sigma' 1.5)
        C          (/ beta beta_op)
        ;; Maximum reaction velocity (1/min)
        Gamma'_max (/ B (+ 495.0 (* 0.0594 B)))]
    ;; Optimum reaction velocity (1/min)
    (-> Gamma'_max
        (* (Math/pow C A))
        (* (Math/exp (* A (- 1.0 C)))))))

(defn calc-heat-per-unit-area ^double [eta_S_i eta_M_i h_i W_n_i]
  (category-sum
   (fn ^double [i]
     (let [^double W_n   (W_n_i i)
           ^double h     (h_i i)
           ^double eta_M (eta_M_i i)
           ^double eta_S (eta_S_i i)]
       (-> W_n (* h) (* eta_M) (* eta_S))))))

(defn calc-reaction-intensity ^double [^double Gamma' ^double Btus]
  (* Gamma' Btus))

(defn calc-propagating-flux-ratio ^double [^double beta ^double sigma']
  (/ (Math/exp (* (+ 0.792 (* 0.681 (Math/pow sigma' 0.5)))
                  (+ beta 0.1)))
     (+ 192.0 (* 0.2595 sigma'))))

(defn calc-heat-of-preignition [M_f]
  (map-size-class (fn ^double [i] (+ 250.0 (* 1116.0 ^double (M_f i))))))

(defn calc-heat-distribution [sigma Q_ig f_ij]
  (size-class-sum (fn ^double [i]
                    (let [^double sigma (sigma i)
                          ^double Q_ig  (Q_ig i)]
                      (if (pos? sigma)
                        (-> ^double (f_ij i)
                            (* (Math/exp (/ -138.0 sigma)))
                            (* Q_ig))
                        0.0)))))

(defn calc-ovendry-bulk-density ^double [w_o ^double delta]
  (let [rho_b_i (size-class-sum (fn ^double [i] ^double (w_o i)))]
    (if (pos? delta)
      (/ (category-sum (fn ^double [i] ^double (rho_b_i i))) delta)
      0.0)))

(defn calc-heat-total ^double [f_i epsilon_i]
  (category-sum (fn ^double [i] (* ^double (f_i i) ^double (epsilon_i i)))))

(defn calc-surface-fire-spread-rate ^double [^double I_R ^double xi ^double rho_b ^double epsilon]
  (let [rho_b-epsilon-Q_ig (* rho_b epsilon)]
    (if (pos? rho_b-epsilon-Q_ig)
      (/ (* I_R xi) rho_b-epsilon-Q_ig)
      0.0)))

;; Addition proposed by Chris Lautenberger (REAX 2015)
(defn calc-suppressed-spread-rate ^double [^double R ^long number grass-suppression?]
  (let [spread-rate-multiplier (if (and grass-suppression?
                                        (is-dynamic-grass-fuel-model-number? number))
                                 0.5
                                 1.0)]
    (* R spread-rate-multiplier)))

(defn calc-residence-time ^double [^double sigma']
  (/ 384.0 sigma'))

(defn get-phi_S-fn
  [^double beta]
  (let [G (* 5.275 (Math/pow beta -0.3))]
    (if (pos? beta)
      (fn ^double [^double slope]
        (if (pos? slope)
          (-> slope
              (Math/pow 2.0)
              (* G))
          0.0))
      (fn ^double [^double _]
        0.0))))

(defn get-phi_W-fn
  [^double beta ^double B ^double C ^double F]
  (let [C-over-F (/ C F)]
    (if (pos? beta)
      (fn ^double [^double midflame-wind-speed]
        (if (pos? midflame-wind-speed)
          (-> midflame-wind-speed
              (Math/pow B)
              (* C-over-F))
          0.0))
      (fn ^double [^double _]
        0.0))))

(defn get-wind-speed-fn
  [^double B ^double C ^double F]
  (let [F-over-C  (/ F C)
        B-inverse (/ 1.0 B)]
    (fn ^double [^double phi_W]
      (-> phi_W
          (* F-over-C)
          (Math/pow B-inverse)))))

(defrecord SurfaceFireMin
    [^double unadj-spread-rate
     ^double reaction-intensity
     ^double residence-time
     ^double fuel-bed-depth
     ^double heat-of-combustion
     get-phi_S
     get-phi_W
     get-wind-speed])

;; TODO: Pass fuel-model in as a typed record instead of a map
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
  [fuel-model grass-suppression?]
  (let [number         (:number fuel-model)
        delta          (:delta fuel-model)
        w_o            (:w_o fuel-model)
        sigma          (:sigma fuel-model)
        h              (:h fuel-model)
        rho_p          (:rho_p fuel-model)
        S_T            (:S_T fuel-model)
        S_e            (:S_e fuel-model)
        M_x            (:M_x fuel-model)
        M_f            (:M_f fuel-model)
        f_ij           (:f_ij fuel-model)
        f_i            (:f_i fuel-model)
        g_ij           (:g_ij fuel-model)
        eta_S_i        (calc-mineral-damping-coefficients f_ij S_e)
        eta_M_i        (calc-moisture-damping-coefficients f_ij M_f M_x)
        h_i            (calc-low-heat-content f_ij h)
        W_n_i          (calc-net-fuel-loading g_ij w_o S_T)                 ; (lb/ft^2)
        beta           (calc-packing-ratio w_o rho_p delta)
        sigma'         (calc-surface-area-to-volume-ratio f_i f_ij sigma)
        beta_op        (calc-optimum-packing-ratio sigma')
        Gamma'         (calc-optimum-reaction-velocity beta sigma' beta_op) ; (1/min)
        Btus           (calc-heat-per-unit-area eta_S_i eta_M_i h_i W_n_i)  ; (Btu/ft^2)
        I_R            (calc-reaction-intensity Gamma' Btus)                ; (Btu/ft^2*min)
        xi             (calc-propagating-flux-ratio beta sigma')
        Q_ig           (calc-heat-of-preignition M_f)                       ; (Btu/lb)
        epsilon_i      (calc-heat-distribution sigma Q_ig f_ij)             ; (Btu/lb)
        rho_b          (calc-ovendry-bulk-density w_o delta)                ; (lb/ft^3)
        epsilon        (calc-heat-total f_i epsilon_i)                      ; (Btu/lb)
        R              (calc-surface-fire-spread-rate I_R xi rho_b epsilon) ; (ft/min)
        R'             (calc-suppressed-spread-rate R number grass-suppression?)
        t_res          (calc-residence-time sigma')
        B              (* 0.02526 (Math/pow sigma' 0.54))
        C              (* 7.47 (Math/exp (* -0.133 (Math/pow sigma' 0.55))))
        E              (* 0.715 (Math/exp (* -3.59 (/ sigma' 10000.0))))
        F              (Math/pow (/ beta beta_op) E)
        get-phi_S      (get-phi_S-fn beta)
        get-phi_W      (get-phi_W-fn beta B C F)
        get-wind-speed (get-wind-speed-fn B C F)]
    (->SurfaceFireMin R'
                      I_R
                      t_res
                      delta
                      (h 0)
                      get-phi_S
                      get-phi_W
                      get-wind-speed)))

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

(defn almost-zero? [^double x]
  (< (Math/abs x) 0.000001))

(defrecord SurfaceFireMax
    [^double max-spread-rate
     ^double max-spread-direction
     ^double effective-wind-speed
     ^double eccentricity])

(defn scale-spread-to-max-wind-speed
  [spread-properties ^double spread-rate ^double max-wind-speed ^double phi-max]
  (let [effective-wind-speed (:effective-wind-speed spread-properties)
        max-spread-direction (:max-spread-direction spread-properties)]
   (if (> ^double effective-wind-speed max-wind-speed)
     (->SurfaceFireMax (* spread-rate (+ 1.0 phi-max)) max-spread-direction max-wind-speed 0.0)
     spread-properties)))

(defn add-eccentricity
  [spread-properties ^double ellipse-adjustment-factor]
  (let [effective-wind-speed (:effective-wind-speed spread-properties)
        length-width-ratio   (+ 1.0 (-> 0.002840909
                                        (* ^double effective-wind-speed)
                                        (* ellipse-adjustment-factor)))
        eccentricity         (/ (Math/sqrt (- (Math/pow length-width-ratio 2.0) 1.0))
                                length-width-ratio)]
    (assoc spread-properties :eccentricity eccentricity)))

(defn smallest-angle-between
  "Computes the absolute difference between two angles as an angle between 0° and 180°.

  The return angle has the same cosine as (- theta1 theta2), but may have an opposite sine."
  ^double [^double theta1 ^double theta2]
  (let [angle (Math/abs (- theta1 theta2))]
    (if (> angle 180.0)
      (- 360.0 angle)
      angle)))

(defn determine-spread-drivers
  [^double midflame-wind-speed ^double wind-to-direction ^double slope ^double slope-direction]
  (if (almost-zero? slope)
    (if (almost-zero? midflame-wind-speed)
      :no-wind-no-slope
      :wind-only)
    (if (almost-zero? midflame-wind-speed)
      :slope-only
      (if (< (smallest-angle-between wind-to-direction slope-direction) 15.0)
        :wind-blows-upslope
        :wind-blows-across-slope))))

(defn spread-info-max-no-wind-no-slope
  [^double spread-rate]
  (->SurfaceFireMax spread-rate 0.0 0.0 0.0))

(defn spread-info-max-wind-only
  [^double spread-rate ^double phi_W ^double midflame-wind-speed ^double wind-to-direction]
  (->SurfaceFireMax (* spread-rate (+ 1.0 phi_W)) wind-to-direction midflame-wind-speed 0.0))

(defn spread-info-max-slope-only
  [^double spread-rate ^double phi_S ^double slope-direction get-wind-speed]
  (->SurfaceFireMax (* spread-rate (+ 1.0 phi_S)) slope-direction (get-wind-speed phi_S) 0.0))

(defn spread-info-max-wind-blows-upslope
  [^double spread-rate ^double phi-combined ^double slope-direction get-wind-speed]
  (->SurfaceFireMax (* spread-rate (+ 1.0 phi-combined)) slope-direction (get-wind-speed phi-combined) 0.0))

(defn spread-info-max-wind-blows-across-slope
  [spread-rate phi_W phi_S wind-to-direction slope-direction get-wind-speed]
  (let [spread-rate        (double spread-rate)
        wind-to-direction  (double wind-to-direction)
        slope-direction    (double slope-direction)
        wind-magnitude     (* spread-rate ^double phi_W)
        slope-magnitude    (* spread-rate ^double phi_S)
        difference-angle   (deg->rad
                            (mod (- wind-to-direction slope-direction) 360.0))
        x                  (+ slope-magnitude
                              (* wind-magnitude (Math/cos difference-angle)))
        y                  (* wind-magnitude (Math/sin difference-angle))
        combined-magnitude (Math/sqrt (+ (* x x) (* y y)))]
    (if (almost-zero? combined-magnitude)
      (->SurfaceFireMax spread-rate 0.0 0.0 0.0)
      (let [max-spread-rate      (+ spread-rate combined-magnitude)
            phi-combined         (- (/ max-spread-rate spread-rate) 1.0)
            offset               (rad->deg
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
        (->SurfaceFireMax max-spread-rate max-spread-direction effective-wind-speed 0.0)))))

(defn resolve-spread-rate ^double
  [^SurfaceFireMin surface-fire-min ^double adjustment-factor]
  (-> surface-fire-min
      (:unadj-spread-rate)
      (double)
      (* adjustment-factor)))

(defn rothermel-surface-fire-spread-max
  "Note: fire ellipse adjustment factor, < 1.0 = more circular, > 1.0 = more elliptical"
  [surface-fire-min midflame-wind-speed wind-from-direction slope aspect ellipse-adjustment-factor spread-rate-adjustment]
  (let [spread-rate               (resolve-spread-rate surface-fire-min (or spread-rate-adjustment 1.0))
        reaction-intensity        (double (:reaction-intensity surface-fire-min))
        get-phi_S                 (:get-phi_S surface-fire-min)
        get-phi_W                 (:get-phi_W surface-fire-min)
        get-wind-speed            (:get-wind-speed surface-fire-min)
        midflame-wind-speed       (double midflame-wind-speed)
        wind-from-direction       (double wind-from-direction)
        slope                     (double slope)
        aspect                    (double aspect)
        ellipse-adjustment-factor (double ellipse-adjustment-factor)
        slope-direction           (mod (+ aspect 180.0) 360.0)
        wind-to-direction         (mod (+ wind-from-direction 180.0) 360.0)
        max-wind-speed            (* 0.9 reaction-intensity)
        ^double phi_S             (get-phi_S slope)
        ^double phi_W             (get-phi_W midflame-wind-speed)
        ^double phi-max           (get-phi_W max-wind-speed)]
    (->
     (case (determine-spread-drivers midflame-wind-speed wind-to-direction slope slope-direction)
       :no-wind-no-slope        (spread-info-max-no-wind-no-slope spread-rate)
       :wind-only               (spread-info-max-wind-only spread-rate phi_W midflame-wind-speed wind-to-direction)
       :slope-only              (spread-info-max-slope-only spread-rate phi_S slope-direction get-wind-speed)
       :wind-blows-upslope      (spread-info-max-wind-blows-upslope spread-rate (+ phi_W phi_S)
                                                                    slope-direction get-wind-speed)
       :wind-blows-across-slope (spread-info-max-wind-blows-across-slope spread-rate phi_W phi_S wind-to-direction
                                                                         slope-direction get-wind-speed))
     (scale-spread-to-max-wind-speed spread-rate max-wind-speed phi-max)
     (add-eccentricity ellipse-adjustment-factor))))

(defn compute-spread-rate ^double
  [^double max-spread-rate ^double max-spread-direction ^double eccentricity ^double spread-direction]
  (let [theta (smallest-angle-between max-spread-direction spread-direction)]
    (if (or (almost-zero? eccentricity) (almost-zero? theta))
      max-spread-rate
      (* max-spread-rate (/ (- 1.0 eccentricity)
                            (- 1.0 (* eccentricity
                                      (Math/cos (deg->rad theta)))))))))

(defn anderson-flame-depth
  "Returns the depth, or front-to-back distance, of the actively flaming zone
   of a free-spreading fire in ft given:
   - spread-rate (ft/min) orthogonal to the fire line.
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

(comment

  (require '[criterium.core :refer [quick-bench]])

  (def test-fuel-model
    {:f_i    {:dead 0.9186991869918698, :live 0.08130081300813012},
     :rho_p
     {:dead {:1hr 32.0, :10hr 32.0, :100hr 32.0, :herbaceous 32.0},
      :live {:herbaceous 32.0, :woody 32.0}},
     :number 101,
     :name   :GR1,
     :sigma
     {:dead {:1hr 2200.0, :10hr 109.0, :100hr 30.0, :herbaceous 2000.0},
      :live {:herbaceous 2000.0, :woody 0.0}},
     :M_x
     {:dead {:1hr 0.15, :10hr 0.15, :100hr 0.15, :herbaceous 0.15},
      :live {:herbaceous 12.539016851465732, :woody 12.539016851465732}},
     :w_o
     {:dead
      {:1hr        0.0046,
       :10hr       0.0,
       :100hr      0.0,
       :herbaceous 0.012266666666666665},
      :live {:herbaceous 0.001533333333333334, :woody 0.0}},
     :S_T
     {:dead
      {:1hr 0.0555, :10hr 0.0555, :100hr 0.0555, :herbaceous 0.0555},
      :live {:herbaceous 0.0555, :woody 0.0555}},
     :h
     {:dead
      {:1hr 8000.0, :10hr 8000.0, :100hr 8000.0, :herbaceous 8000.0},
      :live {:herbaceous 8000.0, :woody 8000.0}},
     :M_f
     {:dead {:1hr 0.1, :10hr 0.2, :100hr 0.3, :herbaceous 0.1},
      :live {:herbaceous 0.4, :woody 0.5}},
     :delta  0.4,
     :f_ij
     {:dead
      {:1hr        0.2920353982300885,
       :10hr       0.0,
       :100hr      0.0,
       :herbaceous 0.7079646017699115},
      :live {:herbaceous 1.0, :woody 0.0}},
     :g_ij
     {:dead {:1hr 1.0, :10hr 0.0, :100hr 0.0, :herbaceous 1.0},
      :live {:herbaceous 1.0, :woody 0.0}},
     :S_e
     {:dead {:1hr 0.01, :10hr 0.01, :100hr 0.01, :herbaceous 0.01},
      :live {:herbaceous 0.01, :woody 0.01}}})

  (defn vectorize-fuel-model [fuel-model]
    (into {}
          (map (fn [[k {:keys [dead live] :as v}]]
                 [k (cond (map? dead)    [(dead :1hr)
                                          (dead :10hr)
                                          (dead :100hr)
                                          (dead :herbaceous)
                                          (live :herbaceous)
                                          (live :woody)]
                          (number? dead) [dead live]
                          :else          v)])
               fuel-model)))

  (def test-fuel-model-vector
    (vectorize-fuel-model test-fuel-model))

  ;; 47-63ns
  (let [f_ij (:f_ij test-fuel-model-vector)
        S_e  (:S_e test-fuel-model-vector)]
    (quick-bench
     (size-class-sum (fn ^double [i] (* ^double (f_ij i) ^double (S_e i))))))

  ;; 18-43ns
  (let [f_i       [0.1 0.2]
        epsilon_i [0.3 0.4]]
    (quick-bench (category-sum (fn ^double [i] (* ^double (f_i i) ^double (epsilon_i i))))))

  ;; 47-49ns
  (let [M_f [0.1 0.2 0.3 0.1 0.4 0.5]]
    (quick-bench (map-size-class (fn ^double [i] (+ 250.0 (* 1116.0 ^double (M_f i)))))))

  ;; 92-97ns
  (let [S_e_i [0.1 0.1]]
    (quick-bench
     (map-category (fn ^double [i]
                     (let [^double S_e_i (S_e_i i)]
                       (if (pos? S_e_i)
                         (/ 0.174 (Math/pow S_e_i 0.19))
                         1.0))))))

  ;; 2.21-2.25us
  (quick-bench (rothermel-surface-fire-spread-no-wind-no-slope test-fuel-model-vector false))

  ;; Notes:
  ;; 1. Stick with longs and doubles. Don't waste time casting to ints and floats.
  ;; 2. Use Java collections for performance:
  ;;    - java.util.ArrayList
  ;;    - java.util.BitSet
  ;;    - boolean[]
  ;;    - java.util.PriorityQueue
  ;;    - primitive arrays (e.g., longs, doubles)

  )
;; gridfire.surface-fire ends here
