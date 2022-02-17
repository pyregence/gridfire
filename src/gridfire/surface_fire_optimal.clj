(ns gridfire.surface-fire-optimal
  (:require [gridfire.fuel-models-optimal :refer [map-category map-size-class category-sum size-class-sum]]))

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

(defn- grass-fuel-model? [^long number]
  (and (> number 100) (< number 110)))

;; Addition proposed by Chris Lautenberger (REAX 2015)
(defn calc-suppressed-spread-rate ^double [^double R ^long number grass-suppression?]
  (let [spread-rate-multiplier (if (and grass-suppression?
                                        (grass-fuel-model? number))
                                 0.5
                                 1.0)]
    (* R spread-rate-multiplier)))

(defn calc-residence-time ^double [^double sigma']
  (/ 384.0 sigma'))

(defn get-wind-and-slope-fns [^double beta ^double beta_op ^double sigma']
  (let [E          (* 0.715 (Math/exp (* -3.59 (/ sigma' 10000.0))))

        B          (* 0.02526 (Math/pow sigma' 0.54))

        C          (* 7.47 (Math/exp (* -0.133 (Math/pow sigma' 0.55))))

        F          (Math/pow (/ beta beta_op) E)

        G          (* 5.275 (Math/pow beta -0.3))

        C-over-F   (/ C F)

        F-over-C   (/ F C)

        B-inverse  (/ 1.0 B)

        ;; Derive slope factor
        get-phi_S  (if (pos? beta)
                     (fn ^double [^double slope]
                       (if (pos? slope)
                         (-> slope
                             (Math/pow 2.0)
                             (* G))
                         0.0))
                     (fn ^double [^double _]
                       0.0))

        ;; Derive wind factor
        get-phi_W  (if (pos? beta)
                     (fn ^double [^double midflame-wind-speed]
                       (if (pos? midflame-wind-speed)
                         (-> midflame-wind-speed
                             (Math/pow B)
                             (* C-over-F))
                         0.0))
                     (fn ^double [^double _]
                       0.0))

        ;; Derive wind speed from wind factor
        get-wind-speed (fn ^double [^double phi_W]
                         (-> phi_W
                             (* F-over-C)
                             (Math/pow B-inverse)))]

    {:get-phi_S      get-phi_S
     :get-phi_W      get-phi_W
     :get-wind-speed get-wind-speed}))

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
  [{:keys [number delta w_o sigma h rho_p S_T S_e M_x M_f f_ij f_i g_ij]} grass-suppression?]
  (let [eta_S_i   (calc-mineral-damping-coefficients f_ij S_e)
        eta_M_i   (calc-moisture-damping-coefficients f_ij M_f M_x)
        h_i       (calc-low-heat-content f_ij h)
        W_n_i     (calc-net-fuel-loading g_ij w_o S_T) ; (lb/ft^2)
        beta      (calc-packing-ratio w_o rho_p delta)
        sigma'    (calc-surface-area-to-volume-ratio f_i f_ij sigma)
        beta_op   (calc-optimum-packing-ratio sigma')
        Gamma'    (calc-optimum-reaction-velocity beta sigma' beta_op) ; (1/min)
        Btus      (calc-heat-per-unit-area eta_S_i eta_M_i h_i W_n_i) ; (Btu/ft^2)
        I_R       (calc-reaction-intensity Gamma' Btus) ; (Btu/ft^2*min)
        xi        (calc-propagating-flux-ratio beta sigma')
        Q_ig      (calc-heat-of-preignition M_f) ; (Btu/lb)
        epsilon_i (calc-heat-distribution sigma Q_ig f_ij) ; (Btu/lb)
        rho_b     (calc-ovendry-bulk-density w_o delta) ; (lb/ft^3)
        epsilon   (calc-heat-total f_i epsilon_i) ; (Btu/lb)
        R         (calc-surface-fire-spread-rate I_R xi rho_b epsilon) ; (ft/min)
        R'        (calc-suppressed-spread-rate R number grass-suppression?)
        t_res     (calc-residence-time sigma')]
    [{:spread-rate        R'
      :reaction-intensity I_R
      :residence-time     t_res}
     (get-wind-and-slope-fns beta beta_op sigma')]))

(comment

  (require '[criterium.core :refer [quick-bench]])

  (def test-fuel-model
    {:f_i {:dead 0.9186991869918698, :live 0.08130081300813012},
     :rho_p
     {:dead {:1hr 32.0, :10hr 32.0, :100hr 32.0, :herbaceous 32.0},
      :live {:herbaceous 32.0, :woody 32.0}},
     :number 101,
     :name :GR1,
     :sigma
     {:dead {:1hr 2200.0, :10hr 109.0, :100hr 30.0, :herbaceous 2000.0},
      :live {:herbaceous 2000.0, :woody 0.0}},
     :M_x
     {:dead {:1hr 0.15, :10hr 0.15, :100hr 0.15, :herbaceous 0.15},
      :live {:herbaceous 12.539016851465732, :woody 12.539016851465732}},
     :w_o
     {:dead
      {:1hr 0.0046,
       :10hr 0.0,
       :100hr 0.0,
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
     :delta 0.4,
     :f_ij
     {:dead
      {:1hr 0.2920353982300885,
       :10hr 0.0,
       :100hr 0.0,
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
