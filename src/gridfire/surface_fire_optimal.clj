(ns gridfire.surface-fire-optimal)

(set! *unchecked-math* nil)
;; (set! *unchecked-math* :warn-on-boxed)

;; BEST CHOICE
(defn map-category [f]
  [(f 0) (f 1)])

(defn map-category-old [f]
  {:dead (f :dead) :live (f :live)})

;; BEST CHOICE
(defn map-size-class [f]
  [(f 0)
   (f 1)
   (f 2)
   (f 3)
   (f 4)
   (f 5)])

(defn map-size-class-old [f]
  {:dead {:1hr        (f :dead :1hr)
          :10hr       (f :dead :10hr)
          :100hr      (f :dead :100hr)
          :herbaceous (f :dead :herbaceous)}
   :live {:herbaceous (f :live :herbaceous)
          :woody      (f :live :woody)}})

;; BEST CHOICE
(defn category-sum
  ^double [f]
  (+ ^double (f 0) ^double (f 1)))

(defn category-sum-old [f]
  (+ (f :dead) (f :live)))

(defn category-sum-vector [f]
  (+ (f 0) (f 1)))

;; BEST CHOICE
;; Uses 6 fn calls, 12 vector lookups, 6 multiplications, 4 additions, 1 vector creation
(defn size-class-sum [f]
  [(-> ^double (f 0)
       (+ ^double (f 1))
       (+ ^double (f 2))
       (+ ^double (f 3)))
   (+ ^double (f 4)
      ^double (f 5))])

;; Uses 6 fn calls, 24 array-map lookups, 6 multiplications, 4 additions, 1 array-map creation
;; Runtime: ~400ns
(defn size-class-sum-old [f]
  {:dead (+ (f :dead :1hr)
            (f :dead :10hr)
            (f :dead :100hr)
            (f :dead :herbaceous))
   :live (+ (f :live :herbaceous)
            (f :live :woody))})

;; Uses 6 fn calls, 12 array-map lookups, 6 multiplications, 4 additions, 1 array-map creation
;; Runtime: ~300ns
(defn size-class-sum-shallow [f]
  {:dead (+ (f :dead-1hr)
            (f :dead-10hr)
            (f :dead-100hr)
            (f :dead-herbaceous))
   :live (+ (f :live-herbaceous)
            (f :live-woody))})

;; Uses 6 fn calls, 12 vector lookups, 6 multiplications, 4 additions, 1 array-map creation
(defn size-class-sum-vector [f]
  {:dead (+ (f 0)
            (f 1)
            (f 2)
            (f 3))
   :live (+ (f 4)
            (f 5))})

;; Uses 6 fn calls, 12 vector lookups, 6 multiplications, 4 additions, 1 vector creation
(defn size-class-sum-vector-2 [f]
  [(+ (f 0)
      (f 1)
      (f 2)
      (f 3))
   (+ (f 4)
      (f 5))])

;; Uses 6 fn calls, 12 vector lookups, 6 multiplications, 4 additions, 1 vector creation
(defn size-class-sum-vector-3 [f]
  [(-> (f 0)
       (+ (f 1))
       (+ (f 2))
       (+ (f 3)))
   (+ (f 4)
      (f 5))])

;; Uses 6 fn calls, 12 array lookups, 6 multiplications, 4 additions, 1 array-map creation
(defn size-class-sum-array [f]
  {:dead (+ (f 0)
            (f 1)
            (f 2)
            (f 3))
   :live (+ (f 4)
            (f 5))})

;; Uses 12 vector lookups, 6 multiplications, 1 vector creation
(defn size-class-multiply [x y]
  [(* ^double (x 0) ^double (y 0))
   (* ^double (x 1) ^double (y 1))
   (* ^double (x 2) ^double (y 2))
   (* ^double (x 3) ^double (y 3))
   (* ^double (x 4) ^double (y 4))
   (* ^double (x 5) ^double (y 5))])

;; Uses 12 vector lookups, 6 multiplications, 4 additions, 1 vector creation
(defn size-class-sum-all-in-one [x y]
  [(-> ^double (* ^double (x 0) ^double (y 0))
       (+ ^double (* ^double (x 1) ^double (y 1)))
       (+ ^double (* ^double (x 2) ^double (y 2)))
       (+ ^double (* ^double (x 3) ^double (y 3))))
   (+ ^double (* ^double (x 4) ^double (y 4))
      ^double (* ^double (x 5) ^double (y 5)))])

(comment

  ;; Clever optimization ideas:
  ;;
  ;; 1. Replace variadic + with threaded + in size-class-sum
  ;; 2. Turn on *unchecked-math*
  ;; 3. Add primitive type hints
  ;; 4. Implement size-class-sum and friends as protocol methods on SizeClassValues
  ;; 5. Implement size-class-sum and friends as Java methods directly
  
  (require '[criterium.core :refer [quick-bench]])

  (defrecord SizeClassValues [^double dead-1hr
                              ^double dead-10hr
                              ^double dead-100hr
                              ^double dead-herbaceous
                              ^double live-herbaceous
                              ^double live-woody])

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

  (def test-fuel-model-shallow
    {:f_ij {:dead-1hr 0.2920353982300885,
            :dead-10hr 0.0,
            :dead-100hr 0.0,
            :dead-herbaceous 0.7079646017699115,
            :live-herbaceous 1.0,
            :live-woody 0.0}
     :S_e  {:dead-1hr 0.01,
            :dead-10hr 0.01,
            :dead-100hr 0.01,
            :dead-herbaceous 0.01,
            :live-herbaceous 0.01,
            :live-woody 0.01}})

  (def test-fuel-model-record
    {:f_ij (map->SizeClassValues (:f_ij test-fuel-model-shallow))
     :S_e  (map->SizeClassValues (:S_e test-fuel-model-shallow))})

  (def test-fuel-model-vector
    {:f_ij [0.2920353982300885
            0.0
            0.0
            0.7079646017699115
            1.0
            0.0]
     :S_e  [0.01
            0.01
            0.01
            0.01
            0.01
            0.01]})

  (def test-fuel-model-array
    {:f_ij (double-array
            [0.2920353982300885
             0.0
             0.0
             0.7079646017699115
             1.0
             0.0])
     :S_e  (double-array
            [0.01
             0.01
             0.01
             0.01
             0.01
             0.01])})

  ;; 335-357ns
  (let [f_ij (:f_ij test-fuel-model)
        S_e (:S_e test-fuel-model)]
    (quick-bench
     (size-class-sum-old (fn [i j] (* (-> f_ij i j) (-> S_e i j))))))

  ;; 283-283ns
  (let [f_ij (:f_ij test-fuel-model-shallow)
        S_e (:S_e test-fuel-model-shallow)]
    (quick-bench
     (size-class-sum-shallow (fn [i] (* (f_ij i) (S_e i))))))

  ;; 425-434ns
  (let [f_ij (:f_ij test-fuel-model-record)
        S_e (:S_e test-fuel-model-record)]
    (quick-bench
     (size-class-sum-shallow (fn [i] (* (-> f_ij i) (-> S_e i))))))

  ;; 333-327ns
  (let [f_ij (:f_ij test-fuel-model-record)
        S_e (:S_e test-fuel-model-record)]
    (quick-bench
     (size-class-sum-shallow (fn ^double [i] (* ^double (get f_ij i) ^double (get S_e i))))))

  ;; 281-283ns
  (let [f_ij (:f_ij test-fuel-model-vector)
        S_e (:S_e test-fuel-model-vector)]
    (quick-bench
     (size-class-sum-vector (fn [i] (* (get f_ij i) (get S_e i))))))

  ;; 207-209ns
  (let [f_ij (:f_ij test-fuel-model-vector)
        S_e (:S_e test-fuel-model-vector)]
    (quick-bench
     (size-class-sum-vector-2 (fn [i] (* (f_ij i) (S_e i))))))

  ;; 163-180ns
  (let [f_ij (:f_ij test-fuel-model-vector)
        S_e (:S_e test-fuel-model-vector)]
    (quick-bench
     (size-class-sum-vector-3 (fn [i] (* (f_ij i) (S_e i))))))

  ;; 120-142ns
  (let [f_ij (:f_ij test-fuel-model-vector)
        S_e (:S_e test-fuel-model-vector)]
    (quick-bench
     (size-class-sum-vector-3 (fn ^double [i] (* ^double (f_ij i) ^double (S_e i))))))

  ;; 47-63ns
  (let [f_ij (:f_ij test-fuel-model-vector)
        S_e (:S_e test-fuel-model-vector)]
    (quick-bench
     (size-class-sum (fn ^double [i] (* ^double (f_ij i) ^double (S_e i))))))

  ;; 39-42ns
  (let [f_ij (:f_ij test-fuel-model-vector)
        S_e (:S_e test-fuel-model-vector)]
    (quick-bench
     (size-class-sum-all-in-one f_ij S_e)))

  ;; 108-109ns
  (let [f_ij (:f_ij test-fuel-model-vector)
        S_e (:S_e test-fuel-model-vector)]
    (quick-bench
     (size-class-sum (size-class-multiply f_ij S_e))))

  ;; 3.92-3.97us
  (let [f_ij (:f_ij test-fuel-model-array)
        S_e (:S_e test-fuel-model-array)]
    (quick-bench
     (size-class-sum-array (fn [i] (* (get f_ij i) (get S_e i))))))

  ;; 198.4-198.7us
  (let [f_ij (:f_ij test-fuel-model-array)
        S_e (:S_e test-fuel-model-array)]
    (quick-bench
     (size-class-sum-array (fn [i] (* (aget f_ij i) (aget S_e i))))))

  ;; 59-70ns
  (let [f_i   {:dead 0.1 :live 0.2}
        foo_i {:dead 0.3 :live 0.4}]
    (quick-bench (category-sum-old (fn [i] (* (f_i i) (foo_i i))))))

  ;; 46-59ns
  (let [f_i   [0.1 0.2]
        foo_i [0.3 0.4]]
    (quick-bench (category-sum-vector (fn [i] (* (f_i i) (foo_i i))))))

  ;; 42-63ns
  (let [f_i   [0.1 0.2]
        foo_i [0.3 0.4]]
    (quick-bench (category-sum (fn [i] (* (f_i i) (foo_i i))))))

  ;; 43-44ns
  (let [f_i   [0.1 0.2]
        foo_i [0.3 0.4]]
    (quick-bench (category-sum (fn ^double [i] (* ^double (f_i i) ^double (foo_i i))))))

  ;; 161-177ns
  (let [M_f {:dead {:1hr 0.1, :10hr 0.2, :100hr 0.3, :herbaceous 0.1},
             :live {:herbaceous 0.4, :woody 0.5}}]
    (quick-bench (map-size-class-old (fn [i j] (+ 250.0 (* 1116.0 (-> M_f i j)))))))

  ;; 58-67ns
  (let [M_f [0.1 0.2 0.3 0.1 0.4 0.5]]
    (quick-bench (map-size-class (fn [i] (+ 250.0 (* 1116.0 (M_f i)))))))

  ;; 47-49ns
  (let [M_f [0.1 0.2 0.3 0.1 0.4 0.5]]
    (quick-bench (map-size-class (fn ^double [i] (+ 250.0 (* 1116.0 ^double (M_f i)))))))

  ;; 105-135ns
  (let [S_e_i {:dead 0.1 :live 0.1}]
    (quick-bench
     (map-category-old (fn [i] (let [S_e_i (S_e_i i)]
                                 (if (pos? S_e_i)
                                   (/ 0.174 (Math/pow S_e_i 0.19))
                                   1.0))))))

  ;; 92-92ns
  (let [S_e_i [0.1 0.1]]
    (quick-bench
     (map-category (fn [i] (let [S_e_i (S_e_i i)]
                             (if (pos? S_e_i)
                               (/ 0.174 (Math/pow S_e_i 0.19))
                               1.0))))))

  ;; 92-97ns
  (let [S_e_i [0.1 0.1]]
    (quick-bench
     (map-category (fn ^double [i]
                     (let [^double S_e_i (S_e_i i)]
                       (if (pos? S_e_i)
                         (/ 0.174 (Math/pow S_e_i 0.19))
                         1.0))))))

  ;; 93-94ns
  (let [S_e_i [0.1 0.1]]
    (quick-bench
     (map-category (fn ^double [i]
                     (let [^double S_e_i (S_e_i i)]
                       (if (> S_e_i 0.0)
                         (/ 0.174 (Math/pow S_e_i 0.19))
                         1.0))))))

  )

(defn calc-mineral-damping-coefficients [f_ij S_e]
  (let [S_e_i (size-class-sum (fn [i j] (* (-> f_ij i j) (-> S_e i j))))]
    (map-category (fn [i] (let [S_e_i (-> S_e_i i)]
                            (if (pos? S_e_i)
                              (/ 0.174 (Math/pow S_e_i 0.19))
                              1.0))))))

(defn calc-moisture-damping-coefficients [f_ij M_f M_x]
  (let [M_f_i (size-class-sum (fn [i j] (* (-> f_ij i j) (-> M_f i j))))
        M_x_i (size-class-sum (fn [i j] (* (-> f_ij i j) (-> M_x i j))))
        r_M_i (map-category (fn [i] (let [M_f (-> M_f_i i)
                                          M_x (-> M_x_i i)]
                                      (if (pos? M_x)
                                        (min 1.0 (/ M_f M_x))
                                        1.0))))]
    (map-category (fn [i] (+ 1.0
                             (* -2.59 (-> r_M_i i))
                             (* 5.11 (Math/pow (-> r_M_i i) 2))
                             (* -3.52 (Math/pow (-> r_M_i i) 3)))))))

(defn calc-low-heat-content [f_ij h]
  (size-class-sum (fn [i j] (* (-> f_ij i j) (-> h i j)))))

(defn calc-net-fuel-loading [g_ij w_o S_T]
  (size-class-sum (fn [i j] (* (-> g_ij i j)
                               (-> w_o i j)
                               (- 1.0 (-> S_T i j))))))

(defn calc-packing-ratio [w_o rho_p delta]
  (let [beta_i (size-class-sum (fn [i j] (/ (-> w_o i j) (-> rho_p i j))))]
    (if (pos? delta)
      (/ (category-sum (fn [i] (-> beta_i i))) delta)
      0.0)))

(defn calc-surface-area-to-volume-ratio [f_i f_ij sigma]
  (let [sigma'_i (size-class-sum (fn [i j] (* (-> f_ij i j) (-> sigma i j))))]
    (category-sum (fn [i] (* (-> f_i i) (-> sigma'_i i))))))

(defn calc-optimum-packing-ratio [sigma']
  (if (pos? sigma')
    (/ 3.348 (Math/pow sigma' 0.8189))
    1.0))

(defn calc-reaction-intensity [eta_S_i eta_M_i h_i W_n_i beta sigma' beta_op]
  (let [;; Albini 1976 replaces (/ 1 (- (* 4.774 (Math/pow sigma' 0.1)) 7.27))
        A          (if (pos? sigma')
                     (/ 133.0 (Math/pow sigma' 0.7913))
                     0.0)
        ;; Maximum reaction velocity (1/min)
        Gamma'_max (/ (Math/pow sigma' 1.5)
                      (+ 495.0 (* 0.0594 (Math/pow sigma' 1.5))))
        ;; Optimum reaction velocity (1/min)
        Gamma'     (* Gamma'_max
                      (Math/pow (/ beta beta_op) A)
                      (Math/exp (* A (- 1.0 (/ beta beta_op)))))]
    (* Gamma' (category-sum (fn [i] (* (W_n_i i) (h_i i) (eta_M_i i) (eta_S_i i)))))))

(defn calc-propagating-flux-ratio [beta sigma']
  (/ (Math/exp (* (+ 0.792 (* 0.681 (Math/pow sigma' 0.5)))
                  (+ beta 0.1)))
     (+ 192.0 (* 0.2595 sigma'))))

(defn calc-heat-of-preignition [M_f]
  (map-size-class (fn [i j] (+ 250.0 (* 1116.0 (-> M_f i j))))))

(defn calc-mystery-term [sigma Q_ig f_ij]
  (size-class-sum (fn [i j] (let [sigma_ij (-> sigma i j)
                                  Q_ig_ij  (-> Q_ig i j)]
                              (if (pos? sigma_ij)
                                (* (-> f_ij i j)
                                   (Math/exp (/ -138 sigma_ij))
                                   Q_ig_ij)
                                0.0)))))

(defn calc-ovendry-bulk-density [w_o delta]
  (let [rho_b_i (size-class-sum (fn [i j] (-> w_o i j)))]
    (if (pos? delta)
      (/ (category-sum (fn [i] (-> rho_b_i i))) delta)
      0.0)))

(defn calc-surface-fire-spread-rate [I_R xi foo_i rho_b f_i]
  (let [rho_b-epsilon-Q_ig (* rho_b (category-sum (fn [i] (* (-> f_i i) (-> foo_i i)))))]
    (if (pos? rho_b-epsilon-Q_ig)
      (/ (* I_R xi) rho_b-epsilon-Q_ig)
      0.0)))

(defn- grass-fuel-model?
  [^long number]
  (and (> number 100) (< number 110)))

;; Addition proposed by Chris Lautenberger (REAX 2015)
(defn calc-suppressed-spread-rate [R number grass-suppression?]
  (let [spread-rate-multiplier (if (and grass-suppression? (grass-fuel-model? number)) 0.5 1.0)]
    (* R spread-rate-multiplier)))

(defn calc-residence-time [sigma']
  (/ 384.0 sigma'))

(defn get-wind-and-slope-fns [beta beta_op sigma']
  (let [E          (* 0.715 (Math/exp (* -3.59 (/ sigma' 10000.0))))

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
                       0.0))]
    {:get-phi_W      get-phi_W
     :get-phi_S      get-phi_S
     :get-wind-speed get-wind-speed}))

;; TODO: Pass fuel-model in as a record instead of a map
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
  [{:keys [number delta w_o sigma h rho_p S_T S_e M_x M_f f_ij f_i g_ij]} & [grass-suppression?]]
  (let [eta_S_i (calc-mineral-damping-coefficients f_ij S_e)
        eta_M_i (calc-moisture-damping-coefficients f_ij M_f M_x)
        h_i     (calc-low-heat-content f_ij h)
        W_n_i   (calc-net-fuel-loading g_ij w_o S_T) ; (lb/ft^2)
        beta    (calc-packing-ratio w_o rho_p delta)
        sigma'  (calc-surface-area-to-volume-ratio f_i f_ij sigma)
        beta_op (calc-optimum-packing-ratio sigma')
        I_R     (calc-reaction-intensity eta_S_i eta_M_i h_i W_n_i beta sigma' beta_op) ; (Btu/ft^2*min)
        xi      (calc-propagating-flux-ratio beta sigma')
        Q_ig    (calc-heat-of-preignition M_f) ; (Btu/lb)
        foo_i   (calc-mystery-term sigma Q_ig f_ij)
        rho_b   (calc-ovendry-bulk-density w_o delta) ; (lb/ft^3)
        R       (calc-surface-fire-spread-rate I_R xi foo_i rho_b f_i) ; (ft/min)
        R'      (calc-suppressed-spread-rate R number grass-suppression?)
        t_res   (calc-residence-time sigma')]
    (-> (get-wind-and-slope-fns beta beta_op sigma')
        (assoc :spread-rate        R'
               :reaction-intensity I_R
               :residence-time     t_res))))
