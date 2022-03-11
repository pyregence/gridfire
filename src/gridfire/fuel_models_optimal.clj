;; [[file:../../org/GridFire.org::fuel-model-definitions][fuel-model-definitions]]
(ns gridfire.fuel-models-optimal)

(set! *unchecked-math* :warn-on-boxed)

(def fuel-models
  "Lookup table including one entry for each of the Anderson 13 and
   Scott & Burgan 40 fuel models. The fields have the following
   meanings:
   {number
    [name delta M_x-dead h
     [w_o-dead-1hr w_o-dead-10hr w_o-dead-100hr w_o-live-herbaceous w_o-live-woody]
     [sigma-dead-1hr sigma-dead-10hr sigma-dead-100hr sigma-live-herbaceous sigma-live-woody]]
   }"
  {
   ;; Grass and Grass-dominated (short-grass,timber-grass-and-understory,tall-grass)
   1   [:R01 1.0 12 8 [0.0340 0.0000 0.0000 0.0000 0.0000] [3500.0   0.0  0.0    0.0    0.0]]
   2   [:R02 1.0 15 8 [0.0920 0.0460 0.0230 0.0230 0.0000] [3000.0 109.0 30.0 1500.0    0.0]]
   3   [:R03 2.5 25 8 [0.1380 0.0000 0.0000 0.0000 0.0000] [1500.0   0.0  0.0    0.0    0.0]]
   ;; Chaparral and Shrubfields (chaparral,brush,dormant-brush-hardwood-slash,southern-rough)
   4   [:R04 6.0 20 8 [0.2300 0.1840 0.0920 0.2300 0.0000] [2000.0 109.0 30.0 1500.0    0.0]]
   5   [:R05 2.0 20 8 [0.0460 0.0230 0.0000 0.0920 0.0000] [2000.0 109.0  0.0 1500.0    0.0]]
   6   [:R06 2.5 25 8 [0.0690 0.1150 0.0920 0.0000 0.0000] [1750.0 109.0 30.0    0.0    0.0]]
   7   [:R07 2.5 40 8 [0.0520 0.0860 0.0690 0.0170 0.0000] [1750.0 109.0 30.0 1550.0    0.0]]
   ;; Timber Litter (closed-timber-litter,hardwood-litter,timber-litter-and-understory)
   8   [:R08 0.2 30 8 [0.0690 0.0460 0.1150 0.0000 0.0000] [2000.0 109.0 30.0    0.0    0.0]]
   9   [:R09 0.2 25 8 [0.1340 0.0190 0.0070 0.0000 0.0000] [2500.0 109.0 30.0    0.0    0.0]]
   10  [:R10 1.0 25 8 [0.1380 0.0920 0.2300 0.0920 0.0000] [2000.0 109.0 30.0 1500.0    0.0]]
   ;; Logging Slash (light-logging-slash,medium-logging-slash,heavy-logging-slash)
   11  [:R11 1.0 15 8 [0.0690 0.2070 0.2530 0.0000 0.0000] [1500.0 109.0 30.0    0.0    0.0]]
   12  [:R12 2.3 20 8 [0.1840 0.6440 0.7590 0.0000 0.0000] [1500.0 109.0 30.0    0.0    0.0]]
   13  [:R13 3.0 25 8 [0.3220 1.0580 1.2880 0.0000 0.0000] [1500.0 109.0 30.0    0.0    0.0]]
   ;; Nonburnable (NB)
   91  [:NB1 0.0  0 0 [0.0000 0.0000 0.0000 0.0000 0.0000] [   0.0   0.0  0.0    0.0    0.0]]
   92  [:NB2 0.0  0 0 [0.0000 0.0000 0.0000 0.0000 0.0000] [   0.0   0.0  0.0    0.0    0.0]]
   93  [:NB3 0.0  0 0 [0.0000 0.0000 0.0000 0.0000 0.0000] [   0.0   0.0  0.0    0.0    0.0]]
   98  [:NB4 0.0  0 0 [0.0000 0.0000 0.0000 0.0000 0.0000] [   0.0   0.0  0.0    0.0    0.0]]
   99  [:NB5 0.0  0 0 [0.0000 0.0000 0.0000 0.0000 0.0000] [   0.0   0.0  0.0    0.0    0.0]]
   ;; Grass (GR)
   101 [:GR1 0.4 15 8 [0.0046 0.0000 0.0000 0.0138 0.0000] [2200.0 109.0 30.0 2000.0    0.0]]
   102 [:GR2 1.0 15 8 [0.0046 0.0000 0.0000 0.0459 0.0000] [2000.0 109.0 30.0 1800.0    0.0]]
   103 [:GR3 2.0 30 8 [0.0046 0.0184 0.0000 0.0689 0.0000] [1500.0 109.0 30.0 1300.0    0.0]]
   104 [:GR4 2.0 15 8 [0.0115 0.0000 0.0000 0.0872 0.0000] [2000.0 109.0 30.0 1800.0    0.0]]
   105 [:GR5 1.5 40 8 [0.0184 0.0000 0.0000 0.1148 0.0000] [1800.0 109.0 30.0 1600.0    0.0]]
   106 [:GR6 1.5 40 9 [0.0046 0.0000 0.0000 0.1561 0.0000] [2200.0 109.0 30.0 2000.0    0.0]]
   107 [:GR7 3.0 15 8 [0.0459 0.0000 0.0000 0.2479 0.0000] [2000.0 109.0 30.0 1800.0    0.0]]
   108 [:GR8 4.0 30 8 [0.0230 0.0459 0.0000 0.3352 0.0000] [1500.0 109.0 30.0 1300.0    0.0]]
   109 [:GR9 5.0 40 8 [0.0459 0.0459 0.0000 0.4132 0.0000] [1800.0 109.0 30.0 1600.0    0.0]]
   ;; Grass-Shrub (GS)
   121 [:GS1 0.9 15 8 [0.0092 0.0000 0.0000 0.0230 0.0298] [2000.0 109.0 30.0 1800.0 1800.0]]
   122 [:GS2 1.5 15 8 [0.0230 0.0230 0.0000 0.0275 0.0459] [2000.0 109.0 30.0 1800.0 1800.0]]
   123 [:GS3 1.8 40 8 [0.0138 0.0115 0.0000 0.0666 0.0574] [1800.0 109.0 30.0 1600.0 1600.0]]
   124 [:GS4 2.1 40 8 [0.0872 0.0138 0.0046 0.1561 0.3260] [1800.0 109.0 30.0 1600.0 1600.0]]
   ;; Shrub (SH)
   141 [:SH1 1.0 15 8 [0.0115 0.0115 0.0000 0.0069 0.0597] [2000.0 109.0 30.0 1800.0 1600.0]]
   142 [:SH2 1.0 15 8 [0.0620 0.1102 0.0344 0.0000 0.1768] [2000.0 109.0 30.0    0.0 1600.0]]
   143 [:SH3 2.4 40 8 [0.0207 0.1377 0.0000 0.0000 0.2847] [1600.0 109.0 30.0    0.0 1400.0]]
   144 [:SH4 3.0 30 8 [0.0390 0.0528 0.0092 0.0000 0.1171] [2000.0 109.0 30.0 1800.0 1600.0]]
   145 [:SH5 6.0 15 8 [0.1653 0.0964 0.0000 0.0000 0.1331] [ 750.0 109.0 30.0    0.0 1600.0]]
   146 [:SH6 2.0 30 8 [0.1331 0.0666 0.0000 0.0000 0.0643] [ 750.0 109.0 30.0    0.0 1600.0]]
   147 [:SH7 6.0 15 8 [0.1607 0.2433 0.1010 0.0000 0.1561] [ 750.0 109.0 30.0    0.0 1600.0]]
   148 [:SH8 3.0 40 8 [0.0941 0.1561 0.0390 0.0000 0.1997] [ 750.0 109.0 30.0    0.0 1600.0]]
   149 [:SH9 4.4 40 8 [0.2066 0.1125 0.0000 0.0712 0.3214] [ 750.0 109.0 30.0 1800.0 1500.0]]
   ;; Timber-Understory (TU)
   161 [:TU1 0.6 20 8 [0.0092 0.0413 0.0689 0.0092 0.0413] [2000.0 109.0 30.0 1800.0 1600.0]]
   162 [:TU2 1.0 30 8 [0.0436 0.0826 0.0574 0.0000 0.0092] [2000.0 109.0 30.0    0.0 1600.0]]
   163 [:TU3 1.3 30 8 [0.0505 0.0069 0.0115 0.0298 0.0505] [1800.0 109.0 30.0 1600.0 1400.0]]
   164 [:TU4 0.5 12 8 [0.2066 0.0000 0.0000 0.0000 0.0918] [2300.0 109.0 30.0    0.0 2000.0]]
   165 [:TU5 1.0 25 8 [0.1837 0.1837 0.1377 0.0000 0.1377] [1500.0 109.0 30.0    0.0  750.0]]
   ;; Timber Litter (TL)
   181 [:TL1 0.2 30 8 [0.0459 0.1010 0.1653 0.0000 0.0000] [2000.0 109.0 30.0    0.0    0.0]]
   182 [:TL2 0.2 25 8 [0.0643 0.1056 0.1010 0.0000 0.0000] [2000.0 109.0 30.0    0.0    0.0]]
   183 [:TL3 0.3 20 8 [0.0230 0.1010 0.1286 0.0000 0.0000] [2000.0 109.0 30.0    0.0    0.0]]
   184 [:TL4 0.4 25 8 [0.0230 0.0689 0.1928 0.0000 0.0000] [2000.0 109.0 30.0    0.0    0.0]]
   185 [:TL5 0.6 25 8 [0.0528 0.1148 0.2020 0.0000 0.0000] [2000.0 109.0 30.0    0.0 1600.0]]
   186 [:TL6 0.3 25 8 [0.1102 0.0551 0.0551 0.0000 0.0000] [2000.0 109.0 30.0    0.0    0.0]]
   187 [:TL7 0.4 25 8 [0.0138 0.0643 0.3719 0.0000 0.0000] [2000.0 109.0 30.0    0.0    0.0]]
   188 [:TL8 0.3 35 8 [0.2663 0.0643 0.0505 0.0000 0.0000] [1800.0 109.0 30.0    0.0    0.0]]
   189 [:TL9 0.6 35 8 [0.3053 0.1515 0.1905 0.0000 0.0000] [1800.0 109.0 30.0    0.0 1600.0]]
   ;; Slash-Blowdown (SB)
   201 [:SB1 1.0 25 8 [0.0689 0.1377 0.5051 0.0000 0.0000] [2000.0 109.0 30.0    0.0    0.0]]
   202 [:SB2 1.0 25 8 [0.2066 0.1951 0.1837 0.0000 0.0000] [2000.0 109.0 30.0    0.0    0.0]]
   203 [:SB3 1.2 25 8 [0.2525 0.1263 0.1377 0.0000 0.0000] [2000.0 109.0 30.0    0.0    0.0]]
   204 [:SB4 2.7 25 8 [0.2410 0.1607 0.2410 0.0000 0.0000] [2000.0 109.0 30.0    0.0    0.0]]
   })

(defrecord FuelModel
    [name
     ^long number
     ^double delta
     M_x
     w_o
     sigma
     h
     rho_p
     S_T
     S_e
     M_f
     f_ij
     f_i
     g_ij])

(defn compute-fuel-model
  [^long fuel-model-number]
  (let [[name delta M_x-dead h
         [w_o-dead-1hr w_o-dead-10hr w_o-dead-100hr
          w_o-live-herbaceous w_o-live-woody]
         [sigma-dead-1hr sigma-dead-10hr sigma-dead-100hr
          sigma-live-herbaceous sigma-live-woody]]
        (fuel-models fuel-model-number)
        M_x-dead   (* ^long M_x-dead 0.01)
        h          (* ^long h 1000.0)
        fuel-model {:name   name
                    :number fuel-model-number
                    :delta  delta
                    :M_x    [M_x-dead M_x-dead M_x-dead 0.0 0.0 0.0]
                    :w_o    [w_o-dead-1hr w_o-dead-10hr w_o-dead-100hr 0.0 w_o-live-herbaceous w_o-live-woody]
                    :sigma  [sigma-dead-1hr sigma-dead-10hr sigma-dead-100hr 0.0 sigma-live-herbaceous sigma-live-woody]
                    :h      [h h h h h h]
                    :rho_p  [32.0 32.0 32.0 32.0 32.0 32.0]
                    :S_T    [0.0555 0.0555 0.0555 0.0555 0.0555 0.0555]
                    :S_e    [0.01 0.01 0.01 0.01 0.01 0.01]}]
    (if (and (> fuel-model-number 100)
             (pos? ^double w_o-live-herbaceous))
      ;; Set dead-herbaceous values
      (-> fuel-model
          (assoc-in [:M_x   3] M_x-dead)
          (assoc-in [:sigma 3] sigma-live-herbaceous))
      ;; No dead-herbaceous values
      fuel-model)))

(def fuel-models-precomputed (into {} (map #(vector % (map->FuelModel (compute-fuel-model %))) (keys fuel-models))))
;; fuel-model-definitions ends here

;; [[file:../../org/GridFire.org::fuel-category-and-size-class-functions][fuel-category-and-size-class-functions]]
(defn map-category [f]
  [(f 0) (f 1)])

(defn map-size-class [f]
  [(f 0) (f 1) (f 2) (f 3) (f 4) (f 5)])

(defn category-sum ^double [f]
  (+ ^double (f 0) ^double (f 1)))

(defn size-class-sum [f]
  [(+ (+ ^double (f 0) ^double (f 1))
      (+ ^double (f 2) ^double (f 3)))
   (+ ^double (f 4) ^double (f 5))])
;; fuel-category-and-size-class-functions ends here

;; [[file:../../org/GridFire.org::add-dynamic-fuel-loading][add-dynamic-fuel-loading]]
(defn add-dynamic-fuel-loading
  [fuel-model M_f]
  (let [^long number                 (:number fuel-model)
        w_o                          (:w_o fuel-model)
        ^double live-herbaceous-load (w_o 4)] ; 4 = live-herbaceous
    (if (and (> number 100) (pos? live-herbaceous-load))
      ;; dynamic fuel model
      (let [name           (:name fuel-model)
            delta          (:delta fuel-model)
            M_x            (:M_x fuel-model)
            sigma          (:sigma fuel-model)
            h              (:h fuel-model)
            rho_p          (:rho_p fuel-model)
            S_T            (:S_T fuel-model)
            S_e            (:S_e fuel-model)
            fraction-green (max 0.0 (min 1.0 (- (/ ^double (M_f 4) 0.9) 0.3333333333333333))) ; 4 = live-herbaceous
            fraction-cured (- 1.0 fraction-green)
            dynamic-M_f    (assoc M_f 3 (M_f 0)) ; 0 = dead-1hr, 3 = dead-herbaceous
            dynamic-w_o    [(w_o 0)
                            (w_o 1)
                            (w_o 2)
                            (* live-herbaceous-load fraction-cured)
                            (* live-herbaceous-load fraction-green)
                            (w_o 5)]]
        (->FuelModel name number delta M_x dynamic-w_o sigma h rho_p S_T S_e dynamic-M_f nil nil nil))
      ;; static fuel model
      (assoc fuel-model :M_f M_f))))
;; add-dynamic-fuel-loading ends here

;; [[file:../../org/GridFire.org::add-weighting-factors][add-weighting-factors]]
(defn add-weighting-factors
  [fuel-model]
  (let [name                 (:name fuel-model)
        number               (:number fuel-model)
        delta                (:delta fuel-model)
        M_x                  (:M_x fuel-model)
        w_o                  (:w_o fuel-model)
        sigma                (:sigma fuel-model)
        h                    (:h fuel-model)
        rho_p                (:rho_p fuel-model)
        S_T                  (:S_T fuel-model)
        S_e                  (:S_e fuel-model)
        M_f                  (:M_f fuel-model)
        A_ij                 (map-size-class (fn ^double [i]
                                               (/ (* ^double (sigma i) ^double (w_o i))
                                                  ^double (rho_p i))))

        A_i                  (size-class-sum (fn ^double [i] ^double (A_ij i)))

        A_T                  (category-sum   (fn ^double [i] ^double (A_i i)))

        f_ij                 (map-size-class (fn ^double [^long i]
                                               (let [^double A (A_i (quot i 4))]
                                                 (if (pos? A)
                                                   (/ ^double (A_ij i) A)
                                                   0.0))))

        f_i                  (map-category   (fn ^double [i]
                                               (if (pos? A_T)
                                                 (/ ^double (A_i i) A_T)
                                                 0.0)))

        firemod-size-classes (map-size-class (fn ^long [i]
                                               (let [^double s (sigma i)]
                                                 (if (>= s 1200.0)
                                                   1
                                                   (if (>= s 192.0)
                                                     2
                                                     (if (>= s 96.0)
                                                       3
                                                       (if (>= s 48.0)
                                                         4
                                                         (if (>= s 16.0)
                                                           5
                                                           6))))))))

        g_ij                 (map-size-class (fn ^double [^long i]
                                               (let [c (firemod-size-classes i)]
                                                 (if (< i 4)
                                                   (+ (+ (if (= c (firemod-size-classes 0)) ^double (f_ij 0) 0.0)
                                                         (if (= c (firemod-size-classes 1)) ^double (f_ij 1) 0.0))
                                                      (+ (if (= c (firemod-size-classes 2)) ^double (f_ij 2) 0.0)
                                                         (if (= c (firemod-size-classes 3)) ^double (f_ij 3) 0.0)))
                                                   (+ (if (= c (firemod-size-classes 4)) ^double (f_ij 4) 0.0)
                                                      (if (= c (firemod-size-classes 5)) ^double (f_ij 5) 0.0))))))]
    (->FuelModel name number delta M_x w_o sigma h rho_p S_T S_e M_f f_ij f_i g_ij)))
;; add-weighting-factors ends here

;; [[file:../../org/GridFire.org::add-live-moisture-of-extinction][add-live-moisture-of-extinction]]
(defn add-live-moisture-of-extinction
  "Equation 88 from Rothermel 1972 adjusted by Albini 1976 Appendix III."
  [fuel-model]
  (let [w_o                            (:w_o fuel-model)
        sigma                          (:sigma fuel-model)
        M_f                            (:M_f fuel-model)
        M_x                            (:M_x fuel-model)
        loading-factors                (map-size-class (fn ^double [^long i]
                                                         (let [^double sigma_ij (sigma i)
                                                               A (if (< i 4) -138.0 -500.0)]
                                                           (if (pos? sigma_ij)
                                                             (* ^double (w_o i)
                                                                (Math/exp (/ A sigma_ij)))
                                                             0.0))))
        [^double dead-loading-factor
         ^double live-loading-factor]  (size-class-sum (fn ^double [i] ^double (loading-factors i)))
        [^double dead-moisture-factor] (size-class-sum (fn ^double [i]
                                                         (* ^double (M_f i)
                                                            ^double (loading-factors i))))
        ^double dead-to-live-ratio     (when (pos? live-loading-factor)
                                         (/ dead-loading-factor live-loading-factor))
        dead-fuel-moisture             (if (pos? dead-loading-factor)
                                         (/ dead-moisture-factor dead-loading-factor)
                                         0.0)
        ^double M_x-dead               (M_x 0)
        M_x-live                       (if (pos? live-loading-factor)
                                         (max M_x-dead
                                              (- (-> 2.9
                                                     (* dead-to-live-ratio)
                                                     (* (- 1.0 (/ dead-fuel-moisture M_x-dead))))
                                                 0.226))
                                         M_x-dead)]
    (assoc fuel-model :M_x [(M_x 0)
                            (M_x 1)
                            (M_x 2)
                            (M_x 3)
                            M_x-live
                            M_x-live])))

(defn moisturize
  [fuel-model fuel-moisture]
  (-> fuel-model
      (add-dynamic-fuel-loading fuel-moisture)
      (add-weighting-factors)
      (add-live-moisture-of-extinction)))
;; add-live-moisture-of-extinction ends here
