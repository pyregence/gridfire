(ns gridfire.surface-fire-test
  (:require [clojure.test :refer :all]
            [gridfire.fuel-models :refer [fuel-models build-fuel-model moisturize]]
            [gridfire.surface-fire :refer :all]
            [gridfire.crown-fire :refer [ft->m]]
            [gridfire.behaveplus-results :refer :all]))

;; Tests fuel model weighting factors, rothermel equations, and byram's flame length and fire line intensity under fully cured conditions
(deftest rothermel-surface-fire-spread-no-wind-no-slope-test-dry
  (doseq [num sb40-fuel-models]
    (let [gridfire-fuel-model                                     (moisturize (build-fuel-model num) (test-fuel-moisture :dry))
          {:keys [spread-rate reaction-intensity residence-time]} (rothermel-surface-fire-spread-no-wind-no-slope gridfire-fuel-model)
          corrected-spread-rate                                   (if (grass-fuel-model? num) (* 2.0 spread-rate) spread-rate)
          corrected-spread-rate-ch-per-hr                         (/ corrected-spread-rate 1.1)
          fire-line-intensity                                     (->> (anderson-flame-depth corrected-spread-rate residence-time)
                                                                       (byram-fire-line-intensity reaction-intensity))
          flame-length                                            (byram-flame-length fire-line-intensity)
          [ros_max fli fl ri _ rt _]                              (behaveplus5-surface-fire-values-dry-no-wind-no-slope (:name gridfire-fuel-model))]
      (is (within ros_max corrected-spread-rate-ch-per-hr 0.1))
      (is (within ri reaction-intensity 12.0))
      (is (within rt residence-time 0.01))
      (is (within fli fire-line-intensity 0.5))
      (is (within fl flame-length 0.05)))))

;; Tests fuel model weighting factors, rothermel equations, and byram's flame length and fire line intensity under 50% cured conditions
(deftest rothermel-surface-fire-spread-no-wind-no-slope-test-mid
  (doseq [num sb40-fuel-models]
    (let [gridfire-fuel-model                                     (moisturize (build-fuel-model num) (test-fuel-moisture :mid))
          {:keys [spread-rate reaction-intensity residence-time]} (rothermel-surface-fire-spread-no-wind-no-slope gridfire-fuel-model)
          corrected-spread-rate                                   (if (grass-fuel-model? num) (* 2.0 spread-rate) spread-rate)
          corrected-spread-rate-ch-per-hr                         (/ corrected-spread-rate 1.1)
          fire-line-intensity                                     (->> (anderson-flame-depth corrected-spread-rate residence-time)
                                                                       (byram-fire-line-intensity reaction-intensity))
          flame-length                                            (byram-flame-length fire-line-intensity)
          [ros_max fli fl ri _ rt _]                              (behaveplus5-surface-fire-values-mid-no-wind-no-slope (:name gridfire-fuel-model))]
      (is (within ros_max corrected-spread-rate-ch-per-hr 0.1))
      (is (within ri reaction-intensity 12.0))
      (is (within rt residence-time 0.01))
      (is (within fli fire-line-intensity 0.85))
      (is (within fl flame-length 0.05)))))

;; Tests fuel model weighting factors, rothermel equations, and
;; byram's flame length and fire line intensity under 50% cured
;; conditions with 10mph wind speed and 20% slope. Also tests phi_W
;; and phi_S from the rothermel equations.
(deftest rothermel-surface-fire-spread-with-wind-and-slope-test-mid
  (doseq [num sb40-fuel-models]
    (let [gridfire-fuel-model                                     (moisturize (build-fuel-model num) (test-fuel-moisture :mid))
          {:keys [spread-rate reaction-intensity residence-time
                  get-phi_W get-phi_S] :as spread-info-min}       (rothermel-surface-fire-spread-no-wind-no-slope gridfire-fuel-model)
          corrected-spread-rate                                   (if (grass-fuel-model? num) (* 2.0 spread-rate) spread-rate)
          midflame-wind-speed                                     (* 10.0 88.0) ;; mi/hr -> ft/min
          slope                                                   0.2 ;; vertical feet/horizontal feet
          {:keys [max-spread-rate max-spread-direction
                  effective-wind-speed eccentricity]}             (rothermel-surface-fire-spread-max (assoc spread-info-min :spread-rate corrected-spread-rate)
                                                                                                     midflame-wind-speed 0 slope 0 1.0)
          max-spread-rate-ch-per-hr                               (/ max-spread-rate 1.1)
          phi_W                                                   (get-phi_W midflame-wind-speed)
          phi_S                                                   (get-phi_S slope)
          fire-line-intensity                                     (->> (anderson-flame-depth max-spread-rate residence-time)
                                                                       (byram-fire-line-intensity reaction-intensity))
          flame-length                                            (byram-flame-length fire-line-intensity)
          [ros_max fli fl ri _ rt _ pW pS]                        (behaveplus5-surface-fire-values-mid-with-wind-and-slope (:name gridfire-fuel-model))]
      (is (within ros_max max-spread-rate-ch-per-hr 0.3))
      (is (within ri reaction-intensity 12.0))
      (is (within rt residence-time 0.01))
      (is (within fli fire-line-intensity 25.1))
      (is (within fl flame-length 0.05))
      (is (within pW phi_W 0.1))
      (is (within pS phi_S 0.05)))))

(deftest wind-adjustment-factor-test
  (doseq [fuel-bed-depth (map second (vals (select-keys fuel-models sb40-fuel-models)))] ;; ft
    (doseq [canopy-height (range 0 121 10)] ;; ft
      (doseq [canopy-cover (range 0 101 10)] ;; %
        (is (within (wind-adjustment-factor fuel-bed-depth canopy-height canopy-cover)
                    (wind-adjustment-factor-elmfire fuel-bed-depth (ft->m canopy-height) (* 0.01 canopy-cover))
                    0.001))))))

;; TODO: Fix phi_W discrepancy on 5 marked fuel models.
;; TODO: Add L/W and eccentricity tests, R_theta test, cross-wind-slope test.

(comment
  (run-tests)

  (testing "Arithmetic"
    (testing "with positive integers"
      (is (= 4 (+ 2 2)))
      (is (= 7 (+ 3 4))))
    (testing "with negative integers"
      (is (= -4 (+ -2 -2)))
      (is (= -1 (+ 3 -4)))))

  (deftest addition
    (is (= 4 (+ 2 2)) "Addition works")
    (is (= 7 (+ 3 4)) "Yep, still works"))

  (deftest subtraction
    (is (= 1 (- 4 3)))
    (is (= 3 (- 7 4))))

  (deftest arithmetic
    (addition)
    (subtraction))

  (defn test-ns-hook []
    (arithmetic))
  )
