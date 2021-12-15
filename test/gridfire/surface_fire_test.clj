(ns gridfire.surface-fire-test
  (:require [clojure.test                :refer [deftest testing is run-tests]]
            [gridfire.fuel-models        :refer [fuel-models build-fuel-model moisturize]]
            [gridfire.surface-fire       :refer [anderson-flame-depth
                                                 grass-fuel-model?
                                                 byram-fire-line-intensity
                                                 byram-flame-length
                                                 rothermel-surface-fire-spread-no-wind-no-slope
                                                 rothermel-surface-fire-spread-max
                                                 wind-adjustment-factor
                                                 wind-adjustment-factor-elmfire]]
            [gridfire.conversion         :refer [ft->m mph->fpm]]
            [gridfire.behaveplus-results :refer [behaveplus5-surface-fire-values-dry-no-wind-no-slope
                                                 behaveplus5-surface-fire-values-mid-no-wind-no-slope
                                                 behaveplus5-surface-fire-values-mid-with-wind-and-slope
                                                 behaveplus5-surface-fire-values-mid-with-cross-wind-and-slope-90-180
                                                 behaveplus5-surface-fire-values-mid-with-cross-wind-and-slope-135-180
                                                 sb40-fuel-models
                                                 test-fuel-moisture
                                                 within]]))

;; Tests fuel model weighting factors, rothermel equations, and byram's flame length and fire line intensity under fully cured conditions
(deftest ^:unit rothermel-surface-fire-spread-no-wind-no-slope-test-dry
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
(deftest ^:unit rothermel-surface-fire-spread-no-wind-no-slope-test-mid
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
(deftest ^:unit rothermel-surface-fire-spread-with-wind-and-slope-test-mid
  (doseq [num sb40-fuel-models]
    (let [gridfire-fuel-model                                     (moisturize (build-fuel-model num) (test-fuel-moisture :mid))
          {:keys [spread-rate reaction-intensity residence-time
                  get-phi_W get-phi_S] :as spread-info-min}       (rothermel-surface-fire-spread-no-wind-no-slope gridfire-fuel-model)
          corrected-spread-rate                                   (if (grass-fuel-model? num) (* 2.0 spread-rate) spread-rate)
          midflame-wind-speed                                     (mph->fpm 10.0)
          slope                                                   0.2 ;; vertical feet/horizontal feet
          {:keys [max-spread-rate max-spread-direction
                  effective-wind-speed eccentricity]}             (rothermel-surface-fire-spread-max (assoc spread-info-min :spread-rate corrected-spread-rate)
                                                                                                     midflame-wind-speed 0.0 slope 0.0 1.0)
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

;; Tests fuel model weighting factors, rothermel equations, and
;; byram's flame length and fire line intensity under 50% cured
;; conditions with 10mph wind speed and 20% slope. Also tests max
;; spread direction and effective wind speed for cross-slope winds.
(deftest ^:unit rothermel-surface-fire-spread-with-cross-wind-and-slope-test-mid-90-180
  (doseq [num sb40-fuel-models]
    (let [gridfire-fuel-model                                     (moisturize (build-fuel-model num) (test-fuel-moisture :mid))
          {:keys [spread-rate reaction-intensity residence-time
                  get-phi_W get-phi_S] :as spread-info-min}       (rothermel-surface-fire-spread-no-wind-no-slope gridfire-fuel-model)
          corrected-spread-rate                                   (if (grass-fuel-model? num) (* 2.0 spread-rate) spread-rate)
          midflame-wind-speed                                     (mph->fpm 10.0)
          slope                                                   0.2 ;; vertical feet/horizontal feet
          {:keys [max-spread-rate max-spread-direction
                  effective-wind-speed eccentricity]}             (rothermel-surface-fire-spread-max (assoc spread-info-min :spread-rate corrected-spread-rate)
                                                                                                     midflame-wind-speed 90.0 slope 180.0 1.0)
          max-spread-rate-ch-per-hr                               (/ max-spread-rate 1.1)
          effective-wind-speed-mph                                (/ effective-wind-speed 88.0)
          fire-line-intensity                                     (->> (anderson-flame-depth max-spread-rate residence-time)
                                                                       (byram-fire-line-intensity reaction-intensity))
          flame-length                                            (byram-flame-length fire-line-intensity)
          [ros_max fli fl max_theta eff_wsp]                      (behaveplus5-surface-fire-values-mid-with-cross-wind-and-slope-90-180 (:name gridfire-fuel-model))]
      (is (within ros_max max-spread-rate-ch-per-hr 0.3))
      (is (within fli fire-line-intensity 25.1))
      (is (within fl flame-length 0.06))
      (is (within max_theta max-spread-direction 0.5))
      (is (within eff_wsp effective-wind-speed-mph 0.1)))))

;; Tests fuel model weighting factors, rothermel equations, and
;; byram's flame length and fire line intensity under 50% cured
;; conditions with 10mph wind speed and 20% slope. Also tests max
;; spread direction and effective wind speed for cross-slope winds.
(deftest ^:unit rothermel-surface-fire-spread-with-cross-wind-and-slope-test-mid-135-180
  (doseq [num sb40-fuel-models]
    (let [gridfire-fuel-model                                     (moisturize (build-fuel-model num) (test-fuel-moisture :mid))
          {:keys [spread-rate reaction-intensity residence-time
                  get-phi_W get-phi_S] :as spread-info-min}       (rothermel-surface-fire-spread-no-wind-no-slope gridfire-fuel-model)
          corrected-spread-rate                                   (if (grass-fuel-model? num) (* 2.0 spread-rate) spread-rate)
          midflame-wind-speed                                     (mph->fpm 10.0)
          slope                                                   0.2 ;; vertical feet/horizontal feet
          {:keys [max-spread-rate max-spread-direction
                  effective-wind-speed eccentricity]}             (rothermel-surface-fire-spread-max (assoc spread-info-min :spread-rate corrected-spread-rate)
                                                                                                     midflame-wind-speed 135.0 slope 180.0 1.0)
          max-spread-rate-ch-per-hr                               (/ max-spread-rate 1.1)
          effective-wind-speed-mph                                (/ effective-wind-speed 88.0)
          fire-line-intensity                                     (->> (anderson-flame-depth max-spread-rate residence-time)
                                                                       (byram-fire-line-intensity reaction-intensity))
          flame-length                                            (byram-flame-length fire-line-intensity)
          [ros_max fli fl max_theta eff_wsp lw]                   (behaveplus5-surface-fire-values-mid-with-cross-wind-and-slope-135-180 (:name gridfire-fuel-model))
          behaveplus-eccentricity                                 (/ (Math/sqrt (- (Math/pow lw 2.0) 1.0)) lw)]
      (is (within ros_max max-spread-rate-ch-per-hr 1.7))
      (is (within fli fire-line-intensity 25.1))
      (is (within fl flame-length 0.06))
      (is (within max_theta max-spread-direction 0.5))
      (is (within eff_wsp effective-wind-speed-mph 0.1))
      (is (within behaveplus-eccentricity eccentricity 0.01)))))

(deftest ^:unit wind-adjustment-factor-test
  (doseq [fuel-bed-depth (map second (vals (select-keys fuel-models sb40-fuel-models)))] ;; ft
    (doseq [canopy-height (range 0 121 10)] ;; ft
      (doseq [canopy-cover (range 0 101 10)] ;; %
        (is (within (wind-adjustment-factor fuel-bed-depth canopy-height canopy-cover)
                    (wind-adjustment-factor-elmfire fuel-bed-depth (ft->m canopy-height) (* 0.01 canopy-cover))
                    0.001))))))

;; TODO: Add R_theta test

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
