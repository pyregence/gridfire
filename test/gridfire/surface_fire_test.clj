(ns gridfire.surface-fire-test
  (:require [clojure.test :refer :all]
            [gridfire.fuel-models :refer [build-fuel-model moisturize]]
            [gridfire.surface-fire :refer :all]
            [gridfire.behaveplus-results :refer :all]))

(deftest rothermel-surface-fire-spread-no-wind-no-slope-test
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
