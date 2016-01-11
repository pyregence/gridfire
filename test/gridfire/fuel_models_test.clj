(ns gridfire.fuel-models-test
  (:require [clojure.test :refer :all]
            [gridfire.fuel-models :refer :all]
            [gridfire.behaveplus-results :refer :all]))

(deftest moisturize-test
  (doseq [num sb40-fuel-models]
    (let [gridfire-fuel-model-dry   (build-fuel-model num)
          gridfire-fuel-model-wet   (moisturize gridfire-fuel-model-dry (test-fuel-moisture :dry))
          gridfire-M_x-live         (Math/round (* 100.0 (-> gridfire-fuel-model-wet :M_x :live :herbaceous)))
          gridfire-fraction-cured   (if (pos? (-> gridfire-fuel-model-dry :w_o :live :herbaceous))
                                      (Math/round
                                       (* 100.0 (/ (-> gridfire-fuel-model-wet :w_o :dead :herbaceous)
                                                   (-> gridfire-fuel-model-dry :w_o :live :herbaceous))))
                                      0)
          behaveplus-outputs        (-> (:name gridfire-fuel-model-dry)
                                        (behaveplus5-surface-fire-values-dry-no-wind-no-slope))
          behaveplus-M_x-live       (behaveplus-outputs 4)
          behaveplus-fraction-cured (behaveplus-outputs 6)]
      (is (<= (Math/abs ^double (- gridfire-M_x-live behaveplus-M_x-live)) 6)) ;; all are within 1% except TU2 at -6%
      (is (= gridfire-fraction-cured behaveplus-fraction-cured)))))

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
