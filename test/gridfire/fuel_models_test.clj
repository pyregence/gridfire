(ns gridfire.fuel-models-test
  (:require [clojure.test :refer :all]
            [gridfire.fuel-models :refer :all]))

(def test-fuel-moisture
  {:dry {:dead {:1hr 0.04 :10hr 0.04 :100hr 0.06}
         :live {:herbaceous 0.30 :woody 0.70}}
   :mid {:dead {:1hr 0.04 :10hr 0.04 :100hr 0.06}
         :live {:herbaceous 0.75 :woody 0.70}}
   :wet {:dead {:1hr 0.04 :10hr 0.04 :100hr 0.06}
         :live {:herbaceous 1.20 :woody 0.70}}})

;; INPUTS TO BEHAVEPLUS5:
;; - fuel-moisture = (test-fuel-moisture :dry)
;; - midflame wind speed (mph) = 0
;; - slope steepness (%) = 0
;;
;; OUTPUTS LISTED IN TABLE BELOW:
;; - ROS_max (ch/h)
;; - Fireline Intensity (Btu/ft/s)
;; - Flame Length (ft)
;; - Reaction Intensity (Btu/ft2/min)
;; - Live Fuel Moisture of Extinction (%)
;; - Residence Time (min)
;; - Fuel Load Transferred (%)
(def behaveplus5-surface-fire-values-dry-no-wind-no-slope
  {:GR1 [1.1  2   0.6  430   15   0.19 100]
   :GR2 [2.3  10  1.3  1135  15   0.21 100]
   :GR3 [2.8  22  1.9  1487  30   0.30 100]
   :GR4 [4.7  40  2.4  2209  15   0.21 100]
   :GR5 [4.8  88  3.5  4260  40   0.24 100]
   :GR6 [7.2  164 4.7  6520  40   0.19 100]
   :GR7 [8.7  258 5.8  7770  15   0.21 100]
   :GR8 [8.6  449 7.5  9700  30   0.29 100]
   :GR9 [14.9 977 10.7 14978 40   0.24 100]
   :GS1 [1.1  6   1.0  1512  258  0.21 100]
   :GS2 [1.6  15  1.6  2467  304  0.21 100]
   :GS3 [2.4  44  2.6  4110  456  0.24 100]
   :GS4 [3.2  245 5.7  17939 227  0.24 100]
   :SH1 [0.4  2   0.6  966   76   0.23 100]
   :SH2 [0.8  20  1.8  6083  124  0.23 0]
   :SH3 [0.3  3   0.8  1981  53   0.28 0]
   :SH4 [1.8  28  2.1  3610  128  0.23 0]
   :SH5 [2.9  93  3.6  5720  337  0.31 0]
   :SH6 [1.6  51  2.8  5375  670  0.34 0]
   :SH7 [2.4  104 3.8  7668  356  0.31 0]
   :SH8 [1.7  73  3.2  8462  197  0.28 0]
   :SH9 [3.1  215 5.3  13425 283  0.28 100]
   :TU1 [0.3  2   0.7  1821  202  0.24 100]
   :TU2 [1.0  8   1.2  2059  2392 0.22 0]
   :TU3 [2.0  36  2.3  4106  519  0.24 100]
   :TU4 [1.4  31  2.2  6882  503  0.17 0]
   :TU5 [1.2  62  3.0  9075  737  0.31 0]
   :TL1 [0.1  0   0.3  527   30   0.22 0]
   :TL2 [0.2  1   0.4  769   25   0.21 0]
   :TL3 [0.2  1   0.4  881   20   0.25 0]
   :TL4 [0.3  2   0.5  1101  25   0.24 0]
   :TL5 [0.5  4   0.8  1751  25   0.22 0]
   :TL6 [0.7  7   1.1  2500  25   0.20 0]
   :TL7 [0.4  4   0.9  1779  25   0.31 0]
   :TL8 [0.9  13  1.5  3617  35   0.22 0]
   :TL9 [1.3  27  2.1  5243  35   0.22 0]
   :SB1 [0.8  9   1.3  2753  25   0.23 0]
   :SB2 [1.7  36  2.3  5547  25   0.20 0]
   :SB3 [2.8  79  3.4  7633  25   0.20 0]
   :SB4 [4.5  134 4.3  8049  25   0.20 0]})

(def sb40-fuel-models (filterv #(> % 100) (sort (keys fuel-models))))

(deftest moisturize-test
  (doseq [num sb40-fuel-models]
    (let [gridfire-fuel-model-dry   (build-fuel-model num)
          gridfire-fuel-model-wet   (moisturize gridfire-fuel-model-dry (test-fuel-moisture :dry))
          gridfire-M_x-live         (Math/round (* 100 (-> gridfire-fuel-model-wet :M_x :live :herbaceous)))
          gridfire-fraction-cured   (if (pos? (-> gridfire-fuel-model-dry :w_o :live :herbaceous))
                                      (Math/round
                                       (* 100 (/ (-> gridfire-fuel-model-wet :w_o :dead :herbaceous)
                                                 (-> gridfire-fuel-model-dry :w_o :live :herbaceous))))
                                      0)
          behaveplus-outputs        (-> (:name gridfire-fuel-model-dry)
                                        (behaveplus5-surface-fire-values-dry-no-wind-no-slope))
          behaveplus-M_x-live       (behaveplus-outputs 4)
          behaveplus-fraction-cured (behaveplus-outputs 6)]
      (is (<= (Math/abs (- gridfire-M_x-live behaveplus-M_x-live)) 6)) ;; all are within 1% except TU2 at -6%
      (is (= gridfire-fraction-cured behaveplus-fraction-cured)))))

;; FIXME: NEED TO RUN THIS THROUGH BEHAVEPLUS AND MATCH FORMAT TO DEFINITION ABOVE
;; fuel-moisture = (test-fuel-moisture :dry)
;; midflame wind speed (mph) = 5 (440 ft/min)
;; slope steepness (%) = 0.1
(def behaveplus5-surface-fire-values-dry-with-wind-and-slope
  {1 [1 108.8 5.0]
   2 [2 50.6 7.8]
   3 [3 144.5 15.6]
   4 [4 109.2 24.5]
   5 [5 36.1 7.8]
   6 [6 41.7 7.1]
   7 [7 38.0 7.1]
   8 [8 2.5 1.3]
   9 [9 10.8 3.4]
   10 [10 12.0 6.4]
   11 [11 7.3 3.9]
   12 [12 16.4 9.4]
   13 [13 19.9 12.4]
   91 [91 0.0 0.0]
   92 [92 0.0 0.0]
   93 [93 0.0 0.0]
   98 [98 0.0 0.0]
   99 [99 0.0 0.0]
   101 [101 21.7 2.3]
   102 [102 54.8 5.8]
   103 [103 77.3 8.8]
   104 [104 110.3 10.8]
   105 [105 92.7 13.9]
   106 [106 121.1 17.4]
   107 [107 161.5 22.8]
   108 [108 169.6 29.9]
   109 [109 274.5 41.2]
   121 [121 23.0 4.2]
   122 [122 32.3 6.2]
   123 [123 50.1 10.1]
   124 [124 42.3 18.4]
   141 [141 5.4 1.5]
   142 [142 9.8 5.7]
   143 [143 5.4 2.9]
   144 [144 42.0 8.8]
   145 [145 77.3 16.4]
   146 [146 31.6 11.0]
   147 [147 51.0 15.6]
   148 [148 29.8 12.1]
   149 [149 55.2 20.0]
   161 [161 4.2 2.2]
   162 [162 15.5 4.2]
   163 [163 37.6 9.0]
   164 [164 16.0 6.7]
   165 [165 12.0 8.7]
   181 [181 1.1 0.7]
   182 [182 1.7 1.0]
   183 [183 2.1 1.2]
   184 [184 3.1 1.6]
   185 [185 5.6 2.5]
   186 [186 7.4 3.1]
   187 [187 3.6 2.3]
   188 [188 7.4 3.9]
   189 [189 11.1 5.6]
   201 [201 7.8 3.6]
   202 [202 18.9 7.0]
   203 [203 34.1 10.5]
   204 [204 65.6 14.7]})

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
