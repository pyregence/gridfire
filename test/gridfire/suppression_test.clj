;; FIXME LP coverage
(ns gridfire.suppression-test
  (:require
   [clojure.test :refer [are deftest testing is]]
   [gridfire.suppression :as su]))


(deftest ^:unit angle-cw-from-east-test
  (are [i j expected] (= (#'gridfire.suppression/angle-cw-from-east i j 10 10) expected)
    10 11 0.0     ;E
    11 11 45.0    ;SE
    11 10 90.0    ;S
    11 9  135.0   ;SW
    10 9  180.0   ;W
    9  9  225.0   ;NW
    9  10 270.0   ;N
    9  11 315.0)) ;NE

(deftest ^:unit nearest-floor-test
  (let [slice-size 5]
    (are [degree expected-bin] (= (#'gridfire.suppression/nearest-angular-slice degree slice-size) expected-bin)
      0.0  0.0
      2.5  0.0
      5.0  1.0
      7.5  1.0
      10.0 2.0
      12.5 2.0
      15.0 3.0)))

(deftest ^:unit combine-remove-average-test
  (let [[avg-old count-old] [100.0 10]
        [avg-new count-new] [75.0  5]]
    (is (= avg-old
           (-> (#'gridfire.suppression/combine-average avg-old count-old avg-new count-new)
               (#'gridfire.suppression/remove-average (+ count-old count-new) avg-new count-new)))
        "should get the original average if adding and then removing the same average.")))

(deftest ^:unit compute-contiguous-slices-test
  (testing "simple case"
    (let [num-cells-to-suppress 100
          avg-dsr-data          {0.0 [2.0 25]
                                 1.0 [2.0 25]
                                 2.0 [2.0 25]
                                 3.0 [2.0 25]}]

      (is (= {['(2.0 1.0 0.0 3.0) 2.0] 100}
             (#'gridfire.suppression/compute-contiguous-slices num-cells-to-suppress avg-dsr-data)))))

  (testing "segments sorted by average spread-rate"
    (let [num-cells-to-suppress 6
          avg-dsr-data          {0.0 [6.0 2]
                                 1.0 [5.0 2]
                                 2.0 [4.0 2]
                                 3.0 [3.0 2]
                                 4.0 [2.0 2]
                                 5.0 [1.0 2]}]
      (is (= {['(5.0 4.0 3.0) 2.0] 6
              ['(0.0 5.0 4.0) 3.0] 6
              ['(1.0 0.0 5.0) 4.0] 6
              ['(2.0 1.0 0.0) 5.0] 6}
             (#'gridfire.suppression/compute-contiguous-slices num-cells-to-suppress avg-dsr-data)))))

  (testing "lowest averge spread rate segments span over bin 0.0"
    (let [num-cells-to-suppress 6
          avg-dsr-data          {0.0 [2.0 2]
                                 1.0 [1.0 2]
                                 2.0 [6.0 2]
                                 3.0 [5.0 2]
                                 4.0 [4.0 2]
                                 5.0 [3.0 2]}]
      (is (= {['(1.0 0.0 5.0) 2.0] 6
              ['(0.0 5.0 4.0) 3.0] 6
              ['(5.0 4.0 3.0) 4.0] 6
              ['(4.0 3.0 2.0) 5.0] 6}
             (#'gridfire.suppression/compute-contiguous-slices num-cells-to-suppress avg-dsr-data))))))
