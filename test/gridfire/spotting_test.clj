(ns gridfire.spotting-test
  (:require [clojure.test      :refer [deftest is testing]]
            [gridfire.spotting :as spotting])
  (:import java.util.Random))

(defn close-to-zero [d]
  (< -0.000001 d 0.000001))

(deftest deltas-axis-test
  (testing "x-axis"
    (testing "east"
      (let [H (spotting/hypotenuse 10 10)]

        (let [[dx dy] (first (spotting/deltas-wind->coord [[10 10]] 45.0))]

          (is (= H dy))

          (is (close-to-zero dx)))

        (let [[dx dy] (first (spotting/deltas-wind->coord [[10 -10]] 135.0))]

          (is (= H dy))

          (is (close-to-zero dx)))))

    (testing "west"
      (let [H (spotting/hypotenuse 10 10)]

        (let [[dx dy] (first (spotting/deltas-wind->coord [[10 10]] 225.0))]

          (is (= (- H) dy))

          (is (close-to-zero dx)))

        (let [[dx dy] (first (spotting/deltas-wind->coord [[10 -10]] 315.0))]

          (is (= (- H) dy))

          (is (close-to-zero dx))))))

  (testing "y-axis"
    (testing "north"
      (let [H (spotting/hypotenuse 10 10)]

        (let [[dx dy] (first (spotting/deltas-wind->coord [[10 -10]] 45.0))]

          (is (close-to-zero dy))

          (is (= (- H) dx)))

        (let [[dx dy] (first (spotting/deltas-wind->coord [[10 10]] 315.0))]

          (is (close-to-zero dy))

          (is (= (- H) dx)))))

    (testing "south"
      (let [H (spotting/hypotenuse 10 10)]

        (let [[dx dy] (first (spotting/deltas-wind->coord [[10 10]] 135.0))]

          (is (close-to-zero dy))

          (is (= H dx)))

        (let [[dx dy] (first (spotting/deltas-wind->coord [[10 -10]] 225.0))]

          (is (close-to-zero dy))

          (is (= H dx)))))))

(deftest surface-spot-percents
  (let [rand-gen (Random.)]
    (testing "expected ranges"
      (let [spot-percents [[[1 140] 1.0]
                           [[141 149] 2.0]
                           [[150 256] 3.0]]]

        (is (= 1.0 (spotting/surface-spot-percent spot-percents 5 rand-gen)))

        (is (= 2.0 (spotting/surface-spot-percent spot-percents 143 rand-gen)))

        (is (= 3.0 (spotting/surface-spot-percent spot-percents 155 rand-gen)))))

    (testing "overlapping ranges"
      (let [spot-percents [[[1 140] 1.0]
                           [[130 149] 2.0]]]

        (is (= 2.0 (spotting/surface-spot-percent spot-percents 133 rand-gen))
            "should overwite percents of previous range in the sequence")))))
