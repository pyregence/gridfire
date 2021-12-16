(ns gridfire.spotting-test
  (:require [clojure.test        :refer [are deftest is testing use-fixtures run-tests]]
            [clojure.core.matrix :as m]
            [gridfire.utils.random :refer [random-float]]
            [gridfire.conversion :as c]
            [gridfire.spotting   :as spotting])
  (:import java.util.Random))

(def ^:private seed 123456789)
(def ^{:dynamic true :private true} *rand-gen* nil)

(defn- create-random-generator [f]
  (binding [*rand-gen* (Random. seed)]
    (f)))

(use-fixtures :each create-random-generator)

(defn- within? [a b epsilon]
  (> epsilon (Math/abs (- a b))))

(defn- close-to-zero [d]
  (< -0.000001 d 0.000001))

(deftest ^:unit deltas-axis-test
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

(deftest ^:unit surface-spot-percents
  (testing "Fuel model is within expected ranges."
    (let [spot-percents [[[1 140] 1.0]
                         [[141 149] 2.0]
                         [[150 256] 3.0]]]

      (is (= 1.0 (spotting/surface-spot-percent spot-percents 5 *rand-gen*)))

      (is (= 2.0 (spotting/surface-spot-percent spot-percents 143 *rand-gen*)))

      (is (= 3.0 (spotting/surface-spot-percent spot-percents 155 *rand-gen*)))))

  (testing "Overlapping ranges"
    (let [spot-percents [[[1 140] 1.0]
                         [[130 149] 2.0]]]

      (is (= 2.0 (spotting/surface-spot-percent spot-percents 133 *rand-gen*))
          "should overwite percents of previous range in the sequence")))

  (testing "TODO: Given a pair of percentages, random percent is generated"))

(deftest ^:unit test-heat-of-preignition
  (testing "Heat of Preignition using eq. 9 of Schroeder (1969)."
    (are [result args] (within? result (apply spotting/heat-of-preignition args) 0.001)
         ; Q_ig (cal/g) [temperature (C) fuel-moisture (%)]
         0.0            [320.0          0.0]
         144.512        [0.0            0.0]

         ; Schroeder Fig. 2
         153.816        [(c/F->C 30.0)  0.01]
         193.996        [(c/F->C 160.0) 0.1]
         248.086        [(c/F->C 70.0)  0.15]
         292.814        [(c/F->C 130.0) 0.25])))

(deftest ^:unit test-schroeder-ign-prob
  (testing "Using Schroeder (1969)."
    (are [result args] (within? result (apply spotting/schroeder-ign-prob args) 0.01)
         ; Probability (0-1) [temperature (C) fuel-moisture (%)]
         0.0                 [0.0   0.3703]
         1.0                 [150.0 0.01]

         ; Schroeder Table 1
         0.87                [(c/F->C 35.0) 0.015]
         0.97                [(c/F->C 75.0) 0.015]
         0.74                [(c/F->C 35.0) 0.025]
         0.84                [(c/F->C 75.0) 0.025]
         0.51                [(c/F->C 35.0) 0.05]
         0.59                [(c/F->C 75.0) 0.05])))

(deftest ^:unit test-spot-ignition-probability
  (testing "Using Perryman (2012)."
    (let [inputs {:cell-size 10 :landfire-rasters {:elevation (m/zero-matrix 100 100)}}
          const  {:decay-constant 0.005}]
      (are [result args] (within? result (apply spotting/spot-ignition-probability args) 0.001)
           ; Probability (0-1) [inputs constants fuel-moisture (%) temperature (C) firebrand-count (int) torched-origin (coords) here (coords)]

           ; Increasing Estimated Fine-Fuel Moisture
           0.0                 [inputs const 0.0   0.0  0.0  [0 0] [5 5]]
           0.663               [inputs const 0.063 80.0 1.0  [0 0] [5 5]]
           0.452               [inputs const 0.094 80.0 1.0  [0 0] [5 5]]
           0.306               [inputs const 0.124 80.0 1.0  [0 0] [5 5]]
           0.100               [inputs const 0.2   80.0 1.0  [0 0] [5 5]]
           0.001               [inputs const 0.37  80.0 1.0  [0 0] [5 5]]

           ; Increasing Temperature
           0.316               [inputs const 0.1   50.0 1.0  [0 0] [5 5]]
           0.346               [inputs const 0.1   60.0 1.0  [0 0] [5 5]]
           0.381               [inputs const 0.1   70.0 1.0  [0 0] [5 5]]
           0.418               [inputs const 0.1   80.0 1.0  [0 0] [5 5]]
           0.459               [inputs const 0.1   90.0 1.0  [0 0] [5 5]]

           ; Increasing Firebrands Count
           0.0                 [inputs const 0.1   80.0 0.0  [0 0] [5 5]]
           0.418               [inputs const 0.1   80.0 1.0  [0 0] [5 5]]
           0.661               [inputs const 0.1   80.0 2.0  [0 0] [5 5]]
           0.995               [inputs const 0.1   80.0 10.0 [0 0] [5 5]]

           ; Increasing Distance
           0.455               [inputs const 0.1   80.0 1.0  [0 0] [1 1]]
           0.418               [inputs const 0.1   80.0 1.0  [0 0] [5 5]]
           0.383               [inputs const 0.1   80.0 1.0  [0 0] [9 9]]
           0.055               [inputs const 0.1   80.0 1.0  [0 0] [99 99]]))))

(deftest ^:unit test-spot-ignition-time
  (testing "Calculating t-max from Albini (1976)."
    (are [result args] (within? result (apply spotting/albini-t-max args) 0.1)
         ; Time to max height (min) [Flame length (m)]
         183.3                      [1.0]
         19.9                       [5.0]
         8.5                        [10.0]
         2.7                        [30.0]
         1.28                       [84.0]))

  (testing "Calculating time to spot ignition using Perryman."
    (are [result args] (within? result (apply spotting/spot-ignition-time args) 0.1)
         ; Ignition time (m) [Global clock time (m) Flame length (m)]
         386.6               [0 1.0]
         59.8                [0 5.0]
         37.0                [0 10.0]
         25.5                [0 30.0]
         22.56               [0 84.0])))

(deftest ^:unit test-spot-ignition?
  (testing "Spot ignition probability exceeds a random number generator."
    (are [result args] (= result (apply spotting/spot-ignition? args))
         ; Result [RNG spot-probability (0-1)]
         true     [*rand-gen* 1.0]
         false    [*rand-gen* 0.0]
         true     [*rand-gen* 0.5])))

(deftest ^:unit test-crown-spot-fire?
  (testing "Whether spotting from crown fire occurs based on a single or range of probabilities."
    (let [->argmap (fn [spotting] {:rand-gen *rand-gen*
                                   :spotting {:crown-fire-spotting-percent spotting}})]
      (are [result args] (= result (apply spotting/crown-spot-fire? args))
           ; Result [crown fire spotting probability (0-1)]
           true     [(->argmap 1.0)]
           false    [(->argmap 0.0)]
           true     [(->argmap 0.5)]
           false    [(->argmap [0.0 0.001])]
           true     [(->argmap [0.99 1.0])]))))

(deftest ^:unit test-surface-fire-spot-fire?
  (testing "Whether spotting from surface fire occurs based on a single or range of probabilities."
    (let [fuel-model (m/add! (m/zero-matrix 10 10) 10)
          ->argmap   (fn [crit-fire-line-intensity spotting]
                       {:rand-gen *rand-gen*
                        :landfire-rasters {:fuel-model fuel-model}
                        :spotting {:surface-fire-spotting
                                   {:critical-fire-line-intensity crit-fire-line-intensity
                                    :spotting-percent spotting}}})]
      (are [result args] (= result (apply spotting/surface-fire-spot-fire? args))
           ; Result [crit. fire line intensity (kW/m) surface fire spotting probability (0-1) here (coords) fire line intensity (kW/m)]
           nil      [(->argmap 1500 nil)                     [0 0] 1000]
           true     [(->argmap 500  [[[1 20] 1.0]])          [0 0] 1000]
           false    [(->argmap 500  [[[1 20] [0.0  0.001]]]) [0 0] 1000]
           true     [(->argmap 500  [[[1 20] [0.99 1.0]]])   [0 0] 1000]))))

(comment
  (run-tests 'gridfire.spotting-test)
  )
