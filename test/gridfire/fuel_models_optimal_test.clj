(ns gridfire.fuel-models-optimal-test
  (:require [clojure.test :refer [deftest is testing]]
            [gridfire.fuel-models-optimal :as f-opt]))

(defn- classical-model-numbers
  []
  (keys f-opt/fuel-models))

(defn- WUI-model-numbers
  []
  (keys f-opt/WUI-model-number->original))

(deftest ^:unit fuel-number-predicates-test
  (testing "The WUI fuel models"
    (testing "are all burnable."
      (is (every? f-opt/is-burnable-fuel-model-number? (WUI-model-numbers))))

    (testing "all support dynamic fuel loading."
      (is (every? f-opt/is-dynamic-fuel-model-number? (WUI-model-numbers)))))

  (testing "Magic numbers:"
    (is (= 204 (apply max (classical-model-numbers))))))
