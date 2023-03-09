;; [[file:../../org/GridFire.org::gridfire.fuel-models-optimal-test][gridfire.fuel-models-optimal-test]]
(ns gridfire.fuel-models-optimal-test
  (:require [clojure.test                 :refer [deftest is testing]]
            [gridfire.fuel-models-optimal :as f-opt]))

(defn- classical-model-numbers
  []
  (keys f-opt/fuel-models))

(defn- WUI-model-numbers
  []
  (keys f-opt/WUI-model-number->original))

(defn- all-model-numbers
  []
  (keys f-opt/fuel-models-precomputed))

(deftest ^:unit fuel-number-predicates-test
  (testing "The WUI fuel models"
    (testing "are all burnable."
      (is (every? f-opt/is-burnable-fuel-model-number? (WUI-model-numbers))))

    (testing "all support dynamic fuel loading."
      (is (every? f-opt/is-dynamic-fuel-model-number? (WUI-model-numbers)))))

  (testing "Magic numbers:"
    (is (= 204 (apply max (classical-model-numbers))))
    (is (= 303 (apply max (all-model-numbers))))))
;; gridfire.fuel-models-optimal-test ends here
