;; [[file:../../org/GridFire.org::gridfire.simulations-test][gridfire.simulations-test]]
(ns gridfire.simulations-test
  (:require [gridfire.grid-lookup :as grid-lookup]
            [gridfire.simulations :as simulations]
            [clojure.test :refer [deftest is testing]]
            [tech.v3.tensor :as t])
  (:import java.util.Random))

(defn- identical-matrix [band width height value]
  (t/->tensor
   (into-array
    (repeat band
            (into-array
             (repeat height
                     (into-array
                      (repeat width value))))))))

(deftest ^:unit get-layer-fn-test
  (testing "no perturbations"
    (testing "scalar"
      (let [inputs       {:perturbations       nil
                          :temperature-samples [10.]}
            get-layer-fn (#'simulations/grid-getter inputs (Random. 1234) :temperature 0)]

        (is (= 10.0 (grid-lookup/double-at get-layer-fn 0 0 0)))))

    (testing "matrix"
      (let [inputs       {:perturbations      nil
                          :temperature-matrix (identical-matrix 72 10. 10. 1.0)}
            get-layer-fn (#'simulations/grid-getter inputs (Random. 1234) :temperature 0)]

        (is (= 1.0 (grid-lookup/double-at get-layer-fn 0 0 0))))))

  (testing "with perturbations"
    (testing "scalar"
      (testing "spatial type pixel"
        (let [inputs       {:perturbations       {:temperature {:spatial-type :pixel
                                                                :range        [-1.0 1.0]}}
                            :temperature-samples [10]}
              get-layer-fn (#'simulations/grid-getter inputs (Random. 1234) :temperature 0)]

          (is (<= 9.0 (grid-lookup/double-at get-layer-fn 0 0 0) 11.0))))

      (testing "spatial type global"
        (let [inputs {:perturbations       {:temperature {:spatial-type :global
                                                          :range        [-1.0 1.0]}}
                      :temperature-samples [10]}

              get-layer-fn (#'simulations/grid-getter inputs (Random. 1234) :temperature 0)]

          (is (<= 9.0 (grid-lookup/double-at get-layer-fn 0 0 0) 11.0)))))

    (testing "matrix"
      (testing "spatial type pixel"
        (let [inputs       {:perturbations      {:temperature {:spatial-type :pixel
                                                               :range        [-1.0 1.0]}}
                            :temperature-matrix (identical-matrix 72 100 100 1.0)}
              get-layer-fn (#'simulations/grid-getter inputs (Random. 1234) :temperature 0)]

          (is (<= 0.0 (grid-lookup/double-at get-layer-fn 0 0 0) 2.0))))

      (testing "spatial type global"
        (let [inputs       {:perturbations      {:temperature {:spatial-type :global
                                                               :range        [-1.0 1.0]}}
                            :temperature-matrix (identical-matrix 72 100 100 1.0)}
              get-layer-fn (#'simulations/grid-getter inputs (Random. 1234) :temperature 0)]

          (is (<= 0.0 (grid-lookup/double-at get-layer-fn 0 0 0) 2.0)))))))
;; gridfire.simulations-test ends here
