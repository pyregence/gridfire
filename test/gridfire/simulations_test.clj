;; [[file:../../org/GridFire.org::gridfire.simulations-test][gridfire.simulations-test]]
(ns gridfire.simulations-test
  (:require [gridfire.grid-lookup        :as grid-lookup]
            [gridfire.simulations        :as simulations]
            [clojure.test                :refer [deftest is testing]]
            [tech.v3.datatype            :as d]
            [tech.v3.datatype.functional :as dfn]
            [tech.v3.tensor              :as t])
  (:import java.util.Random))

(def example-burn-time-matrix
  (-> (t/->tensor [[-1.0 -1.0 -1.0]
                   [-1.0 5.22 -1.0]
                   [1.33 0.0 -1.0]]
                  :datatype :float32)
      (grid-lookup/ensure-flat-jvm-tensor)
      (grid-lookup/add-double-getter)))

(def example-flame-length-matrix
  (-> (t/->tensor [[-1.0 -1.0 -1.0]
                   [-1.0 7.45 -1.0]
                   [2.45 7.44 -1.0]]
                  :datatype :float32)
      (grid-lookup/ensure-flat-jvm-tensor)
      (grid-lookup/add-double-getter)))

(defn- float2darr->matrix
  [float2darr]
  (->> float2darr
       (seq)
       (map t/->tensor)
       (t/->tensor)))

(deftest layer-snapshot-float2darr-test
  (testing (str `simulations/layer-snapshot-float2darr)
    (testing (str "yields an result equivalent to " `simulations/layer-snapshot)
      (is (dfn/equals (simulations/layer-snapshot example-burn-time-matrix
                                                  example-flame-length-matrix
                                                  2.0)
                      (->> (simulations/layer-snapshot-float2darr example-burn-time-matrix
                                                                  example-flame-length-matrix
                                                                  2.0)
                           (float2darr->matrix)))))
    (testing "still works when all rows are null"
      (is (dfn/equals (->> (simulations/layer-snapshot-float2darr (-> (t/const-tensor -1.0 (d/shape example-flame-length-matrix))
                                                                      (grid-lookup/ensure-flat-jvm-tensor)
                                                                      (grid-lookup/add-double-getter))
                                                                  example-flame-length-matrix
                                                                  2.0)
                           (float2darr->matrix))
                      (t/const-tensor 0.0 (d/shape example-flame-length-matrix)))))))

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
