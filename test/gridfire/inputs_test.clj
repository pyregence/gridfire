;; [[file:../../org/GridFire.org::gridfire.inputs-test][gridfire.inputs-test]]
;; FIXME LP coverage
(ns gridfire.inputs-test
  (:require [clojure.test    :refer [are deftest is testing use-fixtures]]
            [gridfire.inputs :as inputs]
            [tech.v3.tensor  :as t])
  (:import java.util.Random))

;;-----------------------------------------------------------------------------
;; Fixtures
;;-----------------------------------------------------------------------------

(def ^:dynamic *base-config* {})

(defn- identical-matrix [width height value]
  (t/->tensor
   (into-array (repeat height (into-array (repeat width value))))))

(defn with-base-config [test-fn]
  (let [width  256
        height 256]
    (binding [*base-config* {:simulations       5
                             :cell-size         98.425
                             :num-rows          width
                             :num-cols          height
                             :rand-gen          (Random. 1234)
                             :fuel-model-matrix (identical-matrix width height 101)}]
      (test-fn))))

(use-fixtures :once with-base-config)

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(deftest  ^:unit add-random-ignition-sites-test
  (let [valid-ignition-count? (fn [inputs]
                                (= (-> inputs :ignition-rows count)
                                   (-> inputs :ignition-cols count)
                                   (:simulations inputs)))]
    (testing "ignition-row and ignition-col"
      (let [add-ignition-params (fn [config ignition-row ignition-col]
                                  (assoc config :ignition-row ignition-row :ignition-col ignition-col))
            test-with           (fn [ignition-row ignition-col]
                                  (-> *base-config*
                                      (add-ignition-params ignition-row ignition-col)
                                      inputs/add-random-ignition-sites))]
        (are [ignition-row ignition-col] (valid-ignition-count? (test-with ignition-row ignition-col))
          50           50           ;scalar
          [0 50]       [0 50]       ;range
          '(1 2 3 4 5) '(1 2 3 4 5) ;list
          '(1)         '(2))))      ;list with less items than number of simulations

    (testing "just ignition-mask"
      (let [ignition-mask-matrix (identical-matrix 256 256 0.0)
            _                    (doseq [i (range 0 10)
                                         j (range 0 10)]
                                   (t/mset! ignition-mask-matrix i j 1.0))
            inputs-before        (-> *base-config*
                                     (assoc :ignition-mask-matrix  ignition-mask-matrix))
            inputs-after         (inputs/add-random-ignition-sites inputs-before)]

        (is (true? (valid-ignition-count? inputs-after)))

        (is (every? pos? (map #(t/mget (:ignition-mask-matrix inputs-after) %1 %2)
                              (:ignition-rows inputs-after) (:ignition-cols inputs-after)))
            "All ignition sites should have positive values in the ignition mask")))))

(deftest ^:unit add-suppression-test
  (let [inputs       {:suppression {:sdi-layer                                     {:type   :geotiff
                                                                                    :source "path/to/suppression/layer"}
                                    :suppression-dt                                42.0
                                    :sdi-sensitivity-to-difficulty                 42.0
                                    :sdi-containment-overwhelming-area-growth-rate 42.0
                                    :sdi-reference-suppression-speed               42.0}
                      :simulations 1
                      :rand-gen    (Random. 1234)}
        inputs-after (inputs/add-suppression inputs)]

    (is (= [42.0] (inputs-after :suppression-dt-samples)))
    (is (= [42.0] (inputs-after :sdi-sensitivity-to-difficulty-samples)))
    (is (= [42.0] (inputs-after :sdi-containment-overwhelming-area-growth-rate-samples)))
    (is (= [42.0] (inputs-after :sdi-reference-suppression-speed-samples)))))

(defn- one-dimensional-double-array? [x]
  (= (Class/forName "[D")
     (class x)))

(deftest ^:unit add-spread-rate-adjustment-factors-test
  (let [inputs       (-> *base-config*
                         (merge {:fuel-number->spread-rate-adjustment-samples [{101 0.1}]}))
        inputs-after (inputs/add-fuel-number->spread-rate-adjustment-array-lookup-samples inputs)]

    (is (contains? inputs-after :fuel-number->spread-rate-adjustment-array-lookup-samples))

    (is (one-dimensional-double-array? (first (:fuel-number->spread-rate-adjustment-array-lookup-samples inputs-after))))

    (is (= 0.1 (aget (doubles (first (:fuel-number->spread-rate-adjustment-array-lookup-samples inputs-after))) 101)))))
;; gridfire.inputs-test ends here
