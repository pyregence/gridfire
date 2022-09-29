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

(deftest add-random-ignition-sites-test
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

(deftest add-pyrome-calibration-constants
  (let [inputs       {:pyrome-calibration-csv "test/gridfire/resources/sample_pyrome_calibration_constants.csv"}
        inputs-after (inputs/add-pyrome-calibration-constants inputs)]
    (is (= {:sdi-sensitivity-to-difficulty                 1.0
            :sdi-reference-suppression-speed               600.0
            :sdi-containment-overwhelming-area-growth-rate 40000.0}
           (get-in inputs-after [:pyrome->constants 1])))))

(deftest add-pyrome-spread-rate-adjustment
  (let [inputs       {:pyrome-spread-rate-adjustment-csv "test/gridfire/resources/sample_pyrome_adjustment_factors.csv"}
        inputs-after (inputs/add-pyrome-spread-rate-adjustment inputs)]

    (is (= 0.9
           (get-in inputs-after [:pyrome->spread-rate-adjustment 1 102])))

    (is (= 0.8
           (get-in inputs-after [:pyrome->spread-rate-adjustment 1 103])))

    (is (= 0.6
           (get-in inputs-after [:pyrome->spread-rate-adjustment 10 101])))))
