(ns gridfire.inputs-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [are deftest is testing use-fixtures]]
            [gridfire.inputs :as inputs]
            [clojure.core.matrix :as m]))

;;-----------------------------------------------------------------------------
;; Fixtures
;;-----------------------------------------------------------------------------

(def ^:dynamic *base-config* {})

(defn with-base-config [test-fn]
  (binding [*base-config* (edn/read-string (slurp "test/gridfire/resources/inputs_test/base_config.edn"))]
    (test-fn)))

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
            inputs-before       (-> *base-config*
                                    inputs/add-input-layers
                                    inputs/add-misc-params)
            inputs-after        (fn [ignition-row ignition-col]
                                  (-> inputs-before
                                      (add-ignition-params ignition-row ignition-col)
                                      inputs/add-random-ignition-sites))]
        (are [ignition-row ignition-col] (valid-ignition-count? (inputs-after ignition-row ignition-col))
          50           50           ;scalar
          [0 50]       [0 50]       ;range
          '(1 2 3 4 5) '(1 2 3 4 5) ;list
          '(1)         '(2))))      ;list with less items than number of simulations

    (testing "just ignition-mask"
      (let [inputs-before (-> *base-config*
                              (assoc-in [:random-ignition :ignition-mask ]
                                        {:raster {:type   :geotiff
                                                  :source "test/gridfire/resources/inputs_test/ignition_mask.tif"}})
                              inputs/add-input-layers
                              inputs/add-misc-params)
            inputs-after  (inputs/add-random-ignition-sites inputs-before)]
0

        (is (true? (valid-ignition-count? inputs-after)))

        (is (every? pos? (map #(m/mget (:ignition-mask-matrix inputs-after) %1 %2)
                              (:ignition-rows inputs-after) (:ignition-cols inputs-after)))
            "All ignition sites should have positive values in the ignition mask")))))
