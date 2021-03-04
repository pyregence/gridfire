(ns gridfire.spec.fuel-moisture-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is]]
            [gridfire.spec.fuel-moisture :as fuel-moisture]
            [gridfire.utils.test :as utils]))

(def resources-path "test/gridfire/resources/weather-test")

(deftest basic-test
  (let [config {:dead {:1hr   {:type   :geotiff
                               :source (utils/in-file-path resources-path "m1_to_sample.tif")}
                       :10hr  {:type   :geotiff
                               :source (utils/in-file-path resources-path "m10_to_sample.tif")}
                       :100hr {:type   :geotiff
                               :source (utils/in-file-path resources-path "m100_to_sample.tif")}}
                :live {:woody      {:type   :geotiff
                                    :source (utils/in-file-path resources-path "mlw_to_sample.tif")}
                       :herbaceous {:type   :geotiff
                                    :source (utils/in-file-path resources-path "mlh_to_sample.tif")}}}]
    (is (s/valid? ::fuel-moisture/fuel-moisture-layers config))))
