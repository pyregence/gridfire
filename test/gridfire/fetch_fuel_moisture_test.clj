(ns gridfire.fetch-fuel-moisture-test
  (:require [clojure.test :refer [deftest is testing]]
            [gridfire.fetch :as fetch]
            [gridfire.magellan-bridge :refer [geotiff-raster-to-matrix]]
            [gridfire.utils.test :as utils]))

(def resources-path "test/gridfire/resources/weather-test")

(deftest fuel-moisture-test
  (let [layers  {:dead {:1hr   {:type   :geotiff
                                :source (utils/in-file-path resources-path "m1_to_sample.tif")}
                        :10hr  {:type   :geotiff
                                :source (utils/in-file-path resources-path "m10_to_sample.tif")}
                        :100hr {:type   :geotiff
                                :source (utils/in-file-path resources-path "m100_to_sample.tif")}}
                 :live {:woody      {:type   :geotiff
                                     :source (utils/in-file-path resources-path "mlw_to_sample.tif")}
                        :herbaceous {:type   :geotiff
                                     :source (utils/in-file-path resources-path "mlh_to_sample.tif")}}}
        rasters (fetch/fuel-moisture-rasters {:fuel-moisture-layers layers})]

    (testing "dead 1hr moisture"
      (let [raster (get-in rasters [:dead :1hr])]

        (is (some? raster))

        (is (= raster (:matrix (geotiff-raster-to-matrix (utils/in-file-path resources-path "m1_to_sample.tif")))))))

    (testing "dead 10hr moisture"
      (let [raster (get-in rasters [:dead :10hr])]

        (is (some? raster))

        (is (= raster (:matrix (geotiff-raster-to-matrix (utils/in-file-path resources-path "m10_to_sample.tif")))))))

    (testing "dead 100hr moisture"
      (let [raster (get-in rasters [:dead :100hr])]

        (is (some? raster))

        (is (= raster (:matrix (geotiff-raster-to-matrix (utils/in-file-path resources-path "m100_to_sample.tif")))))))

    (testing "live woody moisture"
      (let [raster (get-in rasters [:live :woody])]

        (is (some? raster))

        (is (= raster (:matrix (geotiff-raster-to-matrix (utils/in-file-path resources-path "mlw_to_sample.tif")))))))

    (testing "live herbaceous moisture"
      (let [raster (get-in rasters [:live :herbaceous])]

        (is (some? raster))

        (is (= raster (:matrix (geotiff-raster-to-matrix (utils/in-file-path resources-path "mlh_to_sample.tif")))))))))
