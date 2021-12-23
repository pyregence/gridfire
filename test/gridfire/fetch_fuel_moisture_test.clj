(ns gridfire.fetch-fuel-moisture-test
  (:require [clojure.core.matrix       :as m]
            [clojure.test              :refer [deftest is testing]]
            [gridfire.fetch            :as fetch]
            [gridfire.magellan-bridge  :refer [geotiff-raster-to-matrix]]
            [gridfire.utils.test       :as utils]))

(def resources-path "test/gridfire/resources/weather-test")

(deftest ^:unit fuel-moisture-test
  (let [layers        {:dead {:1hr   {:type   :geotiff
                                      :source (utils/in-file-path resources-path "m1_to_sample.tif")}
                              :10hr  {:type   :geotiff
                                      :source (utils/in-file-path resources-path "m10_to_sample.tif")}
                              :100hr {:type   :geotiff
                                      :source (utils/in-file-path resources-path "m100_to_sample.tif")}}
                       :live {:woody      {:type   :geotiff
                                           :source (utils/in-file-path resources-path "mlw_to_sample.tif")}
                              :herbaceous {:type   :geotiff
                                           :source (utils/in-file-path resources-path "mlh_to_sample.tif")}}}
        fuel-moisture (fetch/fuel-moisture {:fuel-moisture layers})]

    (testing "dead 1hr moisture"
      (let [fetched-raster (get-in fuel-moisture [:dead :1hr :matrix])
            raster         (m/emap #(* % 0.01) (:matrix (geotiff-raster-to-matrix (utils/in-file-path resources-path "m1_to_sample.tif"))))]

        (is (some? fetched-raster))

        (is (= fetched-raster raster))))

    (testing "dead 10hr moisture"
      (let [fetched-raster (get-in fuel-moisture [:dead :10hr :matrix])
            raster         (m/emap #(* % 0.01) (:matrix (geotiff-raster-to-matrix (utils/in-file-path resources-path "m10_to_sample.tif"))))]

        (is (some? fetched-raster))

        (is (= fetched-raster raster))))

    (testing "dead 10hr moisture"
      (let [fetched-raster (get-in fuel-moisture [:dead :100hr :matrix])
            raster         (m/emap #(* % 0.01) (:matrix (geotiff-raster-to-matrix (utils/in-file-path resources-path "m100_to_sample.tif"))))]

        (is (some? fetched-raster))

        (is (= fetched-raster raster))))

    (testing "live woody moisture"
      (let [fetched-raster (get-in fuel-moisture [:live :woody :matrix])
            raster         (m/emap #(* % 0.01) (first (:matrix (geotiff-raster-to-matrix (utils/in-file-path resources-path "mlw_to_sample.tif")))))]

        (is (some? fetched-raster))

        (is (= fetched-raster raster))))

    (testing "live herbaceous moisture"
      (let [fetched-raster (get-in fuel-moisture [:live :herbaceous :matrix])
            raster         (m/emap #(* % 0.01) (first (:matrix (geotiff-raster-to-matrix (utils/in-file-path resources-path "mlh_to_sample.tif")))))]

        (is (some? fetched-raster))

        (is (= fetched-raster raster))))))
