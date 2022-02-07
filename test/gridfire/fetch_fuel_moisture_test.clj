(ns gridfire.fetch-fuel-moisture-test
  (:require [clojure.core.matrix       :as m]
            [clojure.test              :refer [deftest are]]
            [gridfire.fetch            :as fetch]
            [gridfire.magellan-bridge  :refer [geotiff-raster-to-matrix]]
            [gridfire.utils.test       :as utils]
            [gridfire.conversion :as convert]))

(def resources-path "test/gridfire/resources/weather-test")

(defn- equal-matrix?
  [inputs category size]
  (let [matrix (if (= category :dead)
                 (-> (get-in inputs [:fuel-moisture category size :source])
                     geotiff-raster-to-matrix
                     :matrix)
                 (-> (get-in inputs [:fuel-moisture category size :source])
                     geotiff-raster-to-matrix
                     :matrix
                     first))]
    (= (fetch/fuel-moisture-matrix inputs category size)
       (m/emap convert/percent->dec matrix))))

(deftest ^:unit fuel-moisture-test
  (let [inputs {:fuel-moisture
                {:dead {:1hr   {:type   :geotiff
                                :source (utils/in-file-path resources-path "m1_to_sample.tif")}
                        :10hr  {:type   :geotiff
                                :source (utils/in-file-path resources-path "m10_to_sample.tif")}
                        :100hr {:type   :geotiff
                                :source (utils/in-file-path resources-path "m100_to_sample.tif")}}
                 :live {:woody      {:type   :geotiff
                                     :source (utils/in-file-path resources-path "mlw_to_sample.tif")}
                        :herbaceous {:type   :geotiff
                                     :source (utils/in-file-path resources-path "mlh_to_sample.tif")}}}}]

    (are [category size] (equal-matrix? inputs category size)
      :dead :1hr
      :dead :10hr
      :dead :100hr
      :live :herbaceous
      :live :woody)))
