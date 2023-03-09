;; [[file:../../org/GridFire.org::gridfire.fetch-fuel-moisture-test][gridfire.fetch-fuel-moisture-test]]
;; FIXME LP coverage
(ns gridfire.fetch-fuel-moisture-test
  (:require
   [clojure.test                :refer [deftest are]]
   [gridfire.fetch              :as fetch]
   [gridfire.magellan-bridge    :refer [geotiff-raster-to-tensor]]
   [gridfire.utils.test         :as utils]
   [gridfire.conversion         :as convert]
   [tech.v3.datatype            :as d]
   [tech.v3.datatype.functional :as dfn]))

(def resources-path "test/gridfire/resources/weather-test")

(defn- equal-matrix?
  [inputs category size]
  (let [matrix (-> (get-in inputs [:fuel-moisture category size :source])
                   geotiff-raster-to-tensor
                   :matrix)]
    (dfn/equals (:matrix (fetch/fuel-moisture-layer inputs category size))
                (d/clone (d/emap convert/percent->dec nil matrix)))))

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
(comment
  (clojure.test/test-vars [#'fuel-moisture-test])

  *e)
;; gridfire.fetch-fuel-moisture-test ends here
