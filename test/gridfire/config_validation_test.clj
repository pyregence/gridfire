;; [[file:../../org/GridFire.org::gridfire.config-validation-test][gridfire.config-validation-test]]
;; FIXME LP coverage
(ns gridfire.config-validation-test
  (:require [clojure.spec.alpha   :as s]
            [clojure.test         :refer [deftest is]]
            [gridfire.spec.config :as spec]))

;;-----------------------------------------------------------------------------
;; Config
;;-----------------------------------------------------------------------------

(def resources-path "test/gridfire/resources/")

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

(defn in-file-path [filename]
  (str resources-path filename))

;;-----------------------------------------------------------------------------
;; Validator tests
;;-----------------------------------------------------------------------------

(deftest ^:unit valid-landfire-geotiff-test
  (let [config {:aspect             {:type   :geotiff
                                     :source (in-file-path "asp.tif")}
                :canopy-base-height {:type   :geotiff
                                     :source (in-file-path "cbh.tif")}
                :canopy-cover       {:type   :geotiff
                                     :source (in-file-path "cc.tif")}
                :canopy-height      {:type   :geotiff
                                     :source (in-file-path "ch.tif")}
                :crown-bulk-density {:type   :geotiff
                                     :source (in-file-path "cbd.tif")}
                :elevation          {:type   :geotiff
                                     :source (in-file-path "dem.tif")}
                :fuel-model         {:type   :geotiff
                                     :source (in-file-path "fbfm40.tif")}
                :slope              {:type   :geotiff
                                     :source (in-file-path "slp.tif")}}]
    (is (s/valid? ::spec/landfire-layers config))))

(deftest ^:unit valid-weather-geotiff-test
  (let [config {:type      :geotiff
                :source    (in-file-path "weather-test/tmpf_to_sample.tif")
                :cell-size 10.0}]
    (is (s/valid? ::spec/weather config))))

(deftest ^:unit valid-weather-postgis-test
  (let [config {:type      :postgis
                :source    "weather.ws WHERE rid=1"
                :cell-size 10.0}]
    (is (s/valid? ::spec/weather config))))
;; gridfire.config-validation-test ends here
