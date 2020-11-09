(ns gridfire.config-validation-test
  (:require [gridfire.spec.config :as spec]
            [gridfire.crown-fire :refer [m->ft]]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing]]))

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

(deftest valid-landfire-geotiff-bacwards-compatable-test
  (let [config {:aspect             (in-file-path "asp.tif")
                :canopy-base-height (in-file-path "cbh.tif")
                :canopy-cover       (in-file-path "cc.tif")
                :canopy-height      (in-file-path "ch.tif")
                :crown-bulk-density (in-file-path "cbd.tif")
                :elevation          (in-file-path "dem.tif")
                :fuel-model         (in-file-path "fbfm40.tif")
                :slope              (in-file-path "slp.tif")}]
    (is (s/valid? ::validation/landfire-layers config))))

(deftest valid-landfire-geotiff-test
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
    (is (s/valid? ::validation/landfire-layers config))))

(deftest valid-weather-geotiff-test
  (let [config {:type      :geotiff
                :source    (in-file-path "weather-test/tmpf_to_sample.tif")
                :cell-size 10.0}]
    (is (s/valid? ::validation/weather config))))

(deftest valid-weather-postgis-test
  (let [config {:type      :postgis
                :source    "weather.ws WHERE rid=1"
                :cell-size 10.0}]
    (is (s/valid? ::validation/weather config))))

(deftest weather-cell-size-test
  (let [high-res (m->ft 30)
        low-res  (* high-res 10)
        temp     (s/conform ::spec/weather
                            {:type      :geotiff
                             :source    (in-file-path "weather-test/tmpf_to_sample.tif")
                             :cell-size low-res})]
    (testing "Valid cell-size for a weather raster"
      (let [config {:cell-size   high-res
                    :temperature temp}]

        (is (true? (spec/valid-weather-cell-sizes? config)))))

    (testing "Valid cell-size for multiple weather raster"
      (let [config {:cell-size         high-res
                    :temperature       temp
                    :relative-humidity temp}]

        (is (true? (spec/valid-weather-cell-sizes? config)))))))

(deftest weather-cell-invalid-test
  (testing "Invalid cell-size for a weather raster"
    (let [cell-size (m->ft 30)
          temp      (s/conform ::spec/weather
                               {:type      :geotiff
                                :source    (in-file-path "weather-test/tmpf_to_sample.tif")
                                :cell-size (+ cell-size (/ cell-size 2))})
          config    {:cell-size   cell-size
                     :temperature temp}]

      (is (false? (spec/valid-weather-cell-sizes? config))))))
