(ns gridfire.core-test
  (:require [clojure.string         :as str]
            [clojure.test           :refer [deftest is testing use-fixtures]]
            [gridfire.binary-output :as binary]
            [gridfire.conversion    :refer [m->ft]]
            [gridfire.core          :as core]
            [gridfire.fetch         :as fetch]
            [gridfire.inputs        :as inputs]
            [gridfire.utils.test    :as utils]))

;;-----------------------------------------------------------------------------
;; Config
;;-----------------------------------------------------------------------------

(def resources-path "test/gridfire/resources")

(def db-spec {:classname   "org.postgresql.Driver"
              :subprotocol "postgresql"
              :subname     "//localhost:5432/gridfire_test"
              :user        "gridfire_test"
              :password    "gridfire_test"})

(def test-config-base
  {:db-spec                   db-spec
   :landfire-layers           {:aspect             "landfire.asp WHERE rid=1"
                               :canopy-base-height "landfire.cbh WHERE rid=1"
                               :canopy-cover       "landfire.cc WHERE rid=1"
                               :canopy-height      "landfire.ch WHERE rid=1"
                               :crown-bulk-density "landfire.cbd WHERE rid=1"
                               :fuel-model         "landfire.fbfm40 WHERE rid=1"
                               :slope              "landfire.slp WHERE rid=1"
                               :elevation          "landfire.dem WHERE rid=1"}
   :srid                      "CUSTOM:900914"
   :cell-size                 98.425     ;; (feet)
   :ignition-row              [10 10]
   :ignition-col              [20 20]
   :max-runtime               60         ;; (minutes)
   :temperature               '(50)      ;; (degrees Fahrenheit)
   :relative-humidity         '(1)       ;; (%)
   :wind-speed-20ft           '(10)      ;; (miles/hour)
   :wind-from-direction       '(0)       ;; (degrees clockwise from north)
   :foliar-moisture           90         ;; (%)
   :ellipse-adjustment-factor 1.0        ;; (< 1.0 = more circular, > 1.0 = more elliptical)
   :simulations               1
   :random-seed               1234567890 ;; long value (optional)
   :output-csvs?              true})

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

(defn in-file-path [filename]
  (str/join "/" [resources-path filename]))

(defn run-test-simulation! [config]
  (let [inputs (core/load-inputs! config)]
    (map #(dissoc % :rand-gen)
         (:summary-stats (core/run-simulations! inputs)))))

(defn valid-exits? [results]
  (when (seq results)
    (every? #(#{:max-runtime-reached :no-burnable-fuels} (:exit-condition %)) results)))

;;-----------------------------------------------------------------------------
;; Fixtures
;;-----------------------------------------------------------------------------

(use-fixtures :once utils/with-temp-output-dir)

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(deftest ^{:database true :simulation true} fetch-landfire-layers-old-config-test
  (is (some? (fetch/landfire-layers test-config-base))))

(deftest ^{:database true :simulation true} fetch-landfire-layers-test
  (testing "Fetching layers from postgis and geotiff files"
    (let [postgis-config {:db-spec         db-spec
                          :srid            "CUSTOM:900914"
                          :landfire-layers {:aspect             {:type   :postgis
                                                                 :source "landfire.asp WHERE rid=1"}
                                            :canopy-base-height {:type   :postgis
                                                                 :source "landfire.cbh WHERE rid=1"}
                                            :canopy-cover       {:type   :postgis
                                                                 :source "landfire.cc WHERE rid=1"}
                                            :canopy-height      {:type   :postgis
                                                                 :source "landfire.ch WHERE rid=1"}
                                            :crown-bulk-density {:type   :postgis
                                                                 :source "landfire.cbd WHERE rid=1"}
                                            :elevation          {:type   :postgis
                                                                 :source "landfire.dem WHERE rid=1"}
                                            :fuel-model         {:type   :postgis
                                                                 :source "landfire.fbfm40 WHERE rid=1"}
                                            :slope              {:type   :postgis
                                                                 :source "landfire.slp WHERE rid=1"}}}
          geotiff-config {:landfire-layers {:aspect             {:type   :geotiff
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
                                                                 :source (in-file-path "slp.tif")}}}
          postgis        (fetch/landfire-layers postgis-config)
          geotiff        (fetch/landfire-layers geotiff-config)]

      (is (= (get-in postgis [:aspect :matrix])
             (get-in geotiff [:aspect :matrix])))

      (is (= (get-in postgis [:canopy-cover :matrix])
             (get-in geotiff [:canopy-cover :matrix])))

      (is (= (get-in postgis [:canopy-height :matrix])
             (get-in geotiff [:canopy-height :matrix])))

      (is (= (get-in postgis [:crown-bulk-density :matrix])
             (get-in geotiff [:crown-bulk-density :matrix])))

      (is (= (get-in postgis [:elevation :matrix])
             (get-in geotiff [:elevation :matrix])))

      (is (= (get-in postgis [:fuel-model :matrix])
             (get-in geotiff [:fuel-model :matrix])))

      (is (= (get-in postgis [:slope :matrix])
             (get-in geotiff [:slope :matrix])))

      ;; TODO Add test for envelope
      )))

;;-----------------------------------------------------------------------------
;; Landfire Layer Tests
;;-----------------------------------------------------------------------------

(deftest ^{:database true :simulation true} run-test-simulation!-test
  (testing "Running simulation with different ways to fetch LANDFIRE layers"
    (let [postgis-config  (merge test-config-base
                                 {:landfire-layers {:aspect             {:type   :postgis
                                                                         :source "landfire.asp WHERE rid=1"}
                                                    :canopy-base-height {:type   :postgis
                                                                         :source "landfire.cbh WHERE rid=1"}
                                                    :canopy-cover       {:type   :postgis
                                                                         :source "landfire.cc WHERE rid=1"}
                                                    :canopy-height      {:type   :postgis
                                                                         :source "landfire.ch WHERE rid=1"}
                                                    :crown-bulk-density {:type   :postgis
                                                                         :source "landfire.cbd WHERE rid=1"}
                                                    :elevation          {:type   :postgis
                                                                         :source "landfire.dem WHERE rid=1"}
                                                    :fuel-model         {:type   :postgis
                                                                         :source "landfire.fbfm40 WHERE rid=1"}
                                                    :slope              {:type   :postgis
                                                                         :source "landfire.slp WHERE rid=1"}}})
          geotiff-config  (merge test-config-base
                                 {:landfire-layers {:aspect             {:type   :geotiff
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
                                                                         :source (in-file-path "slp.tif")}}})
          postgis-results (run-test-simulation! postgis-config)

          geotiff-results (run-test-simulation! geotiff-config)]

      (is (valid-exits? postgis-results))

      (is (valid-exits? geotiff-results))

      (is (= (set postgis-results) (set geotiff-results))))))

;;-----------------------------------------------------------------------------
;; Ignition Layer Tests
;;-----------------------------------------------------------------------------

(deftest ^{:database true :simulation true} geotiff-ignition-test
  (testing "Running simulation with ignition layers read from geotiff files"
    (let [config (merge test-config-base
                        {:ignition-layer {:type   :geotiff
                                          :source (in-file-path "ign.tif")}})]

      (is (valid-exits? (run-test-simulation! config))))))

(deftest ^{:database true :simulation true} postgis-ignition-test
  (testing "Running simulation with ignition layers read from Postgres database"
    (let [config (merge test-config-base
                        {:ignition-layer {:type   :postgis
                                          :source "ignition.ign WHERE rid=1"}})]
      (is (valid-exits? (run-test-simulation! config))))))

(deftest ^{:database true :simulation true} burn-value-test
  (testing "Running simulation with burned and unburned values different from Gridfire's definition"
    (let [config (merge test-config-base
                        {:ignition-layer {:type        :geotiff
                                          :source      (in-file-path "ign-inverted.tif")
                                          :burn-values {:burned   -1.0
                                                        :unburned 1.0}}})]
      (is (valid-exits? (run-test-simulation! config))))))

;;-----------------------------------------------------------------------------
;; Weather Layer Tests
;;-----------------------------------------------------------------------------

(def landfire-layers-weather-test
  {:aspect             {:type   :geotiff
                        :source (in-file-path "weather-test/asp.tif")}
   :canopy-base-height {:type       :geotiff
                        :source     (in-file-path "weather-test/cbh.tif")
                        :unit       :metric
                        :multiplier 0.1}
   :canopy-cover       {:type   :geotiff
                        :source (in-file-path "weather-test/cc.tif")}
   :canopy-height      {:type       :geotiff
                        :source     (in-file-path "weather-test/ch.tif")
                        :unit       :metric
                        :multiplier 0.1}
   :crown-bulk-density {:type       :geotiff
                        :source     (in-file-path "weather-test/cbd.tif")
                        :unit       :metric
                        :multiplier 0.01}
   :elevation          {:type   :geotiff
                        :source (in-file-path "weather-test/dem.tif")}
   :fuel-model         {:type   :geotiff
                        :source (in-file-path "weather-test/fbfm40.tif")}
   :slope              {:type   :geotiff
                        :source (in-file-path "weather-test/slp.tif")}})

(def weather-layers
  {:temperature         {:type   :geotiff
                         :source (in-file-path "weather-test/tmpf_to_sample.tif")}
   :relative-humidity   {:type   :geotiff
                         :source (in-file-path "weather-test/rh_to_sample.tif")}
   :wind-speed-20ft     {:type   :geotiff
                         :source (in-file-path "weather-test/ws_to_sample.tif")}
   :wind-from-direction {:type   :geotiff
                         :source (in-file-path "weather-test/wd_to_sample.tif")}})

(deftest ^{:database true :simulation true} run-test-simulation!-weather-test
  (doseq [weather weather-layers]
    (let [config (merge test-config-base
                        weather
                        {:landfire-layers landfire-layers-weather-test
                         :max-runtime     120})]

      (is (valid-exits? (run-test-simulation! config))))))


(deftest ^:simulation geotiff-landfire-weather-ignition
  (testing "Running simulation using landfire, weather, and ignition data from geotiff files"
    (let [config (merge test-config-base
                        weather-layers
                        {:landfire-layers landfire-layers-weather-test
                         :ignition-layer  {:type   :geotiff
                                           :source (in-file-path "weather-test/phi.tif")}
                         :max-runtime     120})]

      (is (valid-exits? (run-test-simulation! config))))))

(deftest ^:simulation run-test-simulation!-using-lower-resolution-weather-test
  (testing "Running simulation using temperature data from geotiff file"
    (let [config (merge test-config-base
                        {:cell-size       (m->ft 30)
                         :landfire-layers landfire-layers-weather-test
                         :temperature     {:type   :geotiff
                                           :source (in-file-path "weather-test/tmpf_to_sample_lower_res.tif")}})]

      (is (valid-exits? (run-test-simulation! config))))))

(deftest ^:unit multiplier-lookup-test
  (testing "constructing multiplier lookup for weather raster"
    (let [config {:cell-size   (m->ft 30)
                  :temperature {:type   :geotiff
                                :source (in-file-path "weather-test/tmpf_to_sample_lower_res.tif")}}
          lookup (inputs/create-multiplier-lookup (assoc config :weather-layers (fetch/weather-layers config)))]

      (is (= {:temperature 10.0} lookup)))))

;;-----------------------------------------------------------------------------
;; Perturbation Tests
;;-----------------------------------------------------------------------------

(deftest ^{:database true :simulation true} run-test-simulation!-with-landfire-perturbations
  (testing "with global perturbation value"
    (let [config (merge test-config-base
                        {:perturbations {:canopy-height {:spatial-type :global
                                                         :range        [-1.0 1.0]}}})]
      (is (valid-exits? (run-test-simulation! config)))))

  (testing "with pixel by pixel perturbation"
    (let [config (merge test-config-base
                        {:perturbations {:canopy-height {:spatial-type :pixel
                                                         :range        [-1.0 1.0]}}})]
      (is (valid-exits? (run-test-simulation! config))))))

;;-----------------------------------------------------------------------------
;; Outputs
;;-----------------------------------------------------------------------------

(deftest ^{:database true :simulation true} binary-output-files-test
  (let [config         (merge test-config-base
                              {:output-binary?   true
                               :output-directory "test/output"})
        _              (run-test-simulation! config)
        binary-results (binary/read-matrices-as-binary (utils/out-file-path "toa_0001_00001.bin")
                                                       [:float :float :float :int])]
    (is (some? binary-results))))

;;-----------------------------------------------------------------------------
;; Ignition Mask
;;-----------------------------------------------------------------------------

(deftest ^:simulation igniton-mask-test
  (let [config (merge test-config-base
                      {:landfire-layers landfire-layers-weather-test
                       :ignition-row    nil
                       :ignition-col    nil
                       :random-ignition {:ignition-mask {:type   :geotiff
                                                         :source (in-file-path "weather-test/ignition_mask.tif")}
                                         :edge-buffer   9843.0}})]
    (is (valid-exits? (run-test-simulation! config)))))

;;-----------------------------------------------------------------------------
;; Moisture Rasters
;;-----------------------------------------------------------------------------

(deftest ^:simulation moisture-rasters-test
  (let [config (merge test-config-base
                      {:landfire-layers landfire-layers-weather-test
                       :ignition-row    nil
                       :ignition-col    nil
                       :random-ignition {:ignition-mask {:type   :geotiff
                                                         :source (in-file-path "weather-test/ignition_mask.tif")}
                                         :edge-buffer   9843.0}
                       :fuel-moisture   {:dead {:1hr   {:type   :geotiff
                                                        :source (in-file-path "weather-test/m1_to_sample.tif")}
                                                :10hr  {:type   :geotiff
                                                        :source (in-file-path "weather-test/m10_to_sample.tif")}
                                                :100hr {:type   :geotiff
                                                        :source (in-file-path "weather-test/m100_to_sample.tif")}}
                                         :live {:woody      {:type   :geotiff
                                                             :source (in-file-path "weather-test/mlw_to_sample.tif")}
                                                :herbaceous {:type   :geotiff
                                                             :source (in-file-path "weather-test/mlh_to_sample.tif")}}}})]
    (is (valid-exits? (run-test-simulation! config)))))

(deftest ^:simulation moisture-scalars-only-test
  (let [config (merge test-config-base
                      {:landfire-layers landfire-layers-weather-test
                       :ignition-row    nil
                       :ignition-col    nil
                       :random-ignition {:ignition-mask {:type   :geotiff
                                                         :source (in-file-path "weather-test/ignition_mask.tif")}
                                         :edge-buffer   9843.0}
                       :fuel-moisture   {:dead {:1hr   0.10
                                                :10hr  0.10
                                                :100hr 0.10}
                                         :live {:woody      0.80
                                                :herbaceous 0.80}}})]
    (is (valid-exits? (run-test-simulation! config)))))

(deftest ^:simulation moisture-mix-raster-scalars-test
  (let [config (merge test-config-base
                      {:landfire-layers landfire-layers-weather-test
                       :ignition-row    nil
                       :ignition-col    nil
                       :random-ignition {:ignition-mask {:type   :geotiff
                                                         :source (in-file-path "weather-test/ignition_mask.tif")}
                                         :edge-buffer   9843.0}
                       :fuel-moisture   {:dead {:1hr   {:type   :geotiff
                                                        :source (in-file-path "weather-test/m1_to_sample.tif")}
                                                :10hr  {:type   :geotiff
                                                        :source (in-file-path "weather-test/m10_to_sample.tif")}
                                                :100hr {:type   :geotiff
                                                        :source (in-file-path "weather-test/m100_to_sample.tif")}}
                                         :live {:woody      80.0
                                                :herbaceous 30.0}}})]
    (is (valid-exits? (run-test-simulation! config)))))

;;-----------------------------------------------------------------------------
;; Ignition CSV
;;-----------------------------------------------------------------------------

(deftest ^{:database true :simulation true} ignition-csv-test
  (let [results (run-test-simulation! (assoc test-config-base :ignition-csv (in-file-path "sample_ignitions.csv")))]

    (is (valid-exits? results))

    (is (= 3 (count results))
        "Should have the same number of simulations as ignition rows in sample_ignitions.csv")

    (is (= 10.0 (:global-clock (first results)))
        "Global clock should end at start_time + max_runtime in sample_ignitions.csv")

    (is (= 20.0 (:global-clock (second results)))
        "Global clock should end at start_time + max_runtime in sample_ignitions.csv")))
