(ns gridfire.cli-test
  (:require [clojure.test :refer [deftest is testing]]
            [gridfire.cli :as cli]
            [gridfire.fetch :as fetch]
            [gridfire.crown-fire :refer [m->ft]])
  (:import java.util.Random))

;;-----------------------------------------------------------------------------
;; Config
;;-----------------------------------------------------------------------------

(def resources-path "test/gridfire/resources/")

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
  (str resources-path filename))

(defn run-simulation [config]
  (let [simulations       (:simulations config)
        multiplier-lookup (cli/create-multiplier-lookup config)
        rand-generator    (if-let [seed (:random-seed config)]
                            (Random. seed)
                            (Random.))
        landfire-layers   (fetch/landfire-layers config)
        landfire-rasters   (into {} (map (fn [[layer-name info]] [layer-name (first (:matrix info))])) landfire-layers)
        ignition-raster   (fetch/ignition-layer config)]
    (cli/run-simulations
     config
     landfire-rasters
     (cli/get-envelope config landfire-layers)
     (cli/draw-samples rand-generator simulations (:ignition-row config))
     (cli/draw-samples rand-generator simulations (:ignition-col config))
     (cli/draw-samples rand-generator simulations (:max-runtime config))
     (cli/get-weather config rand-generator :temperature)
     (cli/get-weather config rand-generator :relative-humidity)
     (cli/get-weather config rand-generator :wind-speed-20ft)
     (cli/get-weather config rand-generator :wind-from-direction)
     (cli/draw-samples rand-generator simulations (:foliar-moisture config))
     (cli/draw-samples rand-generator simulations (:ellipse-adjustment-factor config))
     ignition-raster
     multiplier-lookup
     (cli/draw-perturbation-samples rand-generator simulations (:perturbations config)))))

(defn valid-exits? [results]
  (when (seq results)
    (every? #(#{:max-runtime-reached :no-burnable-fuels} (:exit-condition %)) results)))

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(deftest fetch-landfire-layers-old-config-test
  (is (some? (fetch/landfire-layers test-config-base))))

(deftest fetch-landfire-layers-test
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

(deftest run-simulation-test
  (testing "Running simulation with different ways to fetch LANDFIRE layers"
    (let [postgis-config  (merge test-config-base
                                 {:landfire-layers    {:aspect             {:type   :postgis
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
                                 {:landfire-layers    {:aspect             {:type   :geotiff
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
          postgis-results (run-simulation postgis-config)

          geotiff-results (run-simulation geotiff-config)]

      (is (valid-exits? postgis-results))

      (is (valid-exits? geotiff-results))

      (is (= (set postgis-results) (set geotiff-results))))))

;;-----------------------------------------------------------------------------
;; Ignition Layer Tests
;;-----------------------------------------------------------------------------

(deftest geotiff-ignition-test
  (testing "Running simulation with ignition layers read from geotiff files"
    (let [geotiff-config (merge test-config-base
                                {:ignition-layer {:type   :geotiff
                                                  :source (in-file-path "ign.tif")}})
          results        (run-simulation geotiff-config)]

      (is (valid-exits? results)))))

(deftest postgis-ignition-test
  (testing "Running simulation with ignition layers read from geotiff files"
    (let [postgis-config (merge test-config-base
                                {:ignition-layer {:type   :postgis
                                                  :source "ignition.ign WHERE rid=1"}})
          results        (run-simulation postgis-config)]
      (is (valid-exits? results)))))

;;-----------------------------------------------------------------------------
;; Weather Layer Tests
;;-----------------------------------------------------------------------------

(def landfire-layers-weather-test
  {:aspect             {:type   :geotiff
                        :source (in-file-path "weather-test/asp.tif")}
   :canopy-base-height {:type   :geotiff
                        :source (in-file-path "weather-test/cbh.tif")}
   :canopy-cover       {:type   :geotiff
                        :source (in-file-path "weather-test/cc.tif")}
   :canopy-height      {:type   :geotiff
                        :source (in-file-path "weather-test/ch.tif")}
   :crown-bulk-density {:type   :geotiff
                        :source (in-file-path "weather-test/cbd.tif")}
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

(deftest run-simulation-weather-test
  (doseq [weather weather-layers]
    (let [config (merge test-config-base
                        weather
                        {:landfire-layers landfire-layers-weather-test
                         :max-runtime     120})]

      (is (valid-exits? (run-simulation config))))))


(deftest geotiff-landfire-weather-ignition
  (testing "Running simulation using landfire, weather, and ignition data from geotiff files"
    (let [config  (merge test-config-base
                         weather-layers
                         {:landfire-layers     landfire-layers-weather-test
                          :ignition-layer      {:type   :geotiff
                                                :source (in-file-path "weather-test/phi.tif")}
                          :max-runtime         120})]

      (is (valid-exits? (run-simulation config))))))

(deftest run-simulation-using-lower-resolution-weather-test
  (testing "Running simulation using temperature data from geotiff file"
    (let [config  (merge test-config-base
                         {:cell-size       (m->ft 30)
                          :landfire-layers landfire-layers-weather-test
                          :temperature     {:type      :geotiff
                                            :source    (in-file-path "weather-test/tmpf_to_sample_lower_res.tif")
                                            :cell-size (m->ft 300)}})]

      (is (valid-exits? (run-simulation config))))))

(deftest multiplier-lookup-test
  (testing "constructing multiplier lookup for weather rasters"
    (testing "with single weather raster"
      (let [config {:cell-size   (m->ft 30)
                    :temperature {:type      :geotiff
                                  :source    (in-file-path "/weather-test/tmpf_to_sample_lower_res.tif")
                                  :cell-size (m->ft 300)}}
            lookup (cli/create-multiplier-lookup config)]

        (is (= {:temperature 10} lookup))))

    (testing "with multiple weather rasters"
      (let [config {:cell-size         (m->ft 30)
                    :temperature       {:source    (in-file-path "/weather-test/tmpf_to_sample_lower_res.tif")
                                        :cell-size (m->ft 300)}
                    :relative-humidity {:source    (in-file-path "/weather-test/rh_to_sample_lower_res.tif")
                                        :cell-size (m->ft 300)}}
            lookup (cli/create-multiplier-lookup config)]

        (is (= {:temperature       10
                :relative-humidity 10} lookup))))))

(deftest run-simulation-with-landfire-perturbations
  (testing "perturbing canopy height landfire layer"
    (let [config  (merge test-config-base
                         {:perturbations {:canopy-height {:spatial-type :global
                                                          :range [-1.0 1.0]}}})
          results (run-simulation config)]
      (is (every? some? results)))))
