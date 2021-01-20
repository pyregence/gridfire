(ns gridfire.ignition-test
  (:require [clojure.test :refer [deftest is testing]]
            [gridfire.cli :as gf]
            [gridfire.fetch :as fetch])
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
   :fetch-layer-method        :geotiff
   :landfire-layers           {:aspect             "test/gridfire/resources/asp.tif"
                               :canopy-base-height "test/gridfire/resources/cbh.tif"
                               :canopy-cover       "test/gridfire/resources/cc.tif"
                               :canopy-height      "test/gridfire/resources/ch.tif"
                               :crown-bulk-density "test/gridfire/resources/cbd.tif"
                               :elevation          "test/gridfire/resources/dem.tif"
                               :fuel-model         "test/gridfire/resources/fbfm40.tif"
                               :slope              "test/gridfire/resources/slp.tif"}
   :srid                      "CUSTOM:900914"
   :cell-size                 98.425     ; (feet)
   :max-runtime               60         ; (minutes)
   :temperature               '(50)      ; (degrees Fahrenheit)
   :relative-humidity         '(1)       ; (%)
   :wind-speed-20ft           '(10)      ; (miles/hour)
   :wind-from-direction       '(0)       ; (degrees clockwise from north)
   :foliar-moisture           90         ; (%)
   :ellipse-adjustment-factor 1.0        ; (< 1.0 = more circular, > 1.0 = more elliptical)
   :simulations               1
   :random-seed               1234567890 ; long value (optional)
   :output-csvs?              true})

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

(defn in-file-path [filename]
  (str resources-path filename))

(defn run-simulation [config]
  (let [simulations      (:simulations config)
        rand-generator   (if-let [seed (:random-seed config)]
                           (Random. seed)
                           (Random.))
        landfire-layers  (fetch/landfire-layers config)
        landfire-rasters (into {} (map (fn [[layer-name info]] [layer-name (first (:matrix info))])) landfire-layers)
        ignition-layer   (fetch/ignition-layer config)]
    (gf/run-simulations
     simulations
     landfire-rasters
     (gf/get-envelope config landfire-layers)
     (:cell-size config)
     (gf/draw-samples rand-generator simulations (:ignition-row config))
     (gf/draw-samples rand-generator simulations (:ignition-col config))
     (gf/draw-samples rand-generator simulations (:max-runtime config))
     (gf/draw-samples rand-generator simulations (:temperature config))
     (gf/draw-samples rand-generator simulations (:relative-humidity config))
     (gf/draw-samples rand-generator simulations (:wind-speed-20ft config))
     (gf/draw-samples rand-generator simulations (:wind-from-direction config))
     (gf/draw-samples rand-generator simulations (:foliar-moisture config))
     (gf/draw-samples rand-generator simulations (:ellipse-adjustment-factor config))
     (:outfile-suffix config)
     (:output-geotiffs? config)
     (:output-pngs? config)
     (:output-csvs? config)
     ignition-layer)))

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(deftest fetch-ignition-layer-test
  (testing "Fetching ignition layer from postgis and geotiff file"
    (let [geotiff-config         (merge test-config-base
                                        {:fetch-ignition-method :geotiff
                                         :ignition-layer        (in-file-path "ign.tif")})
          postgis-config         (merge test-config-base
                                        {:fetch-ignition-method :postgis
                                         :ignition-layer        "ignition.ign WHERE rid=1"})
          geotiff-ignition-layer (fetch/ignition-layer postgis-config)
          postgis-ignition-layer (fetch/ignition-layer geotiff-config)]

      (is (= (:matrix geotiff-ignition-layer)
             (:matrix postgis-ignition-layer))))))

(deftest omit-ignition-method-test
  (testing "Omitting fetch-ignition-method key in config"
    (let [geotiff-config         (merge test-config-base
                                        {:ignition-layer (in-file-path "ign.tif")})
          geotiff-ignition-layer (fetch/ignition-layer geotiff-config)]

      (is (nil? geotiff-ignition-layer)))))

(deftest geotiff-ignition-test
  (testing "Running simulation with ignition layer read from geotiff file"
    (let [geotiff-config (merge test-config-base
                                {:fetch-ignition-method :geotiff
                                 :ignition-layer        (in-file-path "ign.tif")})
          results        (run-simulation geotiff-config)]

      (is (every? some? results)))))

(deftest postgis-ignition-test
  (testing "Running simulation with ignition layer read from geotiff file"
    (let [postgis-config (merge test-config-base
                                {:db-spec               db-spec
                                 :fetch-ignition-method :postgis
                                 :ignition-layer        "ignition.ign WHERE rid=1"})
          results        (run-simulation postgis-config)]
      (is (every? some? results)))))
