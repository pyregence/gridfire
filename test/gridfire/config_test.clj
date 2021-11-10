(ns gridfire.config-test
  (:require [clojure.spec.alpha   :as s]
            [clojure.test         :refer [deftest is use-fixtures]]
            [gridfire.config      :as config]
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

(defn with-elmfire-directory-binding [test-fn]
  (binding [gridfire.config/*elmfire-directory-path* "./test/gridfire/resources/config_test"]
   (test-fn)))

(use-fixtures :once with-elmfire-directory-binding)

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(deftest convert-test
  (is (= 42 (config/convert-val "42")))

  (is (= 42.0 (config/convert-val "42.0")))

  (is (= 42.0 (config/convert-val "42.")))

  (is (= -42.0 (config/convert-val "-42.0")))

  (is (= true (config/convert-val ".TRUE.")))

  (is (= false (config/convert-val ".FALSE.")))

  (is (= "some/directory" (config/convert-val"'some/directory'"))))

(deftest misc-params-test
  (let [config (config/build-edn (->> (slurp (in-file-path "sample-elmfire.data"))
                                      config/parse-elmfire))]
    (is (s/valid? ::spec/config config))

    (is (= 98.43 (:cell-size config)))

    (is (= "EPSG:32610" (:srid config)))

    (is (= 10 (:simulations config)))

    (is (= 2021 (:random-seed config)))

    (is (= 90.0 (:foliar-moisture config)))

    (is (= 1.0 (:ellipse-adjustment-factor config)))

    (is (= :between-fires (:parallel-strategy config)))

    (is (= :sum (:fractional-distance-combination config)))))

;;-----------------------------------------------------------------------------
;; Landfire Layers
;;-----------------------------------------------------------------------------

(deftest process-landfire-layers-test
  (binding [gridfire.config/*elmfire-directory-path* "./test/gridfire/resources/config_test"]
   (let [data    (->> (in-file-path "sample-elmfire.data")
                      slurp
                      config/parse-elmfire)
         results (:landfire-layers (config/process-landfire-layers data {}))]

     (is (s/valid? ::spec/landfire-layers results)))))


;;-----------------------------------------------------------------------------
;; Ignition
;;-----------------------------------------------------------------------------

(deftest process-ignition-test
  (let [data    (->> (in-file-path "sample-elmfire.data")
                     slurp
                     config/parse-elmfire)
        results (:random-ignition (config/process-ignition data {}))]

    (is (= {:edge-buffer 98.43}
           results))))


;;-----------------------------------------------------------------------------
;; Weather
;;-----------------------------------------------------------------------------

(deftest process-weather-test
  (let [data    (->> (in-file-path "sample-elmfire.data")
                     slurp
                     config/parse-elmfire)
        results (config/process-weather data {})]

    (is (s/valid? ::spec/temperature (:temperature results)))

    (is (s/valid? ::spec/relative-humidity (:relative-humidity results)))

    (is (s/valid? ::spec/wind-speed-20ft (:wind-speed-20ft results)))

    (is (s/valid? ::spec/wind-from-direction (:wind-from-direction results)))

    (is (= {:type :geotiff :source "test/gridfire/resources/config_test/weather/tmpf.tif"} (:temperature results)))

    (is (= {:type :geotiff :source "test/gridfire/resources/config_test/weather/rh.tif"} (:relative-humidity results)))

    (is (= {:type :geotiff :source "test/gridfire/resources/config_test/weather/ws.tif"} (:wind-speed-20ft results)))

    (is (= {:type :geotiff :source "test/gridfire/resources/config_test/weather/wd.tif"} (:wind-from-direction results)))))


;;-----------------------------------------------------------------------------
;; Output
;;-----------------------------------------------------------------------------

(deftest process-output-test
  (let [data    (->> (in-file-path "sample-elmfire.data")
                     slurp
                     config/parse-elmfire)
        results (config/process-output data {})]

    (is (= "" (:outfile-suffix results)))

    (is (false? (:output-landfire-inputs? results)))

    (is (false? (:output-geotiffs? results)))

    (is (true?  (:output-binary? results)))

    (is (false?  (:output-pngs? results)))

    (is (true?  (:output-csvs? results)))

    (is (= "test/gridfire/resources/config_test/outputs"  (:output-directory results)))))


;;-----------------------------------------------------------------------------
;; Perturbations
;;-----------------------------------------------------------------------------

(deftest extract-perturbations-test
  (let [config  (->> (in-file-path "sample-elmfire.data")
                     slurp
                     config/parse-elmfire)
        results (config/extract-perturbations config)]

    (is (s/valid? ::spec/perturbations results))

    (is (= {:wind-speed-20ft {:spatial-type :global :range [-2.0 2.0]}
            :wind-direction  {:spatial-type :global :range [-7.5 7.5]}
            :canopy-cover    {:spatial-type :global :range [-0.05 0.05]}
            :canopy-height   {:spatial-type :global :range [-5.0 5.0]
                              :units        :metric}}
           results))))


;;-----------------------------------------------------------------------------
;; Spotting
;;-----------------------------------------------------------------------------

(deftest extract-num-firbrands-test
  (let [config  (->> (in-file-path "sample-elmfire.data")
                     slurp
                     config/parse-elmfire)
        results (config/extract-num-firebrands config)]

    (is (= {:lo 1
            :hi [1 2]}
           results))))

(deftest extract-crown-fire-spotting-percent-test
  (let [config  (->> (in-file-path "sample-elmfire.data")
                     slurp
                     config/parse-elmfire)
        results (config/extract-crown-fire-spotting-percent config)]

    (is (= [0.005 0.02]
           results))))

(deftest process-spotting-test
  (let [data    (->> (in-file-path "sample-elmfire.data")
                     slurp
                     config/parse-elmfire)
        results (:spotting (config/process-spotting data {}))]

    (is (s/valid? ::spec/spotting results))

    (is (= {:mean-distance                {:lo 5.0 :hi 15.0}
            :ws-exp                       {:lo 0.4 :hi 0.7}
            :flin-exp                     {:lo 0.2 :hi 0.4}
            :normalized-distance-variance {:lo 250.0 :hi 600.0}
            :crown-fire-spotting-percent  [0.005 0.02]
            :num-firebrands               {:lo 1 :hi [1 2]}
            :decay-constant               0.005
            :surface-fire-spotting        {:spotting-percent             [[[1 204] [0.001 0.003]]]
                                           :critical-fire-line-intensity 288.879425327306}}
           results))))

;;-----------------------------------------------------------------------------
;; Fuel Moisture
;;-----------------------------------------------------------------------------

(deftest process-fuel-moisture-test
  (let [data    (->> (in-file-path "sample-elmfire.data")
                     slurp
                     config/parse-elmfire)
        results (:fuel-moisture-layers (config/process-fuel-moisture-layers data {}))]

    (is (s/valid? ::spec/fuel-moisture-layers results))

    (is (= {:dead {:1hr   {:type :geotiff :source "test/gridfire/resources/config_test/weather/m1.tif"}
                   :10hr  {:type :geotiff :source "test/gridfire/resources/config_test/weather/m10.tif"}
                   :100hr {:type :geotiff :source "test/gridfire/resources/config_test/weather/m100.tif"}}
            :live {:woody      80.0
                   :herbaceous 30.0}}
           results))))
