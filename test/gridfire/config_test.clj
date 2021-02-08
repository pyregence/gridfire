(ns gridfire.config-test
  (:require [gridfire.config :as config]
            [gridfire.crown-fire :refer [m->ft]]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is]]
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

(deftest extract-perturbations-test
  (let [config  (->> (in-file-path "sample-elmfire.data")
                     slurp
                     config/parse)
        results (config/extract-perturbations config)]

    (is (= {:wind-speed-20ft    {:spatial-type :global :range [-1.0 1.0]}
            :wind-direction     {:spatial-type :global :range [-7.5 7.5]}
            :crown-bulk-density {:spatial-type :global :range [-0.05 0.05]}
            :canopy-base-height {:spatial-type :global :range [-2.0 2.0]}
            :canopy-cover       {:spatial-type :global :range [-0.05 0.05]}
            :canopy-height      {:spatial-type :global :range [-5.0 5.0]}}
           results))))

;; TODO break up into smaller tests
(deftest read-data-test
  (let [config (config/build-edn (->> (in-file-path "sample-elmfire.data")
                                      slurp
                                      config/parse)
                                 nil)]

    (is (= config {:srid                      "EPSG:32610"
                   :cell-size                 98.43
                   :landfire-layers           {:aspect             {:type   :geotiff
                                                                    :source "/fuels_and_topography/asp.tif"}
                                               :canopy-base-height {:type   :geotiff
                                                                    :source "/fuels_and_topography/cbh.tif"}
                                               :canopy-cover       {:type   :geotiff
                                                                    :source "/fuels_and_topography/cc.tif"}
                                               :canopy-height      {:type   :geotiff
                                                                    :source "/fuels_and_topography/ch.tif"}
                                               :crown-bulk-density {:type   :geotiff
                                                                    :source "/fuels_and_topography/cbd.tif"}
                                               :elevation          {:type   :geotiff
                                                                    :source "/fuels_and_topography/dem.tif"}
                                               :fuel-model         {:type   :geotiff
                                                                    :source "/fuels_and_topography/fbfm40.tif"}
                                               :slope              {:type   :geotiff
                                                                    :source "/fuels_and_topography/slp.tif"}}
                   :ignition-layer            {:type        :geotiff
                                               :source      "/fuels_and_topography/phi.tif"
                                               :burn-values {:burned -1.0, :unburned 1.0}}
                   :temperature               {:type   :geotiff
                                               :source "/weather/tmpf_to_sample.tif"}
                   :relative-humidity         {:type   :geotiff
                                               :source "/weather/rh_to_sample.tif"}
                   :wind-speed-20ft           {:type   :geotiff
                                               :source "/weather/ws_to_sample.tif"}
                   :wind-from-direction       {:type   :geotiff
                                               :source "/weather/wd_to_sample.tif"}
                   :perturbations             {:wind-speed-20ft    {:spatial-type :global
                                                                    :range        [-1.0 1.0]}
                                               :wind-direction     {:spatial-type :global
                                                                    :range        [-7.5 7.5]}
                                               :crown-bulk-density {:spatial-type :global
                                                                    :range        [-0.05 0.05]}
                                               :canopy-base-height {:spatial-type :global
                                                                    :range        [-2.0 2.0]}
                                               :canopy-cover       {:spatial-type :global
                                                                    :range        [-0.05 0.05]}
                                               :canopy-height      {:spatial-type :global
                                                                    :range        [-5.0 5.0]}}
                   :burn-probability          60
                   :ellipse-adjustment-factor 1.0
                   :max-runtime               4320
                   :random-seed               2020
                   :simulations               1000
                   :foliar-moisture           90.0
                   :outfile-suffix            ""
                   :output-csvs?              false
                   :output-pngs?              false
                   :output-geotiffs?          true
                   :output-landfire-inputs?   false}))

    (is (s/valid? ::spec/config config))))
