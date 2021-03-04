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

    (is (= {:wind-speed-20ft    {:spatial-type :global :range [-2.0 2.0]}
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

    (is (= config {:srid                      "EPSG:32611"
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
                   :random-ignition           {:ignition-mask {:type   :geotiff
                                                               :source "/fuels_and_topography/ignition_mask.tif"}
                                               :edge-buffer   98.43}
                   :temperature               {:type   :geotiff
                                               :source "/weather/tmpf.tif"}
                   :relative-humidity         {:type   :geotiff
                                               :source "/weather/rh.tif"}
                   :wind-speed-20ft           {:type   :geotiff
                                               :source "/weather/ws.tif"}
                   :wind-from-direction       {:type   :geotiff
                                               :source "/weather/wd.tif"}
                   :perturbations             {:wind-speed-20ft    {:spatial-type :global
                                                                    :range        [-2.0 2.0]}
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
                   :spotting                  {:ambient-gas-density         1.1,
                                               :crown-fire-spotting-percent [0.5 2.0],
                                               :num-firebrands              {:lo 1, :hi [1 2]},
                                               :specific-heat-gas           1121.0,
                                               :surface-fire-spotting
                                               {:spotting-percent             [[[1 204] [0.1 0.3]]],
                                                :critical-fire-line-intensity 1000.0}}
                   :fuel-moisture-layers      {:dead
                                               {:1hr   {:type :geotiff :source "/weather/m1.tif"}
                                                :10hr  {:type :geotiff :source "/weather/m10.tif"}
                                                :100hr {:type :geotiff :source "/weather/m100.tif"}}
                                               :live
                                               {:woody      {:type :geotiff :source "/weather/mlw.tif"}
                                                :herbaceous {:type :geotiff :source "/weather/mlh.tif"}}}
                   :ellipse-adjustment-factor 1.0
                   :max-runtime               4320
                   :random-seed               2021
                   :simulations               1000
                   :foliar-moisture           90.0
                   :outfile-suffix            ""
                   :output-csvs?              true
                   :output-pngs?              false
                   :output-geotiffs?          false
                   :output-binary?            true
                   :output-landfire-inputs?   false
                   :output-directory          "/outputs"})

        (is (s/valid? ::spec/config config)))))

(deftest extract-num-firbrands-test
  (let [config  (->> (in-file-path "sample-elmfire.data")
                     slurp
                     config/parse)
        results (config/extract-num-firebrands config)]

    (is (= {:lo 1
            :hi [1 2]}
           results))))

(deftest extract-crown-fire-spotting-percent-test
  (let [config  (->> (in-file-path "sample-elmfire.data")
                     slurp
                     config/parse)
        results (config/extract-crown-fire-spotting-percent config)]

    (is (= [0.5 2.0]
           results))))
