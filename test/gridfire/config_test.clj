(ns gridfire.config-test
  (:require [gridfire.config :as config]
            [gridfire.crown-fire :refer [m->ft]]
            [clojure.test :refer [deftest is]]))

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

    (is (= {:canopy-bulk-density {:spatial-type :global :pdf-min -0.05 :pdf-max 0.05}
            :canopy-base-height  {:spatial-type :global :pdf-min -2.0 :pdf-max 2.0}
            :canopy-cover        {:spatial-type :global :pdf-min -0.05 :pdf-max 0.05}
            :canopy-height       {:spatial-type :global :pdf-min -5.0 :pdf-max 5.0}}
           results))))

(deftest read-data-test
  (let [config (config/build-edn (->> (in-file-path "sample-elmfire.data")
                                      slurp
                                      config/parse)
                                 nil)]

    (is (= config {:landfire-layers           {:aspect             {:type   :geotiff
                                                                    :source "./fuels_and_topography/asp.tif"}
                                               :canopy-base-height {:type   :geotiff
                                                                    :source "./fuels_and_topography/cbh.tif"}
                                               :canopy-cover       {:type   :geotiff
                                                                    :source "./fuels_and_topography/cc.tif"}
                                               :canopy-height      {:type   :geotiff
                                                                    :source "./fuels_and_topography/ch.tif"}
                                               :crown-bulk-density {:type   :geotiff
                                                                    :source "./fuels_and_topography/cbd.tif"}
                                               :fuel-model         {:type   :geotiff
                                                                    :source "./fuels_and_topography/fbfm40.tif"}
                                               :slope              {:type   :geotiff
                                                                    :source "./fuels_and_topography/slp.tif"}
                                               :elevation          {:type   :geotiff
                                                                    :source "./fuels_and_topography/dem.tif"}}
                   :cell-size                 (m->ft 30.0)
                   :ignition-layer            {:type   :geotiff
                                               :source "./fuels_and_topography/phi.tif"}
                   :max-runtime               3
                   :temperature               {:type   :geotiff
                                               :source "./fuels_and_topography/tmpf_to_sample.tif"}
                   :relative-humidity         {:type   :geotiff
                                               :source "./fuels_and_topography/rh_to_sample.tif"}
                   :wind-speed-20ft           {:type   :geotiff
                                               :source "./fuels_and_topography/ws_to_sample.tif"}
                   :wind-from-direction       {:type   :geotiff
                                               :source "./fuels_and_topography/wd_to_sample.tif"}
                   :ellipse-adjustment-factor 1.0
                   :foliar-moisture           90.0
                   :simulations               1000
                   :random-seed               2020
                   :outfile-suffix            ""
                   :output-geotiffs?          false
                   :output-csvs?              false
                   :output-pngs?              false
                   :output-landfire-inputs?   false
                   :perturbations             {:canopy-bulk-density {:spatial-type :global
                                                                     :range        [-0.05 0.05]}
                                               :canopy-base-height  {:spatial-type :global
                                                                     :range        [-2.0 2.0]}
                                               :canopy-cover        {:spatial-type :global
                                                                     :range        [-0.05 0.05]}
                                               :canopy-height       {:spatial-type :global
                                                                     :range        [-5.0 5.0]}}}))))
