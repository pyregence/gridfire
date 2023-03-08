;; FIXME LP coverage
(ns gridfire.output-test
  (:require [clojure.java.io     :as io]
            [clojure.test        :refer [deftest is use-fixtures]]
            [gridfire.utils.test :as utils]))

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
   :landfire-layers           {:aspect             {:type   :postgis
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
                                                    :source "landfire.slp WHERE rid=1"}}
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
   :output-directory          "test/output"})

;;-----------------------------------------------------------------------------
;; Fixtures
;;-----------------------------------------------------------------------------

(use-fixtures :once utils/with-temp-output-dir)

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(deftest ^:database output_burn_probability_test
  (let [config (merge test-config-base
                      {:output-burn-probability :final})
        _      (utils/run-gridfire! config)]

    (is (.exists (io/file "test/output/burn_probability.tif")))))

(deftest ^:database output_flame_length_sum_test
  (let [config (merge test-config-base
                      {:output-flame-length-sum? true})
        _      (utils/run-gridfire! config)]

    (is (.exists (io/file "test/output/flame_length_sum.tif")))))

(deftest ^:database output_flame_length_max_test
  (let [config (merge test-config-base
                      {:output-flame-length-max? true})
        _      (utils/run-gridfire! config)]

    (is (.exists (io/file "test/output/flame_length_max.tif")))))

(deftest ^:database output_burn_count_test
  (let [config (merge test-config-base
                      {:output-burn-count? true})
        _      (utils/run-gridfire! config)]

    (is (.exists (io/file "test/output/burn_count.tif")))))

(deftest ^:database output_spot_count_test
  (let [config (merge test-config-base
                      {:spotting           {:mean-distance                1.0
                                            :ws-exp                       1.0
                                            :flin-exp                     1.0
                                            :normalized-distance-variance 1.0
                                            :crown-fire-spotting-percent  1.0
                                            :num-firebrands               1.0
                                            :decay-constant               1.0
                                            :surface-fire-spotting
                                            {:spotting-percent             [[[1 204] [0.001 0.003]]]
                                             :critical-fire-line-intensity 1.0}}
                       :output-spot-count? true})
        _      (utils/run-gridfire! config)]

    (is (.exists (io/file "test/output/spot_count.tif")))))
