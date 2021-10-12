(ns gridfire.output-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [gridfire.core :refer [process-config-file!]]
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
;; Utils
;;-----------------------------------------------------------------------------

(deftest output_burn_probability_test
  (let [config (merge test-config-base
                      {:output-burn-probability :final})
        _      (process-config-file! config)]

    (is (.exists (io/file "test/output/burn_probability.tif")))))

(deftest output_flame_length_sum_test
  (let [config (merge test-config-base
                      {:output-flame-length-sum true})
        _      (process-config-file! config)]

    (is (.exists (io/file "test/output/flame_length_sum.tif")))))
