(ns gridfire.random-ignition-test
  (:require [gridfire.random-ignition :as random-ignition]
            [clojure.test :refer [deftest is]]
            [gridfire.magellan-bridge :refer [geotiff-raster-to-matrix]]
            [gridfire.utils.test :as utils])
  (:import java.util.Random))

;;-----------------------------------------------------------------------------
;; Config
;;-----------------------------------------------------------------------------

(def db-spec {:classname   "org.postgresql.Driver"
              :subprotocol "postgresql"
              :subname     "//localhost:5432/gridfire_test"
              :user        "gridfire_test"
              :password    "gridfire_test"})

(def resources-dir "test/gridfire/resources/weather-test")

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(deftest ignition-mask-test
  (let [config            {:db-spec         db-spec
                           :cell-size       98.425
                           :random-ignition {:ignition-mask {:type   :geotiff
                                                             :source (utils/in-file-path resources-dir "ignition_mask.tif")}}
                           :rand-gen        (Random.)}
        fuel-model-matrix (first (:matrix (geotiff-raster-to-matrix (utils/in-file-path resources-dir "fbfm40.tif"))))]

    (is (some? (random-ignition/ignition-site config fuel-model-matrix)))))

(deftest buffer-edge-mask-test
  (let [config            {:db-spec         db-spec
                           :cell-size       98.425
                           :random-ignition {:edge-buffer 39271.575}
                           :rand-gen        (Random.)}
        fuel-model-matrix (first (:matrix (geotiff-raster-to-matrix (utils/in-file-path resources-dir "fbfm40.tif"))))
        [row col]         (random-ignition/ignition-site config fuel-model-matrix)]

    (is (and (>= row 400) (<= row 401)))

    (is (and (>= col 400) (<= col 401)))))
