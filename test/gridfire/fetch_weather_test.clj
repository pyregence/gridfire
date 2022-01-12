(ns gridfire.fetch-weather-test
  (:require [clojure.core.matrix :as m]
            [clojure.java.jdbc   :as jdbc]
            [clojure.test        :refer [deftest is]]
            [gridfire.fetch      :as fetch]
            [gridfire.inputs     :as inputs]
            [magellan.core       :refer [read-raster]])
  (:import java.util.Random))

;;-----------------------------------------------------------------------------
;; Config
;;-----------------------------------------------------------------------------

(def resources-path "test/gridfire/resources/weather-test/")

(def db-spec {:classname   "org.postgresql.Driver"
              :subprotocol "postgresql"
              :subname     "//localhost:5432/gridfire_test"
              :user        "gridfire_test"
              :password    "gridfire_test"})

(def test-config-base
  {:db-spec     db-spec
   :simulations 1
   :random-seed 1234567890
   :rand-gen    (Random. 1234567890)})

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

(defn in-file-path [filename]
  (str resources-path filename))

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(deftest ^:database fetch-temperature-test
  (let [geotiff-file    "tmpf_to_sample.tif"
        geotiff-config  (merge test-config-base
                               {:temperature {:type   :geotiff
                                              :source (in-file-path geotiff-file)}})
        postgis-table   "weather.tmpf WHERE rid=1"
        postgis-config  (merge test-config-base
                               {:temperature {:type   :postgis
                                              :source postgis-table}})
        geotiff-results (:matrix (fetch/weather-layer geotiff-config :temperature))
        postgis-results (:matrix (fetch/weather-layer postgis-config :temperature))]

    (is (every? m/matrix? geotiff-results))

    (is (every? m/matrix? postgis-results))

    (is (= geotiff-results postgis-results))

    (let [numbands (count (:bands (read-raster (in-file-path geotiff-file))))]
      (is (= numbands (m/dimension-count geotiff-results 0))))

    (let [results  (jdbc/with-db-connection [conn (:db-spec test-config-base)]
                     (jdbc/query conn [(str "SELECT (ST_Metadata(rast)).numbands FROM " postgis-table)]))
          numbands (:numbands (first results))]
      (is (= numbands (m/dimension-count geotiff-results 0))))))

(deftest ^:database fetch-relative-humidity-test
  (let [geotiff-file    "rh_to_sample.tif"
        geotiff-config  (merge test-config-base
                               {:relative-humidity {:type   :geotiff
                                                    :source (in-file-path geotiff-file)}})
        postgis-table   "weather.rh WHERE rid=1"
        postgis-config  (merge test-config-base
                               {:relative-humidity {:type   :postgis
                                                    :source postgis-table}})
        geotiff-results (:matrix (fetch/weather-layer geotiff-config :relative-humidity))
        postgis-results (:matrix (fetch/weather-layer postgis-config :relative-humidity))]

    (is (every? m/matrix? geotiff-results))

    (is (every? m/matrix? postgis-results))

    (is (= geotiff-results postgis-results))

    (let [numbands (count (:bands (read-raster (in-file-path geotiff-file))))]
      (is (= numbands (m/dimension-count geotiff-results 0))))

    (let [results  (jdbc/with-db-connection [conn (:db-spec test-config-base)]
                     (jdbc/query conn [(str "SELECT (ST_Metadata(rast)).numbands FROM " postgis-table)]))
          numbands (:numbands (first results))]
      (is (= numbands (m/dimension-count postgis-results 0))))))

(deftest ^:database fetch-wind-speed-20ft-test
  (let [geotiff-file    "ws_to_sample.tif"
        geotiff-config  (merge test-config-base
                               {:wind-speed-20ft {:type   :geotiff
                                                  :source (in-file-path geotiff-file)}})
        postgis-table   "weather.ws WHERE rid=1"
        postgis-config  (merge test-config-base
                               {:wind-speed-20ft {:type   :postgis
                                                  :source postgis-table}})
        geotiff-results (:matrix (fetch/weather-layer geotiff-config :wind-speed-20ft))
        postgis-results (:matrix (fetch/weather-layer postgis-config :wind-speed-20ft))]

    (is (every? m/matrix? geotiff-results))

    (is (every? m/matrix? postgis-results))

    (is (= geotiff-results postgis-results))

    (let [numbands (count (:bands (read-raster (in-file-path geotiff-file))))]
      (is (= numbands (m/dimension-count geotiff-results 0))))

    (let [results  (jdbc/with-db-connection [conn (:db-spec test-config-base)]
                     (jdbc/query conn [(str "SELECT (ST_Metadata(rast)).numbands FROM " postgis-table)]))
          numbands (:numbands (first results))]
      (is (= numbands (m/dimension-count postgis-results 0))))))

(deftest ^:database fetch-wind-from-direction-test
  (let [geotiff-file    "wd_to_sample.tif"
        geotiff-config  (merge test-config-base
                               {:wind-from-direction {:type   :geotiff
                                                      :source (in-file-path geotiff-file)}})
        postgis-table   "weather.wd WHERE rid=1"
        postgis-config  (merge test-config-base
                               {:wind-from-direction {:type   :postgis
                                                      :source postgis-table}})
        geotiff-results (:matrix (fetch/weather-layer geotiff-config :wind-from-direction))
        postgis-results (:matrix (fetch/weather-layer postgis-config :wind-from-direction))]

    (is (every? m/matrix? geotiff-results))

    (is (every? m/matrix? postgis-results))

    (is (= geotiff-results postgis-results))

    (let [numbands (count (:bands (read-raster (in-file-path geotiff-file))))]
      (is (= numbands (m/dimension-count geotiff-results 0))))

    (let [results  (jdbc/with-db-connection [conn (:db-spec test-config-base)]
                     (jdbc/query conn [(str "SELECT (ST_Metadata(rast)).numbands FROM " postgis-table)]))
          numbands (:numbands (first results))]
      (is (= numbands (m/dimension-count postgis-results 0))))))

(deftest ^:database get-weather-from-range-test
  (let [config  (assoc test-config-base
                       :temperature [0 100]
                       :simulations 10)
        results (inputs/get-weather config :temperature)]

    (is (vector results))

    (is (every? int? results))))

(deftest ^:database get-weather-from-list-test
  (let [tmp-list (list 0 10 20 30)
        config   (assoc test-config-base
                        :temperature tmp-list
                        :simulations 10)
        results  (inputs/get-weather config :temperature)]

    (is (vector results))

    (is (every? int? results))

    (is (= (set results) (set tmp-list)))))

(deftest ^:database get-weather-scalar-from-test
  (let [config  (assoc test-config-base
                       :temperature 42
                       :simulations 10)
        results (inputs/get-weather config :temperature)]

    (is (vector results))

    (is (every? int? results))

    (is (apply = results))))
