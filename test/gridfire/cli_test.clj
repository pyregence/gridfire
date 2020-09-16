(ns gridfire.cli-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.jdbc :as jdbc]
            [clojure.java.io :as io]
            [clojure.core.matrix :as m]
            [gridfire.cli :refer [fetch-landfire-layers get-envelope]])
  (:import (com.opentable.db.postgres.embedded EmbeddedPostgres
                                               DatabasePreparer
                                               PgBinaryResolver)
           (org.springframework.core.io ClassPathResource)))

;;-----------------------------------------------------------------------------
;; Config
;;-----------------------------------------------------------------------------

(def resources-path "test/gridfire/resources/")

(def db-spec {:classname   "org.postgresql.Driver"
              :subprotocol "postgresql"
              :subname     "//localhost:5432/gridfire_test"
              :user        "postgres"
              :password    "password"})

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

(defn in-file-path [filename]
  (str resources-path filename))

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(deftest fetch-landfire-layers-test
  (testing "fetching layers from postgis and geotiff files"
    (let [postgis-config {:db-spec            db-spec
                          :srid               "CUSTOM:900914"
                          :layer-tables       {:aspect             "landfire.asp WHERE rid=1"
                                               :canopy-base-height "landfire.cbh WHERE rid=1"
                                               :canopy-cover       "landfire.cc WHERE rid=1"
                                               :canopy-height      "landfire.ch WHERE rid=1"
                                               :crown-bulk-density "landfire.cbd WHERE rid=1"
                                               :elevation          "landfire.dem WHERE rid=1"
                                               :fuel-model         "landfire.fbfm40 WHERE rid=1"
                                               :slope              "landfire.slp WHERE rid=1"}
                          :fetch-layer-method :postgis}
          geotiff-config {:db-spec            db-spec
                          :layer-files        {:aspect             (in-file-path "asp.tif")
                                               :canopy-base-height (in-file-path "cbh.tif")
                                               :canopy-cover       (in-file-path "cc.tif")
                                               :canopy-height      (in-file-path "ch.tif")
                                               :crown-bulk-density (in-file-path "cbd.tif")
                                               :elevation          (in-file-path "dem.tif")
                                               :fuel-model         (in-file-path "fbfm40.tif")
                                               :slope              (in-file-path "slp.tif")}
                          :fetch-layer-method :geotiff}
          postgis        (fetch-landfire-layers postgis-config)
          geotiff        (fetch-landfire-layers geotiff-config)]

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

      (let [postgis-envelope (get-envelope postgis-config postgis)
            geotiff-envelope (get-envelope geotiff-config geotiff)]
        (is (= postgis-envelope geotiff-envelope))

        (is (.equals postgis-envelope geotiff-envelope))
        ))))
