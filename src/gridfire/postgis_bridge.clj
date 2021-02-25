;; [[file:../../org/GridFire.org::postgis-bridge][postgis-bridge]]
(ns gridfire.postgis-bridge
  (:require [clojure.core.matrix :as m]
            [clojure.java.jdbc   :as jdbc])
  (:import org.postgresql.jdbc.PgArray
           java.util.UUID))

(m/set-current-implementation :vectorz)

(defn extract-matrix [result]
  (->> result
       :matrix
       (#(.getArray ^PgArray %))
       (m/emap #(or % -1.0))
       m/matrix))

(defn build-rescale-query [rescaled-table-name resolution table-name]
  (format (str "CREATE TEMPORARY TABLE %s "
               "ON COMMIT DROP AS "
               "SELECT ST_Rescale(rast,%s,-%s,'NearestNeighbor') AS rast "
               "FROM %s")
          rescaled-table-name
          resolution
          resolution
          table-name))

(defn build-threshold-query [threshold]
  (format (str "ST_MapAlgebra(rast,band,NULL,"
               "'CASE WHEN [rast.val] < %s"
               " THEN 0.0 ELSE [rast.val] END')")
          threshold))

(defn build-data-query [threshold threshold-query metadata table-name]
  (format (str "SELECT ST_DumpValues(%s,%s) AS matrix "
               "FROM generate_series(1,%s) AS band "
               "CROSS JOIN %s")
          (if threshold threshold-query "rast")
          (if threshold 1 "band")
          (:numbands metadata)
          table-name))

(defn build-meta-query [table-name]
  (format "SELECT (ST_Metadata(rast)).* FROM %s" table-name))

(defn postgis-raster-to-matrix
  "Send a SQL query to the PostGIS database given by db-spec for a
  raster tile from table table-name. Optionally resample the raster to
  match resolution and set any values below threshold to 0. Return the
  post-processed raster values as a Clojure matrix using the
  core.matrix API along with all of the georeferencing information
  associated with this tile in a hash-map with the following form:
  {:srid 900916,
   :upperleftx -321043.875,
   :upperlefty -1917341.5,
   :width 486,
   :height 534,
   :scalex 2000.0,
   :scaley -2000.0,
   :skewx 0.0,
   :skewy 0.0,
   :numbands 10,
   :matrix #vectorz/matrix Large matrix with shape: [10,534,486]}"
  [db-spec table-name & [resolution threshold]]
  (jdbc/with-db-transaction [conn db-spec]
    (let [table-name      (if-not resolution
                            table-name
                            (let [rescaled-table-name (str "gridfire_" (subs (str (UUID/randomUUID)) 0 8))
                                  rescale-query       (build-rescale-query rescaled-table-name resolution table-name)]
                              ;; Create a temporary table to hold the rescaled raster.
                              ;; It will be dropped when the transaction completes.
                              (jdbc/db-do-commands conn [rescale-query])
                              rescaled-table-name))
          meta-query      (build-meta-query table-name)
          metadata        (first (jdbc/query conn [meta-query]))
          threshold-query (build-threshold-query threshold)
          data-query      (build-data-query threshold threshold-query metadata table-name)
          matrix          (when-let [results (seq (jdbc/query conn [data-query]))]
                            (m/matrix (mapv extract-matrix results)))]
      (assoc metadata :matrix matrix))))
;; postgis-bridge ends here
