(ns gridfire.postgis-bridge
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.core.matrix :as m])
  (:import (org.postgresql.jdbc4 Jdbc4Array)))

(m/set-current-implementation :vectorz)

(defn postgis-raster-to-matrix
  "Send a SQL query to the database given by db-spec for a raster tile from
   table table-name. Resample the raster to match resolution and set any values
   below threshold to 0. Return the post-processed raster values as a Clojure
   matrix using the core.matrix API."
  [db-spec table-name resolution threshold]
  (let [rescale-query   (if resolution
                          (format "ST_Rescale(rast,%s,-%s,'NearestNeighbor')"
                                  resolution resolution)
                          "rast")
        threshold-query (if threshold
                          (format (str "ST_MapAlgebra(%s,NULL,"
                                       "'CASE WHEN [rast.val] < %s"
                                       " THEN 0.0 ELSE [rast.val] END')")
                                  rescale-query threshold)
                          rescale-query)
        data-query      (format "SELECT ST_DumpValues(%s,1) AS matrix FROM %s"
                                threshold-query table-name)]
    ;; (println data-query)
    (jdbc/with-db-transaction [conn db-spec]
      (->> (jdbc/query conn [data-query])
           first
           :matrix
           (#(.getArray ^Jdbc4Array %))
           (m/emap #(or % -1.0))
           m/matrix))))
