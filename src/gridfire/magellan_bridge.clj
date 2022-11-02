;; [[file:../../org/GridFire.org::*Magellan][Magellan:1]]
(ns gridfire.magellan-bridge
  (:require [clojure.java.io         :as io]
            [magellan.core           :refer [read-raster register-new-crs-definitions-from-properties-file!]]
            [magellan.raster.inspect :as inspect]
            [tech.v3.tensor          :as t])
  (:import org.geotools.coverage.grid.GridGeometry2D
           org.geotools.referencing.operation.transform.AffineTransform2D))

(set! *unchecked-math* :warn-on-boxed)

(defn register-custom-projections! []
  (register-new-crs-definitions-from-properties-file! "CUSTOM" (io/resource "custom_projections.properties")))

(defn geotiff-raster-to-matrix
  "Reads a raster from a file using the magellan.core library. Returns the
   post-processed raster values as a Clojure tensor using the dtype-next/tech.v3.tensor API
   along with all of the georeferencing information associated with this tile in a
   hash-map with the following form:
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
   :matrix #tech.v3.tensor<datatype>[10 534 486]}"
  [file-path]
  (let [raster   (read-raster file-path)
        grid     ^GridGeometry2D (:grid raster)
        r-info   (inspect/describe-raster raster)
        matrix   (inspect/extract-matrix raster)
        image    (:image r-info)
        envelope (:envelope r-info)
        crs2d    ^AffineTransform2D (.getGridToCRS2D grid)]
    {:srid       (:srid r-info)
     :upperleftx (get-in envelope [:x :min])
     :upperlefty (get-in envelope [:y :max])
     :width      (:width image)
     :height     (:height image)
     :scalex     (.getScaleX crs2d)
     :scaley     (.getScaleY crs2d)
     :skewx      0.0 ; FIXME not used?
     :skewy      0.0 ; FIXME not used?
     :numbands   (:bands image)
     :matrix     (t/->tensor matrix)}))

(defn geotiff-raster-to-tensor
  "Reads a raster from a file using the magellan.core library. Returns the
   post-processed raster values as a Clojure tensor using the dtype-next/tech.v3.tensor API
   along with all of the georeferencing information associated with this tile in a
   hash-map with the following form:
  {:upperleftx -321043.875,
   :upperlefty -1917341.5,
   :width 486,
   :height 534,
   :scalex 2000.0,
   :scaley -2000.0,
   :skewx 0.0,
   :skewy 0.0,
   :numbands 10,
   :matrix #tech.v3.tensor<datatype>[10 534 486]}"
  [file-path & [datatype convert-fn]]
  (let [raster   (read-raster file-path)
        grid     ^GridGeometry2D (:grid raster)
        image    (inspect/describe-image (:image raster))
        envelope (inspect/describe-envelope (:envelope raster))
        crs2d    ^AffineTransform2D (.getGridToCRS2D grid)]
    {:upperleftx (get-in envelope [:x :min])
     :upperlefty (get-in envelope [:y :max])
     :width      (:width image)
     :height     (:height image)
     :scalex     (.getScaleX crs2d)
     :scaley     (.getScaleY crs2d)
     :skewx      0.0 ; FIXME not used?
     :skewy      0.0 ; FIXME not used?
     :numbands   (:bands image)
     :matrix     (inspect/extract-tensor raster :datatype datatype :convert-fn convert-fn)}))
;; Magellan:1 ends here
