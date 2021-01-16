;; [[file:../../org/GridFire.org::*Section 2: Ignition data from which to build simulation inputs][Section 2: Ignition data from which to build simulation inputs:4]]
(ns gridfire.fetch
  (:require
   [clojure.string :as s]
   [clojure.core.matrix :as m]
   [gridfire.magellan-bridge :refer [geotiff-raster-to-matrix
                                     geotiff-raster-to-matrix-multiband]]
   [gridfire.postgis-bridge :refer [postgis-raster-to-matrix
                                    postgis-raster-to-matrix-multiband]]
   [gridfire.surface-fire :refer [degrees-to-radians]]))

;;-----------------------------------------------------------------------------
;; LANDFIRE
;;-----------------------------------------------------------------------------

(def layer-names
  [:aspect
   :canopy-base-height
   :canopy-cover
   :canopy-height
   :crown-bulk-density
   :elevation
   :fuel-model
   :slope])

(defn convert-metrics
  "Converting metrics in layers:
  meters to feet
  degrees to percent"
  [landfire-layers]
  (-> landfire-layers
      (update-in [:elevation :matrix]
                 (fn [matrix] (m/emap #(* % 3.28) matrix))) ; m -> ft
      (update-in [:slope :matrix]
                 (fn [matrix] (m/emap #(Math/tan (degrees-to-radians %)) matrix))) ; degrees -> %
      (update-in [:canopy-height :matrix]
                 (fn [matrix] (m/emap #(* % 3.28) matrix))) ; m -> ft
      (update-in [:canopy-base-height :matrix]
                 (fn [matrix] (m/emap #(* % 3.28) matrix))) ; m -> ft
      (update-in [:crown-bulk-density :matrix]
                 (fn [matrix] (m/emap #(* % 0.0624) matrix))))) ; kg/m^3 -> lb/ft^3

(defmulti landfire-layers
  "Returns a map of LANDFIRE rasters (represented as maps) with the following units:
   {:elevation          feet
    :slope              vertical feet/horizontal feet
    :aspect             degrees clockwise from north
    :fuel-model         fuel model numbers 1-256
    :canopy-height      feet
    :canopy-base-height feet
    :crown-bulk-density lb/ft^3
    :canopy-cover       % (0-100)}"
  (fn [config]
    (:fetch-layer-method config)))

(defmethod landfire-layers :postgis
  [{:keys [db-spec] :as config}]
  (convert-metrics
   (let [tables (:landfire-layers config)]
     (reduce (fn [amap layer-name]
               (let [table (get tables layer-name)]
                 (assoc amap layer-name
                        (postgis-raster-to-matrix db-spec table))))
             {}
             layer-names))))

(defmethod landfire-layers :geotiff
  [config]
  (convert-metrics
   (let [file-names (:landfire-layers config)]
    (reduce (fn [amap layer-name]
              (let [file-name (get file-names layer-name)]
                (assoc amap layer-name
                       (geotiff-raster-to-matrix file-name))))
            {}
            layer-names))))

;;-----------------------------------------------------------------------------
;; Initial Ignition
;;-----------------------------------------------------------------------------

(defmulti ignition-layer
  (fn [config]
    (:fetch-ignition-method config)))

(defmethod ignition-layer :postgis
  [{:keys [db-spec] :as config}]
  (postgis-raster-to-matrix db-spec (:ignition-layer config)))

(defmethod ignition-layer :geotiff
  [config]
  (geotiff-raster-to-matrix (:ignition-layer config)))

(defmethod ignition-layer :default
  [_]
  nil)

;;-----------------------------------------------------------------------------
;; Weather
;;-----------------------------------------------------------------------------

(defmulti weather
  (fn [config type]
    (let [stype  (name type)
          method ((keyword (s/join "-" ["fetch" stype "method"])) config)]
      (keyword (str (name method) "-" stype)))))

(defmethod weather :postgis-temperature
  [{:keys [temperature db-spec] :as config} type]
  (:matrix (postgis-raster-to-matrix-multiband db-spec temperature)))

(defmethod weather :geotiff-temperature
  [{:keys [temperature] :as config} type]
  (:matrix (geotiff-raster-to-matrix-multiband temperature)))

(defmethod weather :postgis-relative-humidity
  [{:keys [relative-humidity db-spec] :as config} type]
  (:matrix (postgis-raster-to-matrix-multiband db-spec relative-humidity)))

(defmethod weather :geotiff-relative-humidity
  [{:keys [relative-humidity] :as config} type]
  (:matrix (geotiff-raster-to-matrix-multiband relative-humidity)))

(defmethod weather :postgis-wind-speed-20ft
  [{:keys [wind-speed-20ft db-spec] :as config} type]
  (:matrix (postgis-raster-to-matrix-multiband db-spec wind-speed-20ft)))

(defmethod weather :geotiff-wind-speed-20ft
  [{:keys [wind-speed-20ft] :as config} type]
  (:matrix (geotiff-raster-to-matrix-multiband wind-speed-20ft)))

(defmethod weather :postgis-wind-from-direction
  [{:keys [wind-from-direction db-spec] :as config} type]
  (:matrix (postgis-raster-to-matrix-multiband db-spec wind-from-direction)))

(defmethod weather :geotiff-wind-from-direction
  [{:keys [wind-from-direction] :as config} type]
  (:matrix (geotiff-raster-to-matrix-multiband wind-from-direction)))
;; Section 2: Ignition from which to build simulation inputs:4 ends here
