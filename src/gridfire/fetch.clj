;; [[file:../../org/GridFire.org::*Section 2: Ignition data from which to build simulation inputs][Section 2: Ignition data from which to build simulation inputs:4]]
(ns gridfire.fetch
  (:require [clojure.core.matrix      :as m]
            [clojure.string           :as s]
            [gridfire.magellan-bridge :refer [geotiff-raster-to-matrix]]
            [gridfire.postgis-bridge  :refer [postgis-raster-to-matrix]]
            [gridfire.surface-fire    :refer [degrees-to-radians]]))

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
  (fn [_ fetch-method weather-type]
    (keyword (s/join "-" [(name fetch-method) (name weather-type)]))))

(defmethod weather :postgis-temperature
  [{:keys [temperature db-spec]} _ _]
  (:matrix (postgis-raster-to-matrix db-spec temperature)))

(defmethod weather :geotiff-temperature
  [{:keys [temperature]} _ _]
  (:matrix (geotiff-raster-to-matrix temperature)))

(defmethod weather :postgis-relative-humidity
  [{:keys [relative-humidity db-spec]} _ _]
  (:matrix (postgis-raster-to-matrix db-spec relative-humidity)))

(defmethod weather :geotiff-relative-humidity
  [{:keys [relative-humidity]} _ _]
  (:matrix (geotiff-raster-to-matrix relative-humidity)))

(defmethod weather :postgis-wind-speed-20ft
  [{:keys [wind-speed-20ft db-spec]} _ _]
  (:matrix (postgis-raster-to-matrix db-spec wind-speed-20ft)))

(defmethod weather :geotiff-wind-speed-20ft
  [{:keys [wind-speed-20ft]} _ _]
  (:matrix (geotiff-raster-to-matrix wind-speed-20ft)))

(defmethod weather :postgis-wind-from-direction
  [{:keys [wind-from-direction db-spec]} _ _]
  (:matrix (postgis-raster-to-matrix db-spec wind-from-direction)))

(defmethod weather :geotiff-wind-from-direction
  [{:keys [wind-from-direction]} _ _]
  (:matrix (geotiff-raster-to-matrix wind-from-direction)))
;; Section 2: Ignition data from which to build simulation inputs:4 ends here
