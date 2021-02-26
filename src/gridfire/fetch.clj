;; [[file:../../org/GridFire.org::fetch.clj][fetch.clj]]
(ns gridfire.fetch
  (:require [clojure.core.matrix      :as m]
            [gridfire.conversion      :as convert]
            [gridfire.magellan-bridge :refer [geotiff-raster-to-matrix]]
            [gridfire.postgis-bridge  :refer [postgis-raster-to-matrix]]
            [gridfire.spec.config     :as spec]))

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

(defmulti landfire-layer
  (fn [_ {:keys [type]}] type))

(defmethod landfire-layer :postgis
  [db-spec {:keys [source]}]
  (postgis-raster-to-matrix db-spec source))

(defmethod landfire-layer :geotiff
  [_ {:keys [source]}]
  (geotiff-raster-to-matrix source))

(defn landfire-layers
  "Returns a map of LANDFIRE rasters (represented as maps) with the following units:
   {:elevation          feet
    :slope              vertical feet/horizontal feet
    :aspect             degrees clockwise from north
    :fuel-model         fuel model numbers 1-256
    :canopy-height      feet
    :canopy-base-height feet
    :crown-bulk-density lb/ft^3
    :canopy-cover       % (0-100)}"
  [{:keys [db-spec] :as config}]
  (let [layers (:landfire-layers config)]
    (reduce (fn [amap layer-name]
              (let [source (get layers layer-name)]
                (assoc amap
                       layer-name
                       (if (map? source)
                         (-> (landfire-layer db-spec source)
                             (convert/to-imperial source layer-name))
                         (-> (postgis-raster-to-matrix db-spec source)
                             (convert/to-imperial {:units :metric} layer-name))))))
            {}
            layer-names)))

;;-----------------------------------------------------------------------------
;; Initial Ignition
;;-----------------------------------------------------------------------------

(defn convert-burn-values [matrix {:keys [burned unburned]}]
  (m/emap #(condp = %
             (double burned)   1.0
             (double unburned) 0.0
             -1.0)
          matrix))

(defmulti ignition-layer
  (fn [{:keys [ignition-layer]}] (:type ignition-layer)))

(defmethod ignition-layer :postgis
  [{:keys [db-spec ignition-layer]}]
  (let [layer (postgis-raster-to-matrix db-spec (:source ignition-layer))]
    (if-let [bv (:burn-values ignition-layer)]
      (assoc layer :matrix (convert-burn-values (:matrix layer) bv))
      layer)))

(defmethod ignition-layer :geotiff
  [{:keys [ignition-layer]}]
  (let [layer (geotiff-raster-to-matrix (:source ignition-layer))]
    (if-let [bv (:burn-values ignition-layer)]
      (assoc layer :matrix (convert-burn-values (:matrix layer) bv))
      layer)))

(defmethod ignition-layer :default
  [_]
  nil)

;;-----------------------------------------------------------------------------
;; Weather
;;-----------------------------------------------------------------------------

(defmulti weather
  (fn [_ {:keys [type]}] type))

(defmethod weather :postgis
  [{:keys [db-spec]} {:keys [source]}]
  (postgis-raster-to-matrix db-spec source))

(defmethod weather :geotiff
  [_ {:keys [source]}]
  (geotiff-raster-to-matrix source))

(defn weather-layers
  "Returns a map of weather layers (represented as maps) with the following units:
   {:temperature         farenheight
    :relative-humidity   %
    :wind-speed-20ft     mph
    :wind-from-direction degrees clockwise from north}"
  [config]
  (reduce (fn [acc weather-name]
            (let [weather-spec (weather-name config)]
              (if (map? weather-spec)
                (assoc acc weather-name (convert/to-imperial (weather config weather-spec)
                                                             weather-spec
                                                             weather-name))
                acc)))
          {}
          spec/weather-names))

;;-----------------------------------------------------------------------------
;; Ignition Mask
;;-----------------------------------------------------------------------------

(defmulti ignition-mask-layer
  (fn [_ {:keys [type]}] type))

(defmethod ignition-mask-layer :geotiff
  [_ {:keys [source]}]
  (geotiff-raster-to-matrix source))

(defmethod ignition-mask-layer :postgis
  [db-spec {:keys [source]}]
  (postgis-raster-to-matrix db-spec source))
;; fetch.clj ends here
