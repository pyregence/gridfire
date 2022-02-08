;; [[file:../../org/GridFire.org::fetch.clj][fetch.clj]]
(ns gridfire.fetch
  (:require [clojure.java.io          :as io]
            [gridfire.conversion      :as convert]
            [gridfire.magellan-bridge :refer [geotiff-raster-to-matrix]]
            [gridfire.postgis-bridge  :refer [postgis-raster-to-matrix]]
            [magellan.core            :refer [make-envelope
                                              register-new-crs-definitions-from-properties-file!]]
            [tech.v3.datatype           :as d]))

(register-new-crs-definitions-from-properties-file! "CUSTOM" (io/resource "custom_projections.properties"))

(set! *unchecked-math* :warn-on-boxed)

;;-----------------------------------------------------------------------------
;; LANDFIRE
;;-----------------------------------------------------------------------------

(defn landfire-layer
  [{:keys [db-spec] :as config} layer-name]
  (let [layer-spec (get-in config [:landfire-layers layer-name])]
    (if (map? layer-spec)
      (-> (if (= (:type layer-spec) :postgis)
            (postgis-raster-to-matrix db-spec (:source layer-spec))
            (future (geotiff-raster-to-matrix (:source layer-spec))))
          (convert/to-imperial! layer-spec layer-name))
      (-> (postgis-raster-to-matrix db-spec layer-spec)
          (convert/to-imperial! {:units :metric} layer-name)))))

(defn landfire-matrix
  [config layer-name]
  (:matrix (landfire-layer config layer-name)))

(defn landfire-envelope
  [config layer-name]
  (let [{:keys [^double upperleftx
                ^double upperlefty
                ^double width
                ^double height
                ^double scalex
                ^double scaley]} (landfire-layer config layer-name)]
    (make-envelope (:srid config)
                   upperleftx
                   (+ upperlefty (* height scaley))
                   (* width scalex)
                   (* -1.0 height scaley))))

;;-----------------------------------------------------------------------------
;; Initial Ignition
;;-----------------------------------------------------------------------------

(defn convert-burn-values [matrix {:keys [burned unburned]}]
  (d/copy! (d/emap #(condp = %
                      (double burned)   1.0
                      (double unburned) 0.0
                      -1.0)
                   :float64
                   matrix)
           matrix))

(defn ignition-layer
  [{:keys [db-spec ignition-layer]}]
  (if (= (:type ignition-layer) :postgis)
    (postgis-raster-to-matrix db-spec (:source ignition-layer))
    (future (geotiff-raster-to-matrix (:source ignition-layer)))))

(defn ignition-matrix
  [config]
  (when (:ignition-layer config)
    (let [matrix (:matrix (ignition-layer config))]
      (if-let [burn-values (-> config :ignition-layer :burn-values)]
        (convert-burn-values matrix burn-values)
        matrix))))

;;-----------------------------------------------------------------------------
;; Weather
;;-----------------------------------------------------------------------------

(defn weather-layer
  [{:keys [db-spec] :as config} weather-name]
  (let [weather-spec (get config weather-name)]
    (when (map? weather-spec)
      (let [{:keys [type source]} weather-spec]
        (-> (if (= type :postgis)
              (postgis-raster-to-matrix db-spec source)
              (future (geotiff-raster-to-matrix source)))
            (convert/to-imperial! weather-spec weather-name))))))

(defn weather-matrix
  "Returns a matrix for the given weather name. Units of available weather:
  - temperature:         fahrenheit
  - relative-humidity:   percent (0-100)
  - wind-speed-20ft:     mph
  - wind-from-direction: degreees clockwise from north"
  [config weather-name]
  (:matrix (weather-layer config weather-name)))

;;-----------------------------------------------------------------------------
;; Ignition Mask
;;-----------------------------------------------------------------------------

(defn ignition-mask-layer
  [{:keys [db-spec random-ignition]}]
  (when (map? random-ignition)
    (let [spec (:ignition-mask random-ignition)]
      (if (= (:type spec) :postgis)
        (postgis-raster-to-matrix db-spec (:source spec))
        (future (geotiff-raster-to-matrix (:source spec)))))))

(defn ignition-mask-matrix
  [config]
  (when-let [layer (ignition-mask-layer config)]
    (:matrix layer)))

;;-----------------------------------------------------------------------------
;; Moisture Layers
;;-----------------------------------------------------------------------------

(defn fuel-moisture-layer
  [{:keys [db-spec fuel-moisture]} category size]
  (let [spec (get-in fuel-moisture [category size])]
    (when (map? spec)
      (let [{:keys [type source]} spec]
        (-> (if (= type :postgis)
              (postgis-raster-to-matrix db-spec source)
              (future (geotiff-raster-to-matrix source)))
            (update :matrix (fn [matrix]
                              (d/copy! (d/emap convert/percent->dec nil matrix)
                                       matrix))))))))

(defn fuel-moisture-matrix
  "Returns a matrix values for the given fuel category and size
  Units are in ratio (0-1)"
  [config category size]
  (when-let [layer (fuel-moisture-layer config category size)]
    (:matrix layer)))
;; fetch.clj ends here
