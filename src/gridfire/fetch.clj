;; [[file:../../org/GridFire.org::fetch.clj][fetch.clj]]
(ns gridfire.fetch
  (:require [clojure.core.matrix      :as m]
            [clojure.java.io          :as io]
            [gridfire.conversion      :as convert]
            [gridfire.magellan-bridge :refer [geotiff-raster-to-matrix]]
            [gridfire.postgis-bridge  :refer [postgis-raster-to-matrix]]
            [magellan.core            :refer [make-envelope
                                              register-new-crs-definitions-from-properties-file!]]))

(register-new-crs-definitions-from-properties-file! "CUSTOM" (io/resource "custom_projections.properties"))

(m/set-current-implementation :vectorz)

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
            (geotiff-raster-to-matrix (:source layer-spec)))
          (convert/to-imperial! layer-spec layer-name))
      (-> (postgis-raster-to-matrix db-spec layer-spec)
          (convert/to-imperial! {:units :metric} layer-name)))))

(defn landfire-matrix
  [config layer-name]
  (-> (landfire-layer config layer-name)
      :matrix
      first))

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
  (m/emap! #(condp = %
              (double burned)   1.0
              (double unburned) 0.0
              -1.0)
           matrix))

(defn ignition-layer
  [{:keys [db-spec ignition-layer]}]
  (if (= (:type ignition-layer) :postgis)
    (postgis-raster-to-matrix db-spec (:source ignition-layer))
    (geotiff-raster-to-matrix (:source ignition-layer))))

(defn ignition-matrix
  [{:keys [ignition-layer] :as config}]
  (when ignition-layer
   (let [layer (ignition-layer config)]
     (if-let [burn-values (:burn-values ignition-layer)]
       (-> layer :matrix (convert-burn-values burn-values) first)
       (-> layer :matrix first)))))

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
              (geotiff-raster-to-matrix source))
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
    (let [spec (get-in random-ignition [:ignition-mask :raster])]
      (if (= (:type spec) :postgis)
        (postgis-raster-to-matrix db-spec (:source spec))
        (geotiff-raster-to-matrix (:source spec))))))

(defn ignition-mask-matrix
  [config]
  (when-let [layer (ignition-mask-layer config)]
   (-> layer :matrix first)))

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
              (geotiff-raster-to-matrix source))
            (update :matrix #(m/emap! convert/percent->dec)))))))

(defn fuel-moisture-matrix
  "Returns a matrix values for the given fuel category and size
  Units are in ratio (0-1)"
  [config category size]
  (when-let [layer (fuel-moisture-layer config category size)]
    (if (= category :live)
      (-> layer :matrix first)
      (-> layer :matrix))))
;; fetch.clj ends here
