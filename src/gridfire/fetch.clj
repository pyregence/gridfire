;; [[file:../../org/GridFire.org::fetch.clj][fetch.clj]]
(ns gridfire.fetch
  (:require [clojure.string           :as s]
            [gridfire.conversion      :as convert]
            [gridfire.magellan-bridge :refer [geotiff-raster-to-tensor]]
            [gridfire.postgis-bridge  :refer [postgis-raster-to-matrix]]
            [magellan.core            :refer [make-envelope]]
            [tech.v3.datatype         :as d]))

(set! *unchecked-math* :warn-on-boxed)

;;-----------------------------------------------------------------------------
;; Utilities
;;-----------------------------------------------------------------------------

(defn layer->envelope
  [{:keys [^double upperleftx
           ^double upperlefty
           ^double width
           ^double height
           ^double scalex
           ^double scaley]}
   srid]
  (make-envelope srid
                 upperleftx
                 (+ upperlefty (* height scaley))
                 (* width scalex)
                 (* -1.0 height scaley)))

;;-----------------------------------------------------------------------------
;; LANDFIRE
;;-----------------------------------------------------------------------------

(defn landfire-layer
  [{:keys [db-spec landfire-layers]} layer-name]
  (let [layer-spec                             (get landfire-layers layer-name)
        {:keys [type source units multiplier]} (if (map? layer-spec)
                                                 layer-spec
                                                 {:type   :postgis
                                                  :source layer-spec
                                                  :units  :metric})
        convert-fn                             (convert/get-units-converter layer-name
                                                                            units
                                                                            (or multiplier 1.0))
        datatype                               (if (= layer-name :fuel-model)
                                                 :int32
                                                 :float32)]
    (if (= type :postgis)
      (cond-> (postgis-raster-to-matrix db-spec source)
        convert-fn (update :matrix #(d/copy! (d/emap convert-fn datatype %) %)))
      (geotiff-raster-to-tensor source datatype convert-fn))))

(defn landfire-matrix
  [config layer-name]
  (:matrix (landfire-layer config layer-name)))

;;-----------------------------------------------------------------------------
;; Initial Ignition
;;-----------------------------------------------------------------------------

(defn ignition-layer
  [{:keys [db-spec ignition-layer]}]
  (if-let [burn-values (:burn-values ignition-layer)]
    (let [{:keys [burned unburned]} burn-values
          convert-fn                (fn [x] (cond
                                              (= x burned)   1.0
                                              (= x unburned) 0.0
                                              :else          -1.0))]
      (if (= (:type ignition-layer) :postgis)
        (-> (postgis-raster-to-matrix db-spec (:source ignition-layer))
            (update :matrix #(d/copy! (d/emap convert-fn :float32 %) %)))
        (geotiff-raster-to-tensor (:source ignition-layer) :float32 convert-fn)))
    (if (= (:type ignition-layer) :postgis)
      (postgis-raster-to-matrix db-spec (:source ignition-layer))
      (geotiff-raster-to-tensor (:source ignition-layer)))))

(defn ignition-matrix
  [config]
  (when (:ignition-layer config)
    (:matrix (ignition-layer config))))

;;-----------------------------------------------------------------------------
;; Weather
;;-----------------------------------------------------------------------------

(defn weather-layer
  [{:keys [db-spec] :as config} weather-name]
  (let [weather-spec (get config weather-name)]
    (when (map? weather-spec)
      (let [{:keys [type source units multiplier]} weather-spec
            convert-fn (convert/get-units-converter weather-name units (or multiplier 1.0))]
        (if (= type :postgis)
          (cond-> (postgis-raster-to-matrix db-spec source)
            convert-fn (update :matrix #(d/copy! (d/emap convert-fn :float32 %) %)))
          (geotiff-raster-to-tensor source :float32 convert-fn))))))

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
        (geotiff-raster-to-tensor (:source spec))))))

(defn ignition-mask-matrix
  [config]
  (:matrix (ignition-mask-layer config)))

;;-----------------------------------------------------------------------------
;; Moisture Layers
;;-----------------------------------------------------------------------------

(defn fuel-moisture-layer
  [{:keys [db-spec fuel-moisture]} category size]
  (let [spec (get-in fuel-moisture [category size])]
    (when (map? spec)
      (let [{:keys [type source units multiplier]} spec
            fuel-moisture-name                     (keyword (s/join "-" ["fuel-moisture" (name category) (name size)]))
            convert-fn                             (convert/get-units-converter fuel-moisture-name
                                                                                (or units :percent)
                                                                                (or multiplier 1.0))]
        (if (= type :postgis)
          (cond-> (postgis-raster-to-matrix db-spec source)
            convert-fn (update :matrix #(d/copy! (d/emap convert-fn :float32 %) %)))
          (geotiff-raster-to-tensor source :float32 convert-fn))))))

(defn fuel-moisture-matrix
  "Returns a matrix values for the given fuel category and size
  Units are in ratio (0-1)"
  [config category size]
  (:matrix (fuel-moisture-layer config category size)))
;; fetch.clj ends here
