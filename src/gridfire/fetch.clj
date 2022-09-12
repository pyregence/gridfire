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

;;-----------------------------------------------------------------------------
;; Initial Ignition
;;-----------------------------------------------------------------------------

(defn ignition-layer
  [{:keys [db-spec ignition-layer]}]
  (when ignition-layer
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
        (geotiff-raster-to-tensor (:source ignition-layer))))))

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

;;-----------------------------------------------------------------------------
;; Weather
;;-----------------------------------------------------------------------------

(defn weather-layer
  "Returns a layer map for the given weather name. Units of available weather:
  - temperature:         fahrenheit
  - relative-humidity:   percent (0-100)
  - wind-speed-20ft:     mph
  - wind-from-direction: degreees clockwise from north"
  [{:keys [db-spec] :as config} weather-name]
  (let [weather-spec (get config weather-name)]
    (when (map? weather-spec)
      (let [{:keys [type source units multiplier]} weather-spec
            convert-fn                             (convert/get-units-converter weather-name units (or multiplier 1.0))]
        (if (= type :postgis)
          (cond-> (postgis-raster-to-matrix db-spec source)
            convert-fn (update :matrix #(d/copy! (d/emap convert-fn :float32 %) %)))
          (geotiff-raster-to-tensor source :float32 convert-fn))))))

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

;;-----------------------------------------------------------------------------
;; Suppression Difficulty Index Layer
;;-----------------------------------------------------------------------------

(defn suppression-difficulty-layer
  [{:keys [db-spec suppression]}]
  (when-let [{:keys [type source units multiplier] :as _spec} (:suppression-difficulty-index-layer suppression)]
    (let [convert-fn (convert/get-units-converter :suppression
                                                  units
                                                  (or multiplier 1.0))]
      (if (= type :postgis)
        (cond-> (postgis-raster-to-matrix db-spec source)
          convert-fn (update :matrix #(d/copy! (d/emap convert-fn :float32 %) %)))
        (geotiff-raster-to-tensor source :float32 convert-fn)))))
;; fetch.clj ends here
