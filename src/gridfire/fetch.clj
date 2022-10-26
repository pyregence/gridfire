;; [[file:../../org/GridFire.org::fetch.clj][fetch.clj]]
(ns gridfire.fetch
  (:require [clojure.string           :as s]
            [gridfire.conversion      :as convert]
            [gridfire.fetch.base      :refer [convert-tensor-as-requested get-wrapped-tensor get-wrapped-tensor-multi]]
            [gridfire.inputs.envi-bsq :as gf-bsq]
            [gridfire.magellan-bridge :refer [geotiff-raster-to-tensor]]
            [gridfire.postgis-bridge  :refer [postgis-raster-to-matrix]]
            [magellan.core            :refer [make-envelope]]
            [manifold.deferred        :as mfd]
            [tech.v3.datatype         :as d]
            [tech.v3.tensor           :as t]))

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

(defn get-units-convert-fn
  ([layer-name layer-spec fallback-unit]
   (get-units-convert-fn layer-name layer-spec fallback-unit 1.0))
  ([layer-name layer-spec fallback-unit fallback-multiplier]
   (convert/get-units-converter layer-name
                                (or (:units layer-spec) fallback-unit)
                                (or (:multiplier layer-spec) fallback-multiplier))))

;;-----------------------------------------------------------------------------
;; Data sources
;;-----------------------------------------------------------------------------

(defmethod get-wrapped-tensor-multi :postgis
  [{:keys [db-spec]} {:keys [source]} convert-fn target-dtype]
  (-> (postgis-raster-to-matrix db-spec source)
      (convert-tensor-as-requested convert-fn target-dtype)))

(defmethod get-wrapped-tensor-multi :geotiff
  [_env {:keys [source]} convert-fn target-dtype]
  (geotiff-raster-to-tensor source target-dtype convert-fn))

(defn adapt-bsq-tensor
  [tensor3d]
  (let [[n-bands _w _h] (d/shape tensor3d)]
    (-> tensor3d
        (cond->
         ;; Mimicks the behavior of Magellan. (Val, 21 Oct 2022)
         (= 1 n-bands) (t/mget 0)))))

(defn read-bsq-file
  [source]
  (-> (gf-bsq/read-bsq-file source)
      (mfd/chain
       (fn [layer-map]
         (-> layer-map (update :matrix adapt-bsq-tensor))))
      (deref)))

(defn request-dtype-like-magellan
  [tensor-dtype]
  (case tensor-dtype
    :int16 :int32
    nil))

(defmethod get-wrapped-tensor-multi :gridfire-envi-bsq
  [_env {:keys [source]} convert-fn target-dtype]
  (-> (read-bsq-file source)
      (as-> layer
            (let [t (:matrix layer)]
              (convert-tensor-as-requested layer
                                           convert-fn
                                           (or target-dtype
                                               (let [tensor-dtype (d/elemwise-datatype t)]
                                                 (request-dtype-like-magellan tensor-dtype))))))))

;;-----------------------------------------------------------------------------
;; LANDFIRE
;;-----------------------------------------------------------------------------

(defn landfire-layer
  [{:keys [landfire-layers] :as config} layer-name]
  (let [layer-spec (get landfire-layers layer-name)
        layer-spec (if (map? layer-spec)
                     layer-spec
                     {:type   :postgis
                      :source layer-spec
                      :units  :metric})
        convert-fn (get-units-convert-fn layer-name layer-spec nil 1.0)
        datatype   (if (= layer-name :fuel-model)
                     :int32
                     :float32)]
    (get-wrapped-tensor config layer-spec convert-fn datatype)))

;;-----------------------------------------------------------------------------
;; Initial Ignition
;;-----------------------------------------------------------------------------

(defn ignition-layer
  [{:keys [ignition-layer] :as config}]
  (when ignition-layer
    (get-wrapped-tensor config
                        ignition-layer
                        (if-let [burn-values (:burn-values ignition-layer)]
                          (let [{:keys [burned unburned]} burn-values]
                            (fn [x] (cond
                                      (= x burned)   1.0
                                      (= x unburned) 0.0
                                      :else          -1.0)))
                          nil)
                        :float32)))

;;-----------------------------------------------------------------------------
;; Ignition Mask
;;-----------------------------------------------------------------------------

(defn ignition-mask-layer
  [{:keys [random-ignition] :as config}]
  (when (map? random-ignition)
    (let [spec (:ignition-mask random-ignition)]
      (get-wrapped-tensor config spec nil nil))))

;;-----------------------------------------------------------------------------
;; Weather
;;-----------------------------------------------------------------------------

(defn weather-layer
  "Returns a layer map for the given weather name. Units of available weather:
  - temperature:         fahrenheit
  - relative-humidity:   percent (0-100)
  - wind-speed-20ft:     mph
  - wind-from-direction: degreees clockwise from north"
  [config weather-name]
  (let [weather-spec (get config weather-name)]
    (when (map? weather-spec)
      (get-wrapped-tensor config
                          weather-spec
                          (get-units-convert-fn weather-name weather-spec nil)
                          :float32))))

;;-----------------------------------------------------------------------------
;; Moisture Layers
;;-----------------------------------------------------------------------------

(defn fuel-moisture-layer
  [{:keys [fuel-moisture] :as config} category size]
  (let [spec (get-in fuel-moisture [category size])]
    (when (map? spec)
      (get-wrapped-tensor config
                          spec
                          (let [fuel-moisture-name (keyword
                                                    ;; WARNING we rely on keyword structure:
                                                    ;; renaming those keywords would break the program.
                                                    (s/join "-" ["fuel-moisture" (name category) (name size)]))]
                            (get-units-convert-fn fuel-moisture-name spec :percent 1.0))
                          :float32))))


;;-----------------------------------------------------------------------------
;; Suppression Difficulty Index Layer
;;-----------------------------------------------------------------------------

(defn sdi-layer
  [{:keys [suppression] :as config}]
  (when-let [layer-spec (:sdi-layer suppression)]
    (get-wrapped-tensor config
                        layer-spec
                        (get-units-convert-fn :suppression layer-spec nil)
                        :float32)))
;; fetch.clj ends here
