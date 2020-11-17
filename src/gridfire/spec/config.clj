(ns gridfire.spec.config
  (:require [gridfire.spec.ignition :as ignition]
            [gridfire.spec.perturbations :as perturbations]
            [clojure.spec.alpha :as s]))

;;-----------------------------------------------------------------------------
;; Regex
;;-----------------------------------------------------------------------------

(def postgis-sql-regex #"[a-z0-9]+(\.[a-z0-9]+)? WHERE rid=[0-9]+")
(def path-to-geotiff-regex #"[a-z_\-\s0-9\.]+(\/[a-z_\-\s0-9\.]+)*\.tif")

(s/def ::sql (s/and string? #(re-matches postgis-sql-regex %)))
(s/def ::path (s/and string? #(re-matches path-to-geotiff-regex %)))
(s/def ::source (s/or :file-path ::path
                      :sql       ::sql))
(s/def ::type #(contains? #{:geotiff :postgis} %))

;;-----------------------------------------------------------------------------
;; Weather Layers ;;TODO move into own namespace
;;-----------------------------------------------------------------------------

(s/def ::postgis-or-geotiff
  (s/keys :req-un [::type ::source]
          :opt-un [::cell-size]))

(s/def ::weather
  (s/or :vector (s/coll-of int? :kind vector? :count 2)
        :list (s/coll-of int? :kind list?)
        :string string?
        :map ::postgis-or-geotiff))

(s/def ::temperature ::weather)
(s/def ::relative-humidity ::weather)
(s/def ::wind-speed-20ft ::weather)
(s/def ::wind-from-direction ::weather)

(s/def ::weather-layers
  (s/keys
   :req-un [::temperature ::relative-humidity ::wind-speed-20ft ::wind-from-direction]))

(def weather-names
  [:temperature :relative-humidity :wind-speed-20ft :wind-from-direction])

(defn multiple?
  [target cell-size]
  (<= 0.0 (mod cell-size target) 0.002))

(defn valid-weather-cell-sizes?
  [{:keys [cell-size] :as config}]
  (let [weather    (map config weather-names)
        cell-sizes (->> weather
                        (remove nil?)
                        (filter #(= :map (first %)))
                        (map (comp :cell-size second)))]
    (every? #(multiple? cell-size %) cell-sizes)))

;;-----------------------------------------------------------------------------
;; Landfire Layers ;;TODO move into own namespace
;;-----------------------------------------------------------------------------

(s/def ::path-or-map (s/or :path ::path
                           :map  ::postgis-or-geotiff))
(s/def ::aspect ::path-or-map)
(s/def ::canopy-base-height ::path-or-map)
(s/def ::canopy-cover ::path-or-map)
(s/def ::canopy-height ::path-or-map)
(s/def ::crown-bulk-density ::path-or-map)
(s/def ::elevation ::path-or-map)
(s/def ::fuel-model ::path-or-map)
(s/def ::slope ::path-or-map)
(s/def ::cell-size float?)

(s/def ::landfire-layers
  (s/keys
   :req-un [::aspect
            ::canopy-base-height
            ::canopy-cover
            ::canopy-height
            ::crown-bulk-density
            ::elevation
            ::fuel-model
            ::slope]))

;;-----------------------------------------------------------------------------
;; Config
;;-----------------------------------------------------------------------------


(s/def ::config
  (s/and
   (s/keys
    :req-un [::cell-size
             ::landfire-layers]
    :opt-un [::perturbations/perturbations
             ::ignition/ignition-layer])
   ::weather-layers
   #(valid-weather-cell-sizes? %)))
