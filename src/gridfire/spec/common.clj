(ns gridfire.spec.common
  (:require [clojure.spec.alpha :as s]))

;;=============================================================================
;; Numeric Samples
;;=============================================================================

(s/def ::number-sample
  (s/or :scalar number?
        :list   (s/coll-of number? :kind list?)
        :vector (s/coll-of number? :kind vector? :count 2)))

(s/def ::integer-sample
  (s/or :scalar integer?
        :list   (s/coll-of integer? :kind list?)
        :vector (s/coll-of integer? :kind vector? :count 2)))

(s/def ::float-sample
  (s/or :scalar float?
        :list   (s/coll-of float? :kind list?)
        :vector (s/coll-of float? :kind vector? :count 2)))

;;=============================================================================
;; Layer Coords
;;=============================================================================

(def postgis-sql-regex #"[a-z0-9]+(\.[a-z0-9]+)? WHERE rid=[0-9]+")

(def path-to-geotiff-regex #"[a-z_\-\s0-9\.\/]+(\/[a-z_\-\s0-9\.]+)*\.tif")

(s/def ::sql (s/and string? #(re-matches postgis-sql-regex %)))

(s/def ::geotiff (s/and string? #(re-matches path-to-geotiff-regex %)))

(s/def ::source (s/or :sql     ::sql
                      :geotiff ::geotiff))

(s/def ::type #{:geotiff :postgis})

(s/def ::cell-size number?)

(s/def ::unit #{:imperial :metric})

(s/def ::multiplier number?)

(s/def ::spatial-type #{:global :pixel})

(s/def ::range (s/and (s/coll-of number? :kind vector? :count 2)
                      (fn [[min-val max-val]] (< min-val max-val))))

(s/def ::frequency integer?)

(s/def ::perturbation
  (s/keys :req-un [::spatial-type ::range]
          :opt-un [::frequency]))

(s/def ::postgis-or-geotiff
  (s/keys :req-un [::source ::type]
          :opt-un [::cell-size ::unit ::multiplier ::perturbation]))

(s/def ::layer-coords (s/or :sql ::sql
                            :map ::postgis-or-geotiff))

;;=============================================================================
;; File Access
;;=============================================================================

(def file-path-regex #"^(((\.\.){1}/)*|(/){1})?(([\w-]*)/)*([\w-]+)$")

(s/def ::file-path (s/and string? #(re-matches file-path-regex %)))

;;=============================================================================
;; Macros
;;=============================================================================

(defmacro one-or-more-keys [ks]
  (let [keyset (set (map (comp keyword name) ks))]
    `(s/and (s/keys :opt-un ~ks)
            #(some ~keyset (keys %)))))
