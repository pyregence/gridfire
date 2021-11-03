(ns gridfire.spec.common
  (:require [clojure.java.io    :as io]
            [clojure.spec.alpha :as s]))

;;=============================================================================
;; Numeric Samples
;;=============================================================================

(s/def ::ordered-pair (fn [[a b]] (< a b)))

(s/def ::percent (s/and float? #(<= 0.0 % 1.0)))

(s/def ::percent-range
  (s/and (s/coll-of ::percent :kind vector? :count 2)
         ::ordered-pair))

(s/def ::integer-range
  (s/and (s/coll-of integer? :kind vector? :count 2)
         ::ordered-pair))

(s/def ::float-range
  (s/and (s/coll-of float? :kind vector? :count 2)
         ::ordered-pair))

(s/def ::number-range
  (s/and (s/coll-of number? :kind vector? :count 2)
         ::ordered-pair))

(s/def ::percent-or-range
  (s/or :percent ::percent
        :range   ::percent-range))

(s/def ::integer-or-range
  (s/or :integer integer?
        :range  ::integer-range))

(s/def ::float-or-range
  (s/or :float float?
        :range  ::float-range))

(s/def ::number-or-range
  (s/or :number number?
        :range  ::number-range))

(s/def ::percent-sample
  (s/or :percent ::percent
        :range   ::percent-range
        :list    (s/coll-of ::percent :kind list?)))

(s/def ::integer-sample
  (s/or :integer integer?
        :range   ::integer-range
        :list    (s/coll-of integer? :kind list?)))

(s/def ::float-sample
  (s/or :float float?
        :range ::float-range
        :list  (s/coll-of float? :kind list?)))

(s/def ::number-sample
  (s/or :number number?
        :range  ::number-range
        :list   (s/coll-of number? :kind list?)))

(s/def ::lo ::number-or-range)
(s/def ::hi ::number-or-range)

(s/def ::number-or-range-map
  (s/or :number    number?
        :range-map (s/and (s/keys :req-un [::lo ::hi])
                          (fn [{:keys [lo hi]}]
                            (apply <= (flatten [(val lo) (val hi)]))))))

;;=============================================================================
;; Layer Coords
;;=============================================================================

(def postgis-sql-regex #"[a-z0-9]+(\.[a-z0-9]+)? WHERE rid=[0-9]+")

(def path-to-geotiff-regex #"[a-z_\-\s0-9\.\/]+(\/[a-z_\-\s0-9\.]+)*\.tif")

(s/def ::sql (s/and string? #(re-matches postgis-sql-regex %)))

;; FIXME: move File Access section above this one
(s/def ::geotiff (s/and string? #(re-matches path-to-geotiff-regex %) ::readable-file))

(s/def ::source (s/or :sql     ::sql
                      :geotiff ::geotiff))

(s/def ::type #{:geotiff :postgis})

(s/def ::cell-size number?)

(s/def ::unit #{:imperial :metric})

(s/def ::multiplier number?)

(s/def ::spatial-type #{:global :pixel})

(s/def ::range ::number-range)

(s/def ::frequency integer?)

(s/def ::perturbation
  (s/keys :req-un [::spatial-type ::range]
          :opt-un [::frequency]))

(s/def ::postgis-or-geotiff
  (s/keys :req-un [::source ::type]
          :opt-un [::cell-size ::unit ::multiplier ::perturbation]))

(s/def ::layer-coords (s/or :sql ::sql
                            :map ::postgis-or-geotiff))

(s/def ::number-or-layer-coords
  (s/or :number number?
        :raster ::layer-coords))

;;=============================================================================
;; File Access
;;=============================================================================

(def file-path-regex      #"^(((\.\.){1}/)*|(/){1})?(([\w\-]*)/)*([\w\-\.]+)$")
(def directory-path-regex #"^(((\.\.){1}/)*|(/){1})?(([\w\-]*)/)*([\w\-]+)/?$")

(s/def ::file-path      (s/and string? #(re-matches file-path-regex %)))
(s/def ::directory-path (s/and string? #(re-matches directory-path-regex %)))

(defn file-exists? [f]
  (.exists (io/file f)))

(defn file-readable? [f]
  (.canRead (io/file f)))

(defn file-writable? [f]
  (.canWrite (io/file f)))

(s/def ::readable-file (s/and ::file-path file-exists? file-readable?))
(s/def ::writable-file (s/and ::file-path file-exists? file-writable?))

(s/def ::readable-directory (s/and ::directory-path file-exists? file-readable?))
(s/def ::writable-directory (s/and ::directory-path file-exists? file-writable?))

;;=============================================================================
;; Macros
;;=============================================================================

(defmacro one-or-more-keys [ks]
  (let [keyset (set (map (comp keyword name) ks))]
    `(s/and (s/keys :opt-un ~ks)
            #(some ~keyset (keys %)))))
