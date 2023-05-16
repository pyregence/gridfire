;; [[file:../../../org/GridFire.org::gridfire.spec.common][gridfire.spec.common]]
(ns gridfire.spec.common
  (:require [clojure.java.io                      :as io]
            [clojure.spec.alpha                   :as s]
            [gridfire.spec.inputs.file-raster     :as spec-file]
            [gridfire.spec.inputs.grid-of-rasters :as spec-grid]
            [gridfire.spec.inputs.sql             :as spec-sql]))

;;=============================================================================
;; Numeric Samples
;;=============================================================================

(s/def ::ordered-pair (fn [[a b]] (< a b)))

(s/def ::ratio (s/and float? #(<= 0.0 % 10.0)))

(s/def ::ratio-range
  (s/and (s/coll-of ::ratio :kind vector? :count 2)
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

(s/def ::ratio-or-range
  (s/or :ratio ::ratio
        :range ::ratio-range))

(s/def ::integer-or-range
  (s/or :integer integer?
        :range  ::integer-range))

(s/def ::float-or-range
  (s/or :float float?
        :range  ::float-range))

(s/def ::number-or-range
  (s/or :number number?
        :range  ::number-range))

(s/def ::ratio-sample
  (s/or :ratio ::ratio
        :range ::ratio-range
        :list  (s/coll-of ::ratio :kind list?)))

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
;; File Access
;;=============================================================================

(def file-path-regex      #"^(((\.){1,2}/)*|(/){1})?(([\w\-]*)/)*([\w\-\.]+)$")
(def directory-path-regex #"^(((\.){1,2}/)*|(/){1})?(([\w\-]*)/)*([\w\-]+)/?$")

(s/def ::file-path      (s/and string? #(re-matches file-path-regex %)))
(s/def ::directory-path (s/and string? #(re-matches directory-path-regex %)))

(def ^:dynamic *check-files-exist?*
  "Whether to check for existing files when validating the GridFire config."
  true)

(defn file-exists? [f]
  (or (not *check-files-exist?*)
      (.exists (io/file f))))

(defn file-readable? [f]
  (or (not *check-files-exist?*)
      (.canRead (io/file f))))

(defn file-writable? [f]
  (or (not *check-files-exist?*)
      (.canWrite (io/file f))))

(s/def ::readable-file (s/and ::file-path file-exists? file-readable?))
(s/def ::writable-file (s/and ::file-path file-exists? file-writable?))

(s/def ::readable-directory (s/and ::directory-path file-exists? file-readable?))
(s/def ::writable-directory (s/and ::directory-path file-exists? file-writable?))

;;=============================================================================
;; Layer Coords
;;=============================================================================

;; NOTE that this spec is refined in each subtype of inputs.
(s/def ::type keyword?)

(s/def ::raw-layer-coords-map
  (s/and (s/keys :req-un [::type])
         (s/or :postgis         ::spec-sql/postgis-coords-map
               :grid-of-rasters ::spec-grid/raw-layer-coords-map
               :file-raster     ::spec-file/raw-layer-coords-map)))

(s/def ::cell-size number?)

(s/def ::unit #{:imperial :metric})

(s/def ::multiplier number?)

(s/def :gridfire.input/add-correction-angle360 number?)

(s/def ::layer-coords-map
  (s/and (s/nonconforming ::raw-layer-coords-map)
         (s/keys :opt-un [::cell-size ::unit ::multiplier]
                 :opt    [:gridfire.input/add-correction-angle360])))

(s/def ::layer-coords (s/or :sql ::spec-sql/sql
                            :map ::layer-coords-map))

(s/def ::ratio-or-layer-coords
  (s/or :ratio  ::ratio
        :raster ::layer-coords))

;;=============================================================================
;; Macros
;;=============================================================================

(defmacro one-or-more-keys [ks]
  (let [keyseq (map (comp keyword name) ks)]
    `(s/and (s/keys :opt-un ~ks)
            #(some % '~keyseq))))
;; gridfire.spec.common ends here
