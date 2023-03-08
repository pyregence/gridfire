;; FIXME LP coverage
(ns gridfire.spec.inputs.sql
  (:require [clojure.spec.alpha :as s]))

(def postgis-sql-regex #"[a-z0-9]+(\.[a-z0-9]+)? WHERE rid=[0-9]+")

(s/def ::sql (s/and string? #(re-matches postgis-sql-regex %)))

(s/def ::type #{:postgis})

(s/def ::source ::sql)

(s/def ::postgis-coords-map (s/keys :req-un [::type ::source]))
