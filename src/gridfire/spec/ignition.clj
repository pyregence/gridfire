(ns gridfire.spec.ignition
  (:require [clojure.spec.alpha :as s]
            [gridfire.spec.common :as common]
            [gridfire.spec.predicates :refer [file-exits? file-readable?]]))

(s/def ::burned float?)
(s/def ::unburned float?)

(s/def ::burn-values
  (s/keys :req-un [::unburned ::burned]))

(s/def ::burn-values
  (s/keys :req-un [::unburned ::burned]))

(s/def ::ignition-layer
  (s/and
   ::common/postgis-or-geotiff
   (s/keys :opt-un [::burn-values])))

(s/def ::ignition-csv (and file-exits? file-readable?))
