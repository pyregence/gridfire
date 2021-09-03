(ns gridfire.spec.ignition
  (:require [clojure.spec.alpha :as s]
            [gridfire.spec.common :as common]))

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
