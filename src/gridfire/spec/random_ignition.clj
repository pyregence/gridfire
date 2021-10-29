(ns gridfire.spec.random-ignition
  (:require [clojure.spec.alpha :as s]
            [gridfire.spec.common :as common]))

(s/def ::ignition-mask ::common/postgis-or-geotiff)

(s/def ::edge-buffer float?)

(s/def ::random-ignition
  (s/or
   :boolean boolean?
   :map     (s/keys :req-un [::ignition-mask ::edge-buffer])))
