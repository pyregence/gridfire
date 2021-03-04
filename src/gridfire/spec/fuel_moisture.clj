(ns gridfire.spec.fuel-moisture
  (:require [clojure.spec.alpha :as s]
            [gridfire.spec.common :as common]))

(s/def ::1hr ::common/postgis-or-geotiff)

(s/def ::10hr ::common/postgis-or-geotiff)

(s/def ::100hr ::common/postgis-or-geotiff)


(s/def :dead
  (s/keys :req-un [::1hr ::10hr ::100hr]))

(s/def ::woody ::common/postgis-or-geotiff)

(s/def ::herbaceous ::common/postgis-or-geotiff)

(s/def :live
  (s/keys :req-un [::woody ::herbaceous]))

(s/def ::fuel-moisture-layers
  (s/keys :req-un [::dead ::live]))
