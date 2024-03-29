;; [[file:../../../../org/GridFire.org::gridfire.spec.inputs.file-raster][gridfire.spec.inputs.file-raster]]
(ns gridfire.spec.inputs.file-raster
  (:require [clojure.spec.alpha :as s]))

(def raster-file-regex #".*\.(tif|bsq)$")

(s/def ::raster-file-path (s/and :gridfire.spec.common/readable-file #(re-matches raster-file-regex %)))

(s/def ::source ::raster-file-path)

(s/def ::type #{:geotiff :gridfire-envi-bsq})

(s/def ::raw-layer-coords-map (s/keys :req-un [::type ::source]))
;; gridfire.spec.inputs.file-raster ends here
