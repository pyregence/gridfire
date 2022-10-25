(ns gridfire.spec.inputs.grid-of-rasters
  (:require [clojure.spec.alpha :as s]))

(s/def ::type #{:grid_of_rasters})

(s/def ::rasters_grid
  (s/coll-of (s/coll-of ::raw-layer-coords-map)))

(s/def ::raw-layer-coords-map (s/keys :req-un [::type ::rasters_grid]))
