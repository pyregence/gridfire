;; FIXME LP coverage
(ns gridfire.spec.inputs.grid-of-rasters
  (:require [clojure.spec.alpha :as s]))

(s/def ::type #{:grid-of-rasters})

(s/def ::rasters-grid
  ;; NOTE the blocks in the rasters grid must be arranged in an order
  ;; which is consistent with the layout of the sub-tensors.
  ;; Typically, if inside each block the pixels are arranged
  ;; in a [y decreasing, x increasing] layout,
  ;; then the blocks must be arranged in the same way.
  ;; An equivalent way of saying this is that
  ;; the blocks are arranged in a way which is GIS-agnostic,
  ;; and natural for describing a matrix by blocks:
  ;; for example, traversing the stitched matrix in the [i j] -> [i+1 j] direction
  ;; will traverse the blocks in the [grid-i grid-j] -> [grid-i+1 grid-j] direction.
  ;; This organizing principle was chosen because it is the most conceptually simple,
  ;; being agnostic of whatever GIS interpretations we might superimpose on it.
  ;; WARNING: however, this organizing principle might feel at odds with your GIS intuition
  ;; of organizing blocks in a [x increasing, y increasing] layout:
  ;; if so, don't blame :rasters-grid - blame the sub-tensors.
  (s/coll-of (s/coll-of :gridfire.spec.common/raw-layer-coords-map)))

(s/def ::raw-layer-coords-map (s/keys :req-un [::type ::rasters-grid]))
