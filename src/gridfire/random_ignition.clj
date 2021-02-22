;; [[file:../../org/GridFire.org::random_ignition.clj][random_ignition.clj]]
(ns gridfire.random-ignition
  (:require [clojure.core.matrix    :as m]
            [gridfire.utils.random :as random]
            [gridfire.common :refer [burnable-fuel-model?
                                     burnable-neighbors?]]
            [gridfire.fetch :as fetch]))

(defn- in-edge-buffer?
  "Returns true if give [row col] is within the buffer region defined
  by buffer-size. The buffer size is the thickness (pixel) of the edge
  buffer region."
  [num-rows num-cols buffer-size [row col]]
  (or  (<= row buffer-size)
       (> row (- num-rows buffer-size))
       (<= col buffer-size)
       (> col (- num-cols buffer-size))))

(defn- in-ignition-mask? [raster [i j]]
  (pos? (m/mget raster i j)))

(defn- get-ignitable-cells [{:keys [random-ignition cell-size db-spec]} fuel-model-matrix]
  (let [num-rows                            (m/row-count fuel-model-matrix)
        num-cols                            (m/column-count fuel-model-matrix)
        {:keys [ignition-mask edge-buffer]} random-ignition
        ignition-mask-raster                (when ignition-mask
                                              (first (:matrix (fetch/ignition-mask-layer db-spec ignition-mask))))
        fire-spread-matrix                  (m/zero-matrix num-rows num-cols)
        indices                             (for [row (range num-rows)
                                                  col (range num-cols)]
                                              [row col])]
    (cond->> indices
      ignition-mask (filter #(in-ignition-mask? ignition-mask-raster %))
      edge-buffer   (remove #(in-edge-buffer? num-rows num-cols (int (Math/ceil (/ edge-buffer cell-size))) %))
      true          (filter (fn [[i j]] (burnable-fuel-model? (m/mget fuel-model-matrix i j))))
      true          (filter #(burnable-neighbors? fire-spread-matrix fuel-model-matrix
                                                  num-rows num-cols %)))))

(defn ignition-site
  "Returns [x y] coordinate that have been randomly sampled
  from cells in in the computational domain. Ignitable cells can also
  be constrained by ignition mask raster and/or edge-buffer."
  [{:keys [random-ignition rand-gen] :as config} fuel-model-matrix]
  (when random-ignition
    (let [ignitable-cells (get-ignitable-cells config fuel-model-matrix)]
      (first (random/sample-from-list rand-gen 1 ignitable-cells)))))
;; random_ignition.clj ends here
