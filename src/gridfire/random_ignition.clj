;; [[file:../../org/GridFire.org::random_ignition.clj][random_ignition.clj]]
(ns gridfire.random-ignition
  (:require [clojure.core.matrix    :as m]
            [gridfire.utils.random :as random]
            [gridfire.common :refer [burnable-fuel-model? get-neighbors]]))

(defn- in-edge-buffer?
  "Returns true if give [row col] is within the buffer region defined
  by buffer-size. The buffer size is the thickness (pixel) of the edge
  buffer region."
  [num-rows num-cols buffer-size row col]
  (or  (<= row buffer-size)
       (> row (- num-rows buffer-size))
       (<= col buffer-size)
       (> col (- num-cols buffer-size))))

(defn valid-ignition-site? [{:keys [num-rows num-cols random-ignition cell-size landfire-rasters]} row col]
  (let [{:keys [edge-buffer]} random-ignition
        {:keys [fuel-model]}  landfire-rasters]
    (and (if edge-buffer
           (let [buffer-size (int (Math/ceil (/ edge-buffer cell-size)))]
             (not (in-edge-buffer? num-rows num-cols buffer-size row col)))
           true)
         (burnable-fuel-model? (m/mget fuel-model row col))
         (some (fn [[row col]]
                 (burnable-fuel-model? (m/mget fuel-model row col)))
               (get-neighbors [row col])))))

(defn select-ignition-site
  "Returns [x y] coordinate that have been randomly sampled
  from cells in in the computational domain. Ignitable cells can also
  be constrained by ignition mask raster and/or edge-buffer."
  [{:keys [rand-gen ignition-mask-layer num-rows num-cols] :as inputs}]
  (if-let [indices (some->> ignition-mask-layer
                            :matrix
                            first
                            m/non-zero-indices
                            (map-indexed (fn [i v] (when (pos? (count v)) [i v])))
                            (filterv identity))]
    (loop [[row cols] (random/my-rand-nth rand-gen indices)]
      (let [col (random/my-rand-nth rand-gen cols)]
        (if (valid-ignition-site? inputs row col)
          [row col]
          (recur (random/my-rand-nth rand-gen indices)))))
    (loop [row (random/my-rand-int rand-gen num-rows)
           col (random/my-rand-int rand-gen num-cols)]
      (if (valid-ignition-site? inputs row col)
        [row col]
        (recur (random/my-rand-int rand-gen num-rows)
               (random/my-rand-int rand-gen num-cols))))))
;; random_ignition.clj ends here
