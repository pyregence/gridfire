(ns gridfire.fetch.grid-of-rasters
  (:require [tech.v3.datatype :as d]
            [tech.v3.tensor   :as t]))

(defn- all-grid-elements
  [grid]
  (if (vector? grid)
    (mapcat all-grid-elements grid)
    [grid]))

(defn- ij-shape
  [t]
  (->> t (d/shape) (take-last 2) (vec)))

(defn map-grid2d
  "Like clojure.core/mapv, but for matrix-shaped data: [[e ...] ...] -> [[(f e) ...] ...]."
  [f grid2d]
  (vec (for [m-i grid2d]
         (vec (for [m-ij m-i]
                (f m-ij))))))

(defn valid-grid-of-layers?
  [fetched-rasters-grid]
  (and
   (vector? fetched-rasters-grid)
   (not-empty fetched-rasters-grid)
   (every? vector? fetched-rasters-grid)
   (every? not-empty fetched-rasters-grid)
   (apply = (map count fetched-rasters-grid))
   (let [n-y-rows (count fetched-rasters-grid)
         n-x-cols (count (first fetched-rasters-grid))]
     (letfn [(ith-grid-row [i] (nth fetched-rasters-grid i))
             (jth-grid-col [j] (->> (range n-y-rows)
                                    (mapv (fn [i] (nth (ith-grid-row i)
                                                       j)))))
             (all-layers [] (all-grid-elements fetched-rasters-grid))]
       (and (->> (all-layers)
                 (map (fn has-consistent-dims? [layer]
                        (= [(:height layer) (:width layer)]
                           (ij-shape (:matrix layer))))))
            (->> (range n-y-rows)
                 (map ith-grid-row)
                 (every? (fn compatible-widths? [ith-row]
                           (->> (range n-x-cols)
                                (map (fn [j] (nth ith-row j)))
                                (map :height)
                                (apply =)))))
            (->> (range n-x-cols)
                 (map jth-grid-col)
                 (every? (fn compatible-heights? [jth-col]
                           (->> (range n-y-rows)
                                (map (fn [i] (nth jth-col i)))
                                (map :width)
                                (apply =)))))
            (->> (all-layers) (map :scalex) (apply =))
            (->> (all-layers) (map :scaley) (apply =))
            (->> (all-layers)
                 (map (fn non-xy-shape [layer] (->> layer :matrix (d/shape) (drop-last 2))))
                 (apply =)))))))

(defn stitch-tensors-in-grid2d
  "Computes a tensor by stitching a 2D grid of blocks,
  which must be tensors of compatible shapes.
  The block in the grid may have more than 2 dimensions;
  the stitching happens on the last 2 dimensions,
  and on the preceding dimensions all blocks must have identical shape
  (in the case of GIS rasters, that means 'same number of bands')."
  [tensors-grid]
  ;; NOTE this algorithm is deliberately not expressing GIS-specific concepts
  ;; such as width/height or x/y. It is thus made simpler, easier to reason about.
  (let [i-lengths    (->> tensors-grid
                          (map first)
                          (mapv (fn [t-i0] (-> t-i0 (ij-shape) (nth 0)))))
        i-length     (long (reduce + i-lengths))
        j-lengths    (->> tensors-grid
                          (first)
                          (mapv (fn [t-0j] (-> t-0j (ij-shape) (nth 1)))))
        j-length     (long (reduce + j-lengths))
        t00          (get-in tensors-grid [0 0])
        dst-shape    (vec (concat (drop-last 2 (d/shape t00))
                                  [i-length j-length]))
        dst-tensor   (t/new-tensor dst-shape :datatype (d/elemwise-datatype t00))
        dst-i-ranges (->> i-lengths
                          (reductions + 0)
                          (partition 2 1)
                          (vec))
        dst-j-ranges (->> j-lengths
                          (reductions + 0)
                          (partition 2 1)
                          (vec))]
    (->> (for [grid-i (range (count i-lengths))
               grid-j (range (count j-lengths))]
           (let [src-tensor    (get-in tensors-grid [grid-i grid-j])
                 [imin imax+1] (nth dst-i-ranges grid-i)
                 [jmin jmax+1] (nth dst-j-ranges grid-j)
                 dst-selection (concat (repeat (- (count dst-shape) 2)
                                               :all)
                                       [(range imin imax+1)
                                        (range jmin jmax+1)])
                 dst-subtensor (apply t/select dst-tensor dst-selection)]
             [src-tensor dst-subtensor]))
         (run! (fn copy-to-subrect [[src-tensor dst-subtensor]]
                 (t/tensor-copy! src-tensor dst-subtensor))))
    dst-tensor))

(defn stitch-grid-of-layers
  [fetched-rasters-grid]
  {:pre [(valid-grid-of-layers? fetched-rasters-grid)]}     ; IMPROVEMENT detailed error. (Val, 24 Oct 2022)
  (let [layer00      (get-in fetched-rasters-grid [0 0])
        tensors-grid (->> fetched-rasters-grid (map-grid2d :matrix))
        dst-tensor   (stitch-tensors-in-grid2d tensors-grid)
        [h w]        (->> dst-tensor (d/shape) (take-last 2) (vec))]
    (merge {:srid       (->> (all-grid-elements fetched-rasters-grid)
                             (keep :srid)
                             (first))
            ;; NOTE I'm not entirely sure that aggregating with the min and max is always correct, (Val, 24 Oct 2022)
            ;; but it is at least consistent with the current implementation of
            ;; gridfire.magellan-bridge/geotiff-raster-to-tensor, which uses the x-min and y-max of the envelope.
            :upperleftx (->> fetched-rasters-grid
                             (map first)
                             (map :upperleftx)
                             (reduce min))
            :upperlefty (->> fetched-rasters-grid
                             (first)
                             (map :upperlefty)
                             (reduce max))
            :width      w
            :height     h
            :matrix     dst-tensor}
           (select-keys layer00 [:scalex
                                 :scaley
                                 :skewx
                                 :skewy
                                 :numbands]))))
