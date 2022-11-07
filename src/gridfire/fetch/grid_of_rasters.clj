(ns gridfire.fetch.grid-of-rasters
  (:require [clojure.spec.alpha :as s]
            [tech.v3.datatype   :as d]
            [tech.v3.tensor     :as t]))

(defn- all-grid-elements
  [grid]
  (if (vector? grid)
    (mapcat all-grid-elements grid)
    [grid]))

(defn- ij-shape
  [t]
  (->> t (d/shape) (take-last 2) (vec)))

(defn- non-ij-shape
  [t]
  (->> t (d/shape) (drop-last 2)))

(defn- has-consistent-xy-dims?
  [layer]
  (= [(:height layer) (:width layer)]
     (ij-shape (:matrix layer))))

(s/def :gridfire.spec.raster-map/has-consistent-xy-dims? has-consistent-xy-dims?)

(defn map-grid2d
  "Like clojure.core/mapv, but for matrix-shaped data: [[e ...] ...] -> [[(f e) ...] ...]."
  [f grid2d]
  (vec (for [m-i grid2d]
         (vec (for [m-ij m-i]
                (f m-ij))))))

(defn- almost-equal?
  "Like =, but leaves some room for floating-point inaccuracy."
  [^double v1 ^double v2 ^double eps]
  (< (Math/abs (- v1 v2))
     eps))

(defn- is-2D-grid?
  [fetched-rasters-grid]
  (and
   (vector? fetched-rasters-grid)
   (not-empty fetched-rasters-grid)
   (every? vector? fetched-rasters-grid)
   (every? not-empty fetched-rasters-grid)
   (apply = (map count fetched-rasters-grid))))

(s/def ::is-2D-grid? is-2D-grid?)

(defn- is-non-empty?
  [fetched-rasters-grid]
  (and
   (not-empty fetched-rasters-grid)
   (every? not-empty fetched-rasters-grid)))

(s/def ::is-non-empty? is-non-empty?)

(defn- n-y-rows
  [fetched-rasters-grid]
  (count fetched-rasters-grid))

(defn- n-x-cols
  [fetched-rasters-grid]
  (count (first fetched-rasters-grid)))

(defn- ith-grid-row
  [fetched-rasters-grid i]
  (nth fetched-rasters-grid i))

(defn- jth-grid-col
  [fetched-rasters-grid j]
  (->> fetched-rasters-grid
       (mapv (fn [row] (nth row j)))))

(defn- height-is-constant-in-each-row?
  [fetched-rasters-grid]
  (->> (range (n-y-rows fetched-rasters-grid))
       (map #(ith-grid-row fetched-rasters-grid %))
       (every? (fn compatible-heights? [ith-row]
                 (->> ith-row
                      (map :height)
                      (apply =))))))

(s/def ::height-is-constant-in-each-row? height-is-constant-in-each-row?)

(defn- width-is-constant-in-each-col?
  [fetched-rasters-grid]
  (->> (range (n-x-cols fetched-rasters-grid))
       (map #(jth-grid-col fetched-rasters-grid %))
       (every? (fn compatible-heights? [jth-col]
                 (->> jth-col
                      (map :width)
                      (apply =))))))

(s/def ::width-is-constant-in-each-col? width-is-constant-in-each-col?)

(defn- layers-have-the-same?
  [f fetched-rasters-grid]
  (->> (all-grid-elements fetched-rasters-grid) (map f) (apply =)))

(defn- all-blocks-have-the-same-scale?
  [fetched-rasters-grid]
  (and (layers-have-the-same? :scalex fetched-rasters-grid)
       (layers-have-the-same? :scaley fetched-rasters-grid)))

(s/def ::all-blocks-have-the-same-scale? all-blocks-have-the-same-scale?)

(defn- meet-at-corners?
  [fetched-rasters-grid]
  (and
   (->> (ith-grid-row fetched-rasters-grid 0)
        (map (juxt :upperleftx :width :scalex))
        (partition 2 1)
        (every? (fn x-touch? [[[ulx-j w-j sx-j] [ulx-j+1 _w-j+1 _sx-j+1]]]
                  (almost-equal? (+ (double ulx-j)
                                    (* (double w-j) (double sx-j)))
                                 (double ulx-j+1)
                                 (Math/abs (* 1e-4 (double sx-j)))))))
   (->> (jth-grid-col fetched-rasters-grid 0)
        (map (juxt :upperlefty :height :scaley))
        (partition 2 1)
        (every? (fn y-touch? [[[uly-i h-i sy-i] [uly-i+1 _h-i+1 _sy-i+1]]]
                  (almost-equal? (+ (double uly-i)
                                    (* (double h-i) (double sy-i)))
                                 (double uly-i+1)
                                 (Math/abs (* 1e-4 (double sy-i)))))))))

(s/def ::meet-at-corners? meet-at-corners?)

(s/def ::same-bands (fn same-bands? [fetched-rasters-grid]
                      (->> fetched-rasters-grid (layers-have-the-same? #(-> % :matrix (non-ij-shape))))))

(s/def ::stitchable-grid-of-layers
  (s/and ::is-2D-grid?
         ::is-non-empty?
         (s/coll-of (s/coll-of :gridfire.spec.raster-map/has-consistent-xy-dims?))
         ::height-is-constant-in-each-row?
         ::width-is-constant-in-each-col?
         ::all-blocks-have-the-same-scale?
         ::meet-at-corners?
         ::same-bands))

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
  (when-some [expl-data (s/explain-data ::stitchable-grid-of-layers fetched-rasters-grid)]
    (throw (ex-info (format "Invalid grid of rasters: %s"
                            (s/explain-str ::stitchable-grid-of-layers fetched-rasters-grid))
                    {::fetched-rasters-grid fetched-rasters-grid
                     ::explanation          expl-data})))
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
            ;; Kenneth Cheung confirmed that it is correct. (Val, 07 Nov 2022)
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
