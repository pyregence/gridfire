(ns gridfire.fetch.grid-of-rasters-test
  (:require [clojure.test                   :refer [deftest is testing]]
            [clojure.java.io                :as io]
            [gridfire.fetch]                ; :require'd to provide multimethod implementation. :as omitted to pass linting.
            [gridfire.fetch.base            :refer [get-wrapped-tensor]]
            [gridfire.inputs.envi-bsq       :refer [get-gdal-info]]
            [tech.v3.datatype               :as d]
            [tech.v3.datatype.functional    :as dfn]))

(def examples-dir-path "test/gridfire/resources/grid-of-rasters-examples")

(defn- compute-grid2d
  [[i-length j-length] ij-fn]
  (vec (for [grid-i (range i-length)]
         (vec (for [grid-j (range j-length)]
                (ij-fn grid-i grid-j))))))

(defn example-subraster-file-path
  [fname xj yi fext]
  (.getPath (io/file examples-dir-path
                     (str fname "_x" xj "_y" yi "." fext))))

(defn- create-subrasters-shell-cmds
  [fname split-ys-px split-xs-px]
  (let [x-windows (->> split-xs-px
                       (partition 2 1)
                       (mapv vec))
        y-windows (->> split-ys-px
                       (partition 2 1)
                       ;; Reversing is consistent with Elmfire's naming logic for gridded inputs:
                       ;; the y coordinates must be increasing with the file's y number.
                       (reverse)
                       (mapv vec))]
    (for [xj             (range (count x-windows))
          :let           [[xmin xmax+1] (nth x-windows xj)]
          yi             (range (count y-windows))
          :let           [[ymin ymax+1] (nth y-windows yi)]
          [fext gt-opts] [["bsq" ["-of" "ENVI" "-co" "INTERLEAVE=BSQ"]]
                          ["tif" nil]]]
      (->> (concat ["gdal_translate"]
                   ["-srcwin" xmin ymin (- xmax+1 xmin) (- ymax+1 ymin)]
                   gt-opts
                   [(.getPath (io/file examples-dir-path (str fname ".tif")))
                    (example-subraster-file-path fname xj yi fext)])
           (map str)))))

(defn example-grid-layer-spec
  [fname]
  {:type         :grid_of_rasters
   :rasters_grid (compute-grid2d [3 2]
                                 (fn [^long grid-i ^long grid-j]
                                   (let [xj   grid-j
                                         yi   (- (dec 3) grid-i)
                                         fext (nth ["tif" "bsq"] (-> (+ yi xj) (mod 2)))]
                                     {:type   (case fext
                                                "tif" :geotiff
                                                "bsq" :gridfire-envi-bsq)
                                      :source (example-subraster-file-path fname xj yi fext)})))})

(deftest example-file-names-test
  (testing "Our test files are gridded as follows:"
    (is (= {:type         :grid_of_rasters,
            :rasters_grid [[{:type   :geotiff
                             :source "test/gridfire/resources/grid-of-rasters-examples/fbfm40_x0_y2.tif"}
                            {:type   :gridfire-envi-bsq
                             :source "test/gridfire/resources/grid-of-rasters-examples/fbfm40_x1_y2.bsq"}]
                           [{:type   :gridfire-envi-bsq
                             :source "test/gridfire/resources/grid-of-rasters-examples/fbfm40_x0_y1.bsq"}
                            {:type   :geotiff
                             :source "test/gridfire/resources/grid-of-rasters-examples/fbfm40_x1_y1.tif"}]
                           [{:type   :geotiff
                             :source "test/gridfire/resources/grid-of-rasters-examples/fbfm40_x0_y0.tif"}
                            {:type   :gridfire-envi-bsq
                             :source "test/gridfire/resources/grid-of-rasters-examples/fbfm40_x1_y0.bsq"}]]}
           (example-grid-layer-spec "fbfm40"))))
  (testing "And these files all exist:"
    (->> (example-grid-layer-spec "fbfm40")
         :rasters_grid
         (sequence cat)
         (every? (fn [layer-spec]
                   (is (.exists (io/file (:source layer-spec)))))))))

(comment

  ;; How the test files were created: (Val, 25 Oct 2022)
  (require '[clojure.java.shell :as sh])
  (.mkdirs (io/file examples-dir-path))
  ;; Getting 2 original rasters
  (sh/sh "gdal_translate"
         (str "test/gridfire/resources/envi-bsq-examples/" "fbfm40.tif")
         (str examples-dir-path "/" "fbfm40.tif"))
  (sh/sh "gdal_translate"
         "-b" "1" "-b" "2" "-b" "3"                         ; only 3 bands
         (str "test/gridfire/resources/envi-bsq-examples/" "ws_2018.tif")
         (str examples-dir-path "/" "ws_2018.tif"))
  (->> [(create-subrasters-shell-cmds "fbfm40" [0 42 103 120] [0 27 80])
        (create-subrasters-shell-cmds "ws_2018" [0 1 2 3] [0 1 2])]
       (sequence cat)
       (mapv (fn [sh-args] (apply sh/sh sh-args))))

  *e)

(defn- assert-equivalent-layers
  [l1 l2]
  (let [relevant-gis-ks [:upperleftx
                         :upperlefty
                         :width
                         :height
                         :scalex
                         :scaley]]
    (and (is (= (select-keys l1 relevant-gis-ks)
                (select-keys l2 relevant-gis-ks))
             "Same geospatial metadata.")
         (is (= (d/shape (:matrix l1))
                (d/shape (:matrix l2)))
             "Same tensor dimensions.")
         (is (dfn/equals (:matrix l1) (:matrix l2))
             "Same tensor contents."))))

(deftest ^:simulation fetch-grid-of-rasters-test
  (testing ":grid_of_rasters: Stitching the :rasters_grid yields the same layer as the original."
    (->> ["fbfm40"
          "ws_2018"]
         (every? (fn [fname]
                   (let [layer1 (get-wrapped-tensor {}
                                                    (example-grid-layer-spec fname)
                                                    nil
                                                    nil)
                         layer0 (get-wrapped-tensor {}
                                                    {:type   :geotiff
                                                     :source (str examples-dir-path "/" fname ".tif")}
                                                    nil
                                                    nil)]
                     (assert-equivalent-layers layer0 layer1)))))))

(defn are-grid-corner-coords-as-we-expect
  [rasters_grid]
  (letfn [(gdal-corner-coords [input]
            (-> input :source
                (get-gdal-info)
                (get "cornerCoordinates")))
          (corner-x [c] (nth c 0))
          (corner-y [c] (nth c 1))
          (is-lower-i=upper-i+1 [[cci cci+1]]
            ;; Putting the (is ...) assertion inside this function
            ;; yields finer error reporting than around the (every? ...).
            (is (= (-> cci (get "lowerRight") (corner-y))
                   (-> cci+1 (get "upperLeft") (corner-y)))))
          (is-right-j=left-j+1 [[ccj ccj+1]]
            (is (= (-> ccj (get "lowerRight") corner-x)
                   (-> ccj+1 (get "upperLeft") corner-x))))]
    (testing "Reminders about GDAL's cornerCoordinates: in coordinates [x y], "
      (let [cc {"upperLeft"  [-2028825.0 1953435.0],
                "lowerLeft"  [-2028825.0 1952175.0],
                "lowerRight" [-2028015.0 1952175.0],
                "upperRight" [-2028015.0 1953435.0],
                "center"     [-2028420.0 1952805.0]}]
        (testing "x corresponds to left/right."
          (is (= (-> cc (get "lowerLeft") (corner-x))
                 (-> cc (get "upperLeft") (corner-x))))
          (is (= (-> cc (get "lowerRight") (corner-x))
                 (-> cc (get "upperRight") (corner-x)))))
        (testing "y corresponds to up/down."
          (is (= (-> cc (get "lowerLeft") (corner-y))
                 (-> cc (get "lowerRight") (corner-y))))
          (is (= (-> cc (get "upperLeft") (corner-y))
                 (-> cc (get "upperRight") (corner-y)))))))
    (testing "Increasing i advances from upper to lower."
      (let [j=0-col (mapv first rasters_grid)]
        (->> j=0-col
             (pmap gdal-corner-coords)
             (partition 2 1)
             (every? is-lower-i=upper-i+1))))
    (testing "Increasing j advances from left to right."
      (let [i=0-row (first rasters_grid)]
        (->> i=0-row
             (pmap gdal-corner-coords)
             (partition 2 1)
             (every? is-right-j=left-j+1))))))

(deftest ^:simulation gdal-corner-coordinates
  (testing "How our test grids touch."
    (are-grid-corner-coords-as-we-expect (:rasters_grid (example-grid-layer-spec "fbfm40")))))
