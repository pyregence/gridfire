(ns gridfire.fetch.grid-of-rasters-test
  (:require [clojure.test                :refer [deftest is testing]]
            [clojure.java.io             :as io]
            [gridfire.fetch]             ; :require'd to provide multimethod implementation. :as omitted to pass linting.
            [gridfire.fetch.base         :refer [get-wrapped-tensor]]
            [tech.v3.datatype            :as d]
            [tech.v3.datatype.functional :as dfn]))

(def examples-dir-path "test/gridfire/resources/grid-of-rasters-examples")

(defn example-subraster-file-path
  [fname yi xj fext]
  (.getPath (io/file examples-dir-path
                     (str fname "_y" yi "_x" xj "." fext))))

(defn- create-subrasters-shell-cmds
  [fname split-ys-px split-xs-px]
  (let [x-windows   (->> split-xs-px (partition 2 1) (mapv vec))
        y-windows   (->> split-ys-px (partition 2 1) (mapv vec))]
    (for [yi (range (count y-windows))
          :let [[ymin ymax+1] (nth y-windows yi)]
          xj (range (count x-windows))
          :let [[xmin xmax+1] (nth x-windows xj)]
          [fext] [["bsq"]
                  ["tif"]]]
      (->> (concat ["gdal_translate"]
                   ["-srcwin" xmin ymin (- xmax+1 xmin) (- ymax+1 ymin)]
                   (when (= fext "bsq")
                     ["-of" "ENVI" "-co" "INTERLEAVE=BSQ"])
                   [(.getPath (io/file examples-dir-path (str fname ".tif")))
                    (example-subraster-file-path fname yi xj fext)])
           (map str)))))

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

(defn- compute-grid2d
  [[i-length j-length] ij-fn]
  (vec (for [grid-i (range i-length)]
         (vec (for [grid-j (range j-length)]
                (ij-fn grid-i grid-j))))))

(deftest ^:simulation fetch-grid-of-rasters-test
  (testing ":grid_of_rasters: Stitching the :rasters_grid yields the same layer as the original."
   (->> ["fbfm40"
         "ws_2018"]
        (every? (fn [fname]
                  (let [layer1 (get-wrapped-tensor {}
                                                   {:type         :grid_of_rasters
                                                    :rasters_grid (compute-grid2d [3 2]
                                                                                  (fn [^long yi ^long xj]
                                                                                    (let [fext (nth ["tif" "bsq"] (-> (+ yi xj) (mod 2)))]
                                                                                      {:type   (case fext "tif" :geotiff "bsq" :gridfire-envi-bsq)
                                                                                       :source (example-subraster-file-path fname yi xj fext)})))}
                                                   nil
                                                   nil)
                        layer0 (get-wrapped-tensor {}
                                                   {:type   :geotiff
                                                    :source (str examples-dir-path "/" fname ".tif")}
                                                   nil
                                                   nil)]
                    (assert-equivalent-layers layer0 layer1)))))))
