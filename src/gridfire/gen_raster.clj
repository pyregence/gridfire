(ns gridfire.gen-raster
  (:require [clojure.string    :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [magellan.core     :refer [write-raster
                                       make-envelope
                                       matrix-to-raster]]))

(defn matrix->raster [raster-name matrix {:keys [size srid xmin ymax] :or {srid "EPSG:32610"}}]
  (let [width    (count matrix)
        height   (count (first matrix))
        envelope (make-envelope srid
                                xmin
                                ymax
                                (* size width)
                                (* size height))]
    (matrix-to-raster raster-name matrix envelope)))

(defn identical-matrix [width height value]
  {:pre [(number? width) (number? height) (float? value)]}
  (into-array (repeat height (into-array (repeat width value)))))

(defn dem-matrix [size slp height width]
  (into-array (for [row (range height)]
                (into-array (repeat width (* row size slp))))))

(defn basename [path]
  (-> path
      (str/split #"/")
      (last)
      (str/split #"\.")
      (first)))

(defn inputs->output-raster [{:keys [output value width height] :as options}]
  (println "Creating raster with: " options)
  (let [raster (matrix->raster (basename output)
                               (identical-matrix width height value)
                               options)]
    (write-raster raster output)))

;;-----------------------------------------------------------------------------
;; Command Line Interface
;;-----------------------------------------------------------------------------

(def cli-options
  [["-o" "--output OUTPUT" "Raster Output filename."
    :validate [#(str/ends-with? % ".tif") "--output must be filename which ends in '.tif'."]]

   ["-v" "--value VALUE" "Value to use to populate the raster. (default: 0.0)."
    :default  0.0
    :parse-fn #(Float/parseFloat %)
    :validate [float? "--value must be a float."]]

   ["-w" "--width WIDTH" "Raster width in pixels (default: 256)."
    :default  256
    :parse-fn #(Integer/parseInt %)
    :validate [number? "--width must be a number."]]

   ["-h" "--height HEIGHT" "Raster height in pixels (default: 256)."
    :default  256
    :parse-fn #(Integer/parseInt %)
    :validate [number? "--width must be a number."]]

   ["-s" "--size SIZE" "Size of each cell in the units of the provided CRS (default: 30 -- 30 meters in UTM)."
    :default  30
    :parse-fn #(Integer/parseInt %)
    :validate [number? "--size must be a number."]]

   ["-x" "--xmin XMIN" "Upper left origin XMIN in the units of the provided CRS (default: 500000.0)."
    :default  500000.0
    :parse-fn #(Float/parseFloat %)
    :validate [float? "--x-min must be a number."]]

   ["-y" "--ymax YMAX" "Upper left origin YMAX in the units of the provided CRS (default: 4000000.0)."
    :default  4000000.0
    :parse-fn #(Float/parseFloat %)
    :validate [float? "--y-max must be a number."]]

   ["-r" "--srid SRID" "Spatial Reference ID (default: \"EPSG:32610\")"
    :default  "EPSG:32610"
    :validate [string? "--srid must be a string."]]

   [nil "--help" "Show help."
    :default  false]])

(defn -main [& args]
  (let [{:keys [options _ summary errors]} (parse-opts args cli-options)]
    (cond
      (or (empty? args) (:help options))
      (println (str "\nUsage:\n" summary))

      (some? errors)
      (do (println errors)
          (println (str "\nUsage:\n" summary))
          (System/exit 1))

      :else
      (do (println "Creating raster" (:output options) "...")
          (inputs->output-raster options)))
    (System/exit 0)))
