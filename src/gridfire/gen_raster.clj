(ns gridfire.gen-raster
  (:require [clojure.string      :as str]
            [clojure.tools.cli   :refer [parse-opts]]
            [gridfire.conversion :refer [deg->percent]]
            [magellan.core       :refer [write-raster
                                         make-envelope
                                         matrix-to-raster]]))

(defn- matrix->raster [raster-name matrix {:keys [size srid xmin ymax] :or {srid "EPSG:32610"}}]
  (let [width    (count matrix)
        height   (count (first matrix))
        envelope (make-envelope srid
                                xmin
                                ymax
                                (* size width)
                                (* size height))]
    (matrix-to-raster raster-name matrix envelope)))

(defn- identical-matrix [width height value]
  (into-array (repeat height (into-array (repeat width value)))))

(defn- dem-matrix [size degrees height width]
  (let [slope (deg->percent degrees)]
    (into-array (for [row (range height)]
                  (into-array (repeat width (* row size slope)))))))

(defn- generate-dems []
  (for [degree (range 10 60 10)]
    (let [raster (matrix->raster (format "dem-%s-slp" degree)
                                 (dem-matrix 30 degree 256 256)
                                 {:size 30
                                  :xmin 500000.0
                                  :ymax 4000000.0})]
      (write-raster raster (format "test/gridfire/resources/conical_test/dem-%s-slp.tif" degree)))))

(defn- basename [path]
  (-> path
      (str/split #"/")
      (last)
      (str/split #"\.")
      (first)))

(defn- inputs->output-raster [{:keys [output value width height] :as options}]
  (println "Creating raster with: " options)
  (let [raster (matrix->raster (basename output)
                               (identical-matrix width height value)
                               options)]
    (write-raster raster output)))

;;-----------------------------------------------------------------------------
;; Command Line Interface
;;-----------------------------------------------------------------------------

(def ^:private cli-options
  [["-o" "--output OUTPUT" "Raster output filename."
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
    :validate [number? "--height must be a number."]]

   ["-s" "--size SIZE" "Size of each cell in the units of the provided CRS (default: 30 -- 30 meters in UTM)."
    :default  30
    :parse-fn #(Integer/parseInt %)
    :validate [integer? "--size must be a number."]]

   ["-x" "--xmin XMIN" "Upper left origin XMIN in the units of the provided CRS (default: 500000.0)."
    :default  500000.0
    :parse-fn #(Float/parseFloat %)
    :validate [float? "--xmin must be a number."]]

   ["-y" "--ymax YMAX" "Upper left origin YMAX in the units of the provided CRS (default: 4000000.0)."
    :default  4000000.0
    :parse-fn #(Float/parseFloat %)
    :validate [float? "--ymax must be a number."]]

   ["-r" "--srid SRID" "Spatial Reference ID (default: \"EPSG:32610\")"
    :default  "EPSG:32610"
    :validate [string? "--srid must be a string."]]

   [nil "--help" "Show help."
    :default  false]])

(defn -main
  "Command line tool to produce rasters of constant values. Use `clj -M:gen-raster --help` to view help guide."
  [& args]
  (let [{:keys [options _ summary errors]} (parse-opts args cli-options :strict true)]
    (cond
      (or (empty? args) (:help options))
      (println (str "\nUsage:\n" summary))

      errors
      (do (run! println errors)
          (println (str "\nUsage:\n" summary))
          (System/exit 1))

      (nil? (:output options))
      (do (println "Error: -o/--output is required.")
          (println (str "\nUsage:\n" summary))
          (System/exit 1))

      :else
      (do (println "Creating raster" (:output options) "...")
          (println options errors)
          (inputs->output-raster options)))
    (System/exit 0)))
