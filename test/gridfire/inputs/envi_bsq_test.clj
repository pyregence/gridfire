(ns gridfire.inputs.envi-bsq-test
  (:require [clojure.java.io             :as io]
            [clojure.string              :as str]
            [clojure.test                :refer [deftest is testing]]
            [gridfire.inputs.envi-bsq    :as gf-bsq]
            [gridfire.magellan-bridge    :refer [geotiff-raster-to-tensor]]
            [magellan.core               :refer [matrix-to-raster]]
            [tech.v3.datatype            :as d]
            [tech.v3.datatype.functional :as dfn]
            [tech.v3.tensor              :as t])
  (:import (java.io File)
           (org.geotools.geometry Envelope2D)))

(defn- file-name
  [^File f]
  (.getName f))

(defn- test-files-ending-with
  [ext]
  (->> (file-seq (io/file "./test/gridfire/resources/envi-bsq-examples"))
       (filter (fn [f] (-> (file-name f) (str/ends-with? ext))))
       (vec)))

(defn get-test-bsq-files
  []
  (test-files-ending-with ".bsq"))

(deftest available-test-files-test
  (testing "We test the BSQ-parsing on clipped inputs files from a CONUS run:"
   (is (= (into (sorted-set)
                (map file-name)
                (get-test-bsq-files))
          #{"2020_90_SDIx100.bsq"
            "adj.bsq"
            "asp.bsq"
            "cbd.bsq"
            "cbh.bsq"
            "cc.bsq"
            "ch.bsq"
            "dem.bsq"
            "ercperc_2018.bsq"
            "fbfm40.bsq"
            "ignition_density.bsq"
            "lh_2018.bsq"
            "lw_2018.bsq"
            "m100_2018.bsq"
            "m10_2018.bsq"
            "m1_2018.bsq"
            "phi.bsq"
            "pyromes.bsq"
            "slp.bsq"
            "wd_2018.bsq"
            "ws_2018.bsq"}))))

(comment
  ;; How the clipped BSQ files were created: (Val, 21 Oct 2022)
  ;; 1) Let's figure out a reasonable bounding box for clipping.
  (-> (gf-bsq/get-gdal-info "../007_037/ws_2018.bsq")       ; large-pixels weather input.
      (select-keys ["size" "geoTransform" "cornerCoordinates"]))
  ;;=>
  {"size" [40 40],
   "geoTransform" [-2074425.0 1200.0 -0.0 1953435.0 -0.0 -1200.0],
   "cornerCoordinates" {"upperLeft" [-2074425.0 1953435.0],
                        "lowerLeft" [-2074425.0 1905435.0],
                        "lowerRight" [-2026425.0 1905435.0],
                        "upperRight" [-2026425.0 1953435.0],
                        "center" [-2050425.0 1929435.0]}}
  (assert (= (* 40 1200.0) (- -2026425.0 -2074425.0)))
  ;; 2) Creating smaller clipped files.
  (require '[clojure.java.shell :as sh])
  (->> (file-seq (io/file "../007_037"))
       (filter (fn [f] (-> f (str) (str/ends-with? ".bsq"))))
       (pmap
        (fn [^java.io.File src-bsq-file]
          (let [clipped-bsq-file (io/file "./test/gridfire/resources/envi-bsq-examples"
                                          (file-name src-bsq-file))
                xmax -2026425.0
                ymax 1953435.0
                ;; Chose a sub-rectangle of 2x3 "weather pixels". (Val, 21 Oct 2022)
                ;; I checked in QGis that this subrectangle contains representative, varied and non-trivial data.
                xmin (- xmax (* 1200.0 2))
                ymin (- ymax (* 1200.0 3))]
            (io/make-parents clipped-bsq-file)
            ;; NOTE I checked that this transformation did not alter
            ;; the encoding and metadata of the BSQ files.
            (sh/sh "gdalwarp"
                   "-overwrite"
                   "-te" (str xmin) (str ymin) (str xmax) (str ymax)
                   "-of" "ENVI"
                   (.getPath src-bsq-file)
                   (.getPath clipped-bsq-file)))))
       (doall))

  *e)

(defn tif-of-bsq-file
  ^File [bsq-file]
  (let [bsq-file (io/file bsq-file)]
    (-> bsq-file
        (.getParentFile)
        (io/file (str/replace (file-name bsq-file) ".bsq" ".tif")))))

(comment

  ;; How the test Geotiffs were created from the BSQ files: (Val, 21 Oct 2022)
  (->> (get-test-bsq-files)
       (pmap
        (fn [^java.io.File bsq-file]
          (let [geotiff-file (tif-of-bsq-file bsq-file)]
            (sh/sh "gdal_translate" "-if" "ENVI" (.getPath bsq-file) (.getPath geotiff-file)))))
       (doall))

  *e)

(def small-example-filenames #{"fbfm40.bsq" "cbh.bsq" "phi.bsq"})

(deftest ^:bsq-test-suite bsq-metadata-examples-test
  (testing "Examples of .hdr files."
    (is (= (->> (get-test-bsq-files)
                (filter #(contains? small-example-filenames (file-name %)))
                (pmap (fn [bsq-file]
                        [(file-name bsq-file)
                         (-> (gf-bsq/find-hdr-file bsq-file)
                             (slurp)
                             (str/split-lines)
                             (vec))]))
                (into (sorted-map)))
           {"cbh.bsq"    ["ENVI"
                          "description = {"
                          "./test/gridfire/resources/envi-bsq-examples/cbh.bsq}"
                          "samples = 80"
                          "lines   = 120"
                          "bands   = 1"
                          "header offset = 0"
                          "file type = ENVI Standard"
                          "data type = 2"
                          "interleave = bsq"
                          "byte order = 0"
                          "map info = {Albers Conical Equal Area, 1, 1, -2028825, 1953435, 30, 30,North America 1983}"
                          "projection info = {9, 6378137, 6356752.314140356, 23, -96, 0, 0, 29.5, 45.5,North America 1983, Albers Conical Equal Area}"
                          "coordinate system string = {PROJCS[\"unknown\",GEOGCS[\"GCS_North_American_1983\",DATUM[\"D_North_American_1983\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101004]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Albers\"],PARAMETER[\"False_Easting\",0.0],PARAMETER[\"False_Northing\",0.0],PARAMETER[\"Central_Meridian\",-96.0],PARAMETER[\"Standard_Parallel_1\",29.5],PARAMETER[\"Standard_Parallel_2\",45.5],PARAMETER[\"Latitude_Of_Origin\",23.0],UNIT[\"Meter\",1.0]]}"
                          "band names = {"
                          "Band 1}"
                          "data ignore value = -9999"],
            "fbfm40.bsq" ["ENVI"
                          "description = {"
                          "./test/gridfire/resources/envi-bsq-examples/fbfm40.bsq}"
                          "samples = 80"
                          "lines   = 120"
                          "bands   = 1"
                          "header offset = 0"
                          "file type = ENVI Standard"
                          "data type = 2"
                          "interleave = bsq"
                          "byte order = 0"
                          "map info = {Albers Conical Equal Area, 1, 1, -2028825, 1953435, 30, 30,North America 1983}"
                          "projection info = {9, 6378137, 6356752.314140356, 23, -96, 0, 0, 29.5, 45.5,North America 1983, Albers Conical Equal Area}"
                          "coordinate system string = {PROJCS[\"unknown\",GEOGCS[\"GCS_North_American_1983\",DATUM[\"D_North_American_1983\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101004]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Albers\"],PARAMETER[\"False_Easting\",0.0],PARAMETER[\"False_Northing\",0.0],PARAMETER[\"Central_Meridian\",-96.0],PARAMETER[\"Standard_Parallel_1\",29.5],PARAMETER[\"Standard_Parallel_2\",45.5],PARAMETER[\"Latitude_Of_Origin\",23.0],UNIT[\"Meter\",1.0]]}"
                          "band names = {"
                          "Band 1}"
                          "data ignore value = -9999"],
            "phi.bsq"    ["ENVI"
                          "description = {"
                          "./test/gridfire/resources/envi-bsq-examples/phi.bsq}"
                          "samples = 80"
                          "lines   = 120"
                          "bands   = 1"
                          "header offset = 0"
                          "file type = ENVI Standard"
                          "data type = 4"
                          "interleave = bsq"
                          "byte order = 0"
                          "map info = {Albers Conical Equal Area, 1, 1, -2028825, 1953435, 30, 30,North America 1983}"
                          "projection info = {9, 6378137, 6356752.314140356, 23, -96, 0, 0, 29.5, 45.5,North America 1983, Albers Conical Equal Area}"
                          "coordinate system string = {PROJCS[\"unknown\",GEOGCS[\"GCS_North_American_1983\",DATUM[\"D_North_American_1983\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101004]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Albers\"],PARAMETER[\"False_Easting\",0.0],PARAMETER[\"False_Northing\",0.0],PARAMETER[\"Central_Meridian\",-96.0],PARAMETER[\"Standard_Parallel_1\",29.5],PARAMETER[\"Standard_Parallel_2\",45.5],PARAMETER[\"Latitude_Of_Origin\",23.0],UNIT[\"Meter\",1.0]]}"
                          "band names = {"
                          "Band 1}"
                          "data ignore value = -9999"]})))
  (testing "Examples of gdalinfo output."
    (is (= (->> (get-test-bsq-files)
                (filter #(contains? small-example-filenames (file-name %)))
                (pmap (fn [bsq-file]
                        [(file-name bsq-file)
                         (-> (gf-bsq/get-gdal-info bsq-file)
                             (update-in ["coordinateSystem" "wkt"]
                                        (fn elide-wkt [wkt]
                                          (str (subs wkt 0 8) " ...(ELIDED BY TESTING CODE)..."))))]))
                (into (sorted-map)))
           {"cbh.bsq"    {"bands"             [{"band"                1,
                                                "block"               [80 1],
                                                "type"                "Int16",
                                                "colorInterpretation" "Undefined",
                                                "noDataValue"         -9999.0,
                                                "metadata"            {}}],
                          "driverLongName"    "ENVI .hdr Labelled",
                          "coordinateSystem"  {"wkt"                      "PROJCRS[ ...(ELIDED BY TESTING CODE)...",
                                               "dataAxisToSRSAxisMapping" [1 2],
                                               "proj4"                    "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"},
                          "files"             ["./test/gridfire/resources/envi-bsq-examples/cbh.bsq"
                                               "./test/gridfire/resources/envi-bsq-examples/cbh.bsq.aux.xml"
                                               "./test/gridfire/resources/envi-bsq-examples/cbh.hdr"],
                          "wgs84Extent"       {"type"        "Polygon",
                                               "coordinates" [[[-119.672647 38.3266058]
                                                               [-119.662402 38.2954784]
                                                               [-119.6355513 38.3007517]
                                                               [-119.6457857 38.3318813]
                                                               [-119.672647 38.3266058]]]},
                          "metadata"          {"" {"AREA_OR_POINT" "Area", "Band_1" "Band 1"}, "IMAGE_STRUCTURE" {"INTERLEAVE" "BAND"}},
                          "geoTransform"      [-2028825.0 30.0 -0.0 1953435.0 -0.0 -30.0],
                          "size"              [80 120],
                          "cornerCoordinates" {"upperLeft"  [-2028825.0 1953435.0],
                                               "lowerLeft"  [-2028825.0 1949835.0],
                                               "lowerRight" [-2026425.0 1949835.0],
                                               "upperRight" [-2026425.0 1953435.0],
                                               "center"     [-2027625.0 1951635.0]},
                          "driverShortName"   "ENVI",
                          "description"       "./test/gridfire/resources/envi-bsq-examples/cbh.bsq"},
            "fbfm40.bsq" {"bands"             [{"band"                1,
                                                "block"               [80 1],
                                                "type"                "Int16",
                                                "colorInterpretation" "Undefined",
                                                "noDataValue"         -9999.0,
                                                "metadata"            {}}],
                          "driverLongName"    "ENVI .hdr Labelled",
                          "coordinateSystem"  {"wkt"                      "PROJCRS[ ...(ELIDED BY TESTING CODE)...",
                                               "dataAxisToSRSAxisMapping" [1 2],
                                               "proj4"                    "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"},
                          "files"             ["./test/gridfire/resources/envi-bsq-examples/fbfm40.bsq"
                                               "./test/gridfire/resources/envi-bsq-examples/fbfm40.bsq.aux.xml"
                                               "./test/gridfire/resources/envi-bsq-examples/fbfm40.hdr"],
                          "wgs84Extent"       {"type"        "Polygon",
                                               "coordinates" [[[-119.672647 38.3266058]
                                                               [-119.662402 38.2954784]
                                                               [-119.6355513 38.3007517]
                                                               [-119.6457857 38.3318813]
                                                               [-119.672647 38.3266058]]]},
                          "metadata"          {"" {"AREA_OR_POINT" "Area", "Band_1" "Band 1"}, "IMAGE_STRUCTURE" {"INTERLEAVE" "BAND"}},
                          "geoTransform"      [-2028825.0 30.0 -0.0 1953435.0 -0.0 -30.0],
                          "size"              [80 120],
                          "cornerCoordinates" {"upperLeft"  [-2028825.0 1953435.0],
                                               "lowerLeft"  [-2028825.0 1949835.0],
                                               "lowerRight" [-2026425.0 1949835.0],
                                               "upperRight" [-2026425.0 1953435.0],
                                               "center"     [-2027625.0 1951635.0]},
                          "driverShortName"   "ENVI",
                          "description"       "./test/gridfire/resources/envi-bsq-examples/fbfm40.bsq"},
            "phi.bsq"    {"bands"             [{"band"                1,
                                                "block"               [80 1],
                                                "type"                "Float32",
                                                "colorInterpretation" "Undefined",
                                                "noDataValue"         -9999.0,
                                                "metadata"            {}}],
                          "driverLongName"    "ENVI .hdr Labelled",
                          "coordinateSystem"  {"wkt"                      "PROJCRS[ ...(ELIDED BY TESTING CODE)...",
                                               "dataAxisToSRSAxisMapping" [1 2],
                                               "proj4"                    "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"},
                          "files"             ["./test/gridfire/resources/envi-bsq-examples/phi.bsq"
                                               "./test/gridfire/resources/envi-bsq-examples/phi.bsq.aux.xml"
                                               "./test/gridfire/resources/envi-bsq-examples/phi.hdr"],
                          "wgs84Extent"       {"type"        "Polygon",
                                               "coordinates" [[[-119.672647 38.3266058]
                                                               [-119.662402 38.2954784]
                                                               [-119.6355513 38.3007517]
                                                               [-119.6457857 38.3318813]
                                                               [-119.672647 38.3266058]]]},
                          "metadata"          {"" {"AREA_OR_POINT" "Area", "Band_1" "Band 1"}, "IMAGE_STRUCTURE" {"INTERLEAVE" "BAND"}},
                          "geoTransform"      [-2028825.0 30.0 -0.0 1953435.0 -0.0 -30.0],
                          "size"              [80 120],
                          "cornerCoordinates" {"upperLeft"  [-2028825.0 1953435.0],
                                               "lowerLeft"  [-2028825.0 1949835.0],
                                               "lowerRight" [-2026425.0 1949835.0],
                                               "upperRight" [-2026425.0 1953435.0],
                                               "center"     [-2027625.0 1951635.0]},
                          "driverShortName"   "ENVI",
                          "description"       "./test/gridfire/resources/envi-bsq-examples/phi.bsq"}}))))

(deftest ^:bsq-test-suite represented-data-types-test
  (testing "We have tested on the following data types."
    (is (= (->> (get-test-bsq-files)
                (pmap (fn [bsq-file]
                        {(-> (gf-bsq/get-gdal-info bsq-file)
                             (get-in ["bands" 0 "type"]))
                         (sorted-set (file-name bsq-file))}))
                (apply merge-with into))
           {"Int16"   #{"asp.bsq"
                        "cbd.bsq"
                        "cbh.bsq"
                        "cc.bsq"
                        "ch.bsq"
                        "dem.bsq"
                        "fbfm40.bsq"
                        "pyromes.bsq"
                        "slp.bsq"},
            "Float32" #{"2020_90_SDIx100.bsq"
                        "adj.bsq"
                        "ercperc_2018.bsq"
                        "ignition_density.bsq"
                        "lh_2018.bsq"
                        "lw_2018.bsq"
                        "m100_2018.bsq"
                        "m10_2018.bsq"
                        "m1_2018.bsq"
                        "phi.bsq"
                        "wd_2018.bsq"
                        "ws_2018.bsq"}}))))

(deftest ^:bsq-test-suite example-bsq-load-like-the-geotiffs-test
  (testing "We parse our BSQ rasters equivalently to the corresponding GeoTiffs."
    (->> (get-test-bsq-files)
         (pmap
          (fn [bsq-file]
            (let [tif-file (tif-of-bsq-file bsq-file)
                  bsq-map  @(gf-bsq/read-bsq-file bsq-file)
                  tif-map  (geotiff-raster-to-tensor tif-file)]
              (testing "The geospatial metadata are the same."
                (let [relevant-gis-ks [:upperleftx
                                       :upperlefty
                                       :width
                                       :height
                                       :scalex
                                       :scaley]]
                  (is (= (select-keys bsq-map relevant-gis-ks)
                         (select-keys tif-map relevant-gis-ks))
                      "Same GIS metadata.")))
              (testing "We return {:srid nil}, but do provide an Envelope2D at :envelope."
                (is (nil? (:srid bsq-map)))
                (is (instance? Envelope2D (:envelope bsq-map)))
                (testing "This Envelope2D can then be supplied to Magellan to save the data as GeoTiff."
                  (let [tensor   (:matrix bsq-map)
                        matrix2D (t/mget tensor 0)]
                    (is (some? (matrix-to-raster (-> bsq-file (io/file) (file-name))
                                                 matrix2D
                                                 (:envelope bsq-map)))))))
              (testing "The tensors contain the same values."
                (let [bsq-tensor (-> (:matrix bsq-map)
                                     (as-> t
                                           (if (-> t (d/shape) (first) (= 1))
                                             (t/mget t 0)
                                             t)))
                      tif-tensor (:matrix tif-map)]
                  (is (= (d/shape bsq-tensor) (d/shape tif-tensor))
                      "Same matrix dimensions.")
                  (is (dfn/equals bsq-tensor tif-tensor)
                      "Same numeric values."))))))
         (doall))))



