;; [[file:../../../org/GridFire.org::gridfire.inputs.envi-bsq][gridfire.inputs.envi-bsq]]
(ns gridfire.inputs.envi-bsq
  "A custom parser for BSQ-interleaved ENVI raster files.
  This is a special case of GDAL's ENVI .hdr Labelled Raster:
  https://gdal.org/drivers/raster/envi.html#raster-envi."
  (:require [clojure.data.json  :as json]
            [clojure.java.io    :as io]
            [clojure.java.shell :as sh]
            [clojure.string     :as str]
            [manifold.deferred  :as mfd]
            [tech.v3.datatype   :as d]
            [tech.v3.tensor     :as t])
  (:import (java.io ByteArrayOutputStream)
           (java.nio ByteBuffer ByteOrder Buffer)
           (org.geotools.geometry Envelope2D)
           (org.geotools.referencing CRS)
           (org.opengis.referencing.crs CoordinateReferenceSystem)))

;; NOTE Why implement a custom parser? 2 reasons: (Val, 21 Oct 2022)
;; 1) Preserving GridFire's portability and ease of setup.
;; Setting up the native JVM-gdal interop is a tedious and unpredictable endeavor.
;; 2) Performance: there's every reason to believe that going through
;; GDAL then GeoTools would add a lot of overhead.
;; And at the time of writing, we use BSQ precisely because we want high
;; inputs-loading performance. And indeed, this is much faster than
;; loading equivalent GeoTIFFs.

(defn- slurp-array
  ^bytes [in]
  (with-open [baos (ByteArrayOutputStream.)]
    (io/copy in baos)
    (.toByteArray baos)))

(defn- sh-successful-out
  [sh-result]
  (if (zero? (:exit sh-result))
    (:out sh-result)
    (throw (ex-info (format "Shell command return error code %s: %s."
                            (str (:exit sh-result))
                            (:err sh-result))
                    sh-result))))

(defn get-gdal-info [f]
  (-> (sh/sh "gdalinfo" "-json" "-proj4" (-> f (io/file) (.getPath)))
      (sh-successful-out)
      ;; NOTE not parsing keys to keywords because some of them are pathological, like "".
      (json/read-str)))

(defn- check-expectation!
  [err-msg find-error-data]
  (when-some [err-data (find-error-data)]
    (throw (ex-info (format "BSQ file violates the GridFire parser's expectation: %s" err-msg)
                    err-data))))

(defmulti byte-buffer->array (fn [^String gdal-data-type ^ByteBuffer _bytbuf ^long _n-elements]
                               gdal-data-type))

(defmethod byte-buffer->array :default
  [gdal-data-type _bytbuf _n-elements]
  (throw (ex-info (format "Unsupported GDAL data type: %s" (pr-str gdal-data-type))
                  {::gdal-data-type gdal-data-type})))

(defn- parse-integer
  ^long [^String vstr]
  (Long/parseLong vstr 10))

(defn- parse-hdr-txt
  [^String hdr-txt]
  (let [hdr-txt1 (-> hdr-txt
                     ;; Should help remove entries spanning several lines
                     (str/replace "{\n" "{")
                     (str/replace ",\n" ","))]
    (->> (for [l             (str/split-lines hdr-txt1)
               prop-to-parse [[::hdr-header-offset "header offset" parse-integer]
                              [::hdr-byte-order-num "byte order" parse-integer]
                              [::hdr-cs-string "coordinate system string"
                               (fn unwrap-cs-string [cstr]
                                 (-> (re-matches #"\{(.*)\}" cstr)
                                     (nth 1)))]]
               :let          [[k propname coerce-fn] prop-to-parse]
               :when         (str/starts-with? l propname)]
           (let [lrest    (subs l (count propname))
                 [_ vstr] (or (re-matches #"\s*=\s*([^\s](.*[^\s])?)\s*" lrest)
                              (throw (ex-info (format "Failed to parse .hdr property %s" (pr-str propname))
                                              {::hdr-prop-name propname
                                               ::hdr-line      l
                                               ::hdr-txt       hdr-txt})))
                 v        ((or coerce-fn identity) vstr)]
             [k v]))
         (into {}))))

(defn find-hdr-file
  [bsq-file]
  (let [bsq-file    (io/file bsq-file)
        fname       (.getName bsq-file)
        [_ fprefix] (re-matches #"(.+)\.[^\.]+" fname)]
    (io/file (.getParentFile bsq-file) (str fprefix ".hdr"))))

(defn- fetch-hdr-data
  [bsq-file]
  (let [hdr-file (find-hdr-file bsq-file)
        hdr-txt  (slurp hdr-file)]
    (parse-hdr-txt hdr-txt)))

(defn- parse-WKT-into-CRS
  ^CoordinateReferenceSystem [^String wkt]
  (CRS/parseWKT wkt))

(defn read-bsq-file
  "Parses a BSQ-interleaved ENVI file.
  Will shell out to gdalinfo and also use the companion .hdr file,
  which must have the same name and location except for the extension.

  Returns a Manifold Deferred,
  holding 3D (band, x, y)-ordered tensor wrapped in a map of geospatial metadata."
  [bsq-file]
  ;; Returning a Manifold Deferred favors parallel fetching,
  ;; enabling callers to fetch several files in parallel
  ;; without blocking a waiting thread.
  (let [dfr-bsq-bytes (mfd/future (slurp-array (io/file bsq-file)))
        dfr-gdalinfo  (mfd/future (get-gdal-info bsq-file))
        dfr-hdr-data  (mfd/future (fetch-hdr-data bsq-file))]
    ;; Reading files in parallel, to reduce latency.
    (-> (mfd/zip dfr-bsq-bytes dfr-gdalinfo dfr-hdr-data)
        (mfd/chain
         (fn [[^bytes bsq-bytes gdalinfo hdr-data]]
           (check-expectation! "must be ENVI with BSQ layout."
                               (fn []
                                 (when-not (and (= "ENVI .hdr Labelled" (get gdalinfo "driverLongName"))
                                                (= "BAND" (get-in gdalinfo ["metadata" "IMAGE_STRUCTURE" "INTERLEAVE"])))
                                   {::gdalinfo gdalinfo})))
           (check-expectation! (format "must have %s." (pr-str {"dataAxisToSRSAxisMapping" [1 2]}))
                               (fn []
                                 (when-not (= [1 2] (get-in gdalinfo ["coordinateSystem" "dataAxisToSRSAxisMapping"]))
                                   {::gdalinfo gdalinfo})))
           (check-expectation! "must have equivalent bands."
                               (fn []
                                 (when-not (->> (get gdalinfo "bands")
                                                (map #(select-keys % ["noDataValue" "type"]))
                                                (apply =))
                                   {::gdalinfo gdalinfo})))
           (let [[x-width y-length] (get gdalinfo "size")
                 x-width-px         (long x-width)
                 y-height-px        (long y-length)
                 [x-ul y-ul]        (get-in gdalinfo ["cornerCoordinates" "upperLeft"])
                 scalex             (double (get-in gdalinfo ["geoTransform" 1]))
                 scaley             (double (get-in gdalinfo ["geoTransform" 5]))
                 band-info          (get-in gdalinfo ["bands" 0])
                 n-bands            (long (count (get gdalinfo "bands")))
                 byte-order-num     (long (or (::hdr-byte-order-num hdr-data) 0))
                 header-offset      (long (or (::hdr-header-offset hdr-data) 0))
                 data-type          (get band-info "type")
                 n-pixels           (* x-width-px y-height-px n-bands)
                 bytbuf             (-> (ByteBuffer/wrap bsq-bytes header-offset (- (alength bsq-bytes) header-offset))
                                        (.order (case byte-order-num
                                                  0 ByteOrder/LITTLE_ENDIAN
                                                  1 ByteOrder/BIG_ENDIAN)))
                 data-arr           (byte-buffer->array data-type bytbuf n-pixels)
                 tensor             (-> (d/->buffer data-arr)
                                        (t/->tensor)
                                        ;; NOTE if we didn't have {"dataAxisToSRSAxisMapping" [1 2]},
                                        ;; I guess that this should be followed by a (t/transpose [0 2 1]).
                                        (t/reshape [n-bands y-height-px x-width-px]))]
             {;; AFAICT the :srid resolved from layers is never used; (Val, 20 Oct 2022)
              ;; what we use the SRID of the GridFire config.
              ;; The only downstream use of said SRID is to create an Envelope2D for writing outputs,
              ;; and even that does not really require an SRID:
              ;; we can create an Envelope2D from the CRS,
              ;; and that's actually how Magellan does it behind the scenes.
              ;; See the tests for an example.
              ;; NOTE: why not use gdalsrsinfo to resolve the SRID? (Val, 21 Oct 2022)
              ;; For test data on my machine, that did not return any matches;
              ;; so if we can't rely on it, I think it's not worth implementing it,
              ;; all the more so because the :srid is not really needed, as noted above.
              :srid       nil
              ;; NOTE For lack of an SRID leaving around an Envelope2D object directly,
              ;; so that the raster can still be saved.
              :envelope   (let [wkt         (::hdr-cs-string hdr-data)
                                crs         (parse-WKT-into-CRS wkt)
                                [x-lr y-lr] (get-in gdalinfo ["cornerCoordinates" "lowerRight"])
                                xmin        (min x-ul (double x-lr))
                                ymin        (min y-ul (double y-lr))]
                            (Envelope2D. crs
                                         xmin
                                         ymin
                                         (Math/abs (* scalex x-width-px))
                                         (Math/abs (* scaley y-height-px))))
              :upperleftx x-ul
              :upperlefty y-ul
              :width      x-width-px
              :height     y-height-px
              :scalex     (get-in gdalinfo ["geoTransform" 1])
              :scaley     (get-in gdalinfo ["geoTransform" 5])
              :skewx      0.0                               ; currently unused
              :skewy      0.0                               ; currently unused
              :matrix     tensor})))
        (mfd/catch
         (fn [err]
           (let [file-path (.getPath (io/file bsq-file))]
             (throw (ex-info (format "Error parsing BSQ file (%s) with GridFire's custom parser: %s %s"
                                     (-> bsq-file (io/file) (.getPath))
                                     (-> err (type) (str))
                                     (ex-message err))
                             (merge
                              {::bsq-file-path file-path}
                              (when (mfd/realized? dfr-gdalinfo)
                                (-> dfr-gdalinfo
                                    (mfd/chain (fn [gdalinfo] {::gdalinfo gdalinfo}))
                                    (mfd/catch (constantly nil))
                                    (deref))))
                             err))))))))

(defn- check-expected-buffer-size!
  [^Buffer data-buf n-elements]
  (check-expectation! "must have the expected length."
                      (fn []
                        (when-not (= n-elements (.remaining data-buf))
                          (throw (ex-info (format "expected %d elements in Buffer, got %d"
                                                  n-elements
                                                  data-buf)
                                          {::expected-buffer-size n-elements
                                           ::actual-buffer-size   (.remaining data-buf)
                                           ::data-buffer          data-buf}))))))

;; NOTE unsigned types (UInt16, etc.) are not currently supported. (Val, 20 Oct 2022)
(defmethod byte-buffer->array "Int16"
  [_ ^ByteBuffer bytbuf ^long n-elements]
  (let [data-arr (short-array n-elements)
        data-buf (.asShortBuffer bytbuf)]
    (check-expected-buffer-size! data-buf n-elements)
    (.get data-buf (shorts data-arr))
    data-arr))

(defmethod byte-buffer->array "Int32"
  [_ ^ByteBuffer bytbuf ^long n-elements]
  (let [data-arr (int-array n-elements)
        data-buf (.asIntBuffer bytbuf)]
    (check-expected-buffer-size! data-buf n-elements)
    (.get data-buf (ints data-arr))
    data-arr))

(defmethod byte-buffer->array "Float32"
  [_ ^ByteBuffer bytbuf ^long n-elements]
  (let [data-arr (float-array n-elements)
        data-buf (.asFloatBuffer bytbuf)]
    (check-expected-buffer-size! data-buf n-elements)
    (.get data-buf (floats data-arr))
    data-arr))

(defmethod byte-buffer->array "Float64"
  [_ ^ByteBuffer bytbuf ^long n-elements]
  (let [data-arr (double-array n-elements)
        data-buf (.asDoubleBuffer bytbuf)]
    (check-expected-buffer-size! data-buf n-elements)
    (.get data-buf (doubles data-arr))
    data-arr))
;; gridfire.inputs.envi-bsq ends here
