(ns gridfire.lab.replay
  (:require [clojure.java.io              :as io]
            [clojure.java.shell           :as sh]
            [clojure.pprint               :as pprint]
            [clojure.string               :as str]
            [gridfire.core]
            [gridfire.fire-spread-optimal :refer [rothermel-fast-wrapper-optimal run-fire-spread]]
            [gridfire.magellan-bridge     :refer [geotiff-raster-to-tensor]]
            [gridfire.simulations         :as simulations]
            [gridfire.utils.files.tar     :as utar]
            [tech.v3.datatype             :as d]
            [tech.v3.datatype.functional  :as dfn]
            [tech.v3.datatype.statistics  :as dstats])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time ZonedDateTime)
           (java.time.format DateTimeFormatter)
           (java.util Date)))

;;;; How this was set up:

;; How I installed Java and Clojure:
;;sudo apt install openjdk-17-jdk
;;sudo apt install rlwrap
;;curl -O https://download.clojure.org/install/linux-install-1.11.1.1165.sh
;;chmod +x linux-install-1.11.1.1165.sh
;;sudo ./linux-install-1.11.1.1165.sh

;; How I sync the code from my laptop to the remote VM, using Git:
;;vwaeselynck@iiwi:~$ mkdir gridfire.git
;;vwaeselynck@iiwi:~$ cd gridfire.git/
;;vwaeselynck@iiwi:~/gridfire.git$ git init --bare
;;$ git push sigvm --all
;;vwaeselynck@iiwi:~$ git clone gridfire.git/
;;Cloning into 'gridfire'...
;;vwaeselynck@iiwi:~/gridfire$ nohup clj -M:nREPL:mydev:gridfire-my-dev -m nrepl.cmdline --port 8888 >> ../repl-out.txt 2>&1 &
;;vwaeselynck@iiwi:~$ cd gridfire
;;vwaeselynck@iiwi:~/gridfire$ git checkout vwaeselynck-330-gridfire-historical-fires

;; How I start and connect to the remote nREPL server
;;vwaeselynck@iiwi:~/gridfire-scripts/lab$ nohup clj -J-XX:MaxRAMPercentage=90 -J-Djdk.attach.allowAttachSelf=true -M:nREPL:mydev:gridfire-my-dev -m nrepl.cmdline --port 8888 >> ../repl-out.txt 2>&1 &
;;$ ssh -i ~/.ssh/id_rsa -4N -L 18888:localhost:8888 vwaeselynck@10.1.30.147

;; How I fetch the FTP-saved PyreCast snapshots:
;; vwaeselynck@iiwi:~/pyrecast-snapshots-2022-09-30$ wget --timestamping ftps://vwaeselynck:5Km_32erTf@ftp.sig-gis.com/kcheung/*.gz

;; ------------------------------------------------------------------------------
;; File and CLI utilities

;; FIXME move those to generic ns?
(defn sh-safe
  [& args]
  (let [ret (apply sh/sh args)]
    (if (not= 0 (:exit ret))
      (let [err-string (or (not-empty (:err ret)) (:out ret))]
        (throw (ex-info
                (format "Error running sh command: %s" err-string)
                (merge
                 {:sh-args (vec args)}
                 ret))))
      ret)))

(defn file-name
  [^File file]
  (.getName file))

(defn file-abs-path
  ^String [f]
  (.getCanonicalPath (io/file f)))

(defn ensure-file
  [f]
  (let [ret (io/file f)]
    (when-not (.exists ret)
      (throw (ex-info (format "File does not exist: %s" (.getPath ret))
                      {:file  ret
                       :f-arg f})))
    ret))

(defn delete-dir-recursively!
  [file]
  (->> file (file-seq) (reverse) (run! io/delete-file)))

(defn with-tmp-dirs*
  ([prefixes callback-fn] (with-tmp-dirs* nil prefixes callback-fn))
  ([containing-dir prefixes callback-fn]
   (let [tmp-dirs
         (->> prefixes
              (mapv (fn create-tmp-dir [^String prefix]
                      (.toFile
                       (let [empty-fileattrs-array (into-array FileAttribute [])]
                         (if (nil? containing-dir)
                           (Files/createTempDirectory prefix empty-fileattrs-array)
                           (Files/createTempDirectory (-> (io/file containing-dir)
                                                          (doto (.mkdirs))
                                                          (.toPath))
                                                      prefix
                                                      empty-fileattrs-array)))))))]
     (try
       (apply callback-fn tmp-dirs)
       (finally
         (run! delete-dir-recursively! tmp-dirs))))))

(defn archive-entries->files!
  "Saves archive entries as files in a directory, optionally returning a subset of the created files in a map.

  Given:
  - target-dir: the directory into which to write the files,
  - return-key-fn: a function,
  - archive-entries: a sequence of [^String entry-name ^bytes entry-bytes] pairs,

  will create one file or directory per entry under target-dir,
  at relative path given by entry-name, and content entry-bytes.
  Returns a map; whenever (return-key-fn entry-name) returns a non-nil value entry-k,
  the returned map will contain the created java.io.File at key entry-k."
  [target-dir return-key-fn archive-entries]
  (reduce
   (fn write-entry-to-file! [returned-files [entry-name entry-bytes]]
     (let [entry-file (io/file target-dir entry-name)]
       (io/make-parents entry-file)
       (let [is-dir? (str/ends-with? entry-name "/")]
         (if is-dir?
           (.mkdir entry-file)
           (io/copy entry-bytes entry-file)))
       (merge returned-files
              (when-some [ret-k (return-key-fn entry-name)]
                {ret-k entry-file}))))
   {}
   archive-entries))



;; ------------------------------------------------------------------------------
;; Extracting PyreCast snapshots

(defn parse-pyrecast-snapshot-archive-name
  [^File input-deck-targz]
  (let [file-name  (.getName input-deck-targz)
        name-cpnts (re-matches #"(.+)_(\d{8})_(\d{6})_(\d{3})\.tar\.gz" file-name)]
    (if (nil? name-cpnts)
      (throw (ex-info (format "Failed to parse PyreCast snapshot filename: %s" (pr-str file-name))
                      {:pyrcst_snapshot_archive_name file-name}))
      (let [[_ fire-name fire-date-str fire-time-str subnum] name-cpnts
            snap {; INTRO denotes a map holding information about a PyreCast snapshot.
                  :pyrcst_snapshot_archive_name file-name
                  :pyrcst_fire_name             fire-name
                  :pyrcst_fire_subnumber        (Long/parseLong subnum 10)
                  :pyrcst_snapshot_inst         (-> (str fire-date-str " " fire-time-str " Z")
                                                    (ZonedDateTime/parse (DateTimeFormatter/ofPattern "yyyyMMdd HHmmss z"))
                                                    (.toInstant)
                                                    (Date/from))
                  :pyrcst_snapshot_archive      input-deck-targz}]
        snap))))

(defn list-fire-snapshots-from-dir
  [pyrecast-snapshots-dir]
  (let [decks-dir (ensure-file pyrecast-snapshots-dir)]
    (->> (file-seq decks-dir)
         (filter #(-> (file-name %) (str/ends-with? ".tar.gz")))
         (map parse-pyrecast-snapshot-archive-name)
         (sort-by (juxt :pyrcst_fire_name :pyrcst_fire_subnumber :pyrcst_snapshot_inst))
         (vec))))

(defn rewrite-gridfire-config-with-relative-paths
  [^String gridfire-edn-contents ^String absolute-path-prefix]
  (str/replace gridfire-edn-contents (str "\"" absolute-path-prefix "/") "#gridfire.utils.files/from-this-file \""))

(defn sanitize-pyrecast-snapshot-entries
  "Filters, renames and rewrites the TAR entries for a PyreCast snapshot
  into a locally-runnable input deck.

  Accepts and returns a sequence of [entry-name entry-bytes] pairs."
  [snap-entries]
  (let [tar-entry-prefix   "home/gridfire_data/"
        config-path-prefix "/home/gridfire/data/"]
    (->> snap-entries
         (map (fn drop-prefix-path [[entry-name entry-bytes]]
                [(subs entry-name (count tar-entry-prefix))
                 entry-bytes]))
         (remove (fn is-unneeded-file? [[entry-name _entry-bytes]]
                   (let [[_ _snapshot-name rel-path] (re-matches #"([^\/]+)\/(.*)" entry-name)]
                     (re-matches #"outputs/.+" rel-path))))
         (map (fn repath-gridfire-edn [entry-pair]
                (let [[entry-name entry-bytes] entry-pair
                      [_ snapshot-name _rel-path] (re-matches #"([^\/]+)\/(.*)" entry-name)]
                  (if (str/ends-with? entry-name "/gridfire.edn")
                    [entry-name
                     (-> (String. (bytes entry-bytes) "UTF-8")
                         (rewrite-gridfire-config-with-relative-paths (str config-path-prefix snapshot-name))
                         (.getBytes "UTF-8"))]
                    entry-pair)))))))

(defn interesting-file-key
  [entry-name]
  (cond
    (str/ends-with? entry-name "/gridfire.edn")
    ::gridfire-config-file

    (str/ends-with? entry-name "/active_fire_polygon_utm_posnegbuff.shp")
    ::active_fire_polygon_utm_posnegbuff-shp

    (str/ends-with? entry-name "/fuels_and_topography/already_burned.tif")
    ::already_burned-tif))


(defn extract-pyrecast-snapshots!
  [pyrecast-snapshots-dir]
  (let [tmp-dir (.toFile (Files/createTempDirectory (-> (io/file "../tmp-gridfire-replay") (doto (.mkdirs)) (.toPath) (.toAbsolutePath))
                                                    "extracted-snapshots"
                                                    (into-array FileAttribute [])))]
    (->> (list-fire-snapshots-from-dir pyrecast-snapshots-dir)
         (pmap
          (fn extract-snapshot! [snap]
            (merge
             snap
             {::useful-files
              (with-open [tais (utar/targz-input-stream (:pyrcst_snapshot_archive snap))]
                (->> (utar/tar-archive-entries tais)
                     (sanitize-pyrecast-snapshot-entries)
                     (archive-entries->files! tmp-dir interesting-file-key)))})))
         (vec))))


;; ------------------------------------------------------------------------------
;; Comparing observed and simulated burn areas

(defn config-max-runtime-until-stop-inst
  [inputs stop-inst]
  (assoc inputs :max-runtime (/
                              (-
                               (inst-ms stop-inst)
                               (->> [:ignition-start-timestamp
                                     :weather-start-timestamp]
                                    (keep #(get inputs %))
                                    (map inst-ms)
                                    (apply min)))
                              (* 1e3 60))))

(defn run-fire-spread-until
  [config stop-inst]
  (some-> config
          (config-max-runtime-until-stop-inst stop-inst)
          (gridfire.core/load-inputs!)
          (as-> inputs
                {::simulations/inputs inputs
                 ::simulations/result
                 (-> (simulations/prepare-simulation-inputs 0 inputs)
                     (run-fire-spread))})))

(defn resolve-observed-burned-area
  "Computes the observed burned area between 2 PyreCast snapshots (from t0 to t1),
  as the set difference between active-fire-at-t1 and already-burned-at-t0.

  Accepts files and returns a GIS-enveloped matrix."
  [t0-already_burned-tif t1-active_fire_polygon_utm_posnegbuff-shp]
  (with-tmp-dirs*
   ["tmp_resolve-observed-burned-area"]
   (fn [tmp-dir]
     (let [t0-already_burned-tif                     (ensure-file t0-already_burned-tif)
           ;; NOTE 'posnegbuff' hints at the algorithm used for approximating the perimeter from the original hot-spots polygons,
           ;; which tend to paint a dotted landscape; a succession of buffering and negative buffering is used to approximate
           ;; the perimeter from these polygons. See Elmfire script 01-setup_run.sh in input decks. (Val, 29 Sep 2022)
           t1-active_fire_polygon_utm_posnegbuff-shp (ensure-file t1-active_fire_polygon_utm_posnegbuff-shp)
           t1-active_fire_posnegbuff-tif             (io/file tmp-dir "t1-active_fire_polygon_utm_posnegbuff.tif")
           expected-burned-area-tif                  (io/file tmp-dir "expected-burned-area.tif")]
       ;; NOTE I chose GDAL over PostGis for these computations, (Val, 29 Sep 2022)
       ;; because I judged that the GDAL executables are easier to take for granted
       ;; than a running Postgres server.
       ;; Writing a GeoTiff with zeroes
       (sh-safe "gdal_calc.py" "-A" (file-abs-path t0-already_burned-tif)
                "--NoDataValue=255"
                "--type=Byte"
                "--calc=A*0"
                "--overwrite"
                (str "--outfile=" (file-abs-path t1-active_fire_posnegbuff-tif)))
       ;; Burning the polygon into it
       (sh-safe "gdal_rasterize" "-burn" "1" "-at"
                (file-abs-path t1-active_fire_polygon_utm_posnegbuff-shp)
                (file-abs-path t1-active_fire_posnegbuff-tif))
       ;; Computing the expected burned area
       (sh-safe "gdal_calc.py"
                "-A" (file-abs-path t0-already_burned-tif)
                "-B" (file-abs-path t1-active_fire_posnegbuff-tif)
                "--NoDataValue=255"
                "--type=Byte"
                "--calc=(A==0)*(B==1)"                      ; set difference
                (str "--outfile=" (file-abs-path expected-burned-area-tif)))
       (geotiff-raster-to-tensor (file-abs-path expected-burned-area-tif) :float32 identity)))))

(defn simulated-burned-area-matrix
  [stop-inst sim-inputs sim-result]
  (let [minutes-until-stop-inst (/ (-
                                    (inst-ms stop-inst)
                                    (inst-ms (first (:ignition-start-timestamps sim-inputs))))
                                   (* 1e3 60))]
    (dfn/max ; set union
     (:ignition-matrix sim-inputs)
     (d/elemwise-cast
      (dfn/<=
       0.
       (:burn-time-matrix sim-result)
       minutes-until-stop-inst)
      :double))))

(defn fetch-snapshot-config
  [snap]
  (-> (gridfire.core/load-config! (-> snap
                                      ::useful-files
                                      ::gridfire-config-file
                                      (ensure-file)
                                      (.getCanonicalPath)))
      (into (sorted-map))))

(defn replay-snapshot
  [t0-snap t1-snap]
  (let [stop-inst          (:pyrcst_snapshot_inst t1-snap)
        observed-burn-area (resolve-observed-burned-area
                            (-> t0-snap ::useful-files ::already_burned-tif)
                            (-> t1-snap ::useful-files ::active_fire_polygon_utm_posnegbuff-shp))

        config             (-> (fetch-snapshot-config t0-snap)
                               (dissoc :perturbations)      ; to remove variance.
                               ; FIXME shall we run them all? Probably want more control over that.
                               (merge {:simulations 1}))
        sim-output         (run-fire-spread-until config stop-inst)]
    {::observed-burn-area observed-burn-area
     ::sim-output         sim-output}))

(defn burn-trace-stats
  [replay-res]
  (let [stop-inst            (-> replay-res ::t1-fire :pyrcst_snapshot_inst)
        observed-burn-area   (::observed-burn-area replay-res)
        sim-output           (::sim-output replay-res)
        observed-burn-matrix (:matrix observed-burn-area)
        sim-result           (::simulations/result sim-output)]
    (if (nil? sim-result)
      {:observed-burn-n-cells (dfn/sum observed-burn-matrix)
       :sim-inter-obs-n-cells 0
       :sim-minus-obs-n-cells 0
       :obs-minus-sim-n-cells (dfn/sum observed-burn-matrix)
       ::comments             ["run-fire-spread returned nil, which happens when it fails to get perimeter cells with burnable neighbours."]}
      (let [sim-burn-matrix (simulated-burned-area-matrix stop-inst (::simulations/inputs sim-output) sim-result)]
        {:observed-burn-n-cells (dfn/sum observed-burn-matrix)
         :sim-inter-obs-n-cells (dfn/sum (dfn/* sim-burn-matrix observed-burn-matrix))
         :sim-minus-obs-n-cells (dfn/sum (dfn/* sim-burn-matrix (dfn/- 1.0 observed-burn-matrix)))
         :obs-minus-sim-n-cells (dfn/sum (dfn/* observed-burn-matrix (dfn/- 1.0 sim-burn-matrix)))}))))

(defn can-replay-t0-to-t1?
  [t0-snap t1-snap]
  (and
   (and
    ;; Of course, both snapshots should be of the same fire...
    (= (:pyrcst_fire_name t0-snap) (:pyrcst_fire_name t1-snap))
    (= (:pyrcst_fire_subnumber t0-snap) (:pyrcst_fire_subnumber t1-snap)))
   (and
    ;; ... we also want them to have the required files...
    (-> t0-snap ::useful-files ::already_burned-tif (some?))
    (-> t0-snap ::useful-files ::gridfire-config-file (some?))
    (-> t1-snap ::useful-files ::active_fire_polygon_utm_posnegbuff-shp (some?)))
   ;; ... we want the t1 snapshot some time after t0, neither too long nor too short:
   (let [time-lapse-ms (- (-> t1-snap :pyrcst_snapshot_inst (inst-ms))
                          (-> t0-snap :pyrcst_snapshot_inst (inst-ms)))
         ms-3hr        (* 1000 60 60)
         ms-3days      (* 1000 60 60 24 3)]
     (<= ms-3hr
         ;; A too-short time lapse makes us more sensitive to inaccuracies
         ;; in estimating the real burned area.
         time-lapse-ms
         ;; short time lapses seem to be the most important to get right:
         ;; short-term accuracy will probably tend to imply long-term accuracy,
         ;; but the converse is not true.
         ;; What's more, if we let too many days elapse, the weather data
         ;; in t0-snap will be either missing or inaccurate.
         ms-3days))))

(defn replayable-successive-t0t1-pairs
  [snaps]
  (->> snaps
       (sort-by (juxt :pyrcst_fire_name :pyrcst_fire_subnumber :pyrcst_snapshot_inst))
       (partition-all 2 1)
       (filter
        (fn [[t0-snap t1-snap]]
          (can-replay-t0-to-t1? t0-snap t1-snap)))))

(defn replay-snapshots
  [snaps]
  (with-redefs [rothermel-fast-wrapper-optimal (memoize rothermel-fast-wrapper-optimal)]
    (->> (replayable-successive-t0t1-pairs snaps)
         ;(sort-by hash) (take 16) ; uncomment to work on a small sub-sample. (Val, 30 Sep 2022)
         (pmap
          (fn compare-snapshots [[t0-snap t1-snap]]
            (println "Replaying" (:pyrcst_fire_name t0-snap) "from" (-> t0-snap :pyrcst_snapshot_inst) "to" (-> t1-snap :pyrcst_snapshot_inst))
            (merge
             (select-keys t0-snap [:pyrcst_fire_name :pyrcst_fire_subnumber])
             {::t0-fire (select-keys t0-snap [:pyrcst_snapshot_inst])
              ::t1-fire (select-keys t1-snap [:pyrcst_snapshot_inst])}
             (replay-snapshot t0-snap t1-snap))))
         (vec))))

(defn toa-stats
  [replay-res]
  (let [stop-inst            (-> replay-res ::t1-fire :pyrcst_snapshot_inst)
        observed-burn-area   (::observed-burn-area replay-res)
        sim-output           (::sim-output replay-res)
        observed-burn-matrix (:matrix observed-burn-area)
        sim-result           (::simulations/result sim-output)]
    (when-not (nil? sim-result)
      (let [sim-burn-matrix        (simulated-burned-area-matrix stop-inst (::simulations/inputs sim-output) sim-result)
            sim-inter-real-matrix  (dfn/* sim-burn-matrix observed-burn-matrix)
            time-lapse-min         (-> (- (-> replay-res ::t1-fire :pyrcst_snapshot_inst (inst-ms))
                                          (-> replay-res ::t0-fire :pyrcst_snapshot_inst (inst-ms)))
                                       (double)
                                       (/ (* 1000 60)))
            toa-ratio-matrix       (d/emap
                                    (fn [burn-time sim-inter-real]
                                      (if (and (= 1.0 sim-inter-real) (> burn-time 0.))
                                        (/ burn-time
                                           time-lapse-min)
                                        Double/NaN))
                                    :double
                                    (:burn-time-matrix sim-result)
                                    sim-inter-real-matrix)
            stats-opts             {:nan-strategy :remove}]
        (let [[p50 p75 p90 p95] (dstats/percentiles [50 75 90 95] stats-opts toa-ratio-matrix)]
          {::toa-ratio-mean (dstats/mean toa-ratio-matrix stats-opts)
           ::toa-ratio-p50  p50
           ::toa-ratio-p75  p75
           ::toa-ratio-p90  p90
           ::toa-ratio-p95  p95})))))

(defn pprint-table-from-kv-lists
  [pairs-vecs]
  (let [pairs0 (first pairs-vecs)
        cols (mapv first pairs0)]
    (pprint/print-table
     cols
     (map #(into {} %) pairs-vecs))))


(defn print-replayed-results-table
  [replay-results]
  (pprint-table-from-kv-lists
   (->> replay-results
        (pmap (fn [replay-res]
                (let [stats                 (merge (burn-trace-stats replay-res)
                                                   (toa-stats replay-res))
                      n-cells-really-burned (long (:observed-burn-n-cells stats))]
                  (letfn [(format-w-pct [^long n-cells]
                            (if (zero? n-cells-really-burned)
                              (format "%d (-)" n-cells)
                              (format "%d (%2.1f%%)"
                                      n-cells
                                      (* 100.0 (/ n-cells n-cells-really-burned)))))]
                    [["Fire name" (:pyrcst_fire_name replay-res)]
                     ["t0" (-> replay-res ::t0-fire :pyrcst_snapshot_inst)]
                     ["t1" (format "%s (+%2dhr)"
                                   (-> replay-res ::t1-fire :pyrcst_snapshot_inst (str))
                                   (-> (-
                                        (-> replay-res ::t1-fire :pyrcst_snapshot_inst (inst-ms))
                                        (-> replay-res ::t0-fire :pyrcst_snapshot_inst (inst-ms)))
                                       (/ (* 1e3 60 60))
                                       (double)
                                       (Math/round) (long)))]
                     ["n cells really burned" (:observed-burn-n-cells stats)]
                     ["sim ∩ real" (format-w-pct (:sim-inter-obs-n-cells stats))]
                     ["ToA-ratio: mean" (some->> (::toa-ratio-mean stats) (format "%1.2f"))]
                     ["p50" (some->> (::toa-ratio-p50 stats) (format "%1.2f"))]
                     ["p75" (some->> (::toa-ratio-p75 stats) (format "%1.2f"))]
                     ["p90" (some->> (::toa-ratio-p90 stats) (format "%1.2f"))]
                     ["p95" (some->> (::toa-ratio-p95 stats) (format "%1.2f"))]
                     ["sim - real" (format-w-pct (:sim-minus-obs-n-cells stats))]
                     ["real - sim" (format-w-pct (:obs-minus-sim-n-cells stats))]])))))))


(comment

  (def snaps (extract-pyrecast-snapshots! "../pyrecast-snapshots-2022-09-30"))

  (count snaps)
  ;;=> 256

  (->> snaps (map ::useful-files) (mapcat keys) frequencies)
  ;; => ;; Darn, some of them are missing
  #:gridfire.lab.replay{:gridfire-config-file                   253,
                        :already_burned-tif                     247,
                        :active_fire_polygon_utm_posnegbuff-shp 250}

  (-> (replayable-successive-t0t1-pairs snaps) (count))
  ;;=> 187

  (->> snaps
       (filter #(-> % ::useful-files ::gridfire-config-file (some?)))
       (map fetch-snapshot-config)
       (keep :suppression))
  ;;=> ()

  (def replay-results (time (replay-snapshots snaps)))
  ;;"Elapsed time: 1002229.596932 msecs"


  (print-replayed-results-table replay-results)
  ;;|               Fire name |                           t0 |                                   t1 | n cells really burned |    sim ∩ real |      sim - real |    real - sim |
  ;;|-------------------------+------------------------------+--------------------------------------+-----------------------+---------------+-----------------+---------------|
  ;;|               ca-barnes | Tue Sep 13 13:18:00 UTC 2022 | Tue Sep 13 19:12:00 UTC 2022 (+ 6hr) |                 888.0 |   327 (36.8%) |        2 (0.2%) |   561 (63.2%) |
  ;;|             ca-mosquito | Wed Sep 14 09:05:00 UTC 2022 | Wed Sep 14 14:11:00 UTC 2022 (+ 5hr) |               86484.0 | 38150 (44.1%) |        0 (0.0%) | 48334 (55.9%) |
  ;;|             ca-mosquito | Wed Sep 14 14:11:00 UTC 2022 | Wed Sep 14 22:01:00 UTC 2022 (+ 8hr) |               91936.0 | 61882 (67.3%) |   37248 (40.5%) | 30054 (32.7%) |
  ;;|             ca-mosquito | Wed Sep 14 22:01:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+12hr) |              108947.0 | 77291 (70.9%) |   47466 (43.6%) | 31656 (29.1%) |
  ;;|             ca-mosquito | Thu Sep 15 09:34:00 UTC 2022 | Fri Sep 16 09:17:00 UTC 2022 (+24hr) |               96989.0 | 68327 (70.4%) |   80489 (83.0%) | 28662 (29.6%) |
  ;;|             ca-mosquito | Fri Sep 16 09:17:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+24hr) |               44211.0 | 40102 (90.7%) |  76347 (172.7%) |   4109 (9.3%) |
  ;;|             ca-mosquito | Sat Sep 17 08:58:00 UTC 2022 | Sat Sep 17 22:26:00 UTC 2022 (+13hr) |               18460.0 | 16016 (86.8%) |   15966 (86.5%) |  2444 (13.2%) |
  ;;|             ca-mosquito | Sat Sep 17 22:26:00 UTC 2022 | Sun Sep 18 09:32:00 UTC 2022 (+11hr) |               15440.0 | 14867 (96.3%) |  35925 (232.7%) |    573 (3.7%) |
  ;;|                  ca-red | Wed Sep 14 09:05:00 UTC 2022 | Wed Sep 14 16:50:00 UTC 2022 (+ 8hr) |                  96.0 |      0 (0.0%) |        0 (0.0%) |   96 (100.0%) |
  ;;|                  ca-red | Wed Sep 14 16:50:00 UTC 2022 | Thu Sep 15 10:28:00 UTC 2022 (+18hr) |                  96.0 |      0 (0.0%) |        0 (0.0%) |   96 (100.0%) |
  ;;|                  ca-red | Thu Sep 15 10:28:00 UTC 2022 | Thu Sep 15 17:34:00 UTC 2022 (+ 7hr) |                  97.0 |      0 (0.0%) |        0 (0.0%) |   97 (100.0%) |
  ;;|                  ca-red | Thu Sep 15 17:34:00 UTC 2022 | Thu Sep 15 20:57:00 UTC 2022 (+ 3hr) |                  97.0 |      0 (0.0%) |        0 (0.0%) |   97 (100.0%) |
  ;;|                  ca-red | Thu Sep 15 20:57:00 UTC 2022 | Fri Sep 16 09:17:00 UTC 2022 (+12hr) |                  97.0 |      0 (0.0%) |        0 (0.0%) |   97 (100.0%) |
  ;;|                  ca-red | Fri Sep 16 09:17:00 UTC 2022 | Fri Sep 16 20:40:00 UTC 2022 (+11hr) |                  44.0 |      0 (0.0%) |        0 (0.0%) |   44 (100.0%) |
  ;;|                  ca-red | Fri Sep 16 20:40:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+12hr) |                  44.0 |      0 (0.0%) |        0 (0.0%) |   44 (100.0%) |
  ;;|                  ca-red | Sat Sep 17 08:58:00 UTC 2022 | Sat Sep 17 21:12:00 UTC 2022 (+12hr) |                  72.0 |      0 (0.0%) |        0 (0.0%) |   72 (100.0%) |
  ;;|              ca-rodgers | Wed Sep 14 09:56:00 UTC 2022 | Thu Sep 15 21:07:00 UTC 2022 (+35hr) |                  40.0 |      0 (0.0%) |        0 (0.0%) |   40 (100.0%) |
  ;;|              ca-rodgers | Thu Sep 15 21:07:00 UTC 2022 | Fri Sep 16 16:28:00 UTC 2022 (+19hr) |                  40.0 |      0 (0.0%) |        0 (0.0%) |   40 (100.0%) |
  ;;|              ca-rodgers | Fri Sep 16 16:28:00 UTC 2022 | Sat Sep 17 09:49:00 UTC 2022 (+17hr) |                  40.0 |      0 (0.0%) |        0 (0.0%) |   40 (100.0%) |
  ;;|               ca-summit | Sun Sep 18 10:21:00 UTC 2022 | Sun Sep 18 16:30:00 UTC 2022 (+ 6hr) |                2461.0 |   811 (33.0%) |        0 (0.0%) |  1650 (67.0%) |
  ;;|               ca-summit | Sun Sep 18 16:30:00 UTC 2022 | Mon Sep 19 10:02:00 UTC 2022 (+18hr) |                2462.0 |  1917 (77.9%) |       40 (1.6%) |   545 (22.1%) |
  ;;|               ca-summit | Mon Sep 19 10:02:00 UTC 2022 | Mon Sep 19 21:23:00 UTC 2022 (+11hr) |                1396.0 |  1138 (81.5%) |        0 (0.0%) |   258 (18.5%) |
  ;;|               ca-summit | Mon Sep 19 21:23:00 UTC 2022 | Tue Sep 20 08:54:00 UTC 2022 (+12hr) |                1396.0 |  1146 (82.1%) |        2 (0.1%) |   250 (17.9%) |
  ;;|               ca-summit | Tue Sep 20 08:54:00 UTC 2022 | Wed Sep 21 10:15:00 UTC 2022 (+25hr) |                   0.0 |         0 (-) |           0 (-) |         0 (-) |
  ;;|               ca-summit | Wed Sep 21 10:15:00 UTC 2022 | Thu Sep 22 09:58:00 UTC 2022 (+24hr) |                   0.0 |         0 (-) |           0 (-) |         0 (-) |
  ;;|               ca-summit | Thu Sep 22 09:58:00 UTC 2022 | Thu Sep 22 21:18:00 UTC 2022 (+11hr) |                   0.0 |         0 (-) |           0 (-) |         0 (-) |
  ;;|               ca-summit | Thu Sep 22 21:18:00 UTC 2022 | Sat Sep 24 09:20:00 UTC 2022 (+36hr) |                  14.0 |      0 (0.0%) |        0 (0.0%) |   14 (100.0%) |
  ;;|               ca-summit | Sat Sep 24 09:20:00 UTC 2022 | Sun Sep 25 09:49:00 UTC 2022 (+24hr) |                 144.0 |      0 (0.0%) |        0 (0.0%) |  144 (100.0%) |
  ;;|               ca-summit | Sun Sep 25 09:49:00 UTC 2022 | Mon Sep 26 09:32:00 UTC 2022 (+24hr) |                 147.0 |    62 (42.2%) |        0 (0.0%) |    85 (57.8%) |
  ;;|               ca-summit | Mon Sep 26 09:32:00 UTC 2022 | Tue Sep 27 09:11:00 UTC 2022 (+24hr) |                  49.0 |      0 (0.0%) |        0 (0.0%) |   49 (100.0%) |
  ;;|               ca-summit | Tue Sep 27 09:11:00 UTC 2022 | Wed Sep 28 09:43:00 UTC 2022 (+25hr) |                   0.0 |         0 (-) |           0 (-) |         0 (-) |
  ;;|               ca-summit | Wed Sep 28 09:43:00 UTC 2022 | Thu Sep 29 10:13:00 UTC 2022 (+25hr) |                   0.0 |         0 (-) |           0 (-) |         0 (-) |
  ;;|  id-caledonia-blackburn | Wed Sep 14 09:52:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+72hr) |                 188.0 |      0 (0.0%) |        0 (0.0%) |  188 (100.0%) |
  ;;|  id-caledonia-blackburn | Sat Sep 17 09:47:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+11hr) |                 142.0 |      0 (0.0%) |        0 (0.0%) |  142 (100.0%) |
  ;;|  id-columbus-bear-gulch | Tue Sep 13 10:13:00 UTC 2022 | Wed Sep 14 09:05:00 UTC 2022 (+23hr) |                2587.0 |  2485 (96.1%) | 30080 (1162.7%) |    102 (3.9%) |
  ;;|  id-columbus-bear-gulch | Wed Sep 14 09:05:00 UTC 2022 | Wed Sep 14 21:21:00 UTC 2022 (+12hr) |                1464.0 |   868 (59.3%) |        0 (0.0%) |   596 (40.7%) |
  ;;|  id-columbus-bear-gulch | Wed Sep 14 21:21:00 UTC 2022 | Sat Sep 17 10:36:00 UTC 2022 (+61hr) |                 662.0 |   425 (64.2%) | 19703 (2976.3%) |   237 (35.8%) |
  ;;|  id-columbus-bear-gulch | Sat Sep 17 10:36:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+10hr) |                  13.0 |      0 (0.0%) |        0 (0.0%) |   13 (100.0%) |
  ;;|  id-columbus-bear-gulch | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 10:17:00 UTC 2022 (+14hr) |                  28.0 |      0 (0.0%) |        0 (0.0%) |   28 (100.0%) |
  ;;|           id-deep-creek | Sat Sep 17 21:14:00 UTC 2022 | Sun Sep 18 10:17:00 UTC 2022 (+13hr) |                 556.0 |      0 (0.0%) |        0 (0.0%) |  556 (100.0%) |
  ;;|           id-deep-creek | Sun Sep 18 10:17:00 UTC 2022 | Mon Sep 19 10:00:00 UTC 2022 (+24hr) |                 630.0 |   571 (90.6%) | 11009 (1747.5%) |     59 (9.4%) |
  ;;|           id-deep-creek | Mon Sep 19 10:00:00 UTC 2022 | Mon Sep 19 21:25:00 UTC 2022 (+11hr) |                 619.0 |   548 (88.5%) |    946 (152.8%) |    71 (11.5%) |
  ;;|           id-deep-creek | Mon Sep 19 21:25:00 UTC 2022 | Tue Sep 20 09:41:00 UTC 2022 (+12hr) |                 691.0 |   620 (89.7%) |   3538 (512.0%) |    71 (10.3%) |
  ;;|           id-deep-creek | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:19:00 UTC 2022 (+11hr) |                 145.0 |    72 (49.7%) |      93 (64.1%) |    73 (50.3%) |
  ;;|  id-isabella-lower-twin | Tue Sep 13 10:11:00 UTC 2022 | Wed Sep 14 09:05:00 UTC 2022 (+23hr) |                 473.0 |   305 (64.5%) |   1924 (406.8%) |   168 (35.5%) |
  ;;|  id-isabella-lower-twin | Wed Sep 14 09:05:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                 306.0 |   112 (36.6%) |    430 (140.5%) |   194 (63.4%) |
  ;;|  id-isabella-lower-twin | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 08:56:00 UTC 2022 (+47hr) |                 370.0 |   187 (50.5%) |   1578 (426.5%) |   183 (49.5%) |
  ;;|  id-isabella-lower-twin | Sat Sep 17 08:56:00 UTC 2022 | Sun Sep 18 08:37:00 UTC 2022 (+24hr) |                 167.0 |      0 (0.0%) |        0 (0.0%) |  167 (100.0%) |
  ;;|  id-isabella-lower-twin | Sun Sep 18 08:37:00 UTC 2022 | Mon Sep 19 10:49:00 UTC 2022 (+26hr) |                 595.0 |    86 (14.5%) |   1065 (179.0%) |   509 (85.5%) |
  ;;|  id-isabella-lower-twin | Mon Sep 19 10:49:00 UTC 2022 | Mon Sep 19 21:25:00 UTC 2022 (+11hr) |                 466.0 |   333 (71.5%) |     313 (67.2%) |   133 (28.5%) |
  ;;|  id-isabella-lower-twin | Mon Sep 19 21:25:00 UTC 2022 | Tue Sep 20 08:52:00 UTC 2022 (+11hr) |                 466.0 |   381 (81.8%) |   3965 (850.9%) |    85 (18.2%) |
  ;;|  id-isabella-lower-twin | Tue Sep 20 08:52:00 UTC 2022 | Tue Sep 20 22:43:00 UTC 2022 (+14hr) |                 692.0 |   376 (54.3%) |   1759 (254.2%) |   316 (45.7%) |
  ;;|  id-isabella-lower-twin | Tue Sep 20 22:43:00 UTC 2022 | Wed Sep 21 09:22:00 UTC 2022 (+11hr) |                 845.0 |      0 (0.0%) |        0 (0.0%) |  845 (100.0%) |
  ;;|                id-katka | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                 155.0 |   125 (80.6%) |    719 (463.9%) |    30 (19.4%) |
  ;;|                id-katka | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:12:00 UTC 2022 (+40hr) |                  12.0 |      0 (0.0%) |        0 (0.0%) |   12 (100.0%) |
  ;;|  id-kootenai-rv-complex | Tue Sep 13 09:22:00 UTC 2022 | Wed Sep 14 09:05:00 UTC 2022 (+24hr) |                2661.0 |  1510 (56.7%) |   5274 (198.2%) |  1151 (43.3%) |
  ;;|  id-kootenai-rv-complex | Wed Sep 14 09:05:00 UTC 2022 | Wed Sep 14 12:19:00 UTC 2022 (+ 3hr) |                3419.0 |  1380 (40.4%) |        0 (0.0%) |  2039 (59.6%) |
  ;;|  id-kootenai-rv-complex | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                3567.0 |  2652 (74.3%) |   8137 (228.1%) |   915 (25.7%) |
  ;;|  id-kootenai-rv-complex | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:04:00 UTC 2022 (+40hr) |                 488.0 |      0 (0.0%) |        0 (0.0%) |  488 (100.0%) |
  ;;|  id-kootenai-rv-complex | Mon Sep 26 10:17:00 UTC 2022 | Tue Sep 27 10:47:00 UTC 2022 (+25hr) |                1154.0 |   546 (47.3%) |     558 (48.4%) |   608 (52.7%) |
  ;;|  id-kootenai-rv-complex | Tue Sep 27 10:47:00 UTC 2022 | Wed Sep 28 03:16:00 UTC 2022 (+16hr) |                1542.0 |    102 (6.6%) |     510 (33.1%) |  1440 (93.4%) |
  ;;|  id-kootenai-rv-complex | Wed Sep 28 03:16:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+ 6hr) |                2164.0 |   934 (43.2%) |        0 (0.0%) |  1230 (56.8%) |
  ;;|  id-kootenai-rv-complex | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |                4232.0 |  1932 (45.7%) |     602 (14.2%) |  2300 (54.3%) |
  ;;|  id-kootenai-rv-complex | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 02:03:00 UTC 2022 (+ 6hr) |                5185.0 |  3903 (75.3%) |  10315 (198.9%) |  1282 (24.7%) |
  ;;|                id-lemhi | Sun Sep 25 21:12:00 UTC 2022 | Mon Sep 26 10:19:00 UTC 2022 (+13hr) |                1791.0 |   981 (54.8%) |   2213 (123.6%) |   810 (45.2%) |
  ;;|                id-lemhi | Mon Sep 26 10:19:00 UTC 2022 | Tue Sep 27 09:09:00 UTC 2022 (+23hr) |                2002.0 |  1894 (94.6%) |  15492 (773.8%) |    108 (5.4%) |
  ;;|                id-lemhi | Tue Sep 27 09:09:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+24hr) |                1862.0 |  1765 (94.8%) |  15296 (821.5%) |     97 (5.2%) |
  ;;|                id-lemhi | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |                1001.0 |   325 (32.5%) |     204 (20.4%) |   676 (67.5%) |
  ;;|                id-lemhi | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+14hr) |                3684.0 |  3600 (97.7%) |  13368 (362.9%) |     84 (2.3%) |
  ;;| id-lynx-meadows-3-prong | Tue Sep 13 09:22:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+25hr) |                2145.0 |  2040 (95.1%) |   4769 (222.3%) |    105 (4.9%) |
  ;;|  id-patrol-point-dismal | Tue Sep 13 09:22:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+25hr) |                9483.0 |  8436 (89.0%) |  26111 (275.3%) |  1047 (11.0%) |
  ;;|              id-ross-fk | Tue Sep 13 09:24:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+25hr) |                7284.0 |  1964 (27.0%) |    1229 (16.9%) |  5320 (73.0%) |
  ;;|              id-tenmile | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                 289.0 |   242 (83.7%) |    775 (268.2%) |    47 (16.3%) |
  ;;|              id-tenmile | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |                 332.0 |   218 (65.7%) |      49 (14.8%) |   114 (34.3%) |
  ;;|              id-tenmile | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 08:33:00 UTC 2022 (+12hr) |                 160.0 |      0 (0.0%) |        0 (0.0%) |  160 (100.0%) |
  ;;|          id-trail-ridge | Sat Sep 17 10:23:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+10hr) |                   2.0 |      0 (0.0%) |        0 (0.0%) |    2 (100.0%) |
  ;;|          id-trail-ridge | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 08:37:00 UTC 2022 (+12hr) |                   2.0 |      0 (0.0%) |        0 (0.0%) |    2 (100.0%) |
  ;;|             mt-billiard | Tue Sep 13 09:22:00 UTC 2022 | Wed Sep 14 09:52:00 UTC 2022 (+25hr) |                 837.0 |   710 (84.8%) | 12272 (1466.2%) |   127 (15.2%) |
  ;;|             mt-billiard | Wed Sep 14 09:52:00 UTC 2022 | Fri Sep 16 11:11:00 UTC 2022 (+49hr) |                 356.0 |   200 (56.2%) |  4757 (1336.2%) |   156 (43.8%) |
  ;;|               mt-cannon | Wed Sep 14 09:52:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                 120.0 |      0 (0.0%) |        0 (0.0%) |  120 (100.0%) |
  ;;|               mt-cannon | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+59hr) |                 141.0 |   129 (91.5%) |  1545 (1095.7%) |     12 (8.5%) |
  ;;|               mt-cannon | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 08:37:00 UTC 2022 (+12hr) |                  37.0 |      0 (0.0%) |        0 (0.0%) |   37 (100.0%) |
  ;;|          mt-george-lake | Tue Sep 13 09:22:00 UTC 2022 | Thu Sep 15 11:09:00 UTC 2022 (+50hr) |                1349.0 |   778 (57.7%) |   2248 (166.6%) |   571 (42.3%) |
  ;;|           mt-government | Tue Sep 13 09:22:00 UTC 2022 | Wed Sep 14 09:05:00 UTC 2022 (+24hr) |                2474.0 |  2453 (99.2%) | 40244 (1626.7%) |     21 (0.8%) |
  ;;|           mt-government | Wed Sep 14 09:05:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                1137.0 |   773 (68.0%) |     603 (53.0%) |   364 (32.0%) |
  ;;|           mt-government | Thu Sep 15 09:34:00 UTC 2022 | Fri Sep 16 11:01:00 UTC 2022 (+25hr) |                 897.0 |   753 (83.9%) |   5848 (652.0%) |   144 (16.1%) |
  ;;|           mt-government | Fri Sep 16 11:01:00 UTC 2022 | Sat Sep 17 11:20:00 UTC 2022 (+24hr) |                  89.0 |      0 (0.0%) |        0 (0.0%) |   89 (100.0%) |
  ;;|           mt-government | Sat Sep 17 11:20:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+ 9hr) |                  96.0 |    60 (62.5%) |      81 (84.4%) |    36 (37.5%) |
  ;;|           mt-government | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 09:28:00 UTC 2022 (+13hr) |                  96.0 |    87 (90.6%) |  1499 (1561.5%) |      9 (9.4%) |
  ;;|        mt-isabella-lake | Wed Sep 14 03:40:00 UTC 2022 | Fri Sep 16 11:26:00 UTC 2022 (+56hr) |                   4.0 |      0 (0.0%) |        0 (0.0%) |    4 (100.0%) |
  ;;|             mt-margaret | Wed Sep 14 09:52:00 UTC 2022 | Thu Sep 15 08:43:00 UTC 2022 (+23hr) |                 519.0 |   445 (85.7%) |     293 (56.5%) |    74 (14.3%) |
  ;;|             mt-margaret | Thu Sep 15 08:43:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+49hr) |                 679.0 |   648 (95.4%) |   2611 (384.5%) |     31 (4.6%) |
  ;;|             mt-margaret | Sat Sep 17 09:47:00 UTC 2022 | Sun Sep 18 09:28:00 UTC 2022 (+24hr) |                   0.0 |         0 (-) |           0 (-) |         0 (-) |
  ;;|            mt-mill-lake | Sat Sep 17 10:50:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+10hr) |                 871.0 |   135 (15.5%) |       16 (1.8%) |   736 (84.5%) |
  ;;|          or-cedar-creek | Wed Sep 14 03:20:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+ 7hr) |               19163.0 |  8106 (42.3%) |        0 (0.0%) | 11057 (57.7%) |
  ;;|          or-cedar-creek | Wed Sep 14 09:54:00 UTC 2022 | Wed Sep 14 21:18:00 UTC 2022 (+11hr) |               17674.0 |  6231 (35.3%) |      831 (4.7%) | 11443 (64.7%) |
  ;;|          or-cedar-creek | Wed Sep 14 21:18:00 UTC 2022 | Thu Sep 15 09:37:00 UTC 2022 (+12hr) |               11352.0 |  1919 (16.9%) |    1536 (13.5%) |  9433 (83.1%) |
  ;;|          or-cedar-creek | Thu Sep 15 09:37:00 UTC 2022 | Fri Sep 16 09:15:00 UTC 2022 (+24hr) |               12849.0 |  4302 (33.5%) |    2472 (19.2%) |  8547 (66.5%) |
  ;;|          or-cedar-creek | Fri Sep 16 09:15:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+24hr) |               19045.0 |  6322 (33.2%) |    1923 (10.1%) | 12723 (66.8%) |
  ;;|          or-cedar-creek | Sat Sep 17 08:58:00 UTC 2022 | Sat Sep 17 21:14:00 UTC 2022 (+12hr) |               29804.0 |  7061 (23.7%) |      256 (0.9%) | 22743 (76.3%) |
  ;;|          or-cedar-creek | Sat Sep 17 21:14:00 UTC 2022 | Mon Sep 19 10:00:00 UTC 2022 (+37hr) |               27170.0 | 20225 (74.4%) |   14475 (53.3%) |  6945 (25.6%) |
  ;;|          or-cedar-creek | Sun Sep 25 22:03:00 UTC 2022 | Mon Sep 26 11:10:00 UTC 2022 (+13hr) |                8248.0 |    364 (4.4%) |       40 (0.5%) |  7884 (95.6%) |
  ;;|          or-cedar-creek | Mon Sep 26 11:10:00 UTC 2022 | Tue Sep 27 09:09:00 UTC 2022 (+22hr) |                9631.0 |   961 (10.0%) |    1920 (19.9%) |  8670 (90.0%) |
  ;;|         or-double-creek | Wed Sep 14 09:05:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |               12734.0 |  6248 (49.1%) |    3137 (24.6%) |  6486 (50.9%) |
  ;;|         or-double-creek | Thu Sep 15 09:34:00 UTC 2022 | Fri Sep 16 00:06:00 UTC 2022 (+15hr) |                7910.0 |  5401 (68.3%) |    2899 (36.6%) |  2509 (31.7%) |
  ;;|         or-double-creek | Fri Sep 16 00:06:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+34hr) |                2263.0 |   565 (25.0%) |     919 (40.6%) |  1698 (75.0%) |
  ;;|         or-double-creek | Sat Sep 17 09:47:00 UTC 2022 | Sat Sep 17 21:14:00 UTC 2022 (+11hr) |                1808.0 |      0 (0.0%) |        0 (0.0%) | 1808 (100.0%) |
  ;;|         or-double-creek | Sat Sep 17 21:14:00 UTC 2022 | Mon Sep 19 09:09:00 UTC 2022 (+36hr) |                1785.0 |     75 (4.2%) |     953 (53.4%) |  1710 (95.8%) |
  ;;|         or-double-creek | Mon Sep 19 09:09:00 UTC 2022 | Tue Sep 20 08:52:00 UTC 2022 (+24hr) |                1883.0 |      0 (0.0%) |        0 (0.0%) | 1883 (100.0%) |
  ;;|         or-double-creek | Tue Sep 20 08:52:00 UTC 2022 | Tue Sep 20 20:19:00 UTC 2022 (+11hr) |                1810.0 |      0 (0.0%) |        0 (0.0%) | 1810 (100.0%) |
  ;;|         or-double-creek | Tue Sep 20 20:19:00 UTC 2022 | Wed Sep 21 09:22:00 UTC 2022 (+13hr) |                1873.0 |    132 (7.0%) |     194 (10.4%) |  1741 (93.0%) |
  ;;|         or-double-creek | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                4588.0 |  1396 (30.4%) |   5861 (127.7%) |  3192 (69.6%) |
  ;;|         or-double-creek | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |               20434.0 |  2542 (12.4%) |        0 (0.0%) | 17892 (87.6%) |
  ;;|         or-double-creek | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 08:33:00 UTC 2022 (+12hr) |               25821.0 | 19722 (76.4%) |    4966 (19.2%) |  6099 (23.6%) |
  ;;|             or-sturgill | Wed Sep 14 09:54:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                 884.0 |   203 (23.0%) |     210 (23.8%) |   681 (77.0%) |
  ;;|             or-sturgill | Thu Sep 15 09:34:00 UTC 2022 | Fri Sep 16 10:09:00 UTC 2022 (+25hr) |                 728.0 |      0 (0.0%) |        0 (0.0%) |  728 (100.0%) |
  ;;|             or-sturgill | Fri Sep 16 10:09:00 UTC 2022 | Fri Sep 16 20:42:00 UTC 2022 (+11hr) |                 684.0 |      0 (0.0%) |        0 (0.0%) |  684 (100.0%) |
  ;;|             or-sturgill | Fri Sep 16 20:42:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+13hr) |                 921.0 |      0 (0.0%) |        0 (0.0%) |  921 (100.0%) |
  ;;|             or-sturgill | Sat Sep 17 09:47:00 UTC 2022 | Sat Sep 17 16:47:00 UTC 2022 (+ 7hr) |                1210.0 |   257 (21.2%) |        0 (0.0%) |   953 (78.8%) |
  ;;|             or-sturgill | Sat Sep 17 16:47:00 UTC 2022 | Mon Sep 19 17:47:00 UTC 2022 (+49hr) |                1254.0 |   405 (32.3%) |   6868 (547.7%) |   849 (67.7%) |
  ;;|             or-sturgill | Mon Sep 19 17:47:00 UTC 2022 | Tue Sep 20 09:41:00 UTC 2022 (+16hr) |                 679.0 |      0 (0.0%) |        0 (0.0%) |  679 (100.0%) |
  ;;|             or-sturgill | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 16:04:00 UTC 2022 (+ 6hr) |                 731.0 |      0 (0.0%) |        0 (0.0%) |  731 (100.0%) |
  ;;|             or-sturgill | Tue Sep 20 16:04:00 UTC 2022 | Tue Sep 20 20:19:00 UTC 2022 (+ 4hr) |                 804.0 |      0 (0.0%) |        0 (0.0%) |  804 (100.0%) |
  ;;|             or-sturgill | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                6854.0 |  5505 (80.3%) |  15066 (219.8%) |  1349 (19.7%) |
  ;;|             or-sturgill | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |                6717.0 |  4777 (71.1%) |    1009 (15.0%) |  1940 (28.9%) |
  ;;|             or-sturgill | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 00:01:00 UTC 2022 (+ 4hr) |                4895.0 |  3926 (80.2%) |   7241 (147.9%) |   969 (19.8%) |
  ;;|           wa-bolt-creek | Sun Sep 18 05:10:00 UTC 2022 | Mon Sep 19 10:49:00 UTC 2022 (+30hr) |                2555.0 |  1628 (63.7%) |   5195 (203.3%) |   927 (36.3%) |
  ;;|           wa-bolt-creek | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:17:00 UTC 2022 (+11hr) |                1488.0 |   257 (17.3%) |     242 (16.3%) |  1231 (82.7%) |
  ;;|           wa-bolt-creek | Tue Sep 20 20:17:00 UTC 2022 | Wed Sep 21 09:22:00 UTC 2022 (+13hr) |                1977.0 |  1441 (72.9%) |   5601 (283.3%) |   536 (27.1%) |
  ;;|           wa-bolt-creek | Wed Sep 21 09:22:00 UTC 2022 | Thu Sep 22 09:54:00 UTC 2022 (+25hr) |                5246.0 |  2856 (54.4%) |    4994 (95.2%) |  2390 (45.6%) |
  ;;|           wa-bolt-creek | Thu Sep 22 09:54:00 UTC 2022 | Thu Sep 22 20:29:00 UTC 2022 (+11hr) |                4179.0 |  3175 (76.0%) |     857 (20.5%) |  1004 (24.0%) |
  ;;|           wa-bolt-creek | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                4072.0 |  3398 (83.4%) |   6830 (167.7%) |   674 (16.6%) |
  ;;|           wa-bolt-creek | Sat Sep 24 09:15:00 UTC 2022 | Sun Sep 25 09:45:00 UTC 2022 (+25hr) |                1292.0 |      0 (0.0%) |        0 (0.0%) | 1292 (100.0%) |
  ;;|           wa-bolt-creek | Sun Sep 25 09:45:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+24hr) |                1586.0 |   796 (50.2%) |   1642 (103.5%) |   790 (49.8%) |
  ;;|           wa-bolt-creek | Mon Sep 26 09:28:00 UTC 2022 | Tue Sep 27 09:07:00 UTC 2022 (+24hr) |                3373.0 |   954 (28.3%) |     771 (22.9%) |  2419 (71.7%) |
  ;;|           wa-bolt-creek | Tue Sep 27 09:07:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+24hr) |                4446.0 |  2689 (60.5%) |    1901 (42.8%) |  1757 (39.5%) |
  ;;|           wa-goat-rocks | Sun Sep 25 09:45:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+11hr) |                   9.0 |      0 (0.0%) |        0 (0.0%) |    9 (100.0%) |
  ;;|           wa-goat-rocks | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+14hr) |                1787.0 |      0 (0.0%) |        0 (0.0%) | 1787 (100.0%) |
  ;;|           wa-goat-rocks | Mon Sep 26 11:08:00 UTC 2022 | Tue Sep 27 10:00:00 UTC 2022 (+23hr) |                2916.0 |  2241 (76.9%) |    2110 (72.4%) |   675 (23.1%) |
  ;;|           wa-goat-rocks | Tue Sep 27 10:00:00 UTC 2022 | Tue Sep 27 21:25:00 UTC 2022 (+11hr) |                3658.0 |  2779 (76.0%) |     874 (23.9%) |   879 (24.0%) |
  ;;|           wa-goat-rocks | Tue Sep 27 21:25:00 UTC 2022 | Wed Sep 28 10:32:00 UTC 2022 (+13hr) |                3816.0 |  3258 (85.4%) |    2636 (69.1%) |   558 (14.6%) |
  ;;|            wa-irving-pk | Tue Sep 13 09:22:00 UTC 2022 | Tue Sep 13 20:51:00 UTC 2022 (+11hr) |                 139.0 |      0 (0.0%) |        0 (0.0%) |  139 (100.0%) |
  ;;|            wa-irving-pk | Tue Sep 13 20:51:00 UTC 2022 | Thu Sep 15 10:26:00 UTC 2022 (+38hr) |                 209.0 |      0 (0.0%) |        0 (0.0%) |  209 (100.0%) |
  ;;|            wa-irving-pk | Thu Sep 15 10:26:00 UTC 2022 | Fri Sep 16 09:15:00 UTC 2022 (+23hr) |                  81.0 |      0 (0.0%) |        0 (0.0%) |   81 (100.0%) |
  ;;|            wa-irving-pk | Fri Sep 16 09:15:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+25hr) |                 434.0 |      0 (0.0%) |        0 (0.0%) |  434 (100.0%) |
  ;;|            wa-irving-pk | Sat Sep 17 09:47:00 UTC 2022 | Mon Sep 19 10:00:00 UTC 2022 (+48hr) |                1286.0 |   344 (26.7%) |   5712 (444.2%) |   942 (73.3%) |
  ;;|            wa-irving-pk | Mon Sep 19 10:00:00 UTC 2022 | Tue Sep 20 08:50:00 UTC 2022 (+23hr) |                1621.0 |   605 (37.3%) |     805 (49.7%) |  1016 (62.7%) |
  ;;|            wa-irving-pk | Tue Sep 20 08:50:00 UTC 2022 | Tue Sep 20 20:17:00 UTC 2022 (+11hr) |                2083.0 |  1411 (67.7%) |     214 (10.3%) |   672 (32.3%) |
  ;;|            wa-irving-pk | Tue Sep 20 20:17:00 UTC 2022 | Wed Sep 21 09:22:00 UTC 2022 (+13hr) |                3109.0 |  2503 (80.5%) |   5650 (181.7%) |   606 (19.5%) |
  ;;|            wa-irving-pk | Wed Sep 21 09:22:00 UTC 2022 | Thu Sep 22 09:02:00 UTC 2022 (+24hr) |                2997.0 |  1949 (65.0%) |   9974 (332.8%) |  1048 (35.0%) |
  ;;|            wa-irving-pk | Thu Sep 22 09:02:00 UTC 2022 | Thu Sep 22 20:29:00 UTC 2022 (+11hr) |                2540.0 |  1664 (65.5%) |    1058 (41.7%) |   876 (34.5%) |
  ;;|            wa-irving-pk | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2760.0 |  2180 (79.0%) |   9202 (333.4%) |   580 (21.0%) |
  ;;|            wa-irving-pk | Sat Sep 24 09:15:00 UTC 2022 | Sun Sep 25 08:58:00 UTC 2022 (+24hr) |                 183.0 |   107 (58.5%) |    520 (284.2%) |    76 (41.5%) |
  ;;|            wa-irving-pk | Sun Sep 25 08:58:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+12hr) |                 485.0 |      0 (0.0%) |        0 (0.0%) |  485 (100.0%) |
  ;;|            wa-irving-pk | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                 678.0 |   290 (42.8%) |   2846 (419.8%) |   388 (57.2%) |
  ;;|            wa-irving-pk | Mon Sep 26 09:28:00 UTC 2022 | Tue Sep 27 09:07:00 UTC 2022 (+24hr) |                 661.0 |   426 (64.4%) |   5493 (831.0%) |   235 (35.6%) |
  ;;|            wa-irving-pk | Tue Sep 27 09:07:00 UTC 2022 | Wed Sep 28 10:30:00 UTC 2022 (+25hr) |                1112.0 |   250 (22.5%) |     683 (61.4%) |   862 (77.5%) |
  ;;|            wa-irving-pk | Wed Sep 28 10:30:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+24hr) |                 926.0 |   655 (70.7%) |   2802 (302.6%) |   271 (29.3%) |
  ;;|               wa-kalama | Sat Sep 24 11:18:00 UTC 2022 | Sun Sep 25 10:50:00 UTC 2022 (+24hr) |                 431.0 |   294 (68.2%) |     386 (89.6%) |   137 (31.8%) |
  ;;|               wa-kalama | Sun Sep 25 10:50:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+24hr) |                 615.0 |   298 (48.5%) |     215 (35.0%) |   317 (51.5%) |
  ;;|               wa-kalama | Mon Sep 26 11:08:00 UTC 2022 | Tue Sep 27 10:02:00 UTC 2022 (+23hr) |                 833.0 |   603 (72.4%) |     195 (23.4%) |   230 (27.6%) |
  ;;|                  wa-kid | Tue Sep 13 10:13:00 UTC 2022 | Tue Sep 13 20:51:00 UTC 2022 (+11hr) |                  78.0 |      0 (0.0%) |        0 (0.0%) |   78 (100.0%) |
  ;;|                  wa-kid | Tue Sep 13 20:51:00 UTC 2022 | Wed Sep 14 09:52:00 UTC 2022 (+13hr) |                  78.0 |      0 (0.0%) |        0 (0.0%) |   78 (100.0%) |
  ;;|     wa-mcallister-creek | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                4809.0 |  3956 (82.3%) |  12584 (261.7%) |   853 (17.7%) |
  ;;|         wa-minnow-ridge | Tue Sep 13 10:13:00 UTC 2022 | Wed Sep 14 11:32:00 UTC 2022 (+25hr) |                 500.0 |   380 (76.0%) |   4523 (904.6%) |   120 (24.0%) |
  ;;|         wa-minnow-ridge | Wed Sep 14 11:32:00 UTC 2022 | Wed Sep 14 21:21:00 UTC 2022 (+10hr) |                 554.0 |   485 (87.5%) |     426 (76.9%) |    69 (12.5%) |
  ;;|         wa-minnow-ridge | Wed Sep 14 21:21:00 UTC 2022 | Fri Sep 16 10:06:00 UTC 2022 (+37hr) |                 652.0 |   619 (94.9%) |   6000 (920.2%) |     33 (5.1%) |
  ;;|         wa-minnow-ridge | Fri Sep 16 10:06:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+24hr) |                 120.0 |      0 (0.0%) |        0 (0.0%) |  120 (100.0%) |
  ;;|         wa-minnow-ridge | Sat Sep 17 09:47:00 UTC 2022 | Sun Sep 18 10:17:00 UTC 2022 (+25hr) |                 203.0 |  203 (100.0%) |  8797 (4333.5%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge | Sun Sep 18 10:17:00 UTC 2022 | Mon Sep 19 10:49:00 UTC 2022 (+25hr) |                1225.0 |      0 (0.0%) |        0 (0.0%) | 1225 (100.0%) |
  ;;|         wa-minnow-ridge | Mon Sep 19 10:49:00 UTC 2022 | Mon Sep 19 21:25:00 UTC 2022 (+11hr) |                1274.0 |  1200 (94.2%) |    1201 (94.3%) |     74 (5.8%) |
  ;;|         wa-minnow-ridge | Mon Sep 19 21:25:00 UTC 2022 | Tue Sep 20 09:41:00 UTC 2022 (+12hr) |                1767.0 |  1765 (99.9%) |   8721 (493.5%) |      2 (0.1%) |
  ;;|         wa-minnow-ridge | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:17:00 UTC 2022 (+11hr) |                2157.0 |  1790 (83.0%) |    1291 (59.9%) |   367 (17.0%) |
  ;;|         wa-minnow-ridge | Tue Sep 20 20:17:00 UTC 2022 | Wed Sep 21 09:22:00 UTC 2022 (+13hr) |                2489.0 |  2396 (96.3%) |  15614 (627.3%) |     93 (3.7%) |
  ;;|         wa-minnow-ridge | Wed Sep 21 09:22:00 UTC 2022 | Thu Sep 22 10:43:00 UTC 2022 (+25hr) |                3105.0 |  3008 (96.9%) |  13024 (419.5%) |     97 (3.1%) |
  ;;|         wa-minnow-ridge | Thu Sep 22 10:43:00 UTC 2022 | Thu Sep 22 20:29:00 UTC 2022 (+10hr) |                1457.0 |  1225 (84.1%) |    1169 (80.2%) |   232 (15.9%) |
  ;;|         wa-minnow-ridge | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2184.0 |  2149 (98.4%) |  16505 (755.7%) |     35 (1.6%) |
  ;;|         wa-minnow-ridge | Sat Sep 24 09:15:00 UTC 2022 | Sun Sep 25 08:58:00 UTC 2022 (+24hr) |                2382.0 |  2020 (84.8%) |   3806 (159.8%) |   362 (15.2%) |
  ;;|         wa-minnow-ridge | Sun Sep 25 08:58:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+12hr) |                2408.0 |  2026 (84.1%) |     447 (18.6%) |   382 (15.9%) |
  ;;|         wa-minnow-ridge | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                2298.0 |  2186 (95.1%) |   3105 (135.1%) |    112 (4.9%) |
  ;;|         wa-minnow-ridge | Mon Sep 26 09:28:00 UTC 2022 | Tue Sep 27 09:07:00 UTC 2022 (+24hr) |                1159.0 |  1096 (94.6%) |   5439 (469.3%) |     63 (5.4%) |
  ;;|         wa-minnow-ridge | Tue Sep 27 09:07:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+24hr) |                1622.0 |  1363 (84.0%) |   4018 (247.7%) |   259 (16.0%) |
  ;;|         wa-minnow-ridge | Wed Sep 28 08:50:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+25hr) |                1625.0 |  1446 (89.0%) |   4762 (293.0%) |   179 (11.0%) |
  ;;|             wa-siouoxon | Sat Sep 24 20:42:00 UTC 2022 | Sun Sep 25 11:28:00 UTC 2022 (+15hr) |                 141.0 |      0 (0.0%) |        0 (0.0%) |  141 (100.0%) |
  ;;|             wa-siouoxon | Sun Sep 25 11:28:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+24hr) |                 430.0 |   340 (79.1%) |     271 (63.0%) |    90 (20.9%) |
  ;;|             wa-siouoxon | Mon Sep 26 11:08:00 UTC 2022 | Tue Sep 27 10:02:00 UTC 2022 (+23hr) |                 949.0 |   485 (51.1%) |     529 (55.7%) |   464 (48.9%) |
  ;;|             wa-siouoxon | Tue Sep 27 10:02:00 UTC 2022 | Wed Sep 28 10:32:00 UTC 2022 (+25hr) |                1489.0 |  1152 (77.4%) |     438 (29.4%) |   337 (22.6%) |


  ;; 1. Are we spreading too fast?
  ;; Experiment: compute the average (and maybe some quantiles) simulation-ToA in (sim ∩ real). If it's very small compared to (t1-t0)/2,
  ;; it's a sign that we're spreading too fast.
  (->> replay-results
       (pmap toa-stats)
       (vec))


  *e)
