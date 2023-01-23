(ns gridfire.lab.replay
  (:require [clojure.java.io              :as io]
            [clojure.java.shell           :as sh]
            [clojure.pprint               :as pprint]
            [clojure.string               :as str]
            [gridfire.core                :as gridfire]
            [gridfire.fire-spread-optimal :refer [memoize-rfwo rothermel-fast-wrapper-optimal run-fire-spread]]
            [gridfire.magellan-bridge     :refer [geotiff-raster-to-tensor]]
            [gridfire.simulations         :as simulations]
            [gridfire.utils.files.tar     :as utar]
            [gridfire.utils.vsampling     :as vsmpl]
            [hiccup.page                  :as html]
            [matrix-viz.core              :refer [save-matrix-as-png]]
            [tech.v3.datatype             :as d]
            [tech.v3.datatype.functional  :as dfn]
            [tech.v3.datatype.statistics  :as dstats]
            [tech.v3.tensor               :as t])
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
;;vwaeselynck@iiwi:~/gridfire$ nohup clj -J-XX:MaxRAMPercentage=90 -M:nREPL:mydev:gridfire-my-dev:vsampling:dataviz -m nrepl.cmdline --port 8888 >> ../repl-out.txt 2>&1 &
;;vwaeselynck@iiwi:~$ cd gridfire
;;vwaeselynck@iiwi:~/gridfire$ git checkout vwaeselynck-330-gridfire-historical-fires

;; How I start and connect to the remote nREPL server
;;vwaeselynck@iiwi:~/gridfire-scripts/lab$ nohup clj -J-XX:MaxRAMPercentage=90 -J-Djdk.attach.allowAttachSelf=true -M:nREPL:mydev:gridfire-my-dev -m nrepl.cmdline --port 8888 >> ../repl-out.txt 2>&1 &
;;$ ssh -i ~/.ssh/id_rsa -4N -L 18888:localhost:8888 vwaeselynck@10.1.30.147

;; How I fetch the FTP-saved PyreCast snapshots:
;; vwaeselynck@iiwi:~/pyrecast-snapshots-2022-09-30$ wget --timestamping ftps://vwaeselynck:5Km_32erTf@ftp.sig-gis.com/kcheung/*.gz

;; ------------------------------------------------------------------------------
;; File and CLI utilities

;; IMPROVEMENT move those to generic ns?
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
  [config stop-inst variation-name->info]
  (some-> config
          (config-max-runtime-until-stop-inst stop-inst)
          (-> (gridfire/load-inputs!))
          (as-> inputs
                (->> variation-name->info
                     (partition-all 16)
                     (mapcat
                      (fn compute-variations-batch [variation-name->info1]
                        (->> variation-name->info1
                             (pmap (fn compute-variation-results-entry [[variation-name variation-info]]
                                     [variation-name
                                      (binding [vsmpl/db* (atom {})]
                                        (let [transform-inputs-fn (-> variation-info ::transform-inputs-fn (or identity))
                                              inputs              (transform-inputs-fn inputs)]
                                          (merge
                                           {::simulations/inputs inputs
                                            ::simulations/result
                                            (-> (simulations/prepare-simulation-inputs 0 inputs)
                                                (run-fire-spread))}
                                           {::sampled-values (vsmpl/recorded-values)})))]))
                             (vec))))
                     (into {})))))

;; IMPROVEMENT re-implement as t1-active_fire_polygon_utm_posnegbuff-shp - t0-active_fire_polygon_utm_posnegbuff-shp
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
    (dfn/max                                                ; set union
     (:ignition-matrix sim-inputs)
     (d/elemwise-cast
      (dfn/<=
       0.
       (:burn-time-matrix sim-result)
       minutes-until-stop-inst)
      :double))))

(defn fetch-snapshot-config
  [snap]
  (-> (gridfire/load-config! (-> snap
                                 ::useful-files
                                 ::gridfire-config-file
                                 (ensure-file)
                                 (.getCanonicalPath)))
      (into (sorted-map))))

(defn replay-snapshot
  [t0-snap t1-snap variation-name->info]
  (let [stop-inst          (:pyrcst_snapshot_inst t1-snap)
        observed-burn-area (resolve-observed-burned-area
                            (-> t0-snap ::useful-files ::already_burned-tif)
                            (-> t1-snap ::useful-files ::active_fire_polygon_utm_posnegbuff-shp))

        config             (-> (fetch-snapshot-config t0-snap)
                               (dissoc :perturbations)      ; to remove variance.
                               (merge {:simulations 1}))
        sim-outputs        (run-fire-spread-until config stop-inst variation-name->info)]
    {::observed-burn-area observed-burn-area
     ::sim-outputs        sim-outputs}))

(defn burn-trace-stats
  [replay-res sim-output]
  (let [stop-inst            (-> replay-res ::t1-fire :pyrcst_snapshot_inst)
        observed-burn-area   (::observed-burn-area replay-res)
        observed-burn-matrix (:matrix observed-burn-area)
        sim-result           (::simulations/result sim-output)]
    (if (nil? sim-result)
      {:observed-burn-n-cells    (dfn/sum observed-burn-matrix)
       :sim-inter-obs-n-cells    0
       :sim-minus-obs-n-cells    0
       :obs-minus-sim-n-cells    (dfn/sum observed-burn-matrix)
       :sim-inter-obs-n-crowning 0
       :sim-minus-obs-n-crowning 0
       ::comments                ["run-fire-spread returned nil, which happens when it fails to get perimeter cells with burnable neighbours."]}
      (let [sim-burn-matrix      (simulated-burned-area-matrix stop-inst (::simulations/inputs sim-output) sim-result)
            sim-inter-obs-matrix (dfn/* sim-burn-matrix observed-burn-matrix)
            sim-minus-obs-matrix (dfn/* sim-burn-matrix (dfn/- 1.0 observed-burn-matrix))
            did-crown-matrix     (dfn/> (:fire-type-matrix sim-result) 1.0)]
        {:observed-burn-n-cells    (dfn/sum observed-burn-matrix)
         :sim-inter-obs-n-cells    (dfn/sum sim-inter-obs-matrix)
         :sim-minus-obs-n-cells    (dfn/sum sim-minus-obs-matrix)
         :obs-minus-sim-n-cells    (dfn/sum (dfn/* observed-burn-matrix (dfn/- 1.0 sim-burn-matrix)))

         :sim-inter-obs-n-crowning (dfn/sum (dfn/* sim-inter-obs-matrix did-crown-matrix))
         :sim-minus-obs-n-crowning (dfn/sum (dfn/* sim-minus-obs-matrix did-crown-matrix))}))))

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

;; IMPROVEMENT rather than successive pairs
;; let's use pairs that achieve a delta-t closest to 24hr,
;; or some other optimal time lapse.
(defn replayable-successive-t0t1-pairs
  [snaps]
  (->> snaps
       (sort-by (juxt :pyrcst_fire_name :pyrcst_fire_subnumber :pyrcst_snapshot_inst))
       (partition-all 2 1)
       (filter
        (fn [[t0-snap t1-snap]]
          (can-replay-t0-to-t1? t0-snap t1-snap)))))

(defn replay-snapshot-pairs
  [variation-name->info t0+t1-snaps]
  (with-redefs [rothermel-fast-wrapper-optimal (memoize-rfwo rothermel-fast-wrapper-optimal)]
    (->> t0+t1-snaps
         (partition-all 4)                                  ; parallelism
         (mapcat
          (fn [t0+t1-snaps]
            (->> t0+t1-snaps
                 (pmap (fn compare-snapshots [[t0-snap t1-snap]]
                         (println (print-str "Replaying" (:pyrcst_fire_name t0-snap) "from" (-> t0-snap :pyrcst_snapshot_inst) "to" (-> t1-snap :pyrcst_snapshot_inst)))
                         (merge
                          (select-keys t0-snap [:pyrcst_fire_name :pyrcst_fire_subnumber])
                          {::t0-fire (select-keys t0-snap [:pyrcst_snapshot_inst ::useful-files])
                           ::t1-fire (select-keys t1-snap [:pyrcst_snapshot_inst ::useful-files])}
                          (replay-snapshot t0-snap t1-snap variation-name->info))))
                 (vec)))))))

(defn snapshot-pair-hash
  [[t0-snap t1-snap]]
  (hash
   [(select-keys t0-snap [:pyrcst_fire_name :pyrcst_fire_subnumber :pyrcst_snapshot_inst])
    (select-keys t1-snap [:pyrcst_fire_name :pyrcst_fire_subnumber :pyrcst_snapshot_inst])]))

(defn replay-snapshots
  ([variation-name->info snaps] (replay-snapshots variation-name->info nil snaps))
  ([variation-name->info subsample-size snaps]
   (replay-snapshot-pairs variation-name->info
                          (-> (replayable-successive-t0t1-pairs snaps)
                              (cond-> (some? subsample-size) (->> (sort-by snapshot-pair-hash) (take subsample-size)))))))

(defn toa-stats
  [replay-res sim-output]
  (let [stop-inst                  (-> replay-res ::t1-fire :pyrcst_snapshot_inst)
        observed-burn-area         (::observed-burn-area replay-res)
        observed-burn-matrix       (:matrix observed-burn-area)
        sim-result                 (::simulations/result sim-output)]
    (when-not (nil? sim-result)
      (let [sim-burn-matrix       (simulated-burned-area-matrix stop-inst (::simulations/inputs sim-output) sim-result)
            sim-inter-real-matrix (dfn/* sim-burn-matrix observed-burn-matrix)
            time-lapse-min        (-> (- (-> replay-res ::t1-fire :pyrcst_snapshot_inst (inst-ms))
                                         (-> replay-res ::t0-fire :pyrcst_snapshot_inst (inst-ms)))
                                      (double)
                                      (/ (* 1000 60)))
            toa-ratio-matrix      (d/emap
                                   (fn [burn-time sim-inter-real]
                                     (if (and (= 1.0 sim-inter-real) (> burn-time 0.))
                                       (/ burn-time
                                          time-lapse-min)
                                       Double/NaN))
                                   :double
                                   (:burn-time-matrix sim-result)
                                   sim-inter-real-matrix)
            stats-opts            {:nan-strategy :remove}]
        (let [[p50 p75 p90 p95] (dstats/percentiles [50 75 90 95] stats-opts toa-ratio-matrix)]
          {::toa-ratio-mean (dstats/mean toa-ratio-matrix stats-opts)
           ::toa-ratio-p50  p50
           ::toa-ratio-p75  p75
           ::toa-ratio-p90  p90
           ::toa-ratio-p95  p95})))))

(defn all-stats
  [replay-res sim-output]
  (merge (burn-trace-stats replay-res sim-output)
         (toa-stats replay-res sim-output)))

(defn pprint-table-from-kv-lists
  ([pairs-vecs]
   (let [pairs0 (first pairs-vecs)
         cols   (mapv first pairs0)]
     (pprint-table-from-kv-lists cols pairs-vecs)))
  ([cols pairs-vecs]
   (pprint/print-table
    cols
    (map #(into {} %) pairs-vecs))))

(defn format-hours-between
  [t0-inst t1-inst]
  (format "+%2dhr"
          (-> (-
               (-> t1-inst (inst-ms))
               (-> t0-inst (inst-ms)))
              (/ (* 1e3 60 60))
              (double)
              (Math/round) (long))))

(defn replayed-results-stats
  [replay-results]
  (->> replay-results
       (pmap (fn [replay-res]
               (merge (select-keys replay-res [:pyrcst_fire_name])
                      {::t0-fire               (select-keys (::t0-fire replay-res) [:pyrcst_snapshot_inst])
                       ::t1-fire               (select-keys (::t1-fire replay-res) [:pyrcst_snapshot_inst])
                       ::variation-name->stats (->> replay-res ::sim-outputs
                                                    (sort-by key)
                                                    (map
                                                     (fn [[variation-name sim-output]]
                                                       [variation-name (all-stats replay-res sim-output)]))
                                                    (into (sorted-map)))})))))

(defn replayed-results-table-entries
  [rr-stats]
  (->> rr-stats
       (mapcat (fn [replay-res-stats]
                 (->> replay-res-stats ::variation-name->stats
                      (sort-by key)
                      (mapv
                       (fn [[variation-name stats]]
                         (let [n-cells-really-burned (long (:observed-burn-n-cells stats))]
                           (letfn [(format-w-pct
                                     ([^long n-cells] (format-w-pct n-cells n-cells-really-burned))
                                     ([^long n-cells ^long n-cells-denom]
                                      (if (zero? n-cells-denom)
                                        (format "%d (-)" n-cells)
                                        (format "%d (%2.1f%%)"
                                                n-cells
                                                (* 100.0 (/ n-cells n-cells-denom))))))]
                             [["Fire name" (:pyrcst_fire_name replay-res-stats)]
                              ["Variation" variation-name]
                              ["t0" (-> replay-res-stats ::t0-fire :pyrcst_snapshot_inst)]
                              ["t1" (format "%s (%s)"
                                            (-> replay-res-stats ::t1-fire :pyrcst_snapshot_inst (str))
                                            (format-hours-between (-> replay-res-stats ::t0-fire :pyrcst_snapshot_inst)
                                                                  (-> replay-res-stats ::t1-fire :pyrcst_snapshot_inst)))]
                              ["n cells really burned" (:observed-burn-n-cells stats)]
                              ["sim ∩ real" (format-w-pct (:sim-inter-obs-n-cells stats))]
                              ["ToA-ratio: mean" (some->> (::toa-ratio-mean stats) (format "%1.2f"))]
                              ["p50" (some->> (::toa-ratio-p50 stats) (format "%1.2f"))]
                              ["p75" (some->> (::toa-ratio-p75 stats) (format "%1.2f"))]
                              ["p90" (some->> (::toa-ratio-p90 stats) (format "%1.2f"))]
                              ["p95" (some->> (::toa-ratio-p95 stats) (format "%1.2f"))]
                              ["sim - real" (format-w-pct (:sim-minus-obs-n-cells stats))]
                              ["real - sim" (format-w-pct (:obs-minus-sim-n-cells stats))]
                              ["s∩r crowning" (format-w-pct (:sim-inter-obs-n-crowning stats) (:sim-inter-obs-n-cells stats))]
                              ["s-r crowning" (format-w-pct (:sim-minus-obs-n-crowning stats) (:sim-minus-obs-n-cells stats))]])))))))))

(defn print-replayed-results-table
  [rr-stats]
  (pprint-table-from-kv-lists
   (replayed-results-table-entries rr-stats)))


(defn tensor-nan-if
  [pred tensr]
  (d/emap
   (fn nan-if [^double x]
     (if (pred x)
       Double/NaN
       x))
   nil
   tensr))

(defn print-inputs-quantiles-tables
  [matrix-k+expected-units+nan-fn fire-name+sim-inputs]
  (->> fire-name+sim-inputs
       (pmap
        (fn [[fire-name sim-inputs]]
          (->> matrix-k+expected-units+nan-fn
               (mapv (fn [[matrix-k expected-unit nan-fn]]
                       (-> sim-inputs
                           (get matrix-k)
                           (cond-> (some? nan-fn) (->> (tensor-nan-if nan-fn)))
                           (as-> mx
                                 (let [ks [:min :quartile-1 :median :quartile-3]]
                                   (merge
                                    {:pyrcst_fire_name fire-name
                                     :input-name (format "%s (%s)" (pr-str matrix-k) expected-unit)}
                                    (->
                                      (dfn/descriptive-statistics
                                       ks
                                       {:nan-strategy :remove}
                                       mx)
                                      ;; reordering keys
                                      (select-keys ks)))))))))))
       (sequence cat)
       (pprint/print-table)))

(def variation-name->info
  {"00) original" {}
   "01) crowning disabled" {::transform-inputs-fn (fn [inputs] (assoc inputs :canopy-base-height-matrix (:canopy-height-matrix inputs)))}})

(def rr-stats-85f1467
  (delay
   (comment
     ;; How this was computed, at commit 85f1467:
     (->> (replay-snapshots variation-name->info nil snaps)
          (replayed-results-stats)
          (vec)
          (time)))
   (-> (io/resource "gridfire/lab/replay/rr-stats-85f1467.edn")
       slurp
       read-string)))

(def rr-stats-9d09fa9
  (delay
   (comment
     ;; How this was computed, at commit 9d09fa9:
     (->> (replay-snapshots variation-name->info nil snaps)
          (replayed-results-stats)
          (vec)
          (time)))
   (-> (io/resource "gridfire/lab/replay/rr-stats-9d09fa9.edn")
       slurp
       read-string)))

(comment

  (def snaps (extract-pyrecast-snapshots! "../pyrecast-snapshots-2022-09-30"))

  (count snaps)
  ;;=> 256

  (take 3 snaps)

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

  (def replay-results (time (replay-snapshots variation-name->info 64 snaps)))
  ;;"Elapsed time: 59593.890425 msecs"


  (def replay-results2 (time (replay-snapshots variation-name->info 8 snaps)))


  (print-replayed-results-table @rr-stats-85f1467)
  ;; OLD
  ;;|              Fire name |             Variation |                           t0 |                                   t1 | n cells really burned |    sim ∩ real | ToA-ratio: mean |  p50 |  p75 |  p90 |  p95 |      sim - real |    real - sim | s∩r crowning |  s-r crowning |
  ;;|------------------------+-----------------------+------------------------------+--------------------------------------+-----------------------+---------------+-----------------+------+------+------+------+-----------------+---------------+--------------+---------------|
  ;;|              ca-summit |          00) original | Wed Sep 21 10:15:00 UTC 2022 | Thu Sep 22 09:58:00 UTC 2022 (+24hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |
  ;;|              ca-summit | 01) crowning disabled | Wed Sep 21 10:15:00 UTC 2022 | Thu Sep 22 09:58:00 UTC 2022 (+24hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |
  ;;|            or-sturgill |          00) original | Wed Sep 14 09:54:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                 884.0 |   203 (23.0%) |            0.15 | 0.04 | 0.04 | 0.50 | 0.63 |     210 (23.8%) |   681 (77.0%) |     9 (4.4%) |    66 (31.4%) |
  ;;|            or-sturgill | 01) crowning disabled | Wed Sep 14 09:54:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                 884.0 |   203 (23.0%) |            0.15 | 0.04 | 0.04 | 0.52 | 0.63 |       66 (7.5%) |   681 (77.0%) |     0 (0.0%) |      0 (0.0%) |
  ;;|          mt-government |          00) original | Fri Sep 16 11:01:00 UTC 2022 | Sat Sep 17 11:20:00 UTC 2022 (+24hr) |                  89.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   89 (100.0%) |        0 (-) |         0 (-) |
  ;;|          mt-government | 01) crowning disabled | Fri Sep 16 11:01:00 UTC 2022 | Sat Sep 17 11:20:00 UTC 2022 (+24hr) |                  89.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   89 (100.0%) |        0 (-) |         0 (-) |
  ;;|               id-lemhi |          00) original | Sun Sep 25 21:12:00 UTC 2022 | Mon Sep 26 10:19:00 UTC 2022 (+13hr) |                1791.0 |   981 (54.8%) |            0.42 | 0.44 | 0.64 | 0.76 | 0.80 |   2213 (123.6%) |   810 (45.2%) |    90 (9.2%) |  1181 (53.4%) |
  ;;|               id-lemhi | 01) crowning disabled | Sun Sep 25 21:12:00 UTC 2022 | Mon Sep 26 10:19:00 UTC 2022 (+13hr) |                1791.0 |   830 (46.3%) |            0.40 | 0.41 | 0.62 | 0.75 | 0.80 |      128 (7.1%) |   961 (53.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-irving-pk |          00) original | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                 678.0 |   290 (42.8%) |            0.07 | 0.03 | 0.08 | 0.14 | 0.17 |   2846 (419.8%) |   388 (57.2%) |   59 (20.3%) |  1740 (61.1%) |
  ;;|           wa-irving-pk | 01) crowning disabled | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                 678.0 |   270 (39.8%) |            0.19 | 0.02 | 0.26 | 0.70 | 0.76 |       40 (5.9%) |   408 (60.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;| id-isabella-lower-twin |          00) original | Sun Sep 18 08:37:00 UTC 2022 | Mon Sep 19 10:49:00 UTC 2022 (+26hr) |                 595.0 |    86 (14.5%) |            0.28 | 0.02 | 0.50 | 0.76 | 0.82 |   1065 (179.0%) |   509 (85.5%) |     6 (7.0%) |   488 (45.8%) |
  ;;| id-isabella-lower-twin | 01) crowning disabled | Sun Sep 18 08:37:00 UTC 2022 | Mon Sep 19 10:49:00 UTC 2022 (+26hr) |                 595.0 |    83 (13.9%) |            0.26 | 0.02 | 0.50 | 0.71 | 0.76 |     121 (20.3%) |   512 (86.1%) |     0 (0.0%) |      0 (0.0%) |
  ;;|              ca-summit |          00) original | Thu Sep 22 21:18:00 UTC 2022 | Sat Sep 24 09:20:00 UTC 2022 (+36hr) |                  14.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   14 (100.0%) |        0 (-) |         0 (-) |
  ;;|              ca-summit | 01) crowning disabled | Thu Sep 22 21:18:00 UTC 2022 | Sat Sep 24 09:20:00 UTC 2022 (+36hr) |                  14.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   14 (100.0%) |        0 (-) |         0 (-) |
  ;;|          wa-goat-rocks |          00) original | Sun Sep 25 09:45:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+11hr) |                   9.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    9 (100.0%) |        0 (-) |         0 (-) |
  ;;|          wa-goat-rocks | 01) crowning disabled | Sun Sep 25 09:45:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+11hr) |                   9.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    9 (100.0%) |        0 (-) |         0 (-) |
  ;;| id-isabella-lower-twin |          00) original | Mon Sep 19 10:49:00 UTC 2022 | Mon Sep 19 21:25:00 UTC 2022 (+11hr) |                 466.0 |   333 (71.5%) |            0.30 | 0.08 | 0.78 | 0.91 | 0.94 |     313 (67.2%) |   133 (28.5%) |   55 (16.5%) |   271 (86.6%) |
  ;;| id-isabella-lower-twin | 01) crowning disabled | Mon Sep 19 10:49:00 UTC 2022 | Mon Sep 19 21:25:00 UTC 2022 (+11hr) |                 466.0 |   320 (68.7%) |            0.24 | 0.08 | 0.08 | 0.92 | 0.95 |       26 (5.6%) |   146 (31.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;|              ca-summit |          00) original | Mon Sep 26 09:32:00 UTC 2022 | Tue Sep 27 09:11:00 UTC 2022 (+24hr) |                  49.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   49 (100.0%) |        0 (-) |         0 (-) |
  ;;|              ca-summit | 01) crowning disabled | Mon Sep 26 09:32:00 UTC 2022 | Tue Sep 27 09:11:00 UTC 2022 (+24hr) |                  49.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   49 (100.0%) |        0 (-) |         0 (-) |
  ;;|        or-double-creek |          00) original | Mon Sep 19 09:09:00 UTC 2022 | Tue Sep 20 08:52:00 UTC 2022 (+24hr) |                1883.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1883 (100.0%) |        0 (-) |         0 (-) |
  ;;|        or-double-creek | 01) crowning disabled | Mon Sep 19 09:09:00 UTC 2022 | Tue Sep 20 08:52:00 UTC 2022 (+24hr) |                1883.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1883 (100.0%) |        0 (-) |         0 (-) |
  ;;|            mt-billiard |          00) original | Wed Sep 14 09:52:00 UTC 2022 | Fri Sep 16 11:11:00 UTC 2022 (+49hr) |                 356.0 |   200 (56.2%) |            0.23 | 0.02 | 0.30 | 0.76 | 0.78 |  4757 (1336.2%) |   156 (43.8%) |   54 (27.0%) |  3225 (67.8%) |
  ;;|            mt-billiard | 01) crowning disabled | Wed Sep 14 09:52:00 UTC 2022 | Fri Sep 16 11:11:00 UTC 2022 (+49hr) |                 356.0 |   191 (53.7%) |            0.20 | 0.02 | 0.29 | 0.71 | 0.79 |    726 (203.9%) |   165 (46.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;|                 ca-red |          00) original | Wed Sep 14 16:50:00 UTC 2022 | Thu Sep 15 10:28:00 UTC 2022 (+18hr) |                  96.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   96 (100.0%) |        0 (-) |         0 (-) |
  ;;|                 ca-red | 01) crowning disabled | Wed Sep 14 16:50:00 UTC 2022 | Thu Sep 15 10:28:00 UTC 2022 (+18hr) |                  96.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   96 (100.0%) |        0 (-) |         0 (-) |
  ;;|              mt-cannon |          00) original | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+59hr) |                 141.0 |   129 (91.5%) |            0.14 | 0.01 | 0.18 | 0.40 | 0.63 |  1545 (1095.7%) |     12 (8.5%) |    10 (7.8%) |   241 (15.6%) |
  ;;|              mt-cannon | 01) crowning disabled | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+59hr) |                 141.0 |   129 (91.5%) |            0.14 | 0.01 | 0.19 | 0.40 | 0.63 |   1122 (795.7%) |     12 (8.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|          wa-goat-rocks |          00) original | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+14hr) |                1787.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1787 (100.0%) |        0 (-) |         0 (-) |
  ;;|          wa-goat-rocks | 01) crowning disabled | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+14hr) |                1787.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1787 (100.0%) |        0 (-) |         0 (-) |
  ;;|             id-ross-fk |          00) original | Tue Sep 13 09:24:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+25hr) |                7284.0 |  1964 (27.0%) |            0.23 | 0.02 | 0.47 | 0.56 | 0.63 |    1229 (16.9%) |  5320 (73.0%) |    90 (4.6%) |   223 (18.1%) |
  ;;|             id-ross-fk | 01) crowning disabled | Tue Sep 13 09:24:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+25hr) |                7284.0 |  1959 (26.9%) |            0.23 | 0.02 | 0.48 | 0.56 | 0.61 |     783 (10.7%) |  5325 (73.1%) |     0 (0.0%) |      0 (0.0%) |
  ;;|                 ca-red |          00) original | Thu Sep 15 10:28:00 UTC 2022 | Thu Sep 15 17:34:00 UTC 2022 (+ 7hr) |                  97.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   97 (100.0%) |        0 (-) |         0 (-) |
  ;;|                 ca-red | 01) crowning disabled | Thu Sep 15 10:28:00 UTC 2022 | Thu Sep 15 17:34:00 UTC 2022 (+ 7hr) |                  97.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   97 (100.0%) |        0 (-) |         0 (-) |
  ;;|              ca-summit |          00) original | Mon Sep 19 21:23:00 UTC 2022 | Tue Sep 20 08:54:00 UTC 2022 (+12hr) |                1396.0 |  1146 (82.1%) |            0.08 | 0.03 | 0.03 | 0.03 | 0.59 |        2 (0.1%) |   250 (17.9%) |     0 (0.0%) |      0 (0.0%) |
  ;;|              ca-summit | 01) crowning disabled | Mon Sep 19 21:23:00 UTC 2022 | Tue Sep 20 08:54:00 UTC 2022 (+12hr) |                1396.0 |  1146 (82.1%) |            0.08 | 0.03 | 0.03 | 0.03 | 0.59 |        2 (0.1%) |   250 (17.9%) |     0 (0.0%) |      0 (0.0%) |
  ;;|        or-double-creek |          00) original | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |               20434.0 |  2542 (12.4%) |            0.47 | 0.07 | 0.94 | 0.98 | 0.99 |        0 (0.0%) | 17892 (87.6%) |   227 (8.9%) |         0 (-) |
  ;;|        or-double-creek | 01) crowning disabled | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |               20434.0 |  2256 (11.0%) |            0.14 | 0.07 | 0.07 | 0.07 | 0.96 |        0 (0.0%) | 18178 (89.0%) |     0 (0.0%) |         0 (-) |
  ;;|                 wa-kid |          00) original | Tue Sep 13 20:51:00 UTC 2022 | Wed Sep 14 09:52:00 UTC 2022 (+13hr) |                  78.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   78 (100.0%) |        0 (-) |         0 (-) |
  ;;|                 wa-kid | 01) crowning disabled | Tue Sep 13 20:51:00 UTC 2022 | Wed Sep 14 09:52:00 UTC 2022 (+13hr) |                  78.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   78 (100.0%) |        0 (-) |         0 (-) |
  ;;|        wa-minnow-ridge |          00) original | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2184.0 |  2149 (98.4%) |            0.12 | 0.03 | 0.08 | 0.62 | 0.72 |  16505 (755.7%) |     35 (1.6%) |  620 (28.9%) |  9368 (56.8%) |
  ;;|        wa-minnow-ridge | 01) crowning disabled | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2184.0 |  2043 (93.5%) |            0.22 | 0.09 | 0.23 | 0.74 | 0.83 |   3132 (143.4%) |    141 (6.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|                 ca-red |          00) original | Fri Sep 16 20:40:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+12hr) |                  44.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   44 (100.0%) |        0 (-) |         0 (-) |
  ;;|                 ca-red | 01) crowning disabled | Fri Sep 16 20:40:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+12hr) |                  44.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   44 (100.0%) |        0 (-) |         0 (-) |
  ;;|         mt-george-lake |          00) original | Tue Sep 13 09:22:00 UTC 2022 | Thu Sep 15 11:09:00 UTC 2022 (+50hr) |                1349.0 |   778 (57.7%) |            0.22 | 0.21 | 0.27 | 0.71 | 0.80 |   2248 (166.6%) |   571 (42.3%) |  104 (13.4%) |   896 (39.9%) |
  ;;|         mt-george-lake | 01) crowning disabled | Tue Sep 13 09:22:00 UTC 2022 | Thu Sep 15 11:09:00 UTC 2022 (+50hr) |                1349.0 |   757 (56.1%) |            0.28 | 0.24 | 0.44 | 0.77 | 0.81 |     311 (23.1%) |   592 (43.9%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               id-lemhi |          00) original | Mon Sep 26 10:19:00 UTC 2022 | Tue Sep 27 09:09:00 UTC 2022 (+23hr) |                2002.0 |  1894 (94.6%) |            0.26 | 0.36 | 0.40 | 0.44 | 0.48 |  15492 (773.8%) |    108 (5.4%) |    78 (4.1%) |  4577 (29.5%) |
  ;;|               id-lemhi | 01) crowning disabled | Mon Sep 26 10:19:00 UTC 2022 | Tue Sep 27 09:09:00 UTC 2022 (+23hr) |                2002.0 |  1894 (94.6%) |            0.27 | 0.37 | 0.42 | 0.51 | 0.55 |   9215 (460.3%) |    108 (5.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-irving-pk |          00) original | Mon Sep 26 09:28:00 UTC 2022 | Tue Sep 27 09:07:00 UTC 2022 (+24hr) |                 661.0 |   426 (64.4%) |            0.23 | 0.02 | 0.42 | 0.61 | 0.88 |   5493 (831.0%) |   235 (35.6%) |   66 (15.5%) |  3206 (58.4%) |
  ;;|           wa-irving-pk | 01) crowning disabled | Mon Sep 26 09:28:00 UTC 2022 | Tue Sep 27 09:07:00 UTC 2022 (+24hr) |                 661.0 |   408 (61.7%) |            0.20 | 0.02 | 0.48 | 0.56 | 0.68 |     191 (28.9%) |   253 (38.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;|        wa-minnow-ridge |          00) original | Tue Sep 27 09:07:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+24hr) |                1622.0 |  1363 (84.0%) |            0.45 | 0.46 | 0.52 | 0.68 | 0.80 |   4018 (247.7%) |   259 (16.0%) |  627 (46.0%) |  2532 (63.0%) |
  ;;|        wa-minnow-ridge | 01) crowning disabled | Tue Sep 27 09:07:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+24hr) |                1622.0 |   950 (58.6%) |            0.51 | 0.57 | 0.73 | 0.87 | 0.93 |     226 (13.9%) |   672 (41.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|        wa-minnow-ridge |          00) original | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:17:00 UTC 2022 (+11hr) |                2157.0 |  1790 (83.0%) |            0.63 | 0.87 | 0.91 | 0.96 | 0.98 |    1291 (59.9%) |   367 (17.0%) |  515 (28.8%) |  1136 (88.0%) |
  ;;|        wa-minnow-ridge | 01) crowning disabled | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:17:00 UTC 2022 (+11hr) |                2157.0 |  1435 (66.5%) |            0.34 | 0.06 | 0.91 | 0.97 | 0.98 |       69 (3.2%) |   722 (33.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|          mt-government |          00) original | Sat Sep 17 11:20:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+ 9hr) |                  96.0 |    60 (62.5%) |            0.26 | 0.04 | 0.82 | 0.84 | 0.86 |      81 (84.4%) |    36 (37.5%) |   28 (46.7%) |   81 (100.0%) |
  ;;|          mt-government | 01) crowning disabled | Sat Sep 17 11:20:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+ 9hr) |                  96.0 |    53 (55.2%) |            0.08 | 0.04 | 0.04 | 0.04 | 0.83 |        0 (0.0%) |    43 (44.8%) |     0 (0.0%) |         0 (-) |
  ;;|              ca-barnes |          00) original | Tue Sep 13 13:18:00 UTC 2022 | Tue Sep 13 19:12:00 UTC 2022 (+ 6hr) |                 888.0 |   327 (36.8%) |            0.24 | 0.05 | 0.05 | 0.94 | 0.95 |        2 (0.2%) |   561 (63.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;|              ca-barnes | 01) crowning disabled | Tue Sep 13 13:18:00 UTC 2022 | Tue Sep 13 19:12:00 UTC 2022 (+ 6hr) |                 888.0 |   327 (36.8%) |            0.24 | 0.05 | 0.05 | 0.94 | 0.95 |        2 (0.2%) |   561 (63.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;|        wa-minnow-ridge |          00) original | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                2298.0 |  2186 (95.1%) |            0.14 | 0.03 | 0.17 | 0.54 | 0.68 |   3105 (135.1%) |    112 (4.9%) |   186 (8.5%) |  1906 (61.4%) |
  ;;|        wa-minnow-ridge | 01) crowning disabled | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                2298.0 |  2148 (93.5%) |            0.16 | 0.02 | 0.27 | 0.44 | 0.63 |     598 (26.0%) |    150 (6.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|        wa-minnow-ridge |          00) original | Wed Sep 21 09:22:00 UTC 2022 | Thu Sep 22 10:43:00 UTC 2022 (+25hr) |                3105.0 |  3008 (96.9%) |            0.38 | 0.41 | 0.47 | 0.54 | 0.65 |  13024 (419.5%) |     97 (3.1%) |  993 (33.0%) |  8462 (65.0%) |
  ;;|        wa-minnow-ridge | 01) crowning disabled | Wed Sep 21 09:22:00 UTC 2022 | Thu Sep 22 10:43:00 UTC 2022 (+25hr) |                3105.0 |  2749 (88.5%) |            0.47 | 0.51 | 0.69 | 0.80 | 0.84 |    1043 (33.6%) |   356 (11.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-irving-pk |          00) original | Mon Sep 19 10:00:00 UTC 2022 | Tue Sep 20 08:50:00 UTC 2022 (+23hr) |                1621.0 |   605 (37.3%) |            0.53 | 0.48 | 0.65 | 0.81 | 0.87 |     805 (49.7%) |  1016 (62.7%) |    55 (9.1%) |   210 (26.1%) |
  ;;|           wa-irving-pk | 01) crowning disabled | Mon Sep 19 10:00:00 UTC 2022 | Tue Sep 20 08:50:00 UTC 2022 (+23hr) |                1621.0 |   598 (36.9%) |            0.54 | 0.49 | 0.65 | 0.79 | 0.89 |     407 (25.1%) |  1023 (63.1%) |     0 (0.0%) |      0 (0.0%) |
  ;;|        or-double-creek |          00) original | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 08:33:00 UTC 2022 (+12hr) |               25821.0 | 19722 (76.4%) |            0.25 | 0.12 | 0.44 | 0.72 | 0.84 |    4966 (19.2%) |  6099 (23.6%) |   596 (3.0%) |  1382 (27.8%) |
  ;;|        or-double-creek | 01) crowning disabled | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 08:33:00 UTC 2022 (+12hr) |               25821.0 | 19011 (73.6%) |            0.23 | 0.02 | 0.38 | 0.71 | 0.86 |      995 (3.9%) |  6810 (26.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;| id-columbus-bear-gulch |          00) original | Wed Sep 14 21:21:00 UTC 2022 | Sat Sep 17 10:36:00 UTC 2022 (+61hr) |                 662.0 |   425 (64.2%) |            0.16 | 0.01 | 0.38 | 0.41 | 0.42 | 19703 (2976.3%) |   237 (35.8%) |  131 (30.8%) | 12666 (64.3%) |
  ;;| id-columbus-bear-gulch | 01) crowning disabled | Wed Sep 14 21:21:00 UTC 2022 | Sat Sep 17 10:36:00 UTC 2022 (+61hr) |                 662.0 |   370 (55.9%) |            0.10 | 0.01 | 0.03 | 0.46 | 0.68 |   1357 (205.0%) |   292 (44.1%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               id-lemhi |          00) original | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+14hr) |                3684.0 |  3600 (97.7%) |            0.19 | 0.19 | 0.27 | 0.33 | 0.38 |  13368 (362.9%) |     84 (2.3%) |  945 (26.3%) |  4785 (35.8%) |
  ;;|               id-lemhi | 01) crowning disabled | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+14hr) |                3684.0 |  3152 (85.6%) |            0.26 | 0.22 | 0.36 | 0.54 | 0.67 |   5897 (160.1%) |   532 (14.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-irving-pk |          00) original | Wed Sep 28 10:30:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+24hr) |                 926.0 |   655 (70.7%) |            0.18 | 0.02 | 0.37 | 0.48 | 0.54 |   2802 (302.6%) |   271 (29.3%) |  112 (17.1%) |  1517 (54.1%) |
  ;;|           wa-irving-pk | 01) crowning disabled | Wed Sep 28 10:30:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+24hr) |                 926.0 |   649 (70.1%) |            0.20 | 0.02 | 0.47 | 0.52 | 0.57 |     303 (32.7%) |   277 (29.9%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-irving-pk |          00) original | Sat Sep 17 09:47:00 UTC 2022 | Mon Sep 19 10:00:00 UTC 2022 (+48hr) |                1286.0 |   344 (26.7%) |            0.24 | 0.20 | 0.24 | 0.82 | 0.88 |   5712 (444.2%) |   942 (73.3%) |  107 (31.1%) |  2589 (45.3%) |
  ;;|           wa-irving-pk | 01) crowning disabled | Sat Sep 17 09:47:00 UTC 2022 | Mon Sep 19 10:00:00 UTC 2022 (+48hr) |                1286.0 |   306 (23.8%) |            0.19 | 0.23 | 0.27 | 0.35 | 0.47 |     702 (54.6%) |   980 (76.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;|        or-double-creek |          00) original | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                4588.0 |  1396 (30.4%) |            0.47 | 0.48 | 0.72 | 0.86 | 0.92 |   5861 (127.7%) |  3192 (69.6%) |  161 (11.5%) |  1968 (33.6%) |
  ;;|        or-double-creek | 01) crowning disabled | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                4588.0 |  1217 (26.5%) |            0.46 | 0.55 | 0.73 | 0.88 | 0.95 |     573 (12.5%) |  3371 (73.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|              ca-summit |          00) original | Sun Sep 25 09:49:00 UTC 2022 | Mon Sep 26 09:32:00 UTC 2022 (+24hr) |                 147.0 |    62 (42.2%) |            0.07 | 0.03 | 0.03 | 0.03 | 0.61 |        0 (0.0%) |    85 (57.8%) |     0 (0.0%) |         0 (-) |
  ;;|              ca-summit | 01) crowning disabled | Sun Sep 25 09:49:00 UTC 2022 | Mon Sep 26 09:32:00 UTC 2022 (+24hr) |                 147.0 |    62 (42.2%) |            0.07 | 0.03 | 0.03 | 0.03 | 0.61 |        0 (0.0%) |    85 (57.8%) |     0 (0.0%) |         0 (-) |
  ;;| id-caledonia-blackburn |          00) original | Wed Sep 14 09:52:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+72hr) |                 188.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  188 (100.0%) |        0 (-) |         0 (-) |
  ;;| id-caledonia-blackburn | 01) crowning disabled | Wed Sep 14 09:52:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+72hr) |                 188.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  188 (100.0%) |        0 (-) |         0 (-) |
  ;;| id-kootenai-rv-complex |          00) original | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 02:03:00 UTC 2022 (+ 6hr) |                5185.0 |  3903 (75.3%) |            0.16 | 0.05 | 0.19 | 0.47 | 0.63 |  10315 (198.9%) |  1282 (24.7%) |  639 (16.4%) |  6941 (67.3%) |
  ;;| id-kootenai-rv-complex | 01) crowning disabled | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 02:03:00 UTC 2022 (+ 6hr) |                5185.0 |  3747 (72.3%) |            0.20 | 0.05 | 0.29 | 0.64 | 0.78 |    1221 (23.5%) |  1438 (27.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|              ca-summit |          00) original | Tue Sep 27 09:11:00 UTC 2022 | Wed Sep 28 09:43:00 UTC 2022 (+25hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |
  ;;|              ca-summit | 01) crowning disabled | Tue Sep 27 09:11:00 UTC 2022 | Wed Sep 28 09:43:00 UTC 2022 (+25hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |
  ;;|            mt-margaret |          00) original | Thu Sep 15 08:43:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+49hr) |                 679.0 |   648 (95.4%) |            0.34 | 0.26 | 0.70 | 0.84 | 0.92 |   2611 (384.5%) |     31 (4.6%) |   70 (10.8%) |   688 (26.4%) |
  ;;|            mt-margaret | 01) crowning disabled | Thu Sep 15 08:43:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+49hr) |                 679.0 |   625 (92.0%) |            0.33 | 0.29 | 0.49 | 0.82 | 0.90 |    792 (116.6%) |     54 (8.0%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-irving-pk |          00) original | Thu Sep 15 10:26:00 UTC 2022 | Fri Sep 16 09:15:00 UTC 2022 (+23hr) |                  81.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   81 (100.0%) |        0 (-) |         0 (-) |
  ;;|           wa-irving-pk | 01) crowning disabled | Thu Sep 15 10:26:00 UTC 2022 | Fri Sep 16 09:15:00 UTC 2022 (+23hr) |                  81.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   81 (100.0%) |        0 (-) |         0 (-) |
  ;;|            wa-siouoxon |          00) original | Sun Sep 25 11:28:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+24hr) |                 430.0 |   340 (79.1%) |            0.43 | 0.45 | 0.56 | 0.73 | 0.80 |     271 (63.0%) |    90 (20.9%) |    29 (8.5%) |     13 (4.8%) |
  ;;|            wa-siouoxon | 01) crowning disabled | Sun Sep 25 11:28:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+24hr) |                 430.0 |   291 (67.7%) |            0.46 | 0.49 | 0.66 | 0.79 | 0.84 |     184 (42.8%) |   139 (32.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;| id-caledonia-blackburn |          00) original | Sat Sep 17 09:47:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+11hr) |                 142.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  142 (100.0%) |        0 (-) |         0 (-) |
  ;;| id-caledonia-blackburn | 01) crowning disabled | Sat Sep 17 09:47:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+11hr) |                 142.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  142 (100.0%) |        0 (-) |         0 (-) |
  ;;|         or-cedar-creek |          00) original | Fri Sep 16 09:15:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+24hr) |               19045.0 |  6322 (33.2%) |            0.42 | 0.51 | 0.67 | 0.83 | 0.90 |    1923 (10.1%) | 12723 (66.8%) |   288 (4.6%) |     67 (3.5%) |
  ;;|         or-cedar-creek | 01) crowning disabled | Fri Sep 16 09:15:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+24hr) |               19045.0 |  6014 (31.6%) |            0.39 | 0.48 | 0.67 | 0.83 | 0.91 |     1646 (8.6%) | 13031 (68.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-irving-pk |          00) original | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2760.0 |  2180 (79.0%) |            0.17 | 0.02 | 0.08 | 0.73 | 0.82 |   9202 (333.4%) |   580 (21.0%) |  304 (13.9%) |  3574 (38.8%) |
  ;;|           wa-irving-pk | 01) crowning disabled | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2760.0 |  2059 (74.6%) |            0.11 | 0.01 | 0.10 | 0.26 | 0.69 |    2470 (89.5%) |   701 (25.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         id-trail-ridge |          00) original | Sat Sep 17 10:23:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+10hr) |                   2.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    2 (100.0%) |        0 (-) |         0 (-) |
  ;;|         id-trail-ridge | 01) crowning disabled | Sat Sep 17 10:23:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+10hr) |                   2.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    2 (100.0%) |        0 (-) |         0 (-) |
  ;;| id-kootenai-rv-complex |          00) original | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                3567.0 |  2652 (74.3%) |            0.17 | 0.01 | 0.25 | 0.60 | 0.69 |   8137 (228.1%) |   915 (25.7%) |  325 (12.3%) |  4668 (57.4%) |
  ;;| id-kootenai-rv-complex | 01) crowning disabled | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                3567.0 |  2607 (73.1%) |            0.16 | 0.01 | 0.25 | 0.59 | 0.70 |    2989 (83.8%) |   960 (26.9%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               id-katka |          00) original | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:12:00 UTC 2022 (+40hr) |                  12.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   12 (100.0%) |        0 (-) |         0 (-) |
  ;;|               id-katka | 01) crowning disabled | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:12:00 UTC 2022 (+40hr) |                  12.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   12 (100.0%) |        0 (-) |         0 (-) |
  ;;|            or-sturgill |          00) original | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                6854.0 |  5505 (80.3%) |            0.39 | 0.44 | 0.50 | 0.56 | 0.68 |  15066 (219.8%) |  1349 (19.7%) |  723 (13.1%) |  5918 (39.3%) |
  ;;|            or-sturgill | 01) crowning disabled | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                6854.0 |  4906 (71.6%) |            0.45 | 0.49 | 0.69 | 0.86 | 0.92 |    1469 (21.4%) |  1948 (28.4%) |     3 (0.1%) |      0 (0.0%) |
  ;;|          wa-bolt-creek |          00) original | Thu Sep 22 09:54:00 UTC 2022 | Thu Sep 22 20:29:00 UTC 2022 (+11hr) |                4179.0 |  3175 (76.0%) |            0.27 | 0.09 | 0.09 | 0.90 | 0.95 |     857 (20.5%) |  1004 (24.0%) |   260 (8.2%) |   794 (92.6%) |
  ;;|          wa-bolt-creek | 01) crowning disabled | Thu Sep 22 09:54:00 UTC 2022 | Thu Sep 22 20:29:00 UTC 2022 (+11hr) |                4179.0 |  3074 (73.6%) |            0.13 | 0.09 | 0.09 | 0.09 | 0.76 |       14 (0.3%) |  1105 (26.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;| id-kootenai-rv-complex |          00) original | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:04:00 UTC 2022 (+40hr) |                 488.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  488 (100.0%) |        0 (-) |         0 (-) |
  ;;| id-kootenai-rv-complex | 01) crowning disabled | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:04:00 UTC 2022 (+40hr) |                 488.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  488 (100.0%) |        0 (-) |         0 (-) |
  ;;|             ca-rodgers |          00) original | Fri Sep 16 16:28:00 UTC 2022 | Sat Sep 17 09:49:00 UTC 2022 (+17hr) |                  40.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   40 (100.0%) |        0 (-) |         0 (-) |
  ;;|             ca-rodgers | 01) crowning disabled | Fri Sep 16 16:28:00 UTC 2022 | Sat Sep 17 09:49:00 UTC 2022 (+17hr) |                  40.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   40 (100.0%) |        0 (-) |         0 (-) |
  ;;|        wa-minnow-ridge |          00) original | Sun Sep 25 08:58:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+12hr) |                2408.0 |  2026 (84.1%) |            0.48 | 0.08 | 0.89 | 0.97 | 0.98 |     447 (18.6%) |   382 (15.9%) |  253 (12.5%) |   360 (80.5%) |
  ;;|        wa-minnow-ridge | 01) crowning disabled | Sun Sep 25 08:58:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+12hr) |                2408.0 |  1899 (78.9%) |            0.32 | 0.08 | 0.88 | 0.95 | 0.98 |       30 (1.2%) |   509 (21.1%) |     0 (0.0%) |      0 (0.0%) |
  ;;|          mt-government |          00) original | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 09:28:00 UTC 2022 (+13hr) |                  96.0 |    87 (90.6%) |            0.19 | 0.07 | 0.34 | 0.47 | 0.49 |  1499 (1561.5%) |      9 (9.4%) |   44 (50.6%) |   965 (64.4%) |
  ;;|          mt-government | 01) crowning disabled | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 09:28:00 UTC 2022 (+13hr) |                  96.0 |    60 (62.5%) |            0.09 | 0.03 | 0.17 | 0.27 | 0.32 |      76 (79.2%) |    36 (37.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               id-katka |          00) original | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                 155.0 |   125 (80.6%) |            0.33 | 0.32 | 0.56 | 0.68 | 0.73 |    719 (463.9%) |    30 (19.4%) |   41 (32.8%) |   385 (53.5%) |
  ;;|               id-katka | 01) crowning disabled | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                 155.0 |   119 (76.8%) |            0.35 | 0.32 | 0.60 | 0.71 | 0.76 |    205 (132.3%) |    36 (23.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;|          wa-goat-rocks |          00) original | Mon Sep 26 11:08:00 UTC 2022 | Tue Sep 27 10:00:00 UTC 2022 (+23hr) |                2916.0 |  2241 (76.9%) |            0.36 | 0.38 | 0.50 | 0.71 | 0.81 |    2110 (72.4%) |   675 (23.1%) |    92 (4.1%) |   623 (29.5%) |
  ;;|          wa-goat-rocks | 01) crowning disabled | Mon Sep 26 11:08:00 UTC 2022 | Tue Sep 27 10:00:00 UTC 2022 (+23hr) |                2916.0 |  2204 (75.6%) |            0.36 | 0.38 | 0.50 | 0.70 | 0.81 |     879 (30.1%) |   712 (24.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|          id-deep-creek |          00) original | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:19:00 UTC 2022 (+11hr) |                 145.0 |    72 (49.7%) |            0.32 | 0.06 | 0.87 | 0.95 | 0.97 |      93 (64.1%) |    73 (50.3%) |   17 (23.6%) |    35 (37.6%) |
  ;;|          id-deep-creek | 01) crowning disabled | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:19:00 UTC 2022 (+11hr) |                 145.0 |    65 (44.8%) |            0.16 | 0.06 | 0.06 | 0.88 | 0.96 |      28 (19.3%) |    80 (55.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;|              ca-summit |          00) original | Sun Sep 18 10:21:00 UTC 2022 | Sun Sep 18 16:30:00 UTC 2022 (+ 6hr) |                2461.0 |   811 (33.0%) |            0.06 | 0.06 | 0.06 | 0.06 | 0.06 |        0 (0.0%) |  1650 (67.0%) |     0 (0.0%) |         0 (-) |
  ;;|              ca-summit | 01) crowning disabled | Sun Sep 18 10:21:00 UTC 2022 | Sun Sep 18 16:30:00 UTC 2022 (+ 6hr) |                2461.0 |   811 (33.0%) |            0.06 | 0.06 | 0.06 | 0.06 | 0.06 |        0 (0.0%) |  1650 (67.0%) |     0 (0.0%) |         0 (-) |
  ;;| id-isabella-lower-twin |          00) original | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 08:56:00 UTC 2022 (+47hr) |                 370.0 |   187 (50.5%) |            0.23 | 0.24 | 0.31 | 0.35 | 0.72 |   1578 (426.5%) |   183 (49.5%) |   57 (30.5%) |   602 (38.1%) |
  ;;| id-isabella-lower-twin | 01) crowning disabled | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 08:56:00 UTC 2022 (+47hr) |                 370.0 |   166 (44.9%) |            0.35 | 0.31 | 0.71 | 0.82 | 0.84 |     210 (56.8%) |   204 (55.1%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             id-tenmile |          00) original | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                 289.0 |   242 (83.7%) |            0.21 | 0.04 | 0.42 | 0.55 | 0.60 |    775 (268.2%) |    47 (16.3%) |    13 (5.4%) |     46 (5.9%) |
  ;;|             id-tenmile | 01) crowning disabled | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                 289.0 |   242 (83.7%) |            0.22 | 0.04 | 0.44 | 0.56 | 0.62 |    538 (186.2%) |    47 (16.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;|            ca-mosquito |          00) original | Wed Sep 14 22:01:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+12hr) |              108947.0 | 77291 (70.9%) |            0.21 | 0.14 | 0.33 | 0.57 | 0.71 |   47466 (43.6%) | 31656 (29.1%) | 7786 (10.1%) | 27055 (57.0%) |
  ;;|            ca-mosquito | 01) crowning disabled | Wed Sep 14 22:01:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+12hr) |              108947.0 | 72699 (66.7%) |            0.25 | 0.17 | 0.40 | 0.65 | 0.76 |   15640 (14.4%) | 36248 (33.3%) |     0 (0.0%) |      0 (0.0%) |

  (print-replayed-results-table @rr-stats-9d09fa9)
  ;; NEW
  ;;|               Fire name |             Variation |                           t0 |                                   t1 | n cells really burned |    sim ∩ real | ToA-ratio: mean |  p50 |  p75 |  p90 |  p95 |      sim - real |    real - sim | s∩r crowning |  s-r crowning |
  ;;|-------------------------+-----------------------+------------------------------+--------------------------------------+-----------------------+---------------+-----------------+------+------+------+------+-----------------+---------------+--------------+---------------|
  ;;|               ca-barnes |          00) original | Tue Sep 13 13:18:00 UTC 2022 | Tue Sep 13 19:12:00 UTC 2022 (+ 6hr) |                 888.0 |   358 (40.3%) |            0.25 | 0.05 | 0.48 | 0.81 | 0.94 |       69 (7.8%) |   530 (59.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               ca-barnes | 01) crowning disabled | Tue Sep 13 13:18:00 UTC 2022 | Tue Sep 13 19:12:00 UTC 2022 (+ 6hr) |                 888.0 |   358 (40.3%) |            0.25 | 0.05 | 0.48 | 0.81 | 0.94 |       69 (7.8%) |   530 (59.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             ca-mosquito |          00) original | Wed Sep 14 09:05:00 UTC 2022 | Wed Sep 14 14:11:00 UTC 2022 (+ 5hr) |               86484.0 | 45134 (52.2%) |            0.32 | 0.26 | 0.55 | 0.81 | 0.90 |    9001 (10.4%) | 41350 (47.8%) |  2561 (5.7%) |  4048 (45.0%) |
  ;;|             ca-mosquito | 01) crowning disabled | Wed Sep 14 09:05:00 UTC 2022 | Wed Sep 14 14:11:00 UTC 2022 (+ 5hr) |               86484.0 | 43848 (50.7%) |            0.33 | 0.25 | 0.58 | 0.83 | 0.92 |     4338 (5.0%) | 42636 (49.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             ca-mosquito |          00) original | Wed Sep 14 14:11:00 UTC 2022 | Wed Sep 14 22:01:00 UTC 2022 (+ 8hr) |               91936.0 | 60969 (66.3%) |            0.33 | 0.27 | 0.56 | 0.78 | 0.89 |   29708 (32.3%) | 30967 (33.7%) |  2758 (4.5%) | 12276 (41.3%) |
  ;;|             ca-mosquito | 01) crowning disabled | Wed Sep 14 14:11:00 UTC 2022 | Wed Sep 14 22:01:00 UTC 2022 (+ 8hr) |               91936.0 | 59417 (64.6%) |            0.33 | 0.26 | 0.56 | 0.80 | 0.89 |   14241 (15.5%) | 32519 (35.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             ca-mosquito |          00) original | Wed Sep 14 22:01:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+12hr) |              108947.0 | 75383 (69.2%) |            0.27 | 0.18 | 0.44 | 0.75 | 0.87 |   30809 (28.3%) | 33564 (30.8%) |  3045 (4.0%) | 10479 (34.0%) |
  ;;|             ca-mosquito | 01) crowning disabled | Wed Sep 14 22:01:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+12hr) |              108947.0 | 73465 (67.4%) |            0.28 | 0.19 | 0.46 | 0.76 | 0.87 |   18154 (16.7%) | 35482 (32.6%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             ca-mosquito |          00) original | Thu Sep 15 09:34:00 UTC 2022 | Fri Sep 16 09:17:00 UTC 2022 (+24hr) |               96989.0 | 68360 (70.5%) |            0.23 | 0.17 | 0.36 | 0.53 | 0.66 |   63385 (65.4%) | 28629 (29.5%) |  4167 (6.1%) | 16832 (26.6%) |
  ;;|             ca-mosquito | 01) crowning disabled | Thu Sep 15 09:34:00 UTC 2022 | Fri Sep 16 09:17:00 UTC 2022 (+24hr) |               96989.0 | 67630 (69.7%) |            0.27 | 0.21 | 0.42 | 0.61 | 0.72 |   39756 (41.0%) | 29359 (30.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             ca-mosquito |          00) original | Fri Sep 16 09:17:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+24hr) |               44211.0 | 40088 (90.7%) |            0.20 | 0.14 | 0.33 | 0.46 | 0.52 |  64463 (145.8%) |   4123 (9.3%) |  1592 (4.0%) | 15904 (24.7%) |
  ;;|             ca-mosquito | 01) crowning disabled | Fri Sep 16 09:17:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+24hr) |               44211.0 | 40066 (90.6%) |            0.24 | 0.17 | 0.38 | 0.56 | 0.69 |   40422 (91.4%) |   4145 (9.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             ca-mosquito |          00) original | Sat Sep 17 08:58:00 UTC 2022 | Sat Sep 17 22:26:00 UTC 2022 (+13hr) |               18460.0 | 16249 (88.0%) |            0.45 | 0.40 | 0.74 | 0.87 | 0.93 |   18216 (98.7%) |  2211 (12.0%) | 1863 (11.5%) |  4773 (26.2%) |
  ;;|             ca-mosquito | 01) crowning disabled | Sat Sep 17 08:58:00 UTC 2022 | Sat Sep 17 22:26:00 UTC 2022 (+13hr) |               18460.0 | 14939 (80.9%) |            0.41 | 0.34 | 0.68 | 0.86 | 0.93 |   11951 (64.7%) |  3521 (19.1%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             ca-mosquito |          00) original | Sat Sep 17 22:26:00 UTC 2022 | Sun Sep 18 09:32:00 UTC 2022 (+11hr) |               15440.0 | 14434 (93.5%) |            0.18 | 0.10 | 0.23 | 0.44 | 0.62 |  20308 (131.5%) |   1006 (6.5%) |  1352 (9.4%) |  4962 (24.4%) |
  ;;|             ca-mosquito | 01) crowning disabled | Sat Sep 17 22:26:00 UTC 2022 | Sun Sep 18 09:32:00 UTC 2022 (+11hr) |               15440.0 | 14140 (91.6%) |            0.24 | 0.14 | 0.34 | 0.64 | 0.79 |   14193 (91.9%) |   1300 (8.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|                  ca-red |          00) original | Wed Sep 14 09:05:00 UTC 2022 | Wed Sep 14 16:50:00 UTC 2022 (+ 8hr) |                  96.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   96 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  ca-red | 01) crowning disabled | Wed Sep 14 09:05:00 UTC 2022 | Wed Sep 14 16:50:00 UTC 2022 (+ 8hr) |                  96.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   96 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  ca-red |          00) original | Wed Sep 14 16:50:00 UTC 2022 | Thu Sep 15 10:28:00 UTC 2022 (+18hr) |                  96.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   96 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  ca-red | 01) crowning disabled | Wed Sep 14 16:50:00 UTC 2022 | Thu Sep 15 10:28:00 UTC 2022 (+18hr) |                  96.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   96 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  ca-red |          00) original | Thu Sep 15 10:28:00 UTC 2022 | Thu Sep 15 17:34:00 UTC 2022 (+ 7hr) |                  97.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   97 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  ca-red | 01) crowning disabled | Thu Sep 15 10:28:00 UTC 2022 | Thu Sep 15 17:34:00 UTC 2022 (+ 7hr) |                  97.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   97 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  ca-red |          00) original | Thu Sep 15 17:34:00 UTC 2022 | Thu Sep 15 20:57:00 UTC 2022 (+ 3hr) |                  97.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   97 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  ca-red | 01) crowning disabled | Thu Sep 15 17:34:00 UTC 2022 | Thu Sep 15 20:57:00 UTC 2022 (+ 3hr) |                  97.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   97 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  ca-red |          00) original | Thu Sep 15 20:57:00 UTC 2022 | Fri Sep 16 09:17:00 UTC 2022 (+12hr) |                  97.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   97 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  ca-red | 01) crowning disabled | Thu Sep 15 20:57:00 UTC 2022 | Fri Sep 16 09:17:00 UTC 2022 (+12hr) |                  97.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   97 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  ca-red |          00) original | Fri Sep 16 09:17:00 UTC 2022 | Fri Sep 16 20:40:00 UTC 2022 (+11hr) |                  44.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   44 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  ca-red | 01) crowning disabled | Fri Sep 16 09:17:00 UTC 2022 | Fri Sep 16 20:40:00 UTC 2022 (+11hr) |                  44.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   44 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  ca-red |          00) original | Fri Sep 16 20:40:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+12hr) |                  44.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   44 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  ca-red | 01) crowning disabled | Fri Sep 16 20:40:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+12hr) |                  44.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   44 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  ca-red |          00) original | Sat Sep 17 08:58:00 UTC 2022 | Sat Sep 17 21:12:00 UTC 2022 (+12hr) |                  72.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   72 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  ca-red | 01) crowning disabled | Sat Sep 17 08:58:00 UTC 2022 | Sat Sep 17 21:12:00 UTC 2022 (+12hr) |                  72.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   72 (100.0%) |        0 (-) |         0 (-) |
  ;;|              ca-rodgers |          00) original | Wed Sep 14 09:56:00 UTC 2022 | Thu Sep 15 21:07:00 UTC 2022 (+35hr) |                  40.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   40 (100.0%) |        0 (-) |         0 (-) |
  ;;|              ca-rodgers | 01) crowning disabled | Wed Sep 14 09:56:00 UTC 2022 | Thu Sep 15 21:07:00 UTC 2022 (+35hr) |                  40.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   40 (100.0%) |        0 (-) |         0 (-) |
  ;;|              ca-rodgers |          00) original | Thu Sep 15 21:07:00 UTC 2022 | Fri Sep 16 16:28:00 UTC 2022 (+19hr) |                  40.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   40 (100.0%) |        0 (-) |         0 (-) |
  ;;|              ca-rodgers | 01) crowning disabled | Thu Sep 15 21:07:00 UTC 2022 | Fri Sep 16 16:28:00 UTC 2022 (+19hr) |                  40.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   40 (100.0%) |        0 (-) |         0 (-) |
  ;;|              ca-rodgers |          00) original | Fri Sep 16 16:28:00 UTC 2022 | Sat Sep 17 09:49:00 UTC 2022 (+17hr) |                  40.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   40 (100.0%) |        0 (-) |         0 (-) |
  ;;|              ca-rodgers | 01) crowning disabled | Fri Sep 16 16:28:00 UTC 2022 | Sat Sep 17 09:49:00 UTC 2022 (+17hr) |                  40.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   40 (100.0%) |        0 (-) |         0 (-) |
  ;;|               ca-summit |          00) original | Sun Sep 18 10:21:00 UTC 2022 | Sun Sep 18 16:30:00 UTC 2022 (+ 6hr) |                2461.0 |   817 (33.2%) |            0.08 | 0.06 | 0.06 | 0.06 | 0.06 |        1 (0.0%) |  1644 (66.8%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               ca-summit | 01) crowning disabled | Sun Sep 18 10:21:00 UTC 2022 | Sun Sep 18 16:30:00 UTC 2022 (+ 6hr) |                2461.0 |   817 (33.2%) |            0.08 | 0.06 | 0.06 | 0.06 | 0.06 |        1 (0.0%) |  1644 (66.8%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               ca-summit |          00) original | Sun Sep 18 16:30:00 UTC 2022 | Mon Sep 19 10:02:00 UTC 2022 (+18hr) |                2462.0 |  1924 (78.1%) |            0.12 | 0.03 | 0.03 | 0.45 | 0.64 |       62 (2.5%) |   538 (21.9%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               ca-summit | 01) crowning disabled | Sun Sep 18 16:30:00 UTC 2022 | Mon Sep 19 10:02:00 UTC 2022 (+18hr) |                2462.0 |  1924 (78.1%) |            0.12 | 0.03 | 0.03 | 0.45 | 0.64 |       62 (2.5%) |   538 (21.9%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               ca-summit |          00) original | Mon Sep 19 10:02:00 UTC 2022 | Mon Sep 19 21:23:00 UTC 2022 (+11hr) |                1396.0 |  1150 (82.4%) |            0.07 | 0.00 | 0.00 | 0.18 | 0.75 |        4 (0.3%) |   246 (17.6%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               ca-summit | 01) crowning disabled | Mon Sep 19 10:02:00 UTC 2022 | Mon Sep 19 21:23:00 UTC 2022 (+11hr) |                1396.0 |  1150 (82.4%) |            0.07 | 0.00 | 0.00 | 0.18 | 0.75 |        4 (0.3%) |   246 (17.6%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               ca-summit |          00) original | Mon Sep 19 21:23:00 UTC 2022 | Tue Sep 20 08:54:00 UTC 2022 (+12hr) |                1396.0 |  1148 (82.2%) |            0.09 | 0.03 | 0.03 | 0.03 | 0.65 |        2 (0.1%) |   248 (17.8%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               ca-summit | 01) crowning disabled | Mon Sep 19 21:23:00 UTC 2022 | Tue Sep 20 08:54:00 UTC 2022 (+12hr) |                1396.0 |  1148 (82.2%) |            0.09 | 0.03 | 0.03 | 0.03 | 0.65 |        2 (0.1%) |   248 (17.8%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               ca-summit |          00) original | Tue Sep 20 08:54:00 UTC 2022 | Wed Sep 21 10:15:00 UTC 2022 (+25hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |
  ;;|               ca-summit | 01) crowning disabled | Tue Sep 20 08:54:00 UTC 2022 | Wed Sep 21 10:15:00 UTC 2022 (+25hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |
  ;;|               ca-summit |          00) original | Wed Sep 21 10:15:00 UTC 2022 | Thu Sep 22 09:58:00 UTC 2022 (+24hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |
  ;;|               ca-summit | 01) crowning disabled | Wed Sep 21 10:15:00 UTC 2022 | Thu Sep 22 09:58:00 UTC 2022 (+24hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |
  ;;|               ca-summit |          00) original | Thu Sep 22 09:58:00 UTC 2022 | Thu Sep 22 21:18:00 UTC 2022 (+11hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |
  ;;|               ca-summit | 01) crowning disabled | Thu Sep 22 09:58:00 UTC 2022 | Thu Sep 22 21:18:00 UTC 2022 (+11hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |
  ;;|               ca-summit |          00) original | Thu Sep 22 21:18:00 UTC 2022 | Sat Sep 24 09:20:00 UTC 2022 (+36hr) |                  14.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   14 (100.0%) |        0 (-) |         0 (-) |
  ;;|               ca-summit | 01) crowning disabled | Thu Sep 22 21:18:00 UTC 2022 | Sat Sep 24 09:20:00 UTC 2022 (+36hr) |                  14.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   14 (100.0%) |        0 (-) |         0 (-) |
  ;;|               ca-summit |          00) original | Sat Sep 24 09:20:00 UTC 2022 | Sun Sep 25 09:49:00 UTC 2022 (+24hr) |                 144.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  144 (100.0%) |        0 (-) |         0 (-) |
  ;;|               ca-summit | 01) crowning disabled | Sat Sep 24 09:20:00 UTC 2022 | Sun Sep 25 09:49:00 UTC 2022 (+24hr) |                 144.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  144 (100.0%) |        0 (-) |         0 (-) |
  ;;|               ca-summit |          00) original | Sun Sep 25 09:49:00 UTC 2022 | Mon Sep 26 09:32:00 UTC 2022 (+24hr) |                 147.0 |    69 (46.9%) |            0.21 | 0.03 | 0.25 | 0.78 | 0.90 |        1 (0.7%) |    78 (53.1%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               ca-summit | 01) crowning disabled | Sun Sep 25 09:49:00 UTC 2022 | Mon Sep 26 09:32:00 UTC 2022 (+24hr) |                 147.0 |    69 (46.9%) |            0.21 | 0.03 | 0.25 | 0.78 | 0.90 |        1 (0.7%) |    78 (53.1%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               ca-summit |          00) original | Mon Sep 26 09:32:00 UTC 2022 | Tue Sep 27 09:11:00 UTC 2022 (+24hr) |                  49.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   49 (100.0%) |        0 (-) |         0 (-) |
  ;;|               ca-summit | 01) crowning disabled | Mon Sep 26 09:32:00 UTC 2022 | Tue Sep 27 09:11:00 UTC 2022 (+24hr) |                  49.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   49 (100.0%) |        0 (-) |         0 (-) |
  ;;|               ca-summit |          00) original | Tue Sep 27 09:11:00 UTC 2022 | Wed Sep 28 09:43:00 UTC 2022 (+25hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |
  ;;|               ca-summit | 01) crowning disabled | Tue Sep 27 09:11:00 UTC 2022 | Wed Sep 28 09:43:00 UTC 2022 (+25hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |
  ;;|               ca-summit |          00) original | Wed Sep 28 09:43:00 UTC 2022 | Thu Sep 29 10:13:00 UTC 2022 (+25hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |
  ;;|               ca-summit | 01) crowning disabled | Wed Sep 28 09:43:00 UTC 2022 | Thu Sep 29 10:13:00 UTC 2022 (+25hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |
  ;;|  id-caledonia-blackburn |          00) original | Wed Sep 14 09:52:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+72hr) |                 188.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  188 (100.0%) |        0 (-) |         0 (-) |
  ;;|  id-caledonia-blackburn | 01) crowning disabled | Wed Sep 14 09:52:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+72hr) |                 188.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  188 (100.0%) |        0 (-) |         0 (-) |
  ;;|  id-caledonia-blackburn |          00) original | Sat Sep 17 09:47:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+11hr) |                 142.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  142 (100.0%) |        0 (-) |         0 (-) |
  ;;|  id-caledonia-blackburn | 01) crowning disabled | Sat Sep 17 09:47:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+11hr) |                 142.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  142 (100.0%) |        0 (-) |         0 (-) |
  ;;|  id-columbus-bear-gulch |          00) original | Tue Sep 13 10:13:00 UTC 2022 | Wed Sep 14 09:05:00 UTC 2022 (+23hr) |                2587.0 |  2434 (94.1%) |            0.12 | 0.02 | 0.14 | 0.44 | 0.66 |  10711 (414.0%) |    153 (5.9%) |   203 (8.3%) |  4451 (41.6%) |
  ;;|  id-columbus-bear-gulch | 01) crowning disabled | Tue Sep 13 10:13:00 UTC 2022 | Wed Sep 14 09:05:00 UTC 2022 (+23hr) |                2587.0 |  2410 (93.2%) |            0.13 | 0.03 | 0.16 | 0.42 | 0.62 |   4000 (154.6%) |    177 (6.8%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-columbus-bear-gulch |          00) original | Wed Sep 14 09:05:00 UTC 2022 | Wed Sep 14 21:21:00 UTC 2022 (+12hr) |                1464.0 |   987 (67.4%) |            0.14 | 0.04 | 0.22 | 0.43 | 0.51 |     248 (16.9%) |   477 (32.6%) |    69 (7.0%) |   117 (47.2%) |
  ;;|  id-columbus-bear-gulch | 01) crowning disabled | Wed Sep 14 09:05:00 UTC 2022 | Wed Sep 14 21:21:00 UTC 2022 (+12hr) |                1464.0 |   953 (65.1%) |            0.10 | 0.01 | 0.20 | 0.33 | 0.40 |      108 (7.4%) |   511 (34.9%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-columbus-bear-gulch |          00) original | Wed Sep 14 21:21:00 UTC 2022 | Sat Sep 17 10:36:00 UTC 2022 (+61hr) |                 662.0 |   406 (61.3%) |            0.22 | 0.01 | 0.46 | 0.74 | 0.83 |   3425 (517.4%) |   256 (38.7%) |   51 (12.6%) |   686 (20.0%) |
  ;;|  id-columbus-bear-gulch | 01) crowning disabled | Wed Sep 14 21:21:00 UTC 2022 | Sat Sep 17 10:36:00 UTC 2022 (+61hr) |                 662.0 |   386 (58.3%) |            0.18 | 0.01 | 0.37 | 0.79 | 0.93 |   2288 (345.6%) |   276 (41.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-columbus-bear-gulch |          00) original | Sat Sep 17 10:36:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+10hr) |                  13.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   13 (100.0%) |        0 (-) |         0 (-) |
  ;;|  id-columbus-bear-gulch | 01) crowning disabled | Sat Sep 17 10:36:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+10hr) |                  13.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   13 (100.0%) |        0 (-) |         0 (-) |
  ;;|  id-columbus-bear-gulch |          00) original | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 10:17:00 UTC 2022 (+14hr) |                  28.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   28 (100.0%) |        0 (-) |         0 (-) |
  ;;|  id-columbus-bear-gulch | 01) crowning disabled | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 10:17:00 UTC 2022 (+14hr) |                  28.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   28 (100.0%) |        0 (-) |         0 (-) |
  ;;|           id-deep-creek |          00) original | Sat Sep 17 21:14:00 UTC 2022 | Sun Sep 18 10:17:00 UTC 2022 (+13hr) |                 556.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  556 (100.0%) |        0 (-) |         0 (-) |
  ;;|           id-deep-creek | 01) crowning disabled | Sat Sep 17 21:14:00 UTC 2022 | Sun Sep 18 10:17:00 UTC 2022 (+13hr) |                 556.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  556 (100.0%) |        0 (-) |         0 (-) |
  ;;|           id-deep-creek |          00) original | Sun Sep 18 10:17:00 UTC 2022 | Mon Sep 19 10:00:00 UTC 2022 (+24hr) |                 630.0 |   559 (88.7%) |            0.04 | 0.01 | 0.04 | 0.11 | 0.13 |   5802 (921.0%) |    71 (11.3%) |    37 (6.6%) |  2131 (36.7%) |
  ;;|           id-deep-creek | 01) crowning disabled | Sun Sep 18 10:17:00 UTC 2022 | Mon Sep 19 10:00:00 UTC 2022 (+24hr) |                 630.0 |   559 (88.7%) |            0.04 | 0.01 | 0.04 | 0.11 | 0.14 |   2487 (394.8%) |    71 (11.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           id-deep-creek |          00) original | Mon Sep 19 10:00:00 UTC 2022 | Mon Sep 19 21:25:00 UTC 2022 (+11hr) |                 619.0 |   548 (88.5%) |            0.20 | 0.14 | 0.28 | 0.53 | 0.63 |    888 (143.5%) |    71 (11.5%) |    30 (5.5%) |   318 (35.8%) |
  ;;|           id-deep-creek | 01) crowning disabled | Mon Sep 19 10:00:00 UTC 2022 | Mon Sep 19 21:25:00 UTC 2022 (+11hr) |                 619.0 |   548 (88.5%) |            0.21 | 0.15 | 0.29 | 0.54 | 0.66 |    650 (105.0%) |    71 (11.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           id-deep-creek |          00) original | Mon Sep 19 21:25:00 UTC 2022 | Tue Sep 20 09:41:00 UTC 2022 (+12hr) |                 691.0 |   602 (87.1%) |            0.16 | 0.06 | 0.22 | 0.50 | 0.71 |   1438 (208.1%) |    89 (12.9%) |    52 (8.6%) |   509 (35.4%) |
  ;;|           id-deep-creek | 01) crowning disabled | Mon Sep 19 21:25:00 UTC 2022 | Tue Sep 20 09:41:00 UTC 2022 (+12hr) |                 691.0 |   590 (85.4%) |            0.17 | 0.05 | 0.24 | 0.46 | 0.72 |    833 (120.5%) |   101 (14.6%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           id-deep-creek |          00) original | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:19:00 UTC 2022 (+11hr) |                 145.0 |    74 (51.0%) |            0.18 | 0.06 | 0.22 | 0.60 | 0.72 |    208 (143.4%) |    71 (49.0%) |     3 (4.1%) |    25 (12.0%) |
  ;;|           id-deep-creek | 01) crowning disabled | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:19:00 UTC 2022 (+11hr) |                 145.0 |    74 (51.0%) |            0.20 | 0.06 | 0.32 | 0.65 | 0.72 |    159 (109.7%) |    71 (49.0%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-isabella-lower-twin |          00) original | Tue Sep 13 10:11:00 UTC 2022 | Wed Sep 14 09:05:00 UTC 2022 (+23hr) |                 473.0 |   292 (61.7%) |            0.17 | 0.13 | 0.27 | 0.43 | 0.56 |    788 (166.6%) |   181 (38.3%) |   35 (12.0%) |   145 (18.4%) |
  ;;|  id-isabella-lower-twin | 01) crowning disabled | Tue Sep 13 10:11:00 UTC 2022 | Wed Sep 14 09:05:00 UTC 2022 (+23hr) |                 473.0 |   290 (61.3%) |            0.27 | 0.20 | 0.47 | 0.68 | 0.81 |     455 (96.2%) |   183 (38.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-isabella-lower-twin |          00) original | Wed Sep 14 09:05:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                 306.0 |   110 (35.9%) |            0.25 | 0.18 | 0.41 | 0.65 | 0.81 |     207 (67.6%) |   196 (64.1%) |   11 (10.0%) |    48 (23.2%) |
  ;;|  id-isabella-lower-twin | 01) crowning disabled | Wed Sep 14 09:05:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                 306.0 |   108 (35.3%) |            0.28 | 0.22 | 0.45 | 0.66 | 0.84 |     116 (37.9%) |   198 (64.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-isabella-lower-twin |          00) original | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 08:56:00 UTC 2022 (+47hr) |                 370.0 |   183 (49.5%) |            0.22 | 0.19 | 0.34 | 0.51 | 0.62 |    642 (173.5%) |   187 (50.5%) |   25 (13.7%) |   115 (17.9%) |
  ;;|  id-isabella-lower-twin | 01) crowning disabled | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 08:56:00 UTC 2022 (+47hr) |                 370.0 |   177 (47.8%) |            0.24 | 0.21 | 0.37 | 0.60 | 0.69 |    456 (123.2%) |   193 (52.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-isabella-lower-twin |          00) original | Sat Sep 17 08:56:00 UTC 2022 | Sun Sep 18 08:37:00 UTC 2022 (+24hr) |                 167.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  167 (100.0%) |        0 (-) |         0 (-) |
  ;;|  id-isabella-lower-twin | 01) crowning disabled | Sat Sep 17 08:56:00 UTC 2022 | Sun Sep 18 08:37:00 UTC 2022 (+24hr) |                 167.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  167 (100.0%) |        0 (-) |         0 (-) |
  ;;|  id-isabella-lower-twin |          00) original | Sun Sep 18 08:37:00 UTC 2022 | Mon Sep 19 10:49:00 UTC 2022 (+26hr) |                 595.0 |    88 (14.8%) |            0.16 | 0.02 | 0.25 | 0.58 | 0.67 |    986 (165.7%) |   507 (85.2%) |     3 (3.4%) |   357 (36.2%) |
  ;;|  id-isabella-lower-twin | 01) crowning disabled | Sun Sep 18 08:37:00 UTC 2022 | Mon Sep 19 10:49:00 UTC 2022 (+26hr) |                 595.0 |    88 (14.8%) |            0.17 | 0.02 | 0.27 | 0.58 | 0.70 |     372 (62.5%) |   507 (85.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-isabella-lower-twin |          00) original | Mon Sep 19 10:49:00 UTC 2022 | Mon Sep 19 21:25:00 UTC 2022 (+11hr) |                 466.0 |   348 (74.7%) |            0.22 | 0.08 | 0.29 | 0.66 | 0.82 |     186 (39.9%) |   118 (25.3%) |    11 (3.2%) |    33 (17.7%) |
  ;;|  id-isabella-lower-twin | 01) crowning disabled | Mon Sep 19 10:49:00 UTC 2022 | Mon Sep 19 21:25:00 UTC 2022 (+11hr) |                 466.0 |   348 (74.7%) |            0.22 | 0.08 | 0.29 | 0.66 | 0.82 |     168 (36.1%) |   118 (25.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-isabella-lower-twin |          00) original | Mon Sep 19 21:25:00 UTC 2022 | Tue Sep 20 08:52:00 UTC 2022 (+11hr) |                 466.0 |   363 (77.9%) |            0.18 | 0.04 | 0.25 | 0.56 | 0.66 |    511 (109.7%) |   103 (22.1%) |    31 (8.5%) |   138 (27.0%) |
  ;;|  id-isabella-lower-twin | 01) crowning disabled | Mon Sep 19 21:25:00 UTC 2022 | Tue Sep 20 08:52:00 UTC 2022 (+11hr) |                 466.0 |   351 (75.3%) |            0.15 | 0.04 | 0.22 | 0.48 | 0.62 |     259 (55.6%) |   115 (24.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-isabella-lower-twin |          00) original | Tue Sep 20 08:52:00 UTC 2022 | Tue Sep 20 22:43:00 UTC 2022 (+14hr) |                 692.0 |   381 (55.1%) |            0.24 | 0.06 | 0.34 | 0.68 | 0.86 |    756 (109.2%) |   311 (44.9%) |    24 (6.3%) |   265 (35.1%) |
  ;;|  id-isabella-lower-twin | 01) crowning disabled | Tue Sep 20 08:52:00 UTC 2022 | Tue Sep 20 22:43:00 UTC 2022 (+14hr) |                 692.0 |   372 (53.8%) |            0.22 | 0.06 | 0.28 | 0.66 | 0.75 |     353 (51.0%) |   320 (46.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-isabella-lower-twin |          00) original | Tue Sep 20 22:43:00 UTC 2022 | Wed Sep 21 09:22:00 UTC 2022 (+11hr) |                 845.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  845 (100.0%) |        0 (-) |         0 (-) |
  ;;|  id-isabella-lower-twin | 01) crowning disabled | Tue Sep 20 22:43:00 UTC 2022 | Wed Sep 21 09:22:00 UTC 2022 (+11hr) |                 845.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  845 (100.0%) |        0 (-) |         0 (-) |
  ;;|                id-katka |          00) original | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                 155.0 |   128 (82.6%) |            0.14 | 0.05 | 0.26 | 0.47 | 0.57 |    615 (396.8%) |    27 (17.4%) |   26 (20.3%) |    62 (10.1%) |
  ;;|                id-katka | 01) crowning disabled | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                 155.0 |   128 (82.6%) |            0.22 | 0.07 | 0.37 | 0.63 | 0.69 |    519 (334.8%) |    27 (17.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|                id-katka |          00) original | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:12:00 UTC 2022 (+40hr) |                  12.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   12 (100.0%) |        0 (-) |         0 (-) |
  ;;|                id-katka | 01) crowning disabled | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:12:00 UTC 2022 (+40hr) |                  12.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   12 (100.0%) |        0 (-) |         0 (-) |
  ;;|  id-kootenai-rv-complex |          00) original | Tue Sep 13 09:22:00 UTC 2022 | Wed Sep 14 09:05:00 UTC 2022 (+24hr) |                2661.0 |  1229 (46.2%) |            0.36 | 0.31 | 0.62 | 0.84 | 0.91 |   3126 (117.5%) |  1432 (53.8%) |  165 (13.4%) |   948 (30.3%) |
  ;;|  id-kootenai-rv-complex | 01) crowning disabled | Tue Sep 13 09:22:00 UTC 2022 | Wed Sep 14 09:05:00 UTC 2022 (+24hr) |                2661.0 |  1151 (43.3%) |            0.40 | 0.41 | 0.69 | 0.85 | 0.91 |    1127 (42.4%) |  1510 (56.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-kootenai-rv-complex |          00) original | Wed Sep 14 09:05:00 UTC 2022 | Wed Sep 14 12:19:00 UTC 2022 (+ 3hr) |                3419.0 |  1469 (43.0%) |            0.12 | 0.03 | 0.03 | 0.44 | 0.68 |      299 (8.7%) |  1950 (57.0%) |    92 (6.3%) |   207 (69.2%) |
  ;;|  id-kootenai-rv-complex | 01) crowning disabled | Wed Sep 14 09:05:00 UTC 2022 | Wed Sep 14 12:19:00 UTC 2022 (+ 3hr) |                3419.0 |  1447 (42.3%) |            0.10 | 0.03 | 0.03 | 0.36 | 0.58 |      119 (3.5%) |  1972 (57.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-kootenai-rv-complex |          00) original | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                3567.0 |  2664 (74.7%) |            0.12 | 0.01 | 0.06 | 0.55 | 0.69 |  10064 (282.1%) |   903 (25.3%) |   127 (4.8%) |  2350 (23.4%) |
  ;;|  id-kootenai-rv-complex | 01) crowning disabled | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                3567.0 |  2636 (73.9%) |            0.12 | 0.01 | 0.06 | 0.52 | 0.70 |   7647 (214.4%) |   931 (26.1%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-kootenai-rv-complex |          00) original | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:04:00 UTC 2022 (+40hr) |                 488.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  488 (100.0%) |        0 (-) |         0 (-) |
  ;;|  id-kootenai-rv-complex | 01) crowning disabled | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:04:00 UTC 2022 (+40hr) |                 488.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  488 (100.0%) |        0 (-) |         0 (-) |
  ;;|  id-kootenai-rv-complex |          00) original | Mon Sep 26 10:17:00 UTC 2022 | Tue Sep 27 10:47:00 UTC 2022 (+25hr) |                1154.0 |   546 (47.3%) |            0.10 | 0.01 | 0.09 | 0.34 | 0.65 |     268 (23.2%) |   608 (52.7%) |    14 (2.6%) |    44 (16.4%) |
  ;;|  id-kootenai-rv-complex | 01) crowning disabled | Mon Sep 26 10:17:00 UTC 2022 | Tue Sep 27 10:47:00 UTC 2022 (+25hr) |                1154.0 |   546 (47.3%) |            0.12 | 0.01 | 0.15 | 0.42 | 0.72 |     184 (15.9%) |   608 (52.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-kootenai-rv-complex |          00) original | Tue Sep 27 10:47:00 UTC 2022 | Wed Sep 28 03:16:00 UTC 2022 (+16hr) |                1542.0 |    102 (6.6%) |            0.10 | 0.05 | 0.15 | 0.25 | 0.30 |      139 (9.0%) |  1440 (93.4%) |   13 (12.7%) |      8 (5.8%) |
  ;;|  id-kootenai-rv-complex | 01) crowning disabled | Tue Sep 27 10:47:00 UTC 2022 | Wed Sep 28 03:16:00 UTC 2022 (+16hr) |                1542.0 |    102 (6.6%) |            0.12 | 0.05 | 0.20 | 0.28 | 0.31 |       98 (6.4%) |  1440 (93.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-kootenai-rv-complex |          00) original | Wed Sep 28 03:16:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+ 6hr) |                2164.0 |  1331 (61.5%) |            0.22 | 0.16 | 0.30 | 0.56 | 0.73 |    1078 (49.8%) |   833 (38.5%) |  286 (21.5%) |   610 (56.6%) |
  ;;|  id-kootenai-rv-complex | 01) crowning disabled | Wed Sep 28 03:16:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+ 6hr) |                2164.0 |  1250 (57.8%) |            0.32 | 0.18 | 0.58 | 0.80 | 0.87 |     252 (11.6%) |   914 (42.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-kootenai-rv-complex |          00) original | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |                4232.0 |  2095 (49.5%) |            0.27 | 0.18 | 0.39 | 0.74 | 0.86 |    1865 (44.1%) |  2137 (50.5%) |  333 (15.9%) |   733 (39.3%) |
  ;;|  id-kootenai-rv-complex | 01) crowning disabled | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |                4232.0 |  2020 (47.7%) |            0.31 | 0.23 | 0.48 | 0.75 | 0.87 |     945 (22.3%) |  2212 (52.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-kootenai-rv-complex |          00) original | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 02:03:00 UTC 2022 (+ 6hr) |                5185.0 |  3807 (73.4%) |            0.19 | 0.05 | 0.27 | 0.60 | 0.78 |    3121 (60.2%) |  1378 (26.6%) |   312 (8.2%) |  1471 (47.1%) |
  ;;|  id-kootenai-rv-complex | 01) crowning disabled | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 02:03:00 UTC 2022 (+ 6hr) |                5185.0 |  3747 (72.3%) |            0.20 | 0.05 | 0.29 | 0.64 | 0.78 |    1221 (23.5%) |  1438 (27.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|                id-lemhi |          00) original | Sun Sep 25 21:12:00 UTC 2022 | Mon Sep 26 10:19:00 UTC 2022 (+13hr) |                1791.0 |   964 (53.8%) |            0.48 | 0.48 | 0.74 | 0.89 | 0.94 |     476 (26.6%) |   827 (46.2%) |    25 (2.6%) |   126 (26.5%) |
  ;;|                id-lemhi | 01) crowning disabled | Sun Sep 25 21:12:00 UTC 2022 | Mon Sep 26 10:19:00 UTC 2022 (+13hr) |                1791.0 |   956 (53.4%) |            0.48 | 0.49 | 0.73 | 0.89 | 0.94 |     224 (12.5%) |   835 (46.6%) |     0 (0.0%) |      0 (0.0%) |
  ;;|                id-lemhi |          00) original | Mon Sep 26 10:19:00 UTC 2022 | Tue Sep 27 09:09:00 UTC 2022 (+23hr) |                2002.0 |  1894 (94.6%) |            0.07 | 0.03 | 0.08 | 0.20 | 0.25 | 21625 (1080.2%) |    108 (5.4%) |    39 (2.1%) |  4690 (21.7%) |
  ;;|                id-lemhi | 01) crowning disabled | Mon Sep 26 10:19:00 UTC 2022 | Tue Sep 27 09:09:00 UTC 2022 (+23hr) |                2002.0 |  1894 (94.6%) |            0.07 | 0.03 | 0.10 | 0.21 | 0.27 |  14950 (746.8%) |    108 (5.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|                id-lemhi |          00) original | Tue Sep 27 09:09:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+24hr) |                1862.0 |  1765 (94.8%) |            0.03 | 0.01 | 0.03 | 0.09 | 0.11 | 22499 (1208.3%) |     97 (5.2%) |    13 (0.7%) |  4787 (21.3%) |
  ;;|                id-lemhi | 01) crowning disabled | Tue Sep 27 09:09:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+24hr) |                1862.0 |  1765 (94.8%) |            0.03 | 0.01 | 0.03 | 0.09 | 0.12 |  16004 (859.5%) |     97 (5.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;|                id-lemhi |          00) original | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |                1001.0 |   743 (74.2%) |            0.44 | 0.52 | 0.63 | 0.74 | 0.77 |   2854 (285.1%) |   258 (25.8%) |    44 (5.9%) |    252 (8.8%) |
  ;;|                id-lemhi | 01) crowning disabled | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |                1001.0 |   718 (71.7%) |            0.43 | 0.51 | 0.61 | 0.72 | 0.78 |   2485 (248.3%) |   283 (28.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;|                id-lemhi |          00) original | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+14hr) |                3684.0 |  3600 (97.7%) |            0.21 | 0.20 | 0.28 | 0.36 | 0.42 |  12302 (333.9%) |     84 (2.3%) |  732 (20.3%) |  2864 (23.3%) |
  ;;|                id-lemhi | 01) crowning disabled | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+14hr) |                3684.0 |  3219 (87.4%) |            0.28 | 0.23 | 0.38 | 0.60 | 0.76 |   7717 (209.5%) |   465 (12.6%) |     0 (0.0%) |      0 (0.0%) |
  ;;| id-lynx-meadows-3-prong |          00) original | Tue Sep 13 09:22:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+25hr) |                2145.0 |  2049 (95.5%) |            0.06 | 0.01 | 0.06 | 0.14 | 0.22 |   6217 (289.8%) |     96 (4.5%) |    92 (4.5%) |  1286 (20.7%) |
  ;;| id-lynx-meadows-3-prong | 01) crowning disabled | Tue Sep 13 09:22:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+25hr) |                2145.0 |  2045 (95.3%) |            0.06 | 0.01 | 0.07 | 0.14 | 0.22 |   4097 (191.0%) |    100 (4.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|  id-patrol-point-dismal |          00) original | Tue Sep 13 09:22:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+25hr) |                9483.0 |  8438 (89.0%) |            0.05 | 0.01 | 0.04 | 0.09 | 0.13 |  41354 (436.1%) |  1045 (11.0%) |   369 (4.4%) |  7063 (17.1%) |
  ;;|  id-patrol-point-dismal | 01) crowning disabled | Tue Sep 13 09:22:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+25hr) |                9483.0 |  8424 (88.8%) |            0.04 | 0.01 | 0.05 | 0.09 | 0.13 |  32345 (341.1%) |  1059 (11.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;|              id-ross-fk |          00) original | Tue Sep 13 09:24:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+25hr) |                7284.0 |  2012 (27.6%) |            0.10 | 0.02 | 0.05 | 0.47 | 0.69 |    2695 (37.0%) |  5272 (72.4%) |   106 (5.3%) |   353 (13.1%) |
  ;;|              id-ross-fk | 01) crowning disabled | Tue Sep 13 09:24:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+25hr) |                7284.0 |  2004 (27.5%) |            0.10 | 0.02 | 0.05 | 0.44 | 0.69 |    1936 (26.6%) |  5280 (72.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|              id-tenmile |          00) original | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                 289.0 |   244 (84.4%) |            0.11 | 0.04 | 0.09 | 0.28 | 0.45 |   1041 (360.2%) |    45 (15.6%) |     5 (2.0%) |     26 (2.5%) |
  ;;|              id-tenmile | 01) crowning disabled | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                 289.0 |   244 (84.4%) |            0.11 | 0.04 | 0.10 | 0.30 | 0.45 |    971 (336.0%) |    45 (15.6%) |     0 (0.0%) |      0 (0.0%) |
  ;;|              id-tenmile |          00) original | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |                 332.0 |   240 (72.3%) |            0.16 | 0.07 | 0.18 | 0.37 | 0.59 |    367 (110.5%) |    92 (27.7%) |     5 (2.1%) |     13 (3.5%) |
  ;;|              id-tenmile | 01) crowning disabled | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |                 332.0 |   239 (72.0%) |            0.16 | 0.07 | 0.19 | 0.41 | 0.57 |    349 (105.1%) |    93 (28.0%) |     0 (0.0%) |      0 (0.0%) |
  ;;|              id-tenmile |          00) original | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 08:33:00 UTC 2022 (+12hr) |                 160.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  160 (100.0%) |        0 (-) |         0 (-) |
  ;;|              id-tenmile | 01) crowning disabled | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 08:33:00 UTC 2022 (+12hr) |                 160.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  160 (100.0%) |        0 (-) |         0 (-) |
  ;;|          id-trail-ridge |          00) original | Sat Sep 17 10:23:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+10hr) |                   2.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    2 (100.0%) |        0 (-) |         0 (-) |
  ;;|          id-trail-ridge | 01) crowning disabled | Sat Sep 17 10:23:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+10hr) |                   2.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    2 (100.0%) |        0 (-) |         0 (-) |
  ;;|          id-trail-ridge |          00) original | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 08:37:00 UTC 2022 (+12hr) |                   2.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    2 (100.0%) |        0 (-) |         0 (-) |
  ;;|          id-trail-ridge | 01) crowning disabled | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 08:37:00 UTC 2022 (+12hr) |                   2.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    2 (100.0%) |        0 (-) |         0 (-) |
  ;;|             mt-billiard |          00) original | Tue Sep 13 09:22:00 UTC 2022 | Wed Sep 14 09:52:00 UTC 2022 (+25hr) |                 837.0 |   606 (72.4%) |            0.22 | 0.07 | 0.27 | 0.85 | 0.92 |   2915 (348.3%) |   231 (27.6%) |  100 (16.5%) |  1225 (42.0%) |
  ;;|             mt-billiard | 01) crowning disabled | Tue Sep 13 09:22:00 UTC 2022 | Wed Sep 14 09:52:00 UTC 2022 (+25hr) |                 837.0 |   572 (68.3%) |            0.17 | 0.12 | 0.24 | 0.40 | 0.61 |    908 (108.5%) |   265 (31.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             mt-billiard |          00) original | Wed Sep 14 09:52:00 UTC 2022 | Fri Sep 16 11:11:00 UTC 2022 (+49hr) |                 356.0 |   198 (55.6%) |            0.15 | 0.02 | 0.09 | 0.71 | 0.80 |   1554 (436.5%) |   158 (44.4%) |   28 (14.1%) |   296 (19.0%) |
  ;;|             mt-billiard | 01) crowning disabled | Wed Sep 14 09:52:00 UTC 2022 | Fri Sep 16 11:11:00 UTC 2022 (+49hr) |                 356.0 |   192 (53.9%) |            0.12 | 0.02 | 0.08 | 0.66 | 0.75 |   1238 (347.8%) |   164 (46.1%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               mt-cannon |          00) original | Wed Sep 14 09:52:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                 120.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  120 (100.0%) |        0 (-) |         0 (-) |
  ;;|               mt-cannon | 01) crowning disabled | Wed Sep 14 09:52:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                 120.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  120 (100.0%) |        0 (-) |         0 (-) |
  ;;|               mt-cannon |          00) original | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+59hr) |                 141.0 |   132 (93.6%) |            0.09 | 0.02 | 0.12 | 0.20 | 0.54 |  2120 (1503.5%) |      9 (6.4%) |     4 (3.0%) |    182 (8.6%) |
  ;;|               mt-cannon | 01) crowning disabled | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+59hr) |                 141.0 |   132 (93.6%) |            0.09 | 0.02 | 0.12 | 0.20 | 0.55 |  1757 (1246.1%) |      9 (6.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               mt-cannon |          00) original | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 08:37:00 UTC 2022 (+12hr) |                  37.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   37 (100.0%) |        0 (-) |         0 (-) |
  ;;|               mt-cannon | 01) crowning disabled | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 08:37:00 UTC 2022 (+12hr) |                  37.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   37 (100.0%) |        0 (-) |         0 (-) |
  ;;|          mt-george-lake |          00) original | Tue Sep 13 09:22:00 UTC 2022 | Thu Sep 15 11:09:00 UTC 2022 (+50hr) |                1349.0 |   766 (56.8%) |            0.09 | 0.04 | 0.11 | 0.24 | 0.30 |   1395 (103.4%) |   583 (43.2%) |    71 (9.3%) |   172 (12.3%) |
  ;;|          mt-george-lake | 01) crowning disabled | Tue Sep 13 09:22:00 UTC 2022 | Thu Sep 15 11:09:00 UTC 2022 (+50hr) |                1349.0 |   762 (56.5%) |            0.14 | 0.07 | 0.25 | 0.40 | 0.46 |     775 (57.4%) |   587 (43.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           mt-government |          00) original | Tue Sep 13 09:22:00 UTC 2022 | Wed Sep 14 09:05:00 UTC 2022 (+24hr) |                2474.0 |  2375 (96.0%) |            0.29 | 0.15 | 0.52 | 0.72 | 0.81 |  14175 (573.0%) |     99 (4.0%) |  415 (17.5%) |  6281 (44.3%) |
  ;;|           mt-government | 01) crowning disabled | Tue Sep 13 09:22:00 UTC 2022 | Wed Sep 14 09:05:00 UTC 2022 (+24hr) |                2474.0 |  2235 (90.3%) |            0.29 | 0.14 | 0.52 | 0.81 | 0.90 |   3693 (149.3%) |    239 (9.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           mt-government |          00) original | Wed Sep 14 09:05:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                1137.0 |   787 (69.2%) |            0.16 | 0.02 | 0.16 | 0.71 | 0.82 |     810 (71.2%) |   350 (30.8%) |    70 (8.9%) |   167 (20.6%) |
  ;;|           mt-government | 01) crowning disabled | Wed Sep 14 09:05:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                1137.0 |   777 (68.3%) |            0.16 | 0.04 | 0.18 | 0.67 | 0.77 |     553 (48.6%) |   360 (31.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           mt-government |          00) original | Thu Sep 15 09:34:00 UTC 2022 | Fri Sep 16 11:01:00 UTC 2022 (+25hr) |                 897.0 |   740 (82.5%) |            0.12 | 0.02 | 0.14 | 0.38 | 0.50 |   1033 (115.2%) |   157 (17.5%) |    29 (3.9%) |   170 (16.5%) |
  ;;|           mt-government | 01) crowning disabled | Thu Sep 15 09:34:00 UTC 2022 | Fri Sep 16 11:01:00 UTC 2022 (+25hr) |                 897.0 |   740 (82.5%) |            0.13 | 0.02 | 0.15 | 0.38 | 0.50 |     874 (97.4%) |   157 (17.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           mt-government |          00) original | Fri Sep 16 11:01:00 UTC 2022 | Sat Sep 17 11:20:00 UTC 2022 (+24hr) |                  89.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   89 (100.0%) |        0 (-) |         0 (-) |
  ;;|           mt-government | 01) crowning disabled | Fri Sep 16 11:01:00 UTC 2022 | Sat Sep 17 11:20:00 UTC 2022 (+24hr) |                  89.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   89 (100.0%) |        0 (-) |         0 (-) |
  ;;|           mt-government |          00) original | Sat Sep 17 11:20:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+ 9hr) |                  96.0 |    60 (62.5%) |            0.09 | 0.04 | 0.10 | 0.27 | 0.45 |      63 (65.6%) |    36 (37.5%) |   18 (30.0%) |     7 (11.1%) |
  ;;|           mt-government | 01) crowning disabled | Sat Sep 17 11:20:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+ 9hr) |                  96.0 |    60 (62.5%) |            0.14 | 0.04 | 0.27 | 0.47 | 0.53 |      40 (41.7%) |    36 (37.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           mt-government |          00) original | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 09:28:00 UTC 2022 (+13hr) |                  96.0 |    63 (65.6%) |            0.14 | 0.03 | 0.07 | 0.74 | 0.97 |    147 (153.1%) |    33 (34.4%) |   21 (33.3%) |    32 (21.8%) |
  ;;|           mt-government | 01) crowning disabled | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 09:28:00 UTC 2022 (+13hr) |                  96.0 |    60 (62.5%) |            0.09 | 0.03 | 0.17 | 0.27 | 0.32 |      86 (89.6%) |    36 (37.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|        mt-isabella-lake |          00) original | Wed Sep 14 03:40:00 UTC 2022 | Fri Sep 16 11:26:00 UTC 2022 (+56hr) |                   4.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    4 (100.0%) |        0 (-) |         0 (-) |
  ;;|        mt-isabella-lake | 01) crowning disabled | Wed Sep 14 03:40:00 UTC 2022 | Fri Sep 16 11:26:00 UTC 2022 (+56hr) |                   4.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    4 (100.0%) |        0 (-) |         0 (-) |
  ;;|             mt-margaret |          00) original | Wed Sep 14 09:52:00 UTC 2022 | Thu Sep 15 08:43:00 UTC 2022 (+23hr) |                 519.0 |   446 (85.9%) |            0.38 | 0.36 | 0.62 | 0.82 | 0.87 |     180 (34.7%) |    73 (14.1%) |    37 (8.3%) |    33 (18.3%) |
  ;;|             mt-margaret | 01) crowning disabled | Wed Sep 14 09:52:00 UTC 2022 | Thu Sep 15 08:43:00 UTC 2022 (+23hr) |                 519.0 |   426 (82.1%) |            0.37 | 0.34 | 0.62 | 0.79 | 0.89 |     104 (20.0%) |    93 (17.9%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             mt-margaret |          00) original | Thu Sep 15 08:43:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+49hr) |                 679.0 |   661 (97.3%) |            0.25 | 0.18 | 0.41 | 0.69 | 0.79 |   2875 (423.4%) |     18 (2.7%) |    17 (2.6%) |   345 (12.0%) |
  ;;|             mt-margaret | 01) crowning disabled | Thu Sep 15 08:43:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+49hr) |                 679.0 |   661 (97.3%) |            0.25 | 0.18 | 0.41 | 0.69 | 0.79 |   2192 (322.8%) |     18 (2.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             mt-margaret |          00) original | Sat Sep 17 09:47:00 UTC 2022 | Sun Sep 18 09:28:00 UTC 2022 (+24hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |
  ;;|             mt-margaret | 01) crowning disabled | Sat Sep 17 09:47:00 UTC 2022 | Sun Sep 18 09:28:00 UTC 2022 (+24hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |
  ;;|            mt-mill-lake |          00) original | Sat Sep 17 10:50:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+10hr) |                 871.0 |   138 (15.8%) |            0.20 | 0.09 | 0.14 | 0.50 | 0.93 |       16 (1.8%) |   733 (84.2%) |   14 (10.1%) |     6 (37.5%) |
  ;;|            mt-mill-lake | 01) crowning disabled | Sat Sep 17 10:50:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+10hr) |                 871.0 |   136 (15.6%) |            0.18 | 0.09 | 0.09 | 0.44 | 0.69 |       15 (1.7%) |   735 (84.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|          or-cedar-creek |          00) original | Wed Sep 14 03:20:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+ 7hr) |               19163.0 |  8654 (45.2%) |            0.16 | 0.05 | 0.05 | 0.58 | 0.75 |     1085 (5.7%) | 10509 (54.8%) |    15 (0.2%) |     34 (3.1%) |
  ;;|          or-cedar-creek | 01) crowning disabled | Wed Sep 14 03:20:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+ 7hr) |               19163.0 |  8654 (45.2%) |            0.16 | 0.05 | 0.05 | 0.58 | 0.75 |     1061 (5.5%) | 10509 (54.8%) |     0 (0.0%) |      0 (0.0%) |
  ;;|          or-cedar-creek |          00) original | Wed Sep 14 09:54:00 UTC 2022 | Wed Sep 14 21:18:00 UTC 2022 (+11hr) |               17674.0 |  6824 (38.6%) |            0.26 | 0.08 | 0.44 | 0.74 | 0.85 |    2672 (15.1%) | 10850 (61.4%) |    17 (0.2%) |     94 (3.5%) |
  ;;|          or-cedar-creek | 01) crowning disabled | Wed Sep 14 09:54:00 UTC 2022 | Wed Sep 14 21:18:00 UTC 2022 (+11hr) |               17674.0 |  6819 (38.6%) |            0.26 | 0.08 | 0.43 | 0.73 | 0.85 |    2607 (14.8%) | 10855 (61.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|          or-cedar-creek |          00) original | Wed Sep 14 21:18:00 UTC 2022 | Thu Sep 15 09:37:00 UTC 2022 (+12hr) |               11352.0 |  1884 (16.6%) |            0.22 | 0.09 | 0.35 | 0.64 | 0.77 |      848 (7.5%) |  9468 (83.4%) |    14 (0.7%) |      9 (1.1%) |
  ;;|          or-cedar-creek | 01) crowning disabled | Wed Sep 14 21:18:00 UTC 2022 | Thu Sep 15 09:37:00 UTC 2022 (+12hr) |               11352.0 |  1881 (16.6%) |            0.23 | 0.10 | 0.38 | 0.64 | 0.79 |      781 (6.9%) |  9471 (83.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|          or-cedar-creek |          00) original | Thu Sep 15 09:37:00 UTC 2022 | Fri Sep 16 09:15:00 UTC 2022 (+24hr) |               12849.0 |  4380 (34.1%) |            0.31 | 0.24 | 0.53 | 0.77 | 0.88 |    3099 (24.1%) |  8469 (65.9%) |    10 (0.2%) |     51 (1.6%) |
  ;;|          or-cedar-creek | 01) crowning disabled | Thu Sep 15 09:37:00 UTC 2022 | Fri Sep 16 09:15:00 UTC 2022 (+24hr) |               12849.0 |  4371 (34.0%) |            0.31 | 0.24 | 0.52 | 0.76 | 0.88 |    2954 (23.0%) |  8478 (66.0%) |     0 (0.0%) |      0 (0.0%) |
  ;;|          or-cedar-creek |          00) original | Fri Sep 16 09:15:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+24hr) |               19045.0 |  6552 (34.4%) |            0.33 | 0.26 | 0.60 | 0.81 | 0.89 |    3173 (16.7%) | 12493 (65.6%) |    23 (0.4%) |     21 (0.7%) |
  ;;|          or-cedar-creek | 01) crowning disabled | Fri Sep 16 09:15:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+24hr) |               19045.0 |  6526 (34.3%) |            0.33 | 0.25 | 0.60 | 0.80 | 0.89 |    3124 (16.4%) | 12519 (65.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|          or-cedar-creek |          00) original | Sat Sep 17 08:58:00 UTC 2022 | Sat Sep 17 21:14:00 UTC 2022 (+12hr) |               29804.0 |  8638 (29.0%) |            0.39 | 0.31 | 0.68 | 0.88 | 0.94 |     1720 (5.8%) | 21166 (71.0%) |    27 (0.3%) |    129 (7.5%) |
  ;;|          or-cedar-creek | 01) crowning disabled | Sat Sep 17 08:58:00 UTC 2022 | Sat Sep 17 21:14:00 UTC 2022 (+12hr) |               29804.0 |  8608 (28.9%) |            0.39 | 0.31 | 0.68 | 0.88 | 0.94 |     1671 (5.6%) | 21196 (71.1%) |     0 (0.0%) |      0 (0.0%) |
  ;;|          or-cedar-creek |          00) original | Sat Sep 17 21:14:00 UTC 2022 | Mon Sep 19 10:00:00 UTC 2022 (+37hr) |               27170.0 | 20244 (74.5%) |            0.06 | 0.01 | 0.07 | 0.15 | 0.32 |   17592 (64.7%) |  6926 (25.5%) |    25 (0.1%) |    415 (2.4%) |
  ;;|          or-cedar-creek | 01) crowning disabled | Sat Sep 17 21:14:00 UTC 2022 | Mon Sep 19 10:00:00 UTC 2022 (+37hr) |               27170.0 | 20244 (74.5%) |            0.06 | 0.01 | 0.07 | 0.15 | 0.32 |   17268 (63.6%) |  6926 (25.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|          or-cedar-creek |          00) original | Sun Sep 25 22:03:00 UTC 2022 | Mon Sep 26 11:10:00 UTC 2022 (+13hr) |                8248.0 |    404 (4.9%) |            0.38 | 0.33 | 0.64 | 0.85 | 0.95 |       73 (0.9%) |  7844 (95.1%) |     0 (0.0%) |      0 (0.0%) |
  ;;|          or-cedar-creek | 01) crowning disabled | Sun Sep 25 22:03:00 UTC 2022 | Mon Sep 26 11:10:00 UTC 2022 (+13hr) |                8248.0 |    404 (4.9%) |            0.38 | 0.33 | 0.64 | 0.85 | 0.95 |       73 (0.9%) |  7844 (95.1%) |     0 (0.0%) |      0 (0.0%) |
  ;;|          or-cedar-creek |          00) original | Mon Sep 26 11:10:00 UTC 2022 | Tue Sep 27 09:09:00 UTC 2022 (+22hr) |                9631.0 |   962 (10.0%) |            0.07 | 0.01 | 0.11 | 0.19 | 0.34 |    1785 (18.5%) |  8669 (90.0%) |    10 (1.0%) |    119 (6.7%) |
  ;;|          or-cedar-creek | 01) crowning disabled | Mon Sep 26 11:10:00 UTC 2022 | Tue Sep 27 09:09:00 UTC 2022 (+22hr) |                9631.0 |   962 (10.0%) |            0.07 | 0.01 | 0.11 | 0.19 | 0.34 |    1745 (18.1%) |  8669 (90.0%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         or-double-creek |          00) original | Wed Sep 14 09:05:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |               12734.0 |  6434 (50.5%) |            0.29 | 0.20 | 0.53 | 0.72 | 0.84 |    5416 (42.5%) |  6300 (49.5%) |    48 (0.7%) |    150 (2.8%) |
  ;;|         or-double-creek | 01) crowning disabled | Wed Sep 14 09:05:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |               12734.0 |  6400 (50.3%) |            0.29 | 0.19 | 0.53 | 0.73 | 0.85 |    5176 (40.6%) |  6334 (49.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         or-double-creek |          00) original | Thu Sep 15 09:34:00 UTC 2022 | Fri Sep 16 00:06:00 UTC 2022 (+15hr) |                7910.0 |  5413 (68.4%) |            0.18 | 0.04 | 0.27 | 0.57 | 0.66 |    3754 (47.5%) |  2497 (31.6%) |    14 (0.3%) |    156 (4.2%) |
  ;;|         or-double-creek | 01) crowning disabled | Thu Sep 15 09:34:00 UTC 2022 | Fri Sep 16 00:06:00 UTC 2022 (+15hr) |                7910.0 |  5413 (68.4%) |            0.18 | 0.04 | 0.28 | 0.58 | 0.67 |    3554 (44.9%) |  2497 (31.6%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         or-double-creek |          00) original | Fri Sep 16 00:06:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+34hr) |                2263.0 |   566 (25.0%) |            0.10 | 0.02 | 0.12 | 0.22 | 0.53 |    1130 (49.9%) |  1697 (75.0%) |     6 (1.1%) |     13 (1.2%) |
  ;;|         or-double-creek | 01) crowning disabled | Fri Sep 16 00:06:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+34hr) |                2263.0 |   566 (25.0%) |            0.10 | 0.02 | 0.13 | 0.24 | 0.54 |    1113 (49.2%) |  1697 (75.0%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         or-double-creek |          00) original | Sat Sep 17 09:47:00 UTC 2022 | Sat Sep 17 21:14:00 UTC 2022 (+11hr) |                1808.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1808 (100.0%) |        0 (-) |         0 (-) |
  ;;|         or-double-creek | 01) crowning disabled | Sat Sep 17 09:47:00 UTC 2022 | Sat Sep 17 21:14:00 UTC 2022 (+11hr) |                1808.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1808 (100.0%) |        0 (-) |         0 (-) |
  ;;|         or-double-creek |          00) original | Sat Sep 17 21:14:00 UTC 2022 | Mon Sep 19 09:09:00 UTC 2022 (+36hr) |                1785.0 |     75 (4.2%) |            0.04 | 0.01 | 0.04 | 0.15 | 0.21 |    1170 (65.5%) |  1710 (95.8%) |     4 (5.3%) |     15 (1.3%) |
  ;;|         or-double-creek | 01) crowning disabled | Sat Sep 17 21:14:00 UTC 2022 | Mon Sep 19 09:09:00 UTC 2022 (+36hr) |                1785.0 |     75 (4.2%) |            0.04 | 0.01 | 0.04 | 0.15 | 0.21 |    1160 (65.0%) |  1710 (95.8%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         or-double-creek |          00) original | Mon Sep 19 09:09:00 UTC 2022 | Tue Sep 20 08:52:00 UTC 2022 (+24hr) |                1883.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1883 (100.0%) |        0 (-) |         0 (-) |
  ;;|         or-double-creek | 01) crowning disabled | Mon Sep 19 09:09:00 UTC 2022 | Tue Sep 20 08:52:00 UTC 2022 (+24hr) |                1883.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1883 (100.0%) |        0 (-) |         0 (-) |
  ;;|         or-double-creek |          00) original | Tue Sep 20 08:52:00 UTC 2022 | Tue Sep 20 20:19:00 UTC 2022 (+11hr) |                1810.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1810 (100.0%) |        0 (-) |         0 (-) |
  ;;|         or-double-creek | 01) crowning disabled | Tue Sep 20 08:52:00 UTC 2022 | Tue Sep 20 20:19:00 UTC 2022 (+11hr) |                1810.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1810 (100.0%) |        0 (-) |         0 (-) |
  ;;|         or-double-creek |          00) original | Tue Sep 20 20:19:00 UTC 2022 | Wed Sep 21 09:22:00 UTC 2022 (+13hr) |                1873.0 |    132 (7.0%) |            0.09 | 0.07 | 0.15 | 0.20 | 0.22 |     208 (11.1%) |  1741 (93.0%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         or-double-creek | 01) crowning disabled | Tue Sep 20 20:19:00 UTC 2022 | Wed Sep 21 09:22:00 UTC 2022 (+13hr) |                1873.0 |    132 (7.0%) |            0.09 | 0.07 | 0.15 | 0.20 | 0.22 |     208 (11.1%) |  1741 (93.0%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         or-double-creek |          00) original | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                4588.0 |  1439 (31.4%) |            0.34 | 0.25 | 0.58 | 0.78 | 0.89 |    2635 (57.4%) |  3149 (68.6%) |   102 (7.1%) |   603 (22.9%) |
  ;;|         or-double-creek | 01) crowning disabled | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                4588.0 |  1341 (29.2%) |            0.38 | 0.36 | 0.61 | 0.82 | 0.91 |     935 (20.4%) |  3247 (70.8%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         or-double-creek |          00) original | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |               20434.0 |  3637 (17.8%) |            0.47 | 0.47 | 0.74 | 0.92 | 0.96 |       48 (0.2%) | 16797 (82.2%) |   296 (8.1%) |      0 (0.0%) |
  ;;|         or-double-creek | 01) crowning disabled | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |               20434.0 |  3063 (15.0%) |            0.44 | 0.43 | 0.72 | 0.90 | 0.96 |       45 (0.2%) | 17371 (85.0%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         or-double-creek |          00) original | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 08:33:00 UTC 2022 (+12hr) |               25821.0 | 19136 (74.1%) |            0.22 | 0.04 | 0.34 | 0.70 | 0.84 |     1878 (7.3%) |  6685 (25.9%) |   200 (1.0%) |   276 (14.7%) |
  ;;|         or-double-creek | 01) crowning disabled | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 08:33:00 UTC 2022 (+12hr) |               25821.0 | 19022 (73.7%) |            0.23 | 0.02 | 0.39 | 0.72 | 0.87 |     1028 (4.0%) |  6799 (26.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             or-sturgill |          00) original | Wed Sep 14 09:54:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                 884.0 |   203 (23.0%) |            0.08 | 0.04 | 0.04 | 0.22 | 0.29 |     118 (13.3%) |   681 (77.0%) |     4 (2.0%) |      7 (5.9%) |
  ;;|             or-sturgill | 01) crowning disabled | Wed Sep 14 09:54:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                 884.0 |   203 (23.0%) |            0.09 | 0.04 | 0.04 | 0.25 | 0.29 |     107 (12.1%) |   681 (77.0%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             or-sturgill |          00) original | Thu Sep 15 09:34:00 UTC 2022 | Fri Sep 16 10:09:00 UTC 2022 (+25hr) |                 728.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  728 (100.0%) |        0 (-) |         0 (-) |
  ;;|             or-sturgill | 01) crowning disabled | Thu Sep 15 09:34:00 UTC 2022 | Fri Sep 16 10:09:00 UTC 2022 (+25hr) |                 728.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  728 (100.0%) |        0 (-) |         0 (-) |
  ;;|             or-sturgill |          00) original | Fri Sep 16 10:09:00 UTC 2022 | Fri Sep 16 20:42:00 UTC 2022 (+11hr) |                 684.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  684 (100.0%) |        0 (-) |         0 (-) |
  ;;|             or-sturgill | 01) crowning disabled | Fri Sep 16 10:09:00 UTC 2022 | Fri Sep 16 20:42:00 UTC 2022 (+11hr) |                 684.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  684 (100.0%) |        0 (-) |         0 (-) |
  ;;|             or-sturgill |          00) original | Fri Sep 16 20:42:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+13hr) |                 921.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  921 (100.0%) |        0 (-) |         0 (-) |
  ;;|             or-sturgill | 01) crowning disabled | Fri Sep 16 20:42:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+13hr) |                 921.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  921 (100.0%) |        0 (-) |         0 (-) |
  ;;|             or-sturgill |          00) original | Sat Sep 17 09:47:00 UTC 2022 | Sat Sep 17 16:47:00 UTC 2022 (+ 7hr) |                1210.0 |   282 (23.3%) |            0.23 | 0.11 | 0.37 | 0.60 | 0.69 |      116 (9.6%) |   928 (76.7%) |     7 (2.5%) |    45 (38.8%) |
  ;;|             or-sturgill | 01) crowning disabled | Sat Sep 17 09:47:00 UTC 2022 | Sat Sep 17 16:47:00 UTC 2022 (+ 7hr) |                1210.0 |   282 (23.3%) |            0.23 | 0.11 | 0.37 | 0.60 | 0.69 |       91 (7.5%) |   928 (76.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             or-sturgill |          00) original | Sat Sep 17 16:47:00 UTC 2022 | Mon Sep 19 17:47:00 UTC 2022 (+49hr) |                1254.0 |   405 (32.3%) |            0.08 | 0.02 | 0.09 | 0.23 | 0.34 |   4686 (373.7%) |   849 (67.7%) |    33 (8.1%) |   901 (19.2%) |
  ;;|             or-sturgill | 01) crowning disabled | Sat Sep 17 16:47:00 UTC 2022 | Mon Sep 19 17:47:00 UTC 2022 (+49hr) |                1254.0 |   405 (32.3%) |            0.09 | 0.02 | 0.09 | 0.23 | 0.42 |   3029 (241.5%) |   849 (67.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             or-sturgill |          00) original | Mon Sep 19 17:47:00 UTC 2022 | Tue Sep 20 09:41:00 UTC 2022 (+16hr) |                 679.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  679 (100.0%) |        0 (-) |         0 (-) |
  ;;|             or-sturgill | 01) crowning disabled | Mon Sep 19 17:47:00 UTC 2022 | Tue Sep 20 09:41:00 UTC 2022 (+16hr) |                 679.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  679 (100.0%) |        0 (-) |         0 (-) |
  ;;|             or-sturgill |          00) original | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 16:04:00 UTC 2022 (+ 6hr) |                 731.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  731 (100.0%) |        0 (-) |         0 (-) |
  ;;|             or-sturgill | 01) crowning disabled | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 16:04:00 UTC 2022 (+ 6hr) |                 731.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  731 (100.0%) |        0 (-) |         0 (-) |
  ;;|             or-sturgill |          00) original | Tue Sep 20 16:04:00 UTC 2022 | Tue Sep 20 20:19:00 UTC 2022 (+ 4hr) |                 804.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  804 (100.0%) |        0 (-) |         0 (-) |
  ;;|             or-sturgill | 01) crowning disabled | Tue Sep 20 16:04:00 UTC 2022 | Tue Sep 20 20:19:00 UTC 2022 (+ 4hr) |                 804.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  804 (100.0%) |        0 (-) |         0 (-) |
  ;;|             or-sturgill |          00) original | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                6854.0 |  5510 (80.4%) |            0.34 | 0.31 | 0.56 | 0.67 | 0.76 |    5652 (82.5%) |  1344 (19.6%) |   343 (6.2%) |  1108 (19.6%) |
  ;;|             or-sturgill | 01) crowning disabled | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                6854.0 |  5147 (75.1%) |            0.34 | 0.27 | 0.54 | 0.78 | 0.87 |    2615 (38.2%) |  1707 (24.9%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             or-sturgill |          00) original | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |                6717.0 |  5065 (75.4%) |            0.29 | 0.17 | 0.43 | 0.73 | 0.88 |    2488 (37.0%) |  1652 (24.6%) |   139 (2.7%) |   697 (28.0%) |
  ;;|             or-sturgill | 01) crowning disabled | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |                6717.0 |  5015 (74.7%) |            0.29 | 0.18 | 0.45 | 0.71 | 0.85 |    1090 (16.2%) |  1702 (25.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             or-sturgill |          00) original | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 00:01:00 UTC 2022 (+ 4hr) |                4895.0 |  3891 (79.5%) |            0.21 | 0.08 | 0.27 | 0.59 | 0.72 |    1587 (32.4%) |  1004 (20.5%) |   162 (4.2%) |   442 (27.9%) |
  ;;|             or-sturgill | 01) crowning disabled | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 00:01:00 UTC 2022 (+ 4hr) |                4895.0 |  3830 (78.2%) |            0.24 | 0.08 | 0.42 | 0.70 | 0.78 |     813 (16.6%) |  1065 (21.8%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-bolt-creek |          00) original | Sun Sep 18 05:10:00 UTC 2022 | Mon Sep 19 10:49:00 UTC 2022 (+30hr) |                2555.0 |  1685 (65.9%) |            0.14 | 0.05 | 0.25 | 0.44 | 0.56 |   9922 (388.3%) |   870 (34.1%) |  308 (18.3%) |  4781 (48.2%) |
  ;;|           wa-bolt-creek | 01) crowning disabled | Sun Sep 18 05:10:00 UTC 2022 | Mon Sep 19 10:49:00 UTC 2022 (+30hr) |                2555.0 |  1479 (57.9%) |            0.15 | 0.01 | 0.19 | 0.54 | 0.73 |   2562 (100.3%) |  1076 (42.1%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-bolt-creek |          00) original | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:17:00 UTC 2022 (+11hr) |                1488.0 |   274 (18.4%) |            0.19 | 0.06 | 0.25 | 0.57 | 0.68 |    1162 (78.1%) |  1214 (81.6%) |   36 (13.1%) |   800 (68.8%) |
  ;;|           wa-bolt-creek | 01) crowning disabled | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:17:00 UTC 2022 (+11hr) |                1488.0 |   263 (17.7%) |            0.17 | 0.06 | 0.28 | 0.46 | 0.59 |       59 (4.0%) |  1225 (82.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-bolt-creek |          00) original | Tue Sep 20 20:17:00 UTC 2022 | Wed Sep 21 09:22:00 UTC 2022 (+13hr) |                1977.0 |  1439 (72.8%) |            0.15 | 0.12 | 0.20 | 0.34 | 0.51 |   5356 (270.9%) |   538 (27.2%) |  405 (28.1%) |  3389 (63.3%) |
  ;;|           wa-bolt-creek | 01) crowning disabled | Tue Sep 20 20:17:00 UTC 2022 | Wed Sep 21 09:22:00 UTC 2022 (+13hr) |                1977.0 |  1109 (56.1%) |            0.28 | 0.17 | 0.51 | 0.75 | 0.88 |     299 (15.1%) |   868 (43.9%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-bolt-creek |          00) original | Wed Sep 21 09:22:00 UTC 2022 | Thu Sep 22 09:54:00 UTC 2022 (+25hr) |                5246.0 |  2987 (56.9%) |            0.35 | 0.33 | 0.57 | 0.74 | 0.84 |   7962 (151.8%) |  2259 (43.1%) |  875 (29.3%) |  4624 (58.1%) |
  ;;|           wa-bolt-creek | 01) crowning disabled | Wed Sep 21 09:22:00 UTC 2022 | Thu Sep 22 09:54:00 UTC 2022 (+25hr) |                5246.0 |  1951 (37.2%) |            0.37 | 0.35 | 0.61 | 0.81 | 0.90 |      516 (9.8%) |  3295 (62.8%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-bolt-creek |          00) original | Thu Sep 22 09:54:00 UTC 2022 | Thu Sep 22 20:29:00 UTC 2022 (+11hr) |                4179.0 |  3326 (79.6%) |            0.22 | 0.09 | 0.23 | 0.66 | 0.81 |    2683 (64.2%) |   853 (20.4%) |   233 (7.0%) |  1836 (68.4%) |
  ;;|           wa-bolt-creek | 01) crowning disabled | Thu Sep 22 09:54:00 UTC 2022 | Thu Sep 22 20:29:00 UTC 2022 (+11hr) |                4179.0 |  3280 (78.5%) |            0.24 | 0.09 | 0.38 | 0.68 | 0.80 |     461 (11.0%) |   899 (21.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-bolt-creek |          00) original | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                4072.0 |  3401 (83.5%) |            0.10 | 0.01 | 0.06 | 0.37 | 0.62 |   6914 (169.8%) |   671 (16.5%) |   229 (6.7%) |  3803 (55.0%) |
  ;;|           wa-bolt-creek | 01) crowning disabled | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                4072.0 |  3367 (82.7%) |            0.10 | 0.01 | 0.09 | 0.33 | 0.60 |    2469 (60.6%) |   705 (17.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-bolt-creek |          00) original | Sat Sep 24 09:15:00 UTC 2022 | Sun Sep 25 09:45:00 UTC 2022 (+25hr) |                1292.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1292 (100.0%) |        0 (-) |         0 (-) |
  ;;|           wa-bolt-creek | 01) crowning disabled | Sat Sep 24 09:15:00 UTC 2022 | Sun Sep 25 09:45:00 UTC 2022 (+25hr) |                1292.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1292 (100.0%) |        0 (-) |         0 (-) |
  ;;|           wa-bolt-creek |          00) original | Sun Sep 25 09:45:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+24hr) |                1586.0 |   814 (51.3%) |            0.14 | 0.03 | 0.14 | 0.41 | 0.72 |   1744 (110.0%) |   772 (48.7%) |    55 (6.8%) |   411 (23.6%) |
  ;;|           wa-bolt-creek | 01) crowning disabled | Sun Sep 25 09:45:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+24hr) |                1586.0 |   811 (51.1%) |            0.15 | 0.03 | 0.19 | 0.41 | 0.65 |    1073 (67.7%) |   775 (48.9%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-bolt-creek |          00) original | Mon Sep 26 09:28:00 UTC 2022 | Tue Sep 27 09:07:00 UTC 2022 (+24hr) |                3373.0 |   998 (29.6%) |            0.31 | 0.25 | 0.50 | 0.68 | 0.81 |     791 (23.5%) |  2375 (70.4%) |    63 (6.3%) |   123 (15.5%) |
  ;;|           wa-bolt-creek | 01) crowning disabled | Mon Sep 26 09:28:00 UTC 2022 | Tue Sep 27 09:07:00 UTC 2022 (+24hr) |                3373.0 |   935 (27.7%) |            0.32 | 0.28 | 0.54 | 0.71 | 0.81 |     491 (14.6%) |  2438 (72.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-bolt-creek |          00) original | Tue Sep 27 09:07:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+24hr) |                4446.0 |  2711 (61.0%) |            0.18 | 0.09 | 0.28 | 0.51 | 0.65 |    2308 (51.9%) |  1735 (39.0%) |  350 (12.9%) |   704 (30.5%) |
  ;;|           wa-bolt-creek | 01) crowning disabled | Tue Sep 27 09:07:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+24hr) |                4446.0 |  2616 (58.8%) |            0.24 | 0.15 | 0.41 | 0.67 | 0.82 |    1119 (25.2%) |  1830 (41.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-goat-rocks |          00) original | Sun Sep 25 09:45:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+11hr) |                   9.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    9 (100.0%) |        0 (-) |         0 (-) |
  ;;|           wa-goat-rocks | 01) crowning disabled | Sun Sep 25 09:45:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+11hr) |                   9.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    9 (100.0%) |        0 (-) |         0 (-) |
  ;;|           wa-goat-rocks |          00) original | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+14hr) |                1787.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1787 (100.0%) |        0 (-) |         0 (-) |
  ;;|           wa-goat-rocks | 01) crowning disabled | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+14hr) |                1787.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1787 (100.0%) |        0 (-) |         0 (-) |
  ;;|           wa-goat-rocks |          00) original | Mon Sep 26 11:08:00 UTC 2022 | Tue Sep 27 10:00:00 UTC 2022 (+23hr) |                2916.0 |  2349 (80.6%) |            0.25 | 0.11 | 0.42 | 0.74 | 0.86 |    1700 (58.3%) |   567 (19.4%) |    45 (1.9%) |    126 (7.4%) |
  ;;|           wa-goat-rocks | 01) crowning disabled | Mon Sep 26 11:08:00 UTC 2022 | Tue Sep 27 10:00:00 UTC 2022 (+23hr) |                2916.0 |  2349 (80.6%) |            0.25 | 0.13 | 0.42 | 0.74 | 0.86 |    1490 (51.1%) |   567 (19.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-goat-rocks |          00) original | Tue Sep 27 10:00:00 UTC 2022 | Tue Sep 27 21:25:00 UTC 2022 (+11hr) |                3658.0 |  2883 (78.8%) |            0.38 | 0.34 | 0.61 | 0.83 | 0.91 |    1130 (30.9%) |   775 (21.2%) |     6 (0.2%) |   178 (15.8%) |
  ;;|           wa-goat-rocks | 01) crowning disabled | Tue Sep 27 10:00:00 UTC 2022 | Tue Sep 27 21:25:00 UTC 2022 (+11hr) |                3658.0 |  2883 (78.8%) |            0.38 | 0.34 | 0.61 | 0.83 | 0.91 |     849 (23.2%) |   775 (21.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;|           wa-goat-rocks |          00) original | Tue Sep 27 21:25:00 UTC 2022 | Wed Sep 28 10:32:00 UTC 2022 (+13hr) |                3816.0 |  3255 (85.3%) |            0.09 | 0.03 | 0.12 | 0.28 | 0.36 |    1337 (35.0%) |   561 (14.7%) |     9 (0.3%) |     82 (6.1%) |
  ;;|           wa-goat-rocks | 01) crowning disabled | Tue Sep 27 21:25:00 UTC 2022 | Wed Sep 28 10:32:00 UTC 2022 (+13hr) |                3816.0 |  3255 (85.3%) |            0.09 | 0.03 | 0.12 | 0.28 | 0.36 |    1229 (32.2%) |   561 (14.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|            wa-irving-pk |          00) original | Tue Sep 13 09:22:00 UTC 2022 | Tue Sep 13 20:51:00 UTC 2022 (+11hr) |                 139.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  139 (100.0%) |        0 (-) |         0 (-) |
  ;;|            wa-irving-pk | 01) crowning disabled | Tue Sep 13 09:22:00 UTC 2022 | Tue Sep 13 20:51:00 UTC 2022 (+11hr) |                 139.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  139 (100.0%) |        0 (-) |         0 (-) |
  ;;|            wa-irving-pk |          00) original | Tue Sep 13 20:51:00 UTC 2022 | Thu Sep 15 10:26:00 UTC 2022 (+38hr) |                 209.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  209 (100.0%) |        0 (-) |         0 (-) |
  ;;|            wa-irving-pk | 01) crowning disabled | Tue Sep 13 20:51:00 UTC 2022 | Thu Sep 15 10:26:00 UTC 2022 (+38hr) |                 209.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  209 (100.0%) |        0 (-) |         0 (-) |
  ;;|            wa-irving-pk |          00) original | Thu Sep 15 10:26:00 UTC 2022 | Fri Sep 16 09:15:00 UTC 2022 (+23hr) |                  81.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   81 (100.0%) |        0 (-) |         0 (-) |
  ;;|            wa-irving-pk | 01) crowning disabled | Thu Sep 15 10:26:00 UTC 2022 | Fri Sep 16 09:15:00 UTC 2022 (+23hr) |                  81.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   81 (100.0%) |        0 (-) |         0 (-) |
  ;;|            wa-irving-pk |          00) original | Fri Sep 16 09:15:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+25hr) |                 434.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  434 (100.0%) |        0 (-) |         0 (-) |
  ;;|            wa-irving-pk | 01) crowning disabled | Fri Sep 16 09:15:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+25hr) |                 434.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  434 (100.0%) |        0 (-) |         0 (-) |
  ;;|            wa-irving-pk |          00) original | Sat Sep 17 09:47:00 UTC 2022 | Mon Sep 19 10:00:00 UTC 2022 (+48hr) |                1286.0 |   314 (24.4%) |            0.09 | 0.06 | 0.12 | 0.24 | 0.30 |   2531 (196.8%) |   972 (75.6%) |    15 (4.8%) |   403 (15.9%) |
  ;;|            wa-irving-pk | 01) crowning disabled | Sat Sep 17 09:47:00 UTC 2022 | Mon Sep 19 10:00:00 UTC 2022 (+48hr) |                1286.0 |   314 (24.4%) |            0.12 | 0.08 | 0.13 | 0.27 | 0.53 |   1478 (114.9%) |   972 (75.6%) |     0 (0.0%) |      0 (0.0%) |
  ;;|            wa-irving-pk |          00) original | Mon Sep 19 10:00:00 UTC 2022 | Tue Sep 20 08:50:00 UTC 2022 (+23hr) |                1621.0 |   619 (38.2%) |            0.30 | 0.19 | 0.50 | 0.70 | 0.84 |     735 (45.3%) |  1002 (61.8%) |    19 (3.1%) |     56 (7.6%) |
  ;;|            wa-irving-pk | 01) crowning disabled | Mon Sep 19 10:00:00 UTC 2022 | Tue Sep 20 08:50:00 UTC 2022 (+23hr) |                1621.0 |   617 (38.1%) |            0.32 | 0.23 | 0.55 | 0.75 | 0.89 |     658 (40.6%) |  1004 (61.9%) |     0 (0.0%) |      0 (0.0%) |
  ;;|            wa-irving-pk |          00) original | Tue Sep 20 08:50:00 UTC 2022 | Tue Sep 20 20:17:00 UTC 2022 (+11hr) |                2083.0 |  1572 (75.5%) |            0.27 | 0.15 | 0.42 | 0.71 | 0.88 |     655 (31.4%) |   511 (24.5%) |    70 (4.5%) |    83 (12.7%) |
  ;;|            wa-irving-pk | 01) crowning disabled | Tue Sep 20 08:50:00 UTC 2022 | Tue Sep 20 20:17:00 UTC 2022 (+11hr) |                2083.0 |  1545 (74.2%) |            0.27 | 0.16 | 0.40 | 0.68 | 0.79 |     500 (24.0%) |   538 (25.8%) |     0 (0.0%) |      0 (0.0%) |
  ;;|            wa-irving-pk |          00) original | Tue Sep 20 20:17:00 UTC 2022 | Wed Sep 21 09:22:00 UTC 2022 (+13hr) |                3109.0 |  2253 (72.5%) |            0.27 | 0.14 | 0.44 | 0.78 | 0.88 |    1207 (38.8%) |   856 (27.5%) |   142 (6.3%) |   194 (16.1%) |
  ;;|            wa-irving-pk | 01) crowning disabled | Tue Sep 20 20:17:00 UTC 2022 | Wed Sep 21 09:22:00 UTC 2022 (+13hr) |                3109.0 |  2173 (69.9%) |            0.27 | 0.15 | 0.44 | 0.74 | 0.85 |     798 (25.7%) |   936 (30.1%) |     0 (0.0%) |      0 (0.0%) |
  ;;|            wa-irving-pk |          00) original | Wed Sep 21 09:22:00 UTC 2022 | Thu Sep 22 09:02:00 UTC 2022 (+24hr) |                2997.0 |  1852 (61.8%) |            0.15 | 0.03 | 0.22 | 0.48 | 0.65 |    2163 (72.2%) |  1145 (38.2%) |    69 (3.7%) |   393 (18.2%) |
  ;;|            wa-irving-pk | 01) crowning disabled | Wed Sep 21 09:22:00 UTC 2022 | Thu Sep 22 09:02:00 UTC 2022 (+24hr) |                2997.0 |  1836 (61.3%) |            0.16 | 0.02 | 0.23 | 0.46 | 0.59 |    1144 (38.2%) |  1161 (38.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|            wa-irving-pk |          00) original | Thu Sep 22 09:02:00 UTC 2022 | Thu Sep 22 20:29:00 UTC 2022 (+11hr) |                2540.0 |  1821 (71.7%) |            0.25 | 0.09 | 0.40 | 0.81 | 0.95 |    1686 (66.4%) |   719 (28.3%) |   114 (6.3%) |   516 (30.6%) |
  ;;|            wa-irving-pk | 01) crowning disabled | Thu Sep 22 09:02:00 UTC 2022 | Thu Sep 22 20:29:00 UTC 2022 (+11hr) |                2540.0 |  1781 (70.1%) |            0.25 | 0.11 | 0.40 | 0.77 | 0.90 |     785 (30.9%) |   759 (29.9%) |     0 (0.0%) |      0 (0.0%) |
  ;;|            wa-irving-pk |          00) original | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2760.0 |  2087 (75.6%) |            0.10 | 0.01 | 0.07 | 0.28 | 0.73 |   4596 (166.5%) |   673 (24.4%) |   146 (7.0%) |   569 (12.4%) |
  ;;|            wa-irving-pk | 01) crowning disabled | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2760.0 |  2070 (75.0%) |            0.11 | 0.01 | 0.12 | 0.29 | 0.67 |   3483 (126.2%) |   690 (25.0%) |     0 (0.0%) |      0 (0.0%) |
  ;;|            wa-irving-pk |          00) original | Sat Sep 24 09:15:00 UTC 2022 | Sun Sep 25 08:58:00 UTC 2022 (+24hr) |                 183.0 |   107 (58.5%) |            0.04 | 0.01 | 0.07 | 0.09 | 0.15 |    498 (272.1%) |    76 (41.5%) |   21 (19.6%) |    57 (11.4%) |
  ;;|            wa-irving-pk | 01) crowning disabled | Sat Sep 24 09:15:00 UTC 2022 | Sun Sep 25 08:58:00 UTC 2022 (+24hr) |                 183.0 |   107 (58.5%) |            0.06 | 0.01 | 0.09 | 0.13 | 0.34 |    347 (189.6%) |    76 (41.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|            wa-irving-pk |          00) original | Sun Sep 25 08:58:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+12hr) |                 485.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  485 (100.0%) |        0 (-) |         0 (-) |
  ;;|            wa-irving-pk | 01) crowning disabled | Sun Sep 25 08:58:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+12hr) |                 485.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  485 (100.0%) |        0 (-) |         0 (-) |
  ;;|            wa-irving-pk |          00) original | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                 678.0 |   288 (42.5%) |            0.16 | 0.04 | 0.26 | 0.44 | 0.52 |     240 (35.4%) |   390 (57.5%) |    16 (5.6%) |   103 (42.9%) |
  ;;|            wa-irving-pk | 01) crowning disabled | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                 678.0 |   272 (40.1%) |            0.20 | 0.02 | 0.36 | 0.73 | 0.78 |       47 (6.9%) |   406 (59.9%) |     0 (0.0%) |      0 (0.0%) |
  ;;|            wa-irving-pk |          00) original | Mon Sep 26 09:28:00 UTC 2022 | Tue Sep 27 09:07:00 UTC 2022 (+24hr) |                 661.0 |   423 (64.0%) |            0.14 | 0.02 | 0.15 | 0.43 | 0.78 |   2161 (326.9%) |   238 (36.0%) |    16 (3.8%) |   868 (40.2%) |
  ;;|            wa-irving-pk | 01) crowning disabled | Mon Sep 26 09:28:00 UTC 2022 | Tue Sep 27 09:07:00 UTC 2022 (+24hr) |                 661.0 |   418 (63.2%) |            0.13 | 0.02 | 0.15 | 0.41 | 0.69 |     392 (59.3%) |   243 (36.8%) |     0 (0.0%) |      0 (0.0%) |
  ;;|            wa-irving-pk |          00) original | Tue Sep 27 09:07:00 UTC 2022 | Wed Sep 28 10:30:00 UTC 2022 (+25hr) |                1112.0 |   253 (22.8%) |            0.09 | 0.00 | 0.12 | 0.29 | 0.45 |     685 (61.6%) |   859 (77.2%) |    10 (4.0%) |    81 (11.8%) |
  ;;|            wa-irving-pk | 01) crowning disabled | Tue Sep 27 09:07:00 UTC 2022 | Wed Sep 28 10:30:00 UTC 2022 (+25hr) |                1112.0 |   253 (22.8%) |            0.10 | 0.00 | 0.12 | 0.31 | 0.56 |     608 (54.7%) |   859 (77.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;|            wa-irving-pk |          00) original | Wed Sep 28 10:30:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+24hr) |                 926.0 |   657 (71.0%) |            0.10 | 0.02 | 0.13 | 0.23 | 0.44 |   1816 (196.1%) |   269 (29.0%) |    51 (7.8%) |   614 (33.8%) |
  ;;|            wa-irving-pk | 01) crowning disabled | Wed Sep 28 10:30:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+24hr) |                 926.0 |   655 (70.7%) |            0.11 | 0.02 | 0.18 | 0.22 | 0.40 |     606 (65.4%) |   271 (29.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               wa-kalama |          00) original | Sat Sep 24 11:18:00 UTC 2022 | Sun Sep 25 10:50:00 UTC 2022 (+24hr) |                 431.0 |   311 (72.2%) |            0.21 | 0.11 | 0.34 | 0.66 | 0.84 |    568 (131.8%) |   120 (27.8%) |     5 (1.6%) |      1 (0.2%) |
  ;;|               wa-kalama | 01) crowning disabled | Sat Sep 24 11:18:00 UTC 2022 | Sun Sep 25 10:50:00 UTC 2022 (+24hr) |                 431.0 |   311 (72.2%) |            0.22 | 0.11 | 0.35 | 0.66 | 0.84 |    568 (131.8%) |   120 (27.8%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               wa-kalama |          00) original | Sun Sep 25 10:50:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+24hr) |                 615.0 |   336 (54.6%) |            0.29 | 0.18 | 0.48 | 0.77 | 0.90 |     368 (59.8%) |   279 (45.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               wa-kalama | 01) crowning disabled | Sun Sep 25 10:50:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+24hr) |                 615.0 |   336 (54.6%) |            0.29 | 0.18 | 0.48 | 0.77 | 0.90 |     368 (59.8%) |   279 (45.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|               wa-kalama |          00) original | Mon Sep 26 11:08:00 UTC 2022 | Tue Sep 27 10:02:00 UTC 2022 (+23hr) |                 833.0 |   645 (77.4%) |            0.25 | 0.20 | 0.38 | 0.59 | 0.77 |     431 (51.7%) |   188 (22.6%) |     3 (0.5%) |      0 (0.0%) |
  ;;|               wa-kalama | 01) crowning disabled | Mon Sep 26 11:08:00 UTC 2022 | Tue Sep 27 10:02:00 UTC 2022 (+23hr) |                 833.0 |   644 (77.3%) |            0.25 | 0.20 | 0.39 | 0.59 | 0.78 |     431 (51.7%) |   189 (22.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|                  wa-kid |          00) original | Tue Sep 13 10:13:00 UTC 2022 | Tue Sep 13 20:51:00 UTC 2022 (+11hr) |                  78.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   78 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  wa-kid | 01) crowning disabled | Tue Sep 13 10:13:00 UTC 2022 | Tue Sep 13 20:51:00 UTC 2022 (+11hr) |                  78.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   78 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  wa-kid |          00) original | Tue Sep 13 20:51:00 UTC 2022 | Wed Sep 14 09:52:00 UTC 2022 (+13hr) |                  78.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   78 (100.0%) |        0 (-) |         0 (-) |
  ;;|                  wa-kid | 01) crowning disabled | Tue Sep 13 20:51:00 UTC 2022 | Wed Sep 14 09:52:00 UTC 2022 (+13hr) |                  78.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   78 (100.0%) |        0 (-) |         0 (-) |
  ;;|     wa-mcallister-creek |          00) original | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                4809.0 |  3953 (82.2%) |            0.25 | 0.17 | 0.37 | 0.64 | 0.73 |  18614 (387.1%) |   856 (17.8%) |   137 (3.5%) |  7456 (40.1%) |
  ;;|     wa-mcallister-creek | 01) crowning disabled | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                4809.0 |  3933 (81.8%) |            0.26 | 0.18 | 0.40 | 0.66 | 0.81 |    1141 (23.7%) |   876 (18.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge |          00) original | Tue Sep 13 10:13:00 UTC 2022 | Wed Sep 14 11:32:00 UTC 2022 (+25hr) |                 500.0 |   357 (71.4%) |            0.05 | 0.01 | 0.09 | 0.17 | 0.24 |   1806 (361.2%) |   143 (28.6%) |    15 (4.2%) |   637 (35.3%) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Tue Sep 13 10:13:00 UTC 2022 | Wed Sep 14 11:32:00 UTC 2022 (+25hr) |                 500.0 |   357 (71.4%) |            0.06 | 0.01 | 0.09 | 0.17 | 0.26 |   1057 (211.4%) |   143 (28.6%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge |          00) original | Wed Sep 14 11:32:00 UTC 2022 | Wed Sep 14 21:21:00 UTC 2022 (+10hr) |                 554.0 |   488 (88.1%) |            0.25 | 0.07 | 0.41 | 0.67 | 0.86 |     523 (94.4%) |    66 (11.9%) |    37 (7.6%) |   284 (54.3%) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Wed Sep 14 11:32:00 UTC 2022 | Wed Sep 14 21:21:00 UTC 2022 (+10hr) |                 554.0 |   484 (87.4%) |            0.24 | 0.05 | 0.40 | 0.63 | 0.82 |     372 (67.1%) |    70 (12.6%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge |          00) original | Wed Sep 14 21:21:00 UTC 2022 | Fri Sep 16 10:06:00 UTC 2022 (+37hr) |                 652.0 |   605 (92.8%) |            0.14 | 0.04 | 0.18 | 0.42 | 0.79 |   2745 (421.0%) |     47 (7.2%) |    56 (9.3%) |   789 (28.7%) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Wed Sep 14 21:21:00 UTC 2022 | Fri Sep 16 10:06:00 UTC 2022 (+37hr) |                 652.0 |   592 (90.8%) |            0.11 | 0.04 | 0.18 | 0.34 | 0.43 |   1609 (246.8%) |     60 (9.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge |          00) original | Fri Sep 16 10:06:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+24hr) |                 120.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  120 (100.0%) |        0 (-) |         0 (-) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Fri Sep 16 10:06:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+24hr) |                 120.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  120 (100.0%) |        0 (-) |         0 (-) |
  ;;|         wa-minnow-ridge |          00) original | Sat Sep 17 09:47:00 UTC 2022 | Sun Sep 18 10:17:00 UTC 2022 (+25hr) |                 203.0 |   107 (52.7%) |            0.46 | 0.42 | 0.94 | 0.97 | 0.99 |    893 (439.9%) |    96 (47.3%) |   28 (26.2%) |   178 (19.9%) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Sat Sep 17 09:47:00 UTC 2022 | Sun Sep 18 10:17:00 UTC 2022 (+25hr) |                 203.0 |    82 (40.4%) |            0.22 | 0.08 | 0.43 | 0.62 | 0.84 |    417 (205.4%) |   121 (59.6%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge |          00) original | Sun Sep 18 10:17:00 UTC 2022 | Mon Sep 19 10:49:00 UTC 2022 (+25hr) |                1225.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1225 (100.0%) |        0 (-) |         0 (-) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Sun Sep 18 10:17:00 UTC 2022 | Mon Sep 19 10:49:00 UTC 2022 (+25hr) |                1225.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1225 (100.0%) |        0 (-) |         0 (-) |
  ;;|         wa-minnow-ridge |          00) original | Mon Sep 19 10:49:00 UTC 2022 | Mon Sep 19 21:25:00 UTC 2022 (+11hr) |                1274.0 |  1256 (98.6%) |            0.34 | 0.27 | 0.54 | 0.72 | 0.82 |    1115 (87.5%) |     18 (1.4%) |  169 (13.5%) |   413 (37.0%) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Mon Sep 19 10:49:00 UTC 2022 | Mon Sep 19 21:25:00 UTC 2022 (+11hr) |                1274.0 |  1124 (88.2%) |            0.32 | 0.23 | 0.47 | 0.80 | 0.91 |     573 (45.0%) |   150 (11.8%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge |          00) original | Mon Sep 19 21:25:00 UTC 2022 | Tue Sep 20 09:41:00 UTC 2022 (+12hr) |                1767.0 |  1578 (89.3%) |            0.26 | 0.15 | 0.41 | 0.71 | 0.86 |   2171 (122.9%) |   189 (10.7%) |  166 (10.5%) |  1026 (47.3%) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Mon Sep 19 21:25:00 UTC 2022 | Tue Sep 20 09:41:00 UTC 2022 (+12hr) |                1767.0 |  1533 (86.8%) |            0.29 | 0.19 | 0.49 | 0.78 | 0.90 |     890 (50.4%) |   234 (13.2%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge |          00) original | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:17:00 UTC 2022 (+11hr) |                2157.0 |  1926 (89.3%) |            0.33 | 0.24 | 0.51 | 0.79 | 0.91 |    1433 (66.4%) |   231 (10.7%) |  203 (10.5%) |   584 (40.8%) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:17:00 UTC 2022 (+11hr) |                2157.0 |  1870 (86.7%) |            0.37 | 0.31 | 0.59 | 0.84 | 0.93 |     771 (35.7%) |   287 (13.3%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge |          00) original | Tue Sep 20 20:17:00 UTC 2022 | Wed Sep 21 09:22:00 UTC 2022 (+13hr) |                2489.0 |  2383 (95.7%) |            0.14 | 0.06 | 0.17 | 0.36 | 0.54 |   4323 (173.7%) |    106 (4.3%) |   201 (8.4%) |  1710 (39.6%) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Tue Sep 20 20:17:00 UTC 2022 | Wed Sep 21 09:22:00 UTC 2022 (+13hr) |                2489.0 |  2365 (95.0%) |            0.19 | 0.08 | 0.26 | 0.55 | 0.73 |    1635 (65.7%) |    124 (5.0%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge |          00) original | Wed Sep 21 09:22:00 UTC 2022 | Thu Sep 22 10:43:00 UTC 2022 (+25hr) |                3105.0 |  3041 (97.9%) |            0.20 | 0.15 | 0.26 | 0.49 | 0.66 |   6224 (200.5%) |     64 (2.1%) |  537 (17.7%) |  1677 (26.9%) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Wed Sep 21 09:22:00 UTC 2022 | Thu Sep 22 10:43:00 UTC 2022 (+25hr) |                3105.0 |  3031 (97.6%) |            0.32 | 0.27 | 0.50 | 0.73 | 0.84 |    2742 (88.3%) |     74 (2.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge |          00) original | Thu Sep 22 10:43:00 UTC 2022 | Thu Sep 22 20:29:00 UTC 2022 (+10hr) |                1457.0 |  1283 (88.1%) |            0.18 | 0.07 | 0.23 | 0.43 | 0.63 |   1814 (124.5%) |   174 (11.9%) |    76 (5.9%) |   625 (34.5%) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Thu Sep 22 10:43:00 UTC 2022 | Thu Sep 22 20:29:00 UTC 2022 (+10hr) |                1457.0 |  1277 (87.6%) |            0.19 | 0.07 | 0.25 | 0.45 | 0.65 |    1028 (70.6%) |   180 (12.4%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge |          00) original | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2184.0 |  2145 (98.2%) |            0.16 | 0.05 | 0.15 | 0.58 | 0.66 |   7045 (322.6%) |     39 (1.8%) |  324 (15.1%) |  1394 (19.8%) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2184.0 |  2121 (97.1%) |            0.22 | 0.11 | 0.34 | 0.68 | 0.79 |   4652 (213.0%) |     63 (2.9%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge |          00) original | Sat Sep 24 09:15:00 UTC 2022 | Sun Sep 25 08:58:00 UTC 2022 (+24hr) |                2382.0 |  1692 (71.0%) |            0.42 | 0.42 | 0.59 | 0.80 | 0.88 |    1176 (49.4%) |   690 (29.0%) |  292 (17.3%) |   259 (22.0%) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Sat Sep 24 09:15:00 UTC 2022 | Sun Sep 25 08:58:00 UTC 2022 (+24hr) |                2382.0 |  1413 (59.3%) |            0.43 | 0.42 | 0.63 | 0.83 | 0.92 |     658 (27.6%) |   969 (40.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge |          00) original | Sun Sep 25 08:58:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+12hr) |                2408.0 |  2137 (88.7%) |            0.31 | 0.19 | 0.47 | 0.79 | 0.89 |     694 (28.8%) |   271 (11.3%) |    88 (4.1%) |   180 (25.9%) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Sun Sep 25 08:58:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+12hr) |                2408.0 |  2108 (87.5%) |            0.31 | 0.20 | 0.47 | 0.81 | 0.92 |     432 (17.9%) |   300 (12.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge |          00) original | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                2298.0 |  2183 (95.0%) |            0.17 | 0.06 | 0.25 | 0.58 | 0.72 |    1135 (49.4%) |    115 (5.0%) |    90 (4.1%) |   279 (24.6%) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                2298.0 |  2149 (93.5%) |            0.16 | 0.02 | 0.27 | 0.45 | 0.65 |     709 (30.9%) |    149 (6.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge |          00) original | Mon Sep 26 09:28:00 UTC 2022 | Tue Sep 27 09:07:00 UTC 2022 (+24hr) |                1159.0 |  1082 (93.4%) |            0.26 | 0.17 | 0.39 | 0.66 | 0.81 |   1524 (131.5%) |     77 (6.6%) |   105 (9.7%) |   279 (18.3%) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Mon Sep 26 09:28:00 UTC 2022 | Tue Sep 27 09:07:00 UTC 2022 (+24hr) |                1159.0 |  1025 (88.4%) |            0.29 | 0.17 | 0.51 | 0.81 | 0.89 |    1010 (87.1%) |   134 (11.6%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge |          00) original | Tue Sep 27 09:07:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+24hr) |                1622.0 |  1327 (81.8%) |            0.31 | 0.28 | 0.44 | 0.72 | 0.82 |     933 (57.5%) |   295 (18.2%) |  288 (21.7%) |   108 (11.6%) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Tue Sep 27 09:07:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+24hr) |                1622.0 |  1156 (71.3%) |            0.36 | 0.30 | 0.58 | 0.81 | 0.89 |     618 (38.1%) |   466 (28.7%) |     0 (0.0%) |      0 (0.0%) |
  ;;|         wa-minnow-ridge |          00) original | Wed Sep 28 08:50:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+25hr) |                1625.0 |  1422 (87.5%) |            0.19 | 0.11 | 0.25 | 0.50 | 0.68 |   3531 (217.3%) |   203 (12.5%) |    68 (4.8%) |   722 (20.4%) |
  ;;|         wa-minnow-ridge | 01) crowning disabled | Wed Sep 28 08:50:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+25hr) |                1625.0 |  1420 (87.4%) |            0.20 | 0.14 | 0.26 | 0.51 | 0.70 |   2194 (135.0%) |   205 (12.6%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             wa-siouoxon |          00) original | Sat Sep 24 20:42:00 UTC 2022 | Sun Sep 25 11:28:00 UTC 2022 (+15hr) |                 141.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  141 (100.0%) |        0 (-) |         0 (-) |
  ;;|             wa-siouoxon | 01) crowning disabled | Sat Sep 24 20:42:00 UTC 2022 | Sun Sep 25 11:28:00 UTC 2022 (+15hr) |                 141.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  141 (100.0%) |        0 (-) |         0 (-) |
  ;;|             wa-siouoxon |          00) original | Sun Sep 25 11:28:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+24hr) |                 430.0 |   377 (87.7%) |            0.42 | 0.40 | 0.68 | 0.85 | 0.93 |    440 (102.3%) |    53 (12.3%) |     5 (1.3%) |      7 (1.6%) |
  ;;|             wa-siouoxon | 01) crowning disabled | Sun Sep 25 11:28:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+24hr) |                 430.0 |   375 (87.2%) |            0.42 | 0.40 | 0.69 | 0.88 | 0.93 |     417 (97.0%) |    55 (12.8%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             wa-siouoxon |          00) original | Mon Sep 26 11:08:00 UTC 2022 | Tue Sep 27 10:02:00 UTC 2022 (+23hr) |                 949.0 |   527 (55.5%) |            0.30 | 0.23 | 0.50 | 0.73 | 0.85 |    976 (102.8%) |   422 (44.5%) |     0 (0.0%) |     22 (2.3%) |
  ;;|             wa-siouoxon | 01) crowning disabled | Mon Sep 26 11:08:00 UTC 2022 | Tue Sep 27 10:02:00 UTC 2022 (+23hr) |                 949.0 |   527 (55.5%) |            0.30 | 0.23 | 0.50 | 0.73 | 0.85 |     927 (97.7%) |   422 (44.5%) |     0 (0.0%) |      0 (0.0%) |
  ;;|             wa-siouoxon |          00) original | Tue Sep 27 10:02:00 UTC 2022 | Wed Sep 28 10:32:00 UTC 2022 (+25hr) |                1489.0 |  1289 (86.6%) |            0.28 | 0.21 | 0.46 | 0.69 | 0.82 |    1088 (73.1%) |   200 (13.4%) |     6 (0.5%) |     10 (0.9%) |
  ;;|             wa-siouoxon | 01) crowning disabled | Tue Sep 27 10:02:00 UTC 2022 | Wed Sep 28 10:32:00 UTC 2022 (+25hr) |                1489.0 |  1289 (86.6%) |            0.28 | 0.21 | 0.46 | 0.69 | 0.82 |    1079 (72.5%) |   200 (13.4%) |     0 (0.0%) |      0 (0.0%) |

  ;; Disabling crowning removes most of the over-prediction, while only slightly inflating under-prediction:
  (pprint-table-from-kv-lists
   ["Fire name"
    "Variation"
    "sim - real"
    "real - sim"
    "n cells really burned"
    "sim ∩ real"
    "t0"
    "t1"]
   (replayed-results-table-entries
    replay-results))
  ;;|              Fire name |             Variation |      sim - real |    real - sim | n cells really burned |    sim ∩ real |                           t0 |                                   t1 |
  ;;|------------------------+-----------------------+-----------------+---------------+-----------------------+---------------+------------------------------+--------------------------------------|
  ;;|              ca-summit |          00) original |           0 (-) |         0 (-) |                   0.0 |         0 (-) | Wed Sep 21 10:15:00 UTC 2022 | Thu Sep 22 09:58:00 UTC 2022 (+24hr) |
  ;;|              ca-summit | 01) crowning disabled |           0 (-) |         0 (-) |                   0.0 |         0 (-) | Wed Sep 21 10:15:00 UTC 2022 | Thu Sep 22 09:58:00 UTC 2022 (+24hr) |
  ;;|            or-sturgill |          00) original |     210 (23.8%) |   681 (77.0%) |                 884.0 |   203 (23.0%) | Wed Sep 14 09:54:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |
  ;;|            or-sturgill | 01) crowning disabled |       66 (7.5%) |   681 (77.0%) |                 884.0 |   203 (23.0%) | Wed Sep 14 09:54:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |
  ;;|          mt-government |          00) original |        0 (0.0%) |   89 (100.0%) |                  89.0 |      0 (0.0%) | Fri Sep 16 11:01:00 UTC 2022 | Sat Sep 17 11:20:00 UTC 2022 (+24hr) |
  ;;|          mt-government | 01) crowning disabled |        0 (0.0%) |   89 (100.0%) |                  89.0 |      0 (0.0%) | Fri Sep 16 11:01:00 UTC 2022 | Sat Sep 17 11:20:00 UTC 2022 (+24hr) |
  ;;|               id-lemhi |          00) original |   2213 (123.6%) |   810 (45.2%) |                1791.0 |   981 (54.8%) | Sun Sep 25 21:12:00 UTC 2022 | Mon Sep 26 10:19:00 UTC 2022 (+13hr) |
  ;;|               id-lemhi | 01) crowning disabled |      128 (7.1%) |   961 (53.7%) |                1791.0 |   830 (46.3%) | Sun Sep 25 21:12:00 UTC 2022 | Mon Sep 26 10:19:00 UTC 2022 (+13hr) |
  ;;|           wa-irving-pk |          00) original |   2846 (419.8%) |   388 (57.2%) |                 678.0 |   290 (42.8%) | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |
  ;;|           wa-irving-pk | 01) crowning disabled |       40 (5.9%) |   408 (60.2%) |                 678.0 |   270 (39.8%) | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |
  ;;| id-isabella-lower-twin |          00) original |   1065 (179.0%) |   509 (85.5%) |                 595.0 |    86 (14.5%) | Sun Sep 18 08:37:00 UTC 2022 | Mon Sep 19 10:49:00 UTC 2022 (+26hr) |
  ;;| id-isabella-lower-twin | 01) crowning disabled |     121 (20.3%) |   512 (86.1%) |                 595.0 |    83 (13.9%) | Sun Sep 18 08:37:00 UTC 2022 | Mon Sep 19 10:49:00 UTC 2022 (+26hr) |
  ;;|              ca-summit |          00) original |        0 (0.0%) |   14 (100.0%) |                  14.0 |      0 (0.0%) | Thu Sep 22 21:18:00 UTC 2022 | Sat Sep 24 09:20:00 UTC 2022 (+36hr) |
  ;;|              ca-summit | 01) crowning disabled |        0 (0.0%) |   14 (100.0%) |                  14.0 |      0 (0.0%) | Thu Sep 22 21:18:00 UTC 2022 | Sat Sep 24 09:20:00 UTC 2022 (+36hr) |
  ;;|          wa-goat-rocks |          00) original |        0 (0.0%) |    9 (100.0%) |                   9.0 |      0 (0.0%) | Sun Sep 25 09:45:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+11hr) |
  ;;|          wa-goat-rocks | 01) crowning disabled |        0 (0.0%) |    9 (100.0%) |                   9.0 |      0 (0.0%) | Sun Sep 25 09:45:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+11hr) |
  ;;| id-isabella-lower-twin |          00) original |     313 (67.2%) |   133 (28.5%) |                 466.0 |   333 (71.5%) | Mon Sep 19 10:49:00 UTC 2022 | Mon Sep 19 21:25:00 UTC 2022 (+11hr) |
  ;;| id-isabella-lower-twin | 01) crowning disabled |       26 (5.6%) |   146 (31.3%) |                 466.0 |   320 (68.7%) | Mon Sep 19 10:49:00 UTC 2022 | Mon Sep 19 21:25:00 UTC 2022 (+11hr) |
  ;;|              ca-summit |          00) original |        0 (0.0%) |   49 (100.0%) |                  49.0 |      0 (0.0%) | Mon Sep 26 09:32:00 UTC 2022 | Tue Sep 27 09:11:00 UTC 2022 (+24hr) |
  ;;|              ca-summit | 01) crowning disabled |        0 (0.0%) |   49 (100.0%) |                  49.0 |      0 (0.0%) | Mon Sep 26 09:32:00 UTC 2022 | Tue Sep 27 09:11:00 UTC 2022 (+24hr) |
  ;;|        or-double-creek |          00) original |        0 (0.0%) | 1883 (100.0%) |                1883.0 |      0 (0.0%) | Mon Sep 19 09:09:00 UTC 2022 | Tue Sep 20 08:52:00 UTC 2022 (+24hr) |
  ;;|        or-double-creek | 01) crowning disabled |        0 (0.0%) | 1883 (100.0%) |                1883.0 |      0 (0.0%) | Mon Sep 19 09:09:00 UTC 2022 | Tue Sep 20 08:52:00 UTC 2022 (+24hr) |
  ;;|            mt-billiard |          00) original |  4757 (1336.2%) |   156 (43.8%) |                 356.0 |   200 (56.2%) | Wed Sep 14 09:52:00 UTC 2022 | Fri Sep 16 11:11:00 UTC 2022 (+49hr) |
  ;;|            mt-billiard | 01) crowning disabled |    726 (203.9%) |   165 (46.3%) |                 356.0 |   191 (53.7%) | Wed Sep 14 09:52:00 UTC 2022 | Fri Sep 16 11:11:00 UTC 2022 (+49hr) |
  ;;|                 ca-red |          00) original |        0 (0.0%) |   96 (100.0%) |                  96.0 |      0 (0.0%) | Wed Sep 14 16:50:00 UTC 2022 | Thu Sep 15 10:28:00 UTC 2022 (+18hr) |
  ;;|                 ca-red | 01) crowning disabled |        0 (0.0%) |   96 (100.0%) |                  96.0 |      0 (0.0%) | Wed Sep 14 16:50:00 UTC 2022 | Thu Sep 15 10:28:00 UTC 2022 (+18hr) |
  ;;|              mt-cannon |          00) original |  1545 (1095.7%) |     12 (8.5%) |                 141.0 |   129 (91.5%) | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+59hr) |
  ;;|              mt-cannon | 01) crowning disabled |   1122 (795.7%) |     12 (8.5%) |                 141.0 |   129 (91.5%) | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+59hr) |
  ;;|          wa-goat-rocks |          00) original |        0 (0.0%) | 1787 (100.0%) |                1787.0 |      0 (0.0%) | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+14hr) |
  ;;|          wa-goat-rocks | 01) crowning disabled |        0 (0.0%) | 1787 (100.0%) |                1787.0 |      0 (0.0%) | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+14hr) |
  ;;|             id-ross-fk |          00) original |    1229 (16.9%) |  5320 (73.0%) |                7284.0 |  1964 (27.0%) | Tue Sep 13 09:24:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+25hr) |
  ;;|             id-ross-fk | 01) crowning disabled |     783 (10.7%) |  5325 (73.1%) |                7284.0 |  1959 (26.9%) | Tue Sep 13 09:24:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+25hr) |
  ;;|                 ca-red |          00) original |        0 (0.0%) |   97 (100.0%) |                  97.0 |      0 (0.0%) | Thu Sep 15 10:28:00 UTC 2022 | Thu Sep 15 17:34:00 UTC 2022 (+ 7hr) |
  ;;|                 ca-red | 01) crowning disabled |        0 (0.0%) |   97 (100.0%) |                  97.0 |      0 (0.0%) | Thu Sep 15 10:28:00 UTC 2022 | Thu Sep 15 17:34:00 UTC 2022 (+ 7hr) |
  ;;|              ca-summit |          00) original |        2 (0.1%) |   250 (17.9%) |                1396.0 |  1146 (82.1%) | Mon Sep 19 21:23:00 UTC 2022 | Tue Sep 20 08:54:00 UTC 2022 (+12hr) |
  ;;|              ca-summit | 01) crowning disabled |        2 (0.1%) |   250 (17.9%) |                1396.0 |  1146 (82.1%) | Mon Sep 19 21:23:00 UTC 2022 | Tue Sep 20 08:54:00 UTC 2022 (+12hr) |
  ;;|        or-double-creek |          00) original |        0 (0.0%) | 17892 (87.6%) |               20434.0 |  2542 (12.4%) | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |
  ;;|        or-double-creek | 01) crowning disabled |        0 (0.0%) | 18178 (89.0%) |               20434.0 |  2256 (11.0%) | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |
  ;;|                 wa-kid |          00) original |        0 (0.0%) |   78 (100.0%) |                  78.0 |      0 (0.0%) | Tue Sep 13 20:51:00 UTC 2022 | Wed Sep 14 09:52:00 UTC 2022 (+13hr) |
  ;;|                 wa-kid | 01) crowning disabled |        0 (0.0%) |   78 (100.0%) |                  78.0 |      0 (0.0%) | Tue Sep 13 20:51:00 UTC 2022 | Wed Sep 14 09:52:00 UTC 2022 (+13hr) |
  ;;|        wa-minnow-ridge |          00) original |  16505 (755.7%) |     35 (1.6%) |                2184.0 |  2149 (98.4%) | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |
  ;;|        wa-minnow-ridge | 01) crowning disabled |   3132 (143.4%) |    141 (6.5%) |                2184.0 |  2043 (93.5%) | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |
  ;;|                 ca-red |          00) original |        0 (0.0%) |   44 (100.0%) |                  44.0 |      0 (0.0%) | Fri Sep 16 20:40:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+12hr) |
  ;;|                 ca-red | 01) crowning disabled |        0 (0.0%) |   44 (100.0%) |                  44.0 |      0 (0.0%) | Fri Sep 16 20:40:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+12hr) |
  ;;|         mt-george-lake |          00) original |   2248 (166.6%) |   571 (42.3%) |                1349.0 |   778 (57.7%) | Tue Sep 13 09:22:00 UTC 2022 | Thu Sep 15 11:09:00 UTC 2022 (+50hr) |
  ;;|         mt-george-lake | 01) crowning disabled |     311 (23.1%) |   592 (43.9%) |                1349.0 |   757 (56.1%) | Tue Sep 13 09:22:00 UTC 2022 | Thu Sep 15 11:09:00 UTC 2022 (+50hr) |
  ;;|               id-lemhi |          00) original |  15492 (773.8%) |    108 (5.4%) |                2002.0 |  1894 (94.6%) | Mon Sep 26 10:19:00 UTC 2022 | Tue Sep 27 09:09:00 UTC 2022 (+23hr) |
  ;;|               id-lemhi | 01) crowning disabled |   9215 (460.3%) |    108 (5.4%) |                2002.0 |  1894 (94.6%) | Mon Sep 26 10:19:00 UTC 2022 | Tue Sep 27 09:09:00 UTC 2022 (+23hr) |
  ;;|           wa-irving-pk |          00) original |   5493 (831.0%) |   235 (35.6%) |                 661.0 |   426 (64.4%) | Mon Sep 26 09:28:00 UTC 2022 | Tue Sep 27 09:07:00 UTC 2022 (+24hr) |
  ;;|           wa-irving-pk | 01) crowning disabled |     191 (28.9%) |   253 (38.3%) |                 661.0 |   408 (61.7%) | Mon Sep 26 09:28:00 UTC 2022 | Tue Sep 27 09:07:00 UTC 2022 (+24hr) |
  ;;|        wa-minnow-ridge |          00) original |   4018 (247.7%) |   259 (16.0%) |                1622.0 |  1363 (84.0%) | Tue Sep 27 09:07:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+24hr) |
  ;;|        wa-minnow-ridge | 01) crowning disabled |     226 (13.9%) |   672 (41.4%) |                1622.0 |   950 (58.6%) | Tue Sep 27 09:07:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+24hr) |
  ;;|        wa-minnow-ridge |          00) original |    1291 (59.9%) |   367 (17.0%) |                2157.0 |  1790 (83.0%) | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:17:00 UTC 2022 (+11hr) |
  ;;|        wa-minnow-ridge | 01) crowning disabled |       69 (3.2%) |   722 (33.5%) |                2157.0 |  1435 (66.5%) | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:17:00 UTC 2022 (+11hr) |
  ;;|          mt-government |          00) original |      81 (84.4%) |    36 (37.5%) |                  96.0 |    60 (62.5%) | Sat Sep 17 11:20:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+ 9hr) |
  ;;|          mt-government | 01) crowning disabled |        0 (0.0%) |    43 (44.8%) |                  96.0 |    53 (55.2%) | Sat Sep 17 11:20:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+ 9hr) |
  ;;|              ca-barnes |          00) original |        2 (0.2%) |   561 (63.2%) |                 888.0 |   327 (36.8%) | Tue Sep 13 13:18:00 UTC 2022 | Tue Sep 13 19:12:00 UTC 2022 (+ 6hr) |
  ;;|              ca-barnes | 01) crowning disabled |        2 (0.2%) |   561 (63.2%) |                 888.0 |   327 (36.8%) | Tue Sep 13 13:18:00 UTC 2022 | Tue Sep 13 19:12:00 UTC 2022 (+ 6hr) |
  ;;|        wa-minnow-ridge |          00) original |   3105 (135.1%) |    112 (4.9%) |                2298.0 |  2186 (95.1%) | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |
  ;;|        wa-minnow-ridge | 01) crowning disabled |     598 (26.0%) |    150 (6.5%) |                2298.0 |  2148 (93.5%) | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |
  ;;|        wa-minnow-ridge |          00) original |  13024 (419.5%) |     97 (3.1%) |                3105.0 |  3008 (96.9%) | Wed Sep 21 09:22:00 UTC 2022 | Thu Sep 22 10:43:00 UTC 2022 (+25hr) |
  ;;|        wa-minnow-ridge | 01) crowning disabled |    1043 (33.6%) |   356 (11.5%) |                3105.0 |  2749 (88.5%) | Wed Sep 21 09:22:00 UTC 2022 | Thu Sep 22 10:43:00 UTC 2022 (+25hr) |
  ;;|           wa-irving-pk |          00) original |     805 (49.7%) |  1016 (62.7%) |                1621.0 |   605 (37.3%) | Mon Sep 19 10:00:00 UTC 2022 | Tue Sep 20 08:50:00 UTC 2022 (+23hr) |
  ;;|           wa-irving-pk | 01) crowning disabled |     407 (25.1%) |  1023 (63.1%) |                1621.0 |   598 (36.9%) | Mon Sep 19 10:00:00 UTC 2022 | Tue Sep 20 08:50:00 UTC 2022 (+23hr) |
  ;;|        or-double-creek |          00) original |    4966 (19.2%) |  6099 (23.6%) |               25821.0 | 19722 (76.4%) | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 08:33:00 UTC 2022 (+12hr) |
  ;;|        or-double-creek | 01) crowning disabled |      995 (3.9%) |  6810 (26.4%) |               25821.0 | 19011 (73.6%) | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 08:33:00 UTC 2022 (+12hr) |
  ;;| id-columbus-bear-gulch |          00) original | 19703 (2976.3%) |   237 (35.8%) |                 662.0 |   425 (64.2%) | Wed Sep 14 21:21:00 UTC 2022 | Sat Sep 17 10:36:00 UTC 2022 (+61hr) |
  ;;| id-columbus-bear-gulch | 01) crowning disabled |   1357 (205.0%) |   292 (44.1%) |                 662.0 |   370 (55.9%) | Wed Sep 14 21:21:00 UTC 2022 | Sat Sep 17 10:36:00 UTC 2022 (+61hr) |
  ;;|               id-lemhi |          00) original |  13368 (362.9%) |     84 (2.3%) |                3684.0 |  3600 (97.7%) | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+14hr) |
  ;;|               id-lemhi | 01) crowning disabled |   5897 (160.1%) |   532 (14.4%) |                3684.0 |  3152 (85.6%) | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+14hr) |
  ;;|           wa-irving-pk |          00) original |   2802 (302.6%) |   271 (29.3%) |                 926.0 |   655 (70.7%) | Wed Sep 28 10:30:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+24hr) |
  ;;|           wa-irving-pk | 01) crowning disabled |     303 (32.7%) |   277 (29.9%) |                 926.0 |   649 (70.1%) | Wed Sep 28 10:30:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+24hr) |
  ;;|           wa-irving-pk |          00) original |   5712 (444.2%) |   942 (73.3%) |                1286.0 |   344 (26.7%) | Sat Sep 17 09:47:00 UTC 2022 | Mon Sep 19 10:00:00 UTC 2022 (+48hr) |
  ;;|           wa-irving-pk | 01) crowning disabled |     702 (54.6%) |   980 (76.2%) |                1286.0 |   306 (23.8%) | Sat Sep 17 09:47:00 UTC 2022 | Mon Sep 19 10:00:00 UTC 2022 (+48hr) |
  ;;|        or-double-creek |          00) original |   5861 (127.7%) |  3192 (69.6%) |                4588.0 |  1396 (30.4%) | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |
  ;;|        or-double-creek | 01) crowning disabled |     573 (12.5%) |  3371 (73.5%) |                4588.0 |  1217 (26.5%) | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |
  ;;|              ca-summit |          00) original |        0 (0.0%) |    85 (57.8%) |                 147.0 |    62 (42.2%) | Sun Sep 25 09:49:00 UTC 2022 | Mon Sep 26 09:32:00 UTC 2022 (+24hr) |
  ;;|              ca-summit | 01) crowning disabled |        0 (0.0%) |    85 (57.8%) |                 147.0 |    62 (42.2%) | Sun Sep 25 09:49:00 UTC 2022 | Mon Sep 26 09:32:00 UTC 2022 (+24hr) |
  ;;| id-caledonia-blackburn |          00) original |        0 (0.0%) |  188 (100.0%) |                 188.0 |      0 (0.0%) | Wed Sep 14 09:52:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+72hr) |
  ;;| id-caledonia-blackburn | 01) crowning disabled |        0 (0.0%) |  188 (100.0%) |                 188.0 |      0 (0.0%) | Wed Sep 14 09:52:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+72hr) |
  ;;| id-kootenai-rv-complex |          00) original |  10315 (198.9%) |  1282 (24.7%) |                5185.0 |  3903 (75.3%) | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 02:03:00 UTC 2022 (+ 6hr) |
  ;;| id-kootenai-rv-complex | 01) crowning disabled |    1221 (23.5%) |  1438 (27.7%) |                5185.0 |  3747 (72.3%) | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 02:03:00 UTC 2022 (+ 6hr) |
  ;;|              ca-summit |          00) original |           0 (-) |         0 (-) |                   0.0 |         0 (-) | Tue Sep 27 09:11:00 UTC 2022 | Wed Sep 28 09:43:00 UTC 2022 (+25hr) |
  ;;|              ca-summit | 01) crowning disabled |           0 (-) |         0 (-) |                   0.0 |         0 (-) | Tue Sep 27 09:11:00 UTC 2022 | Wed Sep 28 09:43:00 UTC 2022 (+25hr) |
  ;;|            mt-margaret |          00) original |   2611 (384.5%) |     31 (4.6%) |                 679.0 |   648 (95.4%) | Thu Sep 15 08:43:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+49hr) |
  ;;|            mt-margaret | 01) crowning disabled |    792 (116.6%) |     54 (8.0%) |                 679.0 |   625 (92.0%) | Thu Sep 15 08:43:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+49hr) |
  ;;|           wa-irving-pk |          00) original |        0 (0.0%) |   81 (100.0%) |                  81.0 |      0 (0.0%) | Thu Sep 15 10:26:00 UTC 2022 | Fri Sep 16 09:15:00 UTC 2022 (+23hr) |
  ;;|           wa-irving-pk | 01) crowning disabled |        0 (0.0%) |   81 (100.0%) |                  81.0 |      0 (0.0%) | Thu Sep 15 10:26:00 UTC 2022 | Fri Sep 16 09:15:00 UTC 2022 (+23hr) |
  ;;|            wa-siouoxon |          00) original |     271 (63.0%) |    90 (20.9%) |                 430.0 |   340 (79.1%) | Sun Sep 25 11:28:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+24hr) |
  ;;|            wa-siouoxon | 01) crowning disabled |     184 (42.8%) |   139 (32.3%) |                 430.0 |   291 (67.7%) | Sun Sep 25 11:28:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+24hr) |
  ;;| id-caledonia-blackburn |          00) original |        0 (0.0%) |  142 (100.0%) |                 142.0 |      0 (0.0%) | Sat Sep 17 09:47:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+11hr) |
  ;;| id-caledonia-blackburn | 01) crowning disabled |        0 (0.0%) |  142 (100.0%) |                 142.0 |      0 (0.0%) | Sat Sep 17 09:47:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+11hr) |
  ;;|         or-cedar-creek |          00) original |    1923 (10.1%) | 12723 (66.8%) |               19045.0 |  6322 (33.2%) | Fri Sep 16 09:15:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+24hr) |
  ;;|         or-cedar-creek | 01) crowning disabled |     1646 (8.6%) | 13031 (68.4%) |               19045.0 |  6014 (31.6%) | Fri Sep 16 09:15:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+24hr) |
  ;;|           wa-irving-pk |          00) original |   9202 (333.4%) |   580 (21.0%) |                2760.0 |  2180 (79.0%) | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |
  ;;|           wa-irving-pk | 01) crowning disabled |    2470 (89.5%) |   701 (25.4%) |                2760.0 |  2059 (74.6%) | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |
  ;;|         id-trail-ridge |          00) original |        0 (0.0%) |    2 (100.0%) |                   2.0 |      0 (0.0%) | Sat Sep 17 10:23:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+10hr) |
  ;;|         id-trail-ridge | 01) crowning disabled |        0 (0.0%) |    2 (100.0%) |                   2.0 |      0 (0.0%) | Sat Sep 17 10:23:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+10hr) |
  ;;| id-kootenai-rv-complex |          00) original |   8137 (228.1%) |   915 (25.7%) |                3567.0 |  2652 (74.3%) | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |
  ;;| id-kootenai-rv-complex | 01) crowning disabled |    2989 (83.8%) |   960 (26.9%) |                3567.0 |  2607 (73.1%) | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |
  ;;|               id-katka |          00) original |        0 (0.0%) |   12 (100.0%) |                  12.0 |      0 (0.0%) | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:12:00 UTC 2022 (+40hr) |
  ;;|               id-katka | 01) crowning disabled |        0 (0.0%) |   12 (100.0%) |                  12.0 |      0 (0.0%) | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:12:00 UTC 2022 (+40hr) |
  ;;|            or-sturgill |          00) original |  15066 (219.8%) |  1349 (19.7%) |                6854.0 |  5505 (80.3%) | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |
  ;;|            or-sturgill | 01) crowning disabled |    1469 (21.4%) |  1948 (28.4%) |                6854.0 |  4906 (71.6%) | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |
  ;;|          wa-bolt-creek |          00) original |     857 (20.5%) |  1004 (24.0%) |                4179.0 |  3175 (76.0%) | Thu Sep 22 09:54:00 UTC 2022 | Thu Sep 22 20:29:00 UTC 2022 (+11hr) |
  ;;|          wa-bolt-creek | 01) crowning disabled |       14 (0.3%) |  1105 (26.4%) |                4179.0 |  3074 (73.6%) | Thu Sep 22 09:54:00 UTC 2022 | Thu Sep 22 20:29:00 UTC 2022 (+11hr) |
  ;;| id-kootenai-rv-complex |          00) original |        0 (0.0%) |  488 (100.0%) |                 488.0 |      0 (0.0%) | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:04:00 UTC 2022 (+40hr) |
  ;;| id-kootenai-rv-complex | 01) crowning disabled |        0 (0.0%) |  488 (100.0%) |                 488.0 |      0 (0.0%) | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:04:00 UTC 2022 (+40hr) |
  ;;|             ca-rodgers |          00) original |        0 (0.0%) |   40 (100.0%) |                  40.0 |      0 (0.0%) | Fri Sep 16 16:28:00 UTC 2022 | Sat Sep 17 09:49:00 UTC 2022 (+17hr) |
  ;;|             ca-rodgers | 01) crowning disabled |        0 (0.0%) |   40 (100.0%) |                  40.0 |      0 (0.0%) | Fri Sep 16 16:28:00 UTC 2022 | Sat Sep 17 09:49:00 UTC 2022 (+17hr) |
  ;;|        wa-minnow-ridge |          00) original |     447 (18.6%) |   382 (15.9%) |                2408.0 |  2026 (84.1%) | Sun Sep 25 08:58:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+12hr) |
  ;;|        wa-minnow-ridge | 01) crowning disabled |       30 (1.2%) |   509 (21.1%) |                2408.0 |  1899 (78.9%) | Sun Sep 25 08:58:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+12hr) |
  ;;|          mt-government |          00) original |  1499 (1561.5%) |      9 (9.4%) |                  96.0 |    87 (90.6%) | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 09:28:00 UTC 2022 (+13hr) |
  ;;|          mt-government | 01) crowning disabled |      76 (79.2%) |    36 (37.5%) |                  96.0 |    60 (62.5%) | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 09:28:00 UTC 2022 (+13hr) |
  ;;|               id-katka |          00) original |    719 (463.9%) |    30 (19.4%) |                 155.0 |   125 (80.6%) | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |
  ;;|               id-katka | 01) crowning disabled |    205 (132.3%) |    36 (23.2%) |                 155.0 |   119 (76.8%) | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |
  ;;|          wa-goat-rocks |          00) original |    2110 (72.4%) |   675 (23.1%) |                2916.0 |  2241 (76.9%) | Mon Sep 26 11:08:00 UTC 2022 | Tue Sep 27 10:00:00 UTC 2022 (+23hr) |
  ;;|          wa-goat-rocks | 01) crowning disabled |     879 (30.1%) |   712 (24.4%) |                2916.0 |  2204 (75.6%) | Mon Sep 26 11:08:00 UTC 2022 | Tue Sep 27 10:00:00 UTC 2022 (+23hr) |
  ;;|          id-deep-creek |          00) original |      93 (64.1%) |    73 (50.3%) |                 145.0 |    72 (49.7%) | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:19:00 UTC 2022 (+11hr) |
  ;;|          id-deep-creek | 01) crowning disabled |      28 (19.3%) |    80 (55.2%) |                 145.0 |    65 (44.8%) | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:19:00 UTC 2022 (+11hr) |
  ;;|              ca-summit |          00) original |        0 (0.0%) |  1650 (67.0%) |                2461.0 |   811 (33.0%) | Sun Sep 18 10:21:00 UTC 2022 | Sun Sep 18 16:30:00 UTC 2022 (+ 6hr) |
  ;;|              ca-summit | 01) crowning disabled |        0 (0.0%) |  1650 (67.0%) |                2461.0 |   811 (33.0%) | Sun Sep 18 10:21:00 UTC 2022 | Sun Sep 18 16:30:00 UTC 2022 (+ 6hr) |
  ;;| id-isabella-lower-twin |          00) original |   1578 (426.5%) |   183 (49.5%) |                 370.0 |   187 (50.5%) | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 08:56:00 UTC 2022 (+47hr) |
  ;;| id-isabella-lower-twin | 01) crowning disabled |     210 (56.8%) |   204 (55.1%) |                 370.0 |   166 (44.9%) | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 08:56:00 UTC 2022 (+47hr) |
  ;;|             id-tenmile |          00) original |    775 (268.2%) |    47 (16.3%) |                 289.0 |   242 (83.7%) | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |
  ;;|             id-tenmile | 01) crowning disabled |    538 (186.2%) |    47 (16.3%) |                 289.0 |   242 (83.7%) | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |
  ;;|            ca-mosquito |          00) original |   47466 (43.6%) | 31656 (29.1%) |              108947.0 | 77291 (70.9%) | Wed Sep 14 22:01:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+12hr) |
  ;;|            ca-mosquito | 01) crowning disabled |   15640 (14.4%) | 36248 (33.3%) |              108947.0 | 72699 (66.7%) | Wed Sep 14 22:01:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+12hr) |

  ;; - IMPROVEMENT basic statistics across all rows - mean, stdev
  ;; filtered on ignition success
  ;; - pictures - Venn diagram ; 4 colors
  ;; - wind dir
  ;; -

  (-> replay-results
      (->> (filter #(-> % ::sim-output ::simulations/result)))
      (first)
      :gridfire.lab.replay/t0-fire
      keys sort)

  (->> snaps first (keys) (sort))

  ;; same set-logic stats on canonical tests.
  ;;

  ;; 1. Are we spreading too fast?
  ;; Experiment: compute the average (and maybe some quantiles) simulation-ToA in (sim ∩ real). If it's very small compared to (t1-t0)/2,
  ;; it's a sign that we're spreading too fast.
  (->> replay-results
       (pmap toa-stats)
       (vec))

  (def replay-results nil)

  ;; 2. Idea: find features informative about / correlated with prediction failure.
  ;; E.g angular distribution from the ignition-centroid -> actual spread centroid vector.

  ;; 2. Idea: calibrate based on some F-score

  ;; Idea: detect "failed-to-spread" regions:
  ;; - at the boundary of (real - sim) and (real ∩ sim)
  ;; - ToA < t1 (did not fail to spread for lack of time)
  ;; We can then find features that make these failed-to-spread regions distinctive (non-burnable fuels?).

  (def replay-results1 (time (replay-snapshots 20 snaps)))

  (let [prof-maps (for [replay-res replay-results
                        [variation-name sim-output] (sort-by key (::sim-outputs replay-res))
                        rec-maps   (->> sim-output ::sampled-values (vals))]
                    (-> rec-maps
                        (->> (reverse) (into (sorted-map)))
                        (merge {"Fire name" (:pyrcst_fire_name replay-res)
                                "Variation" variation-name})
                        (as-> m
                              (let [submap-names ["surface-fire-min"
                                                  "surface-fire-max"]]
                                (-> m
                                    (into (for [submap-name submap-names
                                                :let [submap (get m submap-name)]
                                                [k v] submap]
                                            [(str submap-name " " k) v]))
                                    (as-> m (apply dissoc m submap-names)))))))]
    (->> prof-maps (mapcat keys) (into (sorted-set)) (vec))
    (pprint/print-table
     ["Variation"
      "Fire name"

      "canopy-cover"
      "canopy-height"
      "canopy-base-height"
      "fire-line-intensity (kW/m)"
      "critical-intensity (kW/m)"
      "crown-bulk-density"
      "surface-fire-min :reaction-intensity"

      "residence-time"
      "surface-fire-min :residence-time"

      "slope"

      "temperature"
      "wind-speed-20ft"
      "wind-from-direction"
      "relative-humidity"
      "foliar-moisture (%)"

      "surface-fire-min :fuel-bed-depth"
      "surface-fire-min :heat-of-combustion"

      "aspect"
      "wind-from-direction"
      "max-spread-direction"

      "surface-fire-max :eccentricity"
      "surface-fire-max :effective-wind-speed"
      "surface-fire-max :max-spread-direction"
      "surface-fire-min :spread-rate"
      "surface-fire-max :max-spread-rate"
      "fire-line-intensity"
      "eccentricity"
      "max-spread-rate"]
     (->> prof-maps (sort-by hash) (take 200))))
  ;;|             Variation |              Fire name | canopy-cover |      canopy-height | canopy-base-height | fire-line-intensity (kW/m) | critical-intensity (kW/m) |    crown-bulk-density | surface-fire-min :reaction-intensity |      residence-time | surface-fire-min :residence-time |                slope | temperature |    wind-speed-20ft | wind-from-direction | relative-humidity | foliar-moisture (%) | surface-fire-min :fuel-bed-depth | surface-fire-min :heat-of-combustion | aspect | wind-from-direction | max-spread-direction | surface-fire-max :eccentricity | surface-fire-max :effective-wind-speed | surface-fire-max :max-spread-direction | surface-fire-min :spread-rate | surface-fire-max :max-spread-rate |   fire-line-intensity |        eccentricity |       max-spread-rate |
  ;;|-----------------------+------------------------+--------------+--------------------+--------------------+----------------------------+---------------------------+-----------------------+--------------------------------------+---------------------+----------------------------------+----------------------+-------------+--------------------+---------------------+-------------------+---------------------+----------------------------------+--------------------------------------+--------+---------------------+----------------------+--------------------------------+----------------------------------------+----------------------------------------+-------------------------------+-----------------------------------+-----------------------+---------------------+-----------------------|
  ;;|          00) original |               id-lemhi |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                   6152.2868922139105 | 0.22971884333771272 |              0.22971884333771272 |  0.48773258924484253 |        80.0 |  2.093663215637207 |  196.81979370117188 |              20.0 |                     |                              1.0 |                               8000.0 |  208.0 |  196.81979370117188 |                 28.0 |             0.8216781800353528 |                      265.5961745675055 |                                   28.0 |            0.9712283859611661 |                 6.356538101551093 |     149.7278554483121 |  0.8216781800353528 |     6.356538101551093 |
  ;;| 01) crowning disabled |        wa-minnow-ridge |         45.0 |   62.3390007019043 |   62.3390007019043 |          521.6547261233543 |         15843.00408983124 |  0.004991999827325344 |                    7659.756729374807 |  0.3138266174914029 |               0.3138266174914029 |    0.466307669878006 |        80.0 |  4.031521797180176 |    216.196044921875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  168.0 |    216.196044921875 |    353.4649584555731 |             0.7917852757118985 |                     224.29382760793757 |                      353.4649584555731 |             0.742004354224792 |                3.7613717245390816 |     150.6953175017878 |  0.7917852757118985 |    3.7613717245390816 |
  ;;|          00) original |               id-lemhi |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                   2579.8031373954946 | 0.21012730729142196 |              0.21012730729142196 |   0.6494075655937195 |        80.0 | 2.8465466499328613 |    213.910400390625 |              20.0 |                     |                              1.5 |                               8000.0 |  159.0 |    213.910400390625 |    346.1636424128775 |             0.8682927155267994 |                      357.6027868243682 |                      346.1636424128775 |             1.955748956092849 |                 31.02220729301808 |    280.27896619104524 |  0.8682927155267994 |     31.02220729301808 |
  ;;|          00) original | id-isabella-lower-twin |         75.0 |  88.58699798583984 | 0.9843000173568726 |          627.4870608761058 |         31.43323063684178 |  0.013728000223636627 |                    7380.473712702578 |  0.3138266174914029 |               0.3138266174914029 |  0.48773258924484253 |        80.0 |  6.539989471435547 |   262.8527526855469 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  241.0 |   262.8527526855469 |    63.84875263791583 |             0.8115585961876153 |                     250.46012937313307 |                      63.84875263791583 |            0.8337306634188677 |                 4.695681405844607 |     8048.251944719745 |  0.8350284778993993 |     49.06194512337178 |
  ;;|          00) original |            ca-mosquito |         68.0 | 114.83499908447266 |  6.561999797821045 |         505.11910258841544 |          541.068310789092 |  0.008112000301480293 |                    7832.734884995386 |  0.3138266174914029 |               0.3138266174914029 |   0.3443275988101959 |        80.0 |  4.630301475524902 |   78.13909912109375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  117.0 |   78.13909912109375 |   290.80369290909164 |             0.6992932446028509 |                     140.42138991318205 |                     290.80369290909164 |             1.063852246987509 |                3.5617091478285934 |      145.918516077586 |  0.6992932446028509 |    3.5617091478285934 |
  ;;|          00) original | id-kootenai-rv-complex |         55.0 |  75.46299743652344 |  2.624799966812134 |         145.72627998289934 |        136.88066227665723 |  0.006864000111818314 |                   1842.4212411329784 |   0.217382350167601 |                0.217382350167601 |    0.600860595703125 |        80.0 |  5.524073123931885 |  202.66102600097656 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |    9.0 |  202.66102600097656 |   188.05589578353806 |             0.8399406316996749 |                     296.63475541386845 |                     188.05589578353806 |            0.6805345824568816 |                 6.306559955354645 |     2178.566006147533 |  0.8062772920366215 |     32.04950473725692 |
  ;;|          00) original |         or-cedar-creek |         65.0 | 101.71099853515625 |  24.93560028076172 |          53.36342800817936 |         4007.998232258368 | 0.0074880002066493034 |                   1817.5602991526362 | 0.19835510292868358 |              0.19835510292868358 |   0.5773502588272095 |        80.0 | 1.3580442667007446 |  43.418365478515625 |              20.0 |  110.00000000000001 |                              0.3 |                               8000.0 |  283.0 |  43.418365478515625 |   103.26191574774583 |             0.8500563525213036 |                     316.32261296572244 |                     103.26191574774583 |           0.39832931267457755 |                2.5655434642995343 |    15.415596416497921 |  0.8500563525213036 |    2.5655434642995343 |
  ;;|          00) original |            ca-mosquito |         21.0 |  39.37200164794922 |  2.952899932861328 |                            |                           | 0.0037440001033246517 |                   1019.0418967578071 | 0.21098446587998754 |              0.21098446587998754 |    0.554309070110321 |        80.0 |  7.331422328948975 |    269.997314453125 |              20.0 |                     |                              1.0 |                               8000.0 |  129.0 |    269.997314453125 |    322.6099951632418 |             0.7910243002874567 |                     223.36578943505918 |                      322.6099951632418 |             2.048445753014511 |                20.265775622551907 |     72.61970831786739 |  0.7910243002874567 |    20.265775622551907 |
  ;;|          00) original |            ca-mosquito |         44.0 |   62.3390007019043 |  3.609100103378296 |         378.03830609469236 |         220.6968690160058 |  0.004991999827325344 |                    8583.023538942785 |  0.3138266174914029 |               0.3138266174914029 |   0.1051042377948761 |        80.0 |  7.268346786499023 |  234.52694702148438 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   13.0 |  234.52694702148438 |      62.320203273608 |             0.5304499940107756 |                      63.23329379831361 |                        62.320203273608 |            1.2669368550152817 |                2.4326161792817587 |    1777.9546275732316 |  0.8517429912141797 |     42.68919546940923 |
  ;;| 01) crowning disabled |        wa-minnow-ridge |         75.0 |  88.58699798583984 |  88.58699798583984 |          718.8990519704455 |        26838.161192852684 |  0.013728000223636627 |                    8131.388158581338 |  0.3138266174914029 |               0.3138266174914029 |   0.4452286958694458 |        80.0 |  8.447413444519043 |   98.99395751953125 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   95.0 |   98.99395751953125 |                275.0 |             0.7971452416705564 |                     230.98647660266315 |                                  275.0 |            0.9368379292355233 |                 4.882939056018482 |    207.67514500156742 |  0.7971452416705564 |     4.882939056018482 |
  ;;|          00) original | id-columbus-bear-gulch |         45.0 | 36.090999603271484 |  1.312399983406067 |          434.5291695894612 |         48.39462225456499 | 0.0074880002066493034 |                    6574.428101909084 |  0.3138266174914029 |               0.3138266174914029 |   0.5317094326019287 |        80.0 |  2.913816452026367 |     167.64990234375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  300.0 |     167.64990234375 |   116.24263545580834 |             0.8090117345573714 |                     246.85080124076345 |                     116.24263545580834 |            0.6572720057754996 |                3.6503881107230236 |      338.537411101574 |   0.680211601782788 |     6.134583747684881 |
  ;;| 01) crowning disabled |            ca-mosquito |         47.0 |  85.30599975585938 |  85.30599975585938 |          512.6550568637955 |        25361.045058233514 |  0.005615999922156334 |                     7902.72590312213 |  0.3138266174914029 |               0.3138266174914029 |  0.38386404514312744 |        80.0 |  4.538540840148926 |   271.0155029296875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  153.0 |   271.0155029296875 |   344.82458541690914 |             0.6962096027972272 |                     138.36096129695366 |                     344.82458541690914 |            1.0832338512453497 |                3.5828317355526624 |    148.09549821795062 |  0.6962096027972272 |    3.5828317355526624 |
  ;;| 01) crowning disabled |            ca-mosquito |         38.0 |  72.18199920654297 |  72.18199920654297 |                            |                           | 0.0043680001981556416 |                    7855.755905950394 |  0.3138266174914029 |               0.3138266174914029 |  0.15838444232940674 |        80.0 | 1.4928983449935913 |   215.5120391845703 |              20.0 |                     |                              1.0 |                               8000.0 |  247.0 |   215.5120391845703 |   58.099152821215284 |             0.4526514477757167 |                      42.75711603999461 |                     58.099152821215284 |            1.0679372955181812 |                1.6885508708675174 |     69.38101599541491 |  0.4526514477757167 |    1.6885508708675174 |
  ;;| 01) crowning disabled |            ca-mosquito |         67.0 | 111.55400085449219 | 111.55400085449219 |          657.5833756714301 |         37924.99272147336 |  0.008112000301480293 |                    8668.895380068247 |  0.3138266174914029 |               0.3138266174914029 |   0.3249197006225586 |        80.0 |  6.765270709991455 |  241.59686279296875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  293.0 |  241.59686279296875 |   100.14965078099027 |             0.6921317465703826 |                     135.68918711321444 |                     100.14965078099027 |             1.286987555603088 |                 4.189528503772019 |    189.96230766875271 |  0.6921317465703826 |     4.189528503772019 |
  ;;| 01) crowning disabled |            ca-mosquito |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                   3365.6229818824945 |  0.2283338581461741 |               0.2283338581461741 |  0.06992681324481964 |        80.0 | 5.2178802490234375 |  237.71560668945312 |              20.0 |                     |                              3.0 |                               8000.0 |  152.0 |  237.71560668945312 |    56.52434442913989 |             0.7803313992232508 |                     210.87019308269208 |                      56.52434442913989 |             2.053850144115899 |                17.833398677641313 |     228.4118586457962 |  0.7803313992232508 |    17.833398677641313 |
  ;;|          00) original |            ca-mosquito |         65.0 | 114.83499908447266 |  6.890100002288818 |         1057.1170272179543 |         582.1515630707672 | 0.0074880002066493034 |                    7913.539267854313 |  0.3138266174914029 |               0.3138266174914029 |   0.5773502588272095 |        80.0 | 1.6414607763290405 |    329.696044921875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  331.0 |    329.696044921875 |                151.0 |             0.8435151422514039 |                       303.364866604731 |                                  151.0 |            1.0848052781246151 |                 7.377859702184428 |     1384.606318919649 |  0.5581355528609002 |    10.013965174237738 |
  ;;|          00) original |        wa-minnow-ridge |         55.0 |  75.46299743652344 |  5.249599933624268 |          395.6676395288223 |        387.15697803651994 |  0.006864000111818314 |                    7997.156144586368 |  0.3138266174914029 |               0.3138266174914029 |  0.24932800233364105 |        80.0 |  7.829322338104248 |    104.923095703125 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  125.0 |    104.923095703125 |    295.4671897485481 |             0.6693572008342449 |                     121.79284199253493 |                      295.4671897485481 |            0.9149557930162706 |                2.7325809875602367 |     6466.791481131798 |  0.8628903455694295 |     98.85714596092859 |
  ;;|          00) original | id-columbus-bear-gulch |         45.0 |  49.21500015258789 | 0.9843000173568726 |         144.53666096879684 |         31.43323063684178 |  0.004991999827325344 |                   2858.7930042828193 | 0.21692336615184582 |              0.21692336615184582 |    0.600860595703125 |        80.0 |  8.282451629638672 |  263.23162841796875 |              20.0 |  110.00000000000001 |                              0.3 |                               8000.0 |  186.0 |  263.23162841796875 |   13.131814458943778 |             0.8666287192014795 |                     353.47647756043517 |                     13.131814458943778 |            0.6515744023067206 |                  4.03977180441428 |    1147.9065988609816 |  0.8709754695879962 |     34.45706720506592 |
  ;;|          00) original |            mt-billiard |         55.0 |  75.46299743652344 |  5.249599933624268 |         25.505772950142998 |        387.15697803651994 |  0.006864000111818314 |                    1516.011474155147 | 0.22971884333771272 |              0.22971884333771272 |    0.554309070110321 |        80.0 | 2.6616756916046143 |   272.6834411621094 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  222.0 |   272.6834411621094 |    43.25906166221961 |             0.8355431524669894 |                     288.67000228161123 |                      43.25906166221961 |            0.1756887017949409 |                1.2694255697373622 |    7.3680930323660565 |  0.8355431524669894 |    1.2694255697373622 |
  ;;|          00) original | id-columbus-bear-gulch |         15.0 | 36.090999603271484 |  2.952899932861328 |                            |                           |  0.003120000008493662 |                4.887441800717763E-12 | 0.22971884333771272 |              0.22971884333771272 |      0.5095254778862 |        80.0 |  2.945021152496338 |  185.82406616210938 |              20.0 |                     |                              1.0 |                               8000.0 |  287.0 |  185.82406616210938 |                  0.0 |                            0.0 |                                    0.0 |                                    0.0 |         4.914862028502479E-16 |             4.914862028502479E-16 | 9.196832992268804E-30 |                 0.0 | 4.914862028502479E-16 |
  ;;| 01) crowning disabled |            ca-mosquito |         29.0 |   68.9010009765625 |   68.9010009765625 |                            |                           |  0.003120000008493662 |                   3165.0449892378465 |  0.2283338581461741 |               0.2283338581461741 |   0.4244748055934906 |        80.0 | 3.4706811904907227 |   82.68026733398438 |              20.0 |                     |                              3.0 |                               8000.0 |  125.0 |   82.68026733398438 |   300.37744279451863 |             0.7573142313613783 |                      187.0087314952444 |                     300.37744279451863 |            1.9074903651459276 |                14.303240665459498 |     172.2794189507987 |  0.7573142313613783 |    14.303240665459498 |
  ;;|          00) original |            ca-mosquito |         67.0 |   82.0250015258789 | 3.9372000694274902 |          634.6987533726874 |        251.46584509473425 |  0.010607999749481678 |                    8187.046767620315 |  0.3138266174914029 |               0.3138266174914029 |   0.4244748055934906 |        80.0 |  5.537454128265381 |  231.25347900390625 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   92.0 |  231.25347900390625 |      279.79510276181 |             0.7221017081474693 |                      156.8272132655204 |                        279.79510276181 |             1.165656020993763 |                 4.281721937251236 |    2530.9635057418404 |  0.8067059413363571 |     21.25547943683844 |
  ;;|          00) original |            ca-mosquito |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                    765.4107460806198 | 0.21098446587998754 |              0.21098446587998754 |  0.19438031315803528 |        80.0 |  2.574422597885132 |  193.71856689453125 |              20.0 |                     |                              1.0 |                               8000.0 |  135.0 |  193.71856689453125 |   350.77527442201324 |             0.6407768943730999 |                      106.4962302973496 |                     350.77527442201324 |            1.3354529987252097 |                 5.377728398036172 |    14.474136035582934 |  0.6407768943730999 |     5.377728398036172 |
  ;;|          00) original |            ca-mosquito |         52.0 | 108.27300262451172 |  6.890100002288818 |         215.60237201264684 |         582.1515630707672 |  0.004991999827325344 |                    7754.954542663765 |  0.3138266174914029 |               0.3138266174914029 |  0.14054083824157715 |        80.0 | 1.6575403213500977 |  194.70547485351562 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  201.0 |  194.70547485351562 |                 21.0 |            0.42253391169587756 |                      36.37217562804306 |                                   21.0 |            1.0370750796844421 |                1.5355089921347445 |    62.283089326217464 | 0.42253391169587756 |    1.5355089921347445 |
  ;;| 01) crowning disabled |               id-lemhi |         15.0 | 36.090999603271484 | 36.090999603271484 |                            |                           |  0.003120000008493662 |                   2663.1058032703004 | 0.21012730729142196 |              0.21012730729142196 |    0.554309070110321 |        80.0 |  3.595763921737671 |  258.21221923828125 |              20.0 |                     |                              1.5 |                               8000.0 |  275.0 |  258.21221923828125 |     93.0608836448497 |              0.837204316657259 |                     291.63921824199315 |                       93.0608836448497 |            2.0319654413304438 |                24.463559686364846 |    228.15989967309727 |   0.837204316657259 |    24.463559686364846 |
  ;;|          00) original |        wa-minnow-ridge |         65.0 | 101.71099853515625 | 7.8744001388549805 |         244.01288189910642 |           711.25281721317 | 0.0074880002066493034 |                    7348.131313906286 |  0.3138266174914029 |               0.3138266174914029 |   0.3249197006225586 |        80.0 |  2.600282907485962 |  131.21231079101562 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  276.0 |  131.21231079101562 |    91.54860981683873 |             0.6328574644178984 |                     102.62276310069677 |                      91.54860981683873 |            0.6987772118140155 |                1.8340616563100394 |     70.49030109547365 |  0.6328574644178984 |    1.8340616563100394 |
  ;;|          00) original |            ca-mosquito |         66.0 |  75.46299743652344 |  3.609100103378296 |          826.6591828585987 |         220.6968690160058 |  0.009983999654650688 |                    7726.056009656106 |  0.3138266174914029 |               0.3138266174914029 |   0.5317094326019287 |        80.0 |  2.515465259552002 |  104.14871215820312 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  356.0 |  104.14871215820312 |   178.60268521736577 |             0.8152903527828183 |                     255.88976336658743 |                     178.60268521736577 |            1.0276814452873677 |                 5.909445451726743 |    1247.8656719170453 |  0.6490049119421031 |    10.549302119146791 |
  ;;|          00) original |           wa-irving-pk |         65.0 |   62.3390007019043 |  2.624799966812134 |          505.5295341909708 |        136.88066227665723 |  0.009983999654650688 |                    8050.969968367119 |  0.3138266174914029 |               0.3138266174914029 |  0.40402624011039734 |        80.0 |  4.977797031402588 |   155.0367431640625 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  183.0 |   155.0367431640625 |    358.5792373004672 |              0.756168824260314 |                     185.91656721420705 |                      358.5792373004672 |            0.8132311413260591 |                 3.467978628455019 |     6910.249953262493 |  0.7874038865535439 |     85.09349251458164 |
  ;;|          00) original |            ca-mosquito |         65.0 | 104.99199676513672 |  5.905799865722656 |          1331.125405283245 |          461.972228189994 | 0.0074880002066493034 |                    8532.271733903792 |  0.3138266174914029 |               0.3138266174914029 |   0.5773502588272095 |        80.0 |   8.33970832824707 |  258.92071533203125 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  340.0 |  258.92071533203125 |   151.76643963799006 |             0.8451219563546744 |                     306.46880405766586 |                     151.76643963799006 |            1.2540702573671834 |                 8.616529513733347 |    10305.748292342465 |  0.8719444463555227 |    100.28755737556675 |
  ;;|          00) original |         or-cedar-creek |         65.0 | 101.71099853515625 |  7.218200206756592 |          54.87369020572321 |         624.2250106714278 | 0.0074880002066493034 |                   1364.2983282181394 | 0.22412908981846083 |              0.22412908981846083 |   0.6745085120201111 |        80.0 |  5.312047481536865 |   340.9200439453125 |              20.0 |  110.00000000000001 |                              0.6 |                               8000.0 |  278.0 |   340.9200439453125 |    99.89011535188793 |             0.8818590027069938 |                     394.53266273288415 |                      99.89011535188793 |            0.3460278213708999 |                3.1104588092917367 |    15.851880092217941 |  0.8818590027069938 |    3.1104588092917367 |
  ;;|          00) original |            ca-mosquito |         38.0 |  72.18199920654297 |  4.921500205993652 |                            |                           | 0.0043680001981556416 |                    7826.277747017552 |  0.3138266174914029 |               0.3138266174914029 |  0.19438031315803528 |        80.0 | 2.8544299602508545 |   72.27606201171875 |              20.0 |                     |                              1.0 |                               8000.0 |   97.0 |   72.27606201171875 |    268.0476891747088 |             0.5434565728722464 |                      67.32852949182218 |                      268.0476891747088 |            1.0627887464853993 |                 2.115412643229239 |     86.59421463074153 |  0.5434565728722464 |     2.115412643229239 |
  ;;| 01) crowning disabled |           wa-irving-pk |         55.0 |  88.58699798583984 |  88.58699798583984 |         235.45251070481999 |        26838.161192852684 |  0.006864000111818314 |                    7328.113171288817 |  0.3138266174914029 |               0.3138266174914029 |  0.24932800233364105 |        80.0 |  5.558180809020996 |     83.494873046875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  144.0 |     83.494873046875 |    302.5901222840041 |             0.6135995731975343 |                      93.78554554508455 |                      302.5901222840041 |            0.7208796193204248 |                 1.774554100202734 |     68.01738598427977 |  0.6135995731975343 |     1.774554100202734 |
  ;;| 01) crowning disabled |               id-lemhi |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                    1166.166572497857 | 0.21098446587998754 |              0.21098446587998754 |  0.19438031315803528 |        80.0 | 0.9168508052825928 |   268.1692199707031 |              20.0 |                     |                              1.0 |                               8000.0 |  173.0 |   268.1692199707031 |   11.923289485093619 |             0.5302405795246055 |                      63.16913503932763 |                     11.923289485093619 |            2.6758667071479905 |                 6.463906423834123 |     26.50665218952122 |  0.5302405795246055 |     6.463906423834123 |
  ;;| 01) crowning disabled |            or-sturgill |         45.0 |  75.46299743652344 |  75.46299743652344 |          58.40930615708683 |        21100.796906307733 |  0.004991999827325344 |                   3411.8378594242154 | 0.21692336615184582 |              0.21692336615184582 |  0.14054083824157715 |        80.0 |  6.970670223236084 |  139.58273315429688 |              20.0 |  110.00000000000001 |                              0.3 |                               8000.0 |  232.0 |  139.58273315429688 |   351.39359980084737 |             0.5542536498706487 |                      70.90013129288432 |                     351.39359980084737 |            0.9000066560617687 |                1.3679026473205496 |     16.87324679642592 |  0.5542536498706487 |    1.3679026473205496 |
  ;;|          00) original |        wa-minnow-ridge |         65.0 | 101.71099853515625 |  3.609100103378296 |          848.4813331569235 |         220.6968690160058 | 0.0074880002066493034 |                      8544.7399740707 |  0.3138266174914029 |               0.3138266174914029 |      0.5095254778862 |        80.0 | 2.9839539527893066 |   231.0802001953125 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  197.0 |   231.0802001953125 |    18.77974250734961 |             0.8117381992064782 |                     250.71755955304906 |                      18.77974250734961 |            0.9727870008775998 |                 5.484304085710783 |     3039.418534696989 |  0.6852210417764236 |     28.52939405605024 |
  ;;|          00) original |         or-cedar-creek |         65.0 | 127.95899963378906 |  32.48189926147461 |         20.571010698882997 |         5958.815004183507 | 0.0074880002066493034 |                   2552.1063408955392 | 0.21692336615184582 |              0.21692336615184582 |  0.17632697522640228 |        80.0 |  1.300394058227539 |     59.104736328125 |              20.0 |  110.00000000000001 |                              0.3 |                               8000.0 |  294.0 |     59.104736328125 |   116.90899398338533 |             0.5224827128974266 |                     60.830647932615115 |                     116.90899398338533 |            0.4543886274658087 |                0.6440482289047791 |     5.942541749095184 |  0.5224827128974266 |    0.6440482289047791 |
  ;;|          00) original |            ca-mosquito |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                   3407.7700322780847 |  0.2283338581461741 |               0.2283338581461741 |  0.17632697522640228 |        80.0 |  6.526383876800537 |  228.25376892089844 |              20.0 |                     |                              3.0 |                               8000.0 |  179.0 |  228.25376892089844 |   44.292305720506135 |             0.8280090842576239 |                       275.770264010387 |                     44.292305720506135 |            2.0867791043446173 |                25.393367982160576 |     329.3135875943643 |  0.8280090842576239 |    25.393367982160576 |
  ;;|          00) original |        wa-minnow-ridge |         75.0 |  88.58699798583984 |  5.249599933624268 |          158.8199172748726 |        387.15697803651994 |  0.013728000223636627 |                   7352.8226128614915 |  0.3138266174914029 |               0.3138266174914029 |  0.23086819052696228 |        80.0 |  6.823634147644043 |      263.8486328125 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  121.0 |      263.8486328125 |    346.2050993563899 |             0.4447309219893325 |                     41.004421382513904 |                      346.2050993563899 |            0.7680410127002281 |                 1.192968462384285 |    45.879806432895485 |  0.4447309219893325 |     1.192968462384285 |
  ;;|          00) original | id-columbus-bear-gulch |         85.0 |  88.58699798583984 |  4.593400001525879 |          915.9213604241659 |        316.88317388600854 |  0.018719999119639397 |                    7114.712208762804 |  0.3138266174914029 |               0.3138266174914029 |    0.700207531452179 |        80.0 |  2.368009090423584 |   276.4334716796875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  331.0 |   276.4334716796875 |   150.03738781360056 |             0.8894195824372925 |                     418.08622001157374 |                     150.03738781360056 |             0.752046523801912 |                  7.11015283103047 |     2193.685493691315 |  0.6361028715972248 |     9.201588180674337 |
  ;;|          00) original |        wa-minnow-ridge |         65.0 |  88.58699798583984 |  5.905799865722656 |          964.0300090891706 |          461.972228189994 |  0.009983999654650688 |                    7804.263547656061 |  0.3138266174914029 |               0.3138266174914029 |   0.6494075655937195 |        80.0 |  6.936948776245117 |    125.705810546875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  257.0 |    125.705810546875 |    72.38406751618146 |             0.8628022782463174 |                      344.2830776503092 |                      72.38406751618146 |            0.8825509721634992 |                 6.822392671575144 |      9338.34204013981 |   0.844481567163017 |     82.31351070269872 |
  ;;|          00) original |               id-lemhi |         45.0 |  49.21500015258789 | 0.9843000173568726 |           423.811173772631 |         31.43323063684178 |  0.004991999827325344 |                    3574.801523181199 | 0.21692336615184582 |              0.21692336615184582 |   0.7812856435775757 |        80.0 |  4.252416610717773 |   251.7484893798828 |              20.0 |  110.00000000000001 |                              0.3 |                               8000.0 |  263.0 |   251.7484893798828 |                 83.0 |             0.9122757791059072 |                      507.4243406675079 |                                   83.0 |            0.9734751098696889 |                 9.472879113489782 |    1395.5689151222196 |  0.7574161722255408 |    39.658731270508454 |
  ;;|          00) original |               id-lemhi |         25.0 | 36.090999603271484 | 1.9686000347137451 |                            |                           | 0.0037440001033246517 |                     2476.61092677706 | 0.21012730729142196 |              0.21012730729142196 |   0.6248693466186523 |        80.0 |  4.890122413635254 |  203.66098022460938 |              20.0 |                     |                              1.5 |                               8000.0 |  325.0 |  203.66098022460938 |   139.19467863966963 |             0.8443409862915394 |                      304.9539435867013 |                     139.19467863966963 |            1.8609428302972566 |                 23.78646087887814 |     206.3093253638137 |  0.8443409862915394 |     23.78646087887814 |
  ;;|          00) original |               id-lemhi |         55.0 |   62.3390007019043 | 3.9372000694274902 |          767.5179921674588 |        251.46584509473425 |  0.006864000111818314 |                    6202.950028616129 | 0.22971884333771272 |              0.22971884333771272 |   0.6494075655937195 |        80.0 |  4.076582908630371 |   241.9470977783203 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  310.0 |   241.9470977783203 |   127.88412879705749 |             0.8699673169605389 |                      361.8375684613872 |                     127.88412879705749 |            0.9805177384777416 |                 9.336014627813949 |    1963.8494134750813 |  0.7491158235008449 |    32.594034308204975 |
  ;;|          00) original |               id-lemhi |         35.0 |   62.3390007019043 | 7.8744001388549805 |                            |                           | 0.0037440001033246517 |                   2117.2122916928097 |   0.217382350167601 |                0.217382350167601 |   0.6745085120201111 |        80.0 |  6.177764415740967 |  220.00112915039062 |              20.0 |                     |                              1.0 |                               8000.0 |  293.0 |  220.00112915039062 |   108.23801998947067 |             0.8740315345623928 |                      372.4748338992636 |                     108.23801998947067 |            1.1751383707301324 |                 14.63392495182644 |    112.25307830671343 |  0.8740315345623928 |     14.63392495182644 |
  ;;|          00) original |            ca-mosquito |         47.0 |  95.14900207519531 |   6.23390007019043 |          386.6322007484547 |          500.999779032296 |  0.005615999922156334 |                    8628.666859020306 |  0.3138266174914029 |               0.3138266174914029 |   0.1051042377948761 |        80.0 |  6.207117557525635 |  242.42237854003906 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  239.0 |  242.42237854003906 |                 59.0 |             0.5338081095860032 |                       64.2696937764574 |                                   59.0 |            1.2770892097102369 |                 2.474756112853115 |    111.69008796524521 |  0.5338081095860032 |     2.474756112853115 |
  ;;|          00) original |          wa-goat-rocks |         75.0 |  88.58699798583984 | 12.139699935913086 |          87.82275448270023 |        1361.4751165363427 |  0.013728000223636627 |                    4604.796707969722 | 0.22152102232360932 |              0.22152102232360932 |  0.15838444232940674 |        80.0 |   2.22289776802063 |      173.2158203125 |              20.0 |  110.00000000000001 |                              0.6 |                               8000.0 |  311.0 |      173.2158203125 |   123.23000385340566 |            0.47294477094396686 |                      47.50421931108491 |                     123.23000385340566 |            1.1296835973487314 |                1.4922772127331168 |    25.370186845623532 | 0.47294477094396686 |    1.4922772127331168 |
  ;;|          00) original |            mt-billiard |         65.0 |  88.58699798583984 |  5.905799865722656 |          858.2109787383764 |          461.972228189994 |  0.009983999654650688 |                    7517.831068463927 |  0.3138266174914029 |               0.3138266174914029 |   0.5773502588272095 |        80.0 | 4.1738786697387695 |  278.39886474609375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  288.0 |  278.39886474609375 |                108.0 |             0.8496723569684143 |                     315.53776886281156 |                                  108.0 |            0.8911077605191343 |                 6.304920130531578 |     2436.181181115005 |   0.753764437118371 |     19.88150246404969 |
  ;;|          00) original |               id-lemhi |         35.0 |   62.3390007019043 | 7.8744001388549805 |                            |                           | 0.0037440001033246517 |                   1859.1874916224308 |   0.217382350167601 |                0.217382350167601 |    0.700207531452179 |        80.0 | 1.0776437520980835 |      24.26513671875 |              20.0 |                     |                              1.0 |                               8000.0 |  295.0 |      24.26513671875 |    115.3899916088583 |             0.8784082016451698 |                     384.53865081583297 |                      115.3899916088583 |            0.9587829472382998 |                12.452412203316342 |     83.87831674598428 |  0.8784082016451698 |    12.452412203316342 |
  ;;| 01) crowning disabled |            ca-mosquito |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                    5119.915819228796 | 0.30680487360142095 |              0.30680487360142095 |   0.9004040360450745 |        80.0 |  5.550533294677734 |   246.0772705078125 |              20.0 |                     |                              6.0 |                               8000.0 |  185.0 |   246.0772705078125 |    24.77005416229023 |             0.9282442581095335 |                      594.3097599535338 |                      24.77005416229023 |            3.0720737191908514 |                114.53804444468261 |    2998.6348781606907 |  0.9282442581095335 |    114.53804444468261 |
  ;;|          00) original | id-kootenai-rv-complex |         55.0 |   62.3390007019043 | 2.2967000007629395 |          623.8228235931384 |        112.03512054935626 |  0.006864000111818314 |                    7308.494304146861 |  0.3138266174914029 |               0.3138266174914029 |   0.6248693466186523 |        80.0 | 3.7117786407470703 |    169.518310546875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  329.0 |    169.518310546875 |   147.65460736203875 |             0.8535963894360992 |                      323.7081996385064 |                     147.65460736203875 |            0.6492981400699789 |                 4.714237228931518 |    1568.5761078155135 |  0.7303500872963171 |    25.265668367408765 |
  ;;|          00) original |            ca-mosquito |         19.0 |  65.62000274658203 |  5.249599933624268 |                            |                           |  0.002495999913662672 |                    3421.865637681625 |  0.2283338581461741 |               0.2283338581461741 |      0.5095254778862 |        80.0 |    6.3645920753479 |  233.85214233398438 |              20.0 |                     |                              3.0 |                               8000.0 |  101.0 |  233.85214233398438 |   295.85833726804304 |             0.7688362188707816 |                     198.47672687815285 |                     295.85833726804304 |            2.0976924174977714 |                16.908909068757083 |      220.190007291581 |  0.7688362188707816 |    16.908909068757083 |
  ;;| 01) crowning disabled |               id-lemhi |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                   2547.3555888323403 | 0.21012730729142196 |              0.21012730729142196 |   0.8692867159843445 |        80.0 |  2.380803346633911 |   139.4661865234375 |              20.0 |                     |                              1.5 |                               8000.0 |  265.0 |   139.4661865234375 |    81.47741543949263 |              0.907071519360335 |                      484.1510954188742 |                      81.47741543949263 |            1.9265500659601125 |                46.465065468205225 |     414.5217960299979 |   0.907071519360335 |    46.465065468205225 |
  ;;| 01) crowning disabled |            ca-mosquito |         43.0 |   82.0250015258789 |   82.0250015258789 |         419.85433407035316 |         23912.06875011421 |  0.004991999827325344 |                    7689.779444845635 |  0.3138266174914029 |               0.3138266174914029 |   0.3249197006225586 |        80.0 | 1.8360458612442017 |   85.72442626953125 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   52.0 |   85.72442626953125 |   235.19242854126958 |             0.6674281559177635 |                      120.6899431225504 |                     235.19242854126958 |             1.016881049167188 |                 3.015524372339407 |    121.28727874742239 |  0.6674281559177635 |     3.015524372339407 |
  ;;|          00) original | id-kootenai-rv-complex |         55.0 |   62.3390007019043 | 1.9686000347137451 |          70.74112932958013 |         88.90660215164625 |  0.006864000111818314 |                   1812.0675018597474 |   0.217382350167601 |                0.217382350167601 |  0.36397022008895874 |        80.0 |  6.878763198852539 |  208.81600952148438 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  145.0 |  208.81600952148438 |   336.78514012705097 |             0.7369584440773783 |                     168.75778543198308 |                     336.78514012705097 |             0.664172126432442 |                3.1127281927326713 |     20.43565678773374 |  0.7369584440773783 |    3.1127281927326713 |
  ;;|          00) original |              mt-cannon |         55.0 |  75.46299743652344 |  2.624799966812134 |          193.8835803957408 |        136.88066227665723 |  0.006864000111818314 |                   1747.7150479548368 |   0.217382350167601 |                0.217382350167601 |   0.6494075655937195 |        80.0 | 3.0675477981567383 |   269.0916442871094 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  315.0 |   269.0916442871094 |   134.02484629494637 |             0.8652797236530712 |                      350.1892933378531 |                     134.02484629494637 |            0.7701867372057785 |                 8.845328932648856 |     1512.358921225787 |  0.6910216988017823 |    21.846935935833454 |
  ;;|          00) original |            or-sturgill |         55.0 |  75.46299743652344 | 1.6404999494552612 |          96.91183758450151 |          67.6335388486365 |  0.006864000111818314 |                   2052.4611819288057 |   0.217382350167601 |                0.217382350167601 |  0.40402624011039734 |        80.0 |  8.857708930969238 |  148.46820068359375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  287.0 |  148.46820068359375 |    93.55519783444771 |             0.7181867523893805 |                     153.85585632835884 |                      93.55519783444771 |            0.8901218170940279 |                 3.764830763204717 |     10281.81923757587 |  0.8802229764726149 |    151.76830409380577 |
  ;;|          00) original | id-kootenai-rv-complex |         55.0 |  49.21500015258789 | 1.6404999494552612 |          380.4218259643599 |          67.6335388486365 |  0.006864000111818314 |                    7442.265366180263 |  0.3138266174914029 |               0.3138266174914029 |    0.466307669878006 |        80.0 |  5.185909748077393 |   200.8376922607422 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   62.0 |   200.8376922607422 |   249.68653941910796 |             0.7548063084486039 |                     184.62812259613563 |                     249.68653941910796 |            0.6661748112129817 |                2.8231786450635292 |    1442.8625768419777 |  0.7949246004215933 |      30.6146285452677 |
  ;;|          00) original | id-columbus-bear-gulch |         75.0 |  75.46299743652344 | 3.9372000694274902 |          600.8851354676544 |        251.46584509473425 |  0.013728000223636627 |                    7374.093142795872 |  0.3138266174914029 |               0.3138266174914029 |      0.5095254778862 |        80.0 |  5.561666965484619 |  276.62860107421875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  195.0 |  276.62860107421875 |   21.703365356540562 |             0.8082703644132138 |                      245.8143881715129 |                     21.703365356540562 |             0.813627351534236 |                 4.500501675589589 |     6947.920794020659 |  0.8074777821714699 |    51.743774266578626 |
  ;;|          00) original |           wa-irving-pk |         75.0 | 101.71099853515625 |  6.890100002288818 |         148.90979232783505 |         582.1515630707672 |  0.009983999654650688 |                    6824.984292896804 |  0.3138266174914029 |               0.3138266174914029 |  0.24932800233364105 |        80.0 |  5.118134498596191 |    299.942626953125 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  187.0 |    299.942626953125 |   31.852381138225795 |             0.5408103316788269 |                      66.47741156910462 |                     31.852381138225795 |            0.6099127521888967 |                1.2050350738022488 |     43.01697523327348 |  0.5408103316788269 |    1.2050350738022488 |
  ;;|          00) original |               id-lemhi |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                   1265.4929957843847 | 0.21098446587998754 |              0.21098446587998754 |   0.2867453992366791 |        80.0 |  4.230474948883057 |    265.745849609375 |              20.0 |                     |                              1.0 |                               8000.0 |  309.0 |    265.745849609375 |   103.43938114099103 |             0.7554717925011089 |                     185.25598027618352 |                     103.43938114099103 |            3.0060933649980814 |                23.369399426886254 |    103.99357965238595 |  0.7554717925011089 |    23.369399426886254 |
  ;;|          00) original |            ca-mosquito |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                    986.4968645143333 | 0.21098446587998754 |              0.21098446587998754 |  0.21255655586719513 |        80.0 |  5.098724365234375 |  237.22976684570312 |              20.0 |                     |                              1.0 |                               8000.0 |  257.0 |  237.22976684570312 |   61.695516858308736 |             0.7631121488848447 |                      192.6673034641475 |                     61.695516858308736 |            1.8812770512167813 |                 15.37359111593563 |      53.3298381623008 |  0.7631121488848447 |     15.37359111593563 |
  ;;|          00) original |          wa-goat-rocks |         75.0 |   62.3390007019043 | 7.8744001388549805 |          40.95351745501327 |           711.25281721317 |  0.013728000223636627 |                   1553.5978695060005 | 0.22412908981846083 |              0.22412908981846083 |   0.4244748055934906 |        80.0 |  3.841567039489746 |    146.305419921875 |              20.0 |  110.00000000000001 |                              0.6 |                               8000.0 |  200.0 |    146.305419921875 |   17.149905665759945 |             0.7772473461142227 |                     207.44485989781424 |                     17.149905665759945 |           0.48182990078143617 |                2.0385540283942962 |     11.83062858753603 |  0.7772473461142227 |    2.0385540283942962 |
  ;;|          00) original |           wa-irving-pk |         35.0 |  75.46299743652344 | 3.2809998989105225 |                            |                           | 0.0037440001033246517 |                    1565.514242369693 |   0.217382350167601 |                0.217382350167601 |   0.6745085120201111 |        80.0 | 10.095656394958496 |   282.4736022949219 |              20.0 |                     |                              1.0 |                               8000.0 |  343.0 |   282.4736022949219 |    155.2262098798053 |             0.8797902892924254 |                      388.4882880535096 |                      155.2262098798053 |            0.5301647778884776 |                 6.979305020429336 |     39.58605568663771 |  0.8797902892924254 |     6.979305020429336 |
  ;;|          00) original |            ca-mosquito |         51.0 |  78.74400329589844 |  4.593400001525879 |          518.5381117152432 |        316.88317388600854 |  0.006240000016987324 |                    8334.378270723908 |  0.3138266174914029 |               0.3138266174914029 |   0.3249197006225586 |        80.0 | 4.8548455238342285 |  222.65687561035156 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  316.0 |  222.65687561035156 |   118.28208422406533 |             0.6568027066384492 |                     114.80483122628277 |                     118.28208422406533 |            1.2042637442728563 |                 3.436256386343878 |    2012.9791783705427 |   0.782752267490534 |    30.200753128112048 |
  ;;|          00) original |           wa-irving-pk |         65.0 | 114.83499908447266 |  9.514900207519531 |         373.99446094762334 |         944.7222340710287 | 0.0074880002066493034 |                    7722.918412284979 |  0.3138266174914029 |               0.3138266174914029 |  0.38386404514312744 |        80.0 |  2.047827959060669 |     172.02490234375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   70.0 |     172.02490234375 |    253.5952100592813 |             0.7091012386714682 |                     147.21524245416987 |                      253.5952100592813 |            0.7681529266303655 |                 2.674618284570686 |    108.03930495414502 |  0.7091012386714682 |     2.674618284570686 |
  ;;|          00) original |        wa-minnow-ridge |         65.0 |  88.58699798583984 |  4.593400001525879 |          437.0069031679615 |        316.88317388600854 |  0.009983999654650688 |                    7672.269213122762 |  0.3138266174914029 |               0.3138266174914029 |  0.38386404514312744 |        80.0 |  6.980268955230713 |      344.9521484375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   62.0 |      344.9521484375 |   227.17011265173403 |             0.7279564360117788 |                     161.40139614410114 |                     227.17011265173403 |             0.835556210807969 |                3.1458828792180737 |     8398.772131501824 |  0.8454618609431448 |     73.98588320933216 |
  ;;|          00) original |            or-sturgill |         35.0 |   62.3390007019043 |  2.624799966812134 |                            |                           | 0.0037440001033246517 |                   1976.2063364961534 |   0.217382350167601 |                0.217382350167601 |  0.23086819052696228 |        80.0 |  7.688671588897705 |  195.51626586914062 |              20.0 |                     |                              1.0 |                               8000.0 |  263.0 |  195.51626586914062 |   48.440051721386794 |             0.6630352901734371 |                     118.21851379072754 |                     48.440051721386794 |             0.844739393042372 |                 2.715562183162512 |    19.443080257450774 |  0.6630352901734371 |     2.715562183162512 |
  ;;|          00) original |        wa-minnow-ridge |         55.0 |  75.46299743652344 |  5.249599933624268 |         475.15200974729754 |        387.15697803651994 |  0.006864000111818314 |                     7616.55803384907 |  0.3138266174914029 |               0.3138266174914029 |   0.4452286958694458 |        80.0 | 12.469437599182129 |  290.02093505859375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  182.0 |  290.02093505859375 |   30.788309375356924 |              0.756387894602279 |                     186.12481329253575 |                     30.788309375356924 |            0.8071465538377772 |                3.4454971973376525 |     7732.960099852431 |  0.9204643368938613 |    118.20387355203083 |
  ;;|          00) original | id-kootenai-rv-complex |         85.0 |  88.58699798583984 |  4.593400001525879 |          220.1077179433313 |        316.88317388600854 |  0.018719999119639397 |                    7176.137168511981 |  0.3138266174914029 |               0.3138266174914029 |   0.3443275988101959 |        80.0 |  5.552461624145508 |  218.57345581054688 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   18.0 |  218.57345581054688 |    191.9637540965192 |              0.628304426362454 |                     100.46101996340079 |                      191.9637540965192 |            0.6554445989973512 |                1.6940359621746053 |     63.58459106957431 |   0.628304426362454 |    1.6940359621746053 |
  ;;| 01) crowning disabled |        or-double-creek |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                   193.28398386901017 | 0.20955665024630543 |              0.20955665024630543 |   0.8097840547561646 |        80.0 |  5.001132488250732 |  228.59991455078125 |              20.0 |                     |                              0.9 |                               8000.0 |   94.0 |  228.59991455078125 |    284.1604243924098 |              0.743030064851352 |                     173.95558548210917 |                      284.1604243924098 |           0.06465703545692396 |                0.4181015697441539 |   0.28224604422159677 |   0.743030064851352 |    0.4181015697441539 |
  ;;|          00) original |        wa-minnow-ridge |         75.0 | 101.71099853515625 |  6.890100002288818 |         231.35152020188903 |         582.1515630707672 |  0.009983999654650688 |                   6623.4891572231645 |  0.3138266174914029 |               0.3138266174914029 |   0.3057306706905365 |        80.0 |  9.066580772399902 |   300.4925537109375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   11.0 |   300.4925537109375 |    167.4293459218377 |             0.6754198636997323 |                     125.33125018377275 |                      167.4293459218377 |            0.6315700511684299 |                 1.929139412656189 |     66.83269420452034 |  0.6754198636997323 |     1.929139412656189 |
  ;;|          00) original |            ca-mosquito |         46.0 |  95.14900207519531 |   6.23390007019043 |          503.9229189571198 |          500.999779032296 |  0.005615999922156334 |                    7826.558281503025 |  0.3138266174914029 |               0.3138266174914029 |  0.36397022008895874 |        80.0 |  4.017601490020752 |  110.22235107421875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   26.0 |  110.22235107421875 |    217.1344277565262 |             0.6992028725086964 |                      140.3605048912797 |                      217.1344277565262 |            1.0625498872492638 |                3.5560787777380716 |    1352.8231493577573 |  0.7462281866810857 |    18.132426272179906 |
  ;;|          00) original |            ca-mosquito |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                    587.1316319712079 | 0.21098446587998754 |              0.21098446587998754 | 0.034920770674943924 |        80.0 |  1.683468222618103 |       98.3779296875 |              20.0 |                     |                              1.0 |                               8000.0 |   -1.0 |       98.3779296875 |    276.1071827011406 |             0.4962001950963131 |                      53.43273799158039 |                      276.1071827011406 |            0.9936258283014515 |                2.0961665945062977 |     4.327733455182371 |  0.4962001950963131 |    2.0961665945062977 |
  ;;| 01) crowning disabled |         or-cedar-creek |         75.0 | 101.71099853515625 | 101.71099853515625 |          184.0809093889122 |         33017.91589403395 |  0.009983999654650688 |                   4063.6578135886894 | 0.22152102232360932 |              0.22152102232360932 |   0.4452286958694458 |        80.0 |  3.784116744995117 |   329.8726806640625 |              20.0 |  110.00000000000001 |                              0.6 |                               8000.0 |    8.0 |   329.8726806640625 |   186.45606553778998 |              0.796104780931127 |                      229.6655103676032 |                     186.45606553778998 |            0.8872363833905258 |                3.5444147532078647 |     53.17718731799685 |   0.796104780931127 |    3.5444147532078647 |
  ;;| 01) crowning disabled |             id-tenmile |         55.0 |   62.3390007019043 |   62.3390007019043 |          322.8767805819743 |         15843.00408983124 |  0.006864000111818314 |                    8896.563029929428 |  0.3138266174914029 |               0.3138266174914029 |   0.1227845624089241 |        80.0 |  6.877633094787598 |  221.51461791992188 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  109.0 |  221.51461791992188 |    24.32438683200229 |             0.5067110819325499 |                      56.29775749167509 |                      24.32438683200229 |            1.1118808670961926 |                2.0044380587323958 |     93.27245882605142 |  0.5067110819325499 |    2.0044380587323958 |
  ;;|          00) original |        wa-minnow-ridge |         65.0 |  88.58699798583984 |  5.905799865722656 |          768.5714309908515 |          461.972228189994 |  0.009983999654650688 |                     7616.55803384907 |  0.3138266174914029 |               0.3138266174914029 |   0.5773502588272095 |        80.0 | 12.469437599182129 |  290.02093505859375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  202.0 |  290.02093505859375 |    36.05889539507534 |             0.8458936687245456 |                      307.9774381230774 |                      36.05889539507534 |            0.8071465538377772 |                 5.573186384797416 |    14192.138347889064 |  0.9204643368938613 |    126.92579461592463 |
  ;;|          00) original |        wa-minnow-ridge |         65.0 |  88.58699798583984 | 1.9686000347137451 |           686.279452199223 |         88.90660215164625 |  0.009983999654650688 |                    6868.058191754124 |  0.3138266174914029 |               0.3138266174914029 |   0.6494075655937195 |        80.0 | 3.0798003673553467 |      1.506103515625 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   92.0 |      1.506103515625 |   269.78470075387827 |             0.8708441328620676 |                       364.088662206428 |                     269.78470075387827 |            0.6740779258267879 |                 5.518805846335476 |    1369.5890142626681 |   0.691856875059312 |    10.158472779393254 |
  ;;|          00) original |             id-tenmile |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                   1877.2784938276118 | 0.21012730729142198 |              0.21012730729142198 |   0.1051042377948761 |        80.0 | 2.1501920223236084 |    26.4268798828125 |              20.0 |                     |                              1.5 |                               8000.0 |   87.0 |    26.4268798828125 |   216.78844779028736 |             0.5824174177008641 |                      81.02298653122243 |                     216.78844779028736 |            0.8694324545845852 |                2.3521099104773437 |    15.463847619616217 |  0.5824174177008641 |    2.3521099104773437 |
  ;;| 01) crowning disabled |               id-lemhi |         15.0 | 22.966999053955078 | 22.966999053955078 |                            |                           |  0.003120000008493662 |                   2617.3647510461506 | 0.21012730729142196 |              0.21012730729142196 |   0.5317094326019287 |        80.0 |  2.781301736831665 |   248.4667205810547 |              20.0 |                     |                              1.5 |                               8000.0 |  305.0 |   248.4667205810547 |   119.99036553440192 |             0.8213357477506207 |                      265.0620399533814 |                     119.99036553440192 |            1.9903300908827657 |                21.104434375484193 |    193.45021252968596 |  0.8213357477506207 |    21.104434375484193 |
  ;;|          00) original | id-columbus-bear-gulch |         55.0 |  75.46299743652344 |  5.249599933624268 |         11.115152420240248 |        387.15697803651994 |  0.006864000111818314 |                    1580.260281950674 | 0.22971884333771272 |              0.22971884333771272 |   0.3057306706905365 |        80.0 |  8.615198135375977 |   269.1917419433594 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  345.0 |   269.1917419433594 |   142.15048184585697 |              0.697408375031613 |                     139.15779987439473 |                     142.15048184585697 |           0.16288110578753362 |                0.5307109381667683 |    3.2109388435844175 |   0.697408375031613 |    0.5307109381667683 |
  ;;|          00) original |            mt-billiard |         85.0 |  75.46299743652344 |  3.609100103378296 |           835.328585841024 |         220.6968690160058 |  0.018719999119639397 |                   7721.3817605098475 |  0.3138266174914029 |               0.3138266174914029 |    0.554309070110321 |        80.0 |  2.361907720565796 |   272.2395935058594 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  264.0 |   272.2395935058594 |                 84.0 |             0.8340974323961247 |                      286.1237585595362 |                                   84.0 |            0.9312857619669981 |                 5.975034321364428 |     2391.186257989239 |  0.6355513460042069 |    11.987226931701967 |
  ;;|          00) original |            ca-mosquito |         55.0 |  75.46299743652344 | 4.2652997970581055 |         510.61661876656285 |         283.5450846339534 |  0.006864000111818314 |                     8215.46120573934 |  0.3138266174914029 |               0.3138266174914029 |   0.3249197006225586 |        80.0 |  5.025650978088379 |  244.73843383789062 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  159.0 |  244.73843383789062 |    356.1454475312205 |              0.663795887691999 |                      118.6424900075261 |                      356.1454475312205 |            1.1730377175605453 |                 3.432741355834306 |    1959.5436477538638 |  0.7891716049164961 |     27.80898231423284 |
  ;;| 01) crowning disabled |          wa-goat-rocks |         75.0 | 101.71099853515625 | 101.71099853515625 |           653.989808152065 |         33017.91589403395 |  0.009983999654650688 |                     5439.09901044323 | 0.22152102232360932 |              0.22152102232360932 |    0.600860595703125 |        80.0 | 3.1513161659240723 |  296.73126220703125 |              20.0 |  110.00000000000001 |                              0.6 |                               8000.0 |   27.0 |  296.73126220703125 |   205.90275673431552 |               0.86182600195923 |                     342.00078656407845 |                     205.90275673431552 |            1.5014687982040704 |                  9.40799180795114 |    188.92419994888368 |    0.86182600195923 |      9.40799180795114 |
  ;;|          00) original |            ca-mosquito |         43.0 | 104.99199676513672 |  17.71739959716797 |          198.7774267596442 |        2400.4781127326182 | 0.0037440001033246517 |                    7919.768858523452 |  0.3138266174914029 |               0.3138266174914029 |  0.19438031315803528 |        80.0 |  4.436306953430176 |  244.74649047851562 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   89.0 |  244.74649047851562 |    319.9321203915373 |             0.3414163248359625 |                      22.50307672585725 |                      319.9321203915373 |            1.0885254292177255 |                1.3862215417164365 |     57.42270881036668 |  0.3414163248359625 |    1.3862215417164365 |
  ;;|          00) original |        wa-minnow-ridge |         75.0 | 114.83499908447266 | 3.9372000694274902 |         1106.4829827522708 |        251.46584509473425 |  0.009983999654650688 |                    7752.786473463326 |  0.3138266174914029 |               0.3138266174914029 |    0.700207531452179 |        80.0 | 1.1160056591033936 |   153.2872314453125 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  348.0 |   153.2872314453125 |   168.12601321619704 |             0.8871772892423706 |                     410.84807582370166 |                     168.12601321619704 |            0.8491536288632766 |                 7.882518563036473 |     2082.529542973968 | 0.47943509663307715 |     11.94149880410073 |
  ;;| 01) crowning disabled |               id-lemhi |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                    2578.320668244056 | 0.21012730729142196 |              0.21012730729142196 |   0.8097840547561646 |        80.0 | 3.0274393558502197 |    65.1168212890625 |              20.0 |                     |                              1.5 |                               8000.0 |   18.0 |    65.1168212890625 |     202.595225655975 |             0.9055201859374011 |                      477.5954847174078 |                       202.595225655975 |            1.9546983229697572 |                46.254452209741906 |    417.65887739232807 |  0.9055201859374011 |    46.254452209741906 |
  ;;|          00) original |            ca-mosquito |         45.0 |   62.3390007019043 | 14.436400413513184 |          531.8789225883484 |        1765.5749521280463 |  0.004991999827325344 |                    884.0219785802115 | 0.21098446587998754 |              0.21098446587998754 |   0.9004040360450745 |        80.0 |  2.846623659133911 |  121.41192626953125 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  116.0 |  121.41192626953125 |                296.0 |              0.914017870166459 |                      515.6919699273143 |                                  296.0 |             1.592069761790453 |                 49.42732405864679 |    153.64887750102875 |   0.914017870166459 |     49.42732405864679 |
  ;;| 01) crowning disabled |            ca-mosquito |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                   1360.0412490287397 |  0.2095566502463054 |               0.2095566502463054 |   0.1227845624089241 |        80.0 |  5.238315582275391 |   284.3222351074219 |              20.0 |                     |                              0.9 |                               8000.0 |  170.0 |   284.3222351074219 |    98.86777409735492 |             0.7254160720938886 |                     159.39701836501644 |                      98.86777409735492 |            1.1033863042597567 |                 6.412221508283509 |      30.4586600796304 |  0.7254160720938886 |     6.412221508283509 |
  ;;| 01) crowning disabled |            ca-mosquito |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                   1360.0412490287397 |  0.2095566502463054 |               0.2095566502463054 |   0.1227845624089241 |        80.0 |  5.238315582275391 |   284.3222351074219 |              20.0 |                     |                              0.9 |                               8000.0 |  170.0 |   284.3222351074219 |    98.86777409735492 |             0.7254160720938886 |                     159.39701836501644 |                      98.86777409735492 |            1.1033863042597567 |                 6.412221508283509 |      30.4586600796304 |  0.7254160720938886 |     6.412221508283509 |
  ;;|          00) original |        or-double-creek |         45.0 |   62.3390007019043 | 1.6404999494552612 |         411.99390046893296 |          67.6335388486365 |  0.004991999827325344 |                   3832.3457747550888 | 0.21692336615184582 |              0.21692336615184582 |    0.700207531452179 |        80.0 |  2.206490993499756 |   336.3756103515625 |              20.0 |  110.00000000000001 |                              0.3 |                               8000.0 |   26.0 |   336.3756103515625 |    205.4112709855425 |             0.8927398308026766 |                        429.22830770741 |                      205.4112709855425 |            1.0916410112546917 |                 8.589890479261728 |    1422.3364406934627 |  0.6209944920337507 |    32.259657869122236 |
  ;;|          00) original |        wa-minnow-ridge |         45.0 |   62.3390007019043 | 2.2967000007629395 |         45.860603169701214 |        112.03512054935626 |  0.004991999827325344 |                    1637.798984612725 |   0.217382350167601 |                0.217382350167601 |  0.24932800233364105 |        80.0 |   9.63703441619873 |     280.38720703125 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  211.0 |     280.38720703125 |    66.33587718330762 |             0.6833145985909634 |                      130.1098453295795 |                      66.33587718330762 |            0.6306192463791853 |                2.2326607984546367 |    13.248184688826916 |  0.6833145985909634 |    2.2326607984546367 |
  ;;| 01) crowning disabled |            ca-mosquito |         67.0 | 118.11599731445312 | 118.11599731445312 |         2427.8227118765694 |        41320.052237994845 |  0.008112000301480293 |                    8368.782994736432 |  0.3138266174914029 |               0.3138266174914029 |    0.839099645614624 |        80.0 |  6.800766468048096 |  262.34954833984375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  334.0 |  262.34954833984375 |   151.17397517799816 |             0.9245443614351609 |                      571.7040750669928 |                     151.17397517799816 |            1.2130323790655435 |                 16.02259108764151 |      701.348029803485 |  0.9245443614351609 |     16.02259108764151 |
  ;;|          00) original |            ca-mosquito |         36.0 |  59.05799865722656 |  1.312399983406067 |                            |                           | 0.0037440001033246517 |                    8309.230904367794 |  0.3138266174914029 |               0.3138266174914029 |    0.466307669878006 |        80.0 |  6.808093070983887 |   269.1046142578125 |              20.0 |                     |                              1.0 |                               8000.0 |  175.0 |   269.1046142578125 |   12.006918010091567 |             0.7805680910077307 |                     211.13625051180188 |                     12.006918010091567 |             1.198317119879811 |                 5.740367635767342 |    249.48191008114236 |  0.7805680910077307 |     5.740367635767342 |
  ;;|          00) original |        wa-minnow-ridge |         75.0 |  88.58699798583984 | 0.9843000173568726 |           47.4338821638519 |         31.43323063684178 |  0.013728000223636627 |                   1418.6061556356788 | 0.22412908981846083 |              0.22412908981846083 |   0.5317094326019287 |        80.0 | 11.007691383361816 |  271.53070068359375 |              20.0 |  110.00000000000001 |                              0.6 |                               8000.0 |  321.0 |  271.53070068359375 |   134.68924002815572 |              0.841499353651763 |                     299.54042335759226 |                     134.68924002815572 |           0.40275763847493756 |                2.5858091311991585 |    15605.297258423594 |  0.9071152782020073 |     97.23598944699856 |
  ;;|          00) original |        wa-minnow-ridge |         65.0 |  88.58699798583984 |  5.905799865722656 |          544.3497892161382 |          461.972228189994 |  0.009983999654650688 |                    7346.065062055349 |  0.3138266174914029 |               0.3138266174914029 |    0.466307669878006 |        80.0 | 10.138382911682129 |    271.013427734375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  208.0 |    271.013427734375 |     41.3858842456969 |             0.8014757600914935 |                     236.60193664830848 |                       41.3858842456969 |            0.7674650917095148 |                 4.092619550908673 |     9879.475541672939 |  0.8974807392097284 |     88.33149312912577 |
  ;;| 01) crowning disabled |            ca-mosquito |         58.0 | 111.55400085449219 | 111.55400085449219 |          182.5810740145999 |         37924.99272147336 |  0.005615999922156334 |                    7819.235548784448 |  0.3138266174914029 |               0.3138266174914029 | 0.052407778799533844 |        80.0 |   2.28684139251709 |   188.9629364013672 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  253.0 |   188.9629364013672 |   19.986551199519567 |             0.3135695393383931 |                     18.695962677232263 |                     19.986551199519567 |            1.0570880723126572 |                1.2896431249421565 |     52.74391573697994 |  0.3135695393383931 |    1.2896431249421565 |
  ;;|          00) original |            ca-mosquito |         58.0 |  95.14900207519531 |  5.577700138092041 |          247.9358542055096 |         424.0143197316814 |  0.008112000301480293 |                    7740.902253891371 |  0.3138266174914029 |               0.3138266174914029 |  0.19438031315803528 |        80.0 | 2.2381858825683594 |   191.0260772705078 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  269.0 |   191.0260772705078 |    73.06946024218041 |            0.48641939382881244 |                      50.87254859103973 |                      73.06946024218041 |            1.0328720653052152 |                 1.768991871711639 |     71.62356708092234 | 0.48641939382881244 |     1.768991871711639 |
  ;;|          00) original |               id-lemhi |         15.0 | 22.966999053955078 | 2.2967000007629395 |                            |                           |  0.003120000008493662 |                   2586.1106635109536 | 0.21012730729142196 |              0.21012730729142196 |      0.5095254778862 |        80.0 |  7.542016983032227 |  238.76979064941406 |              20.0 |                     |                              1.5 |                               8000.0 |  285.0 |  238.76979064941406 |    89.55608847337066 |              0.843364521088659 |                      303.0764518492789 |                      89.55608847337066 |             1.961695561070788 |                24.867066582911114 |    225.21790125821877 |   0.843364521088659 |    24.867066582911114 |
  ;;|          00) original |            ca-mosquito |         61.0 | 111.55400085449219 |  6.890100002288818 |          813.7108607815711 |         582.1515630707672 |  0.006240000016987324 |                    8532.271733903792 |  0.3138266174914029 |               0.3138266174914029 |   0.4452286958694458 |        80.0 |   8.33970832824707 |  258.92071533203125 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |    8.0 |  258.92071533203125 |   173.16479248712335 |             0.7528434642709587 |                     182.79218442837475 |                     173.16479248712335 |            1.2540702573671834 |                 5.267245009179171 |     3387.878177321839 |  0.8719444463555227 |     36.20579477892321 |
  ;;|          00) original |        wa-minnow-ridge |         75.0 | 101.71099853515625 |  6.890100002288818 |         183.06853022659837 |         582.1515630707672 |  0.009983999654650688 |                    7672.914936683209 |  0.3138266174914029 |               0.3138266174914029 |  0.17632697522640228 |        80.0 | 0.7873350381851196 |   245.1250457763672 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  226.0 |   245.1250457763672 |    47.60978405823667 |            0.45137776512246003 |                      42.47158922371622 |                      47.60978405823667 |            0.8358257499290918 |                1.3177450612236687 |     52.88473180737429 | 0.45137776512246003 |    1.3177450612236687 |
  ;;| 01) crowning disabled |           wa-irving-pk |         45.0 |  75.46299743652344 |  75.46299743652344 |          295.5272559766352 |        21100.796906307733 |  0.004991999827325344 |                    7328.113171288817 |  0.3138266174914029 |               0.3138266174914029 |  0.36397022008895874 |        80.0 |  5.558180809020996 |     83.494873046875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  196.0 |     83.494873046875 |   357.41018826147393 |             0.6784774746924949 |                     127.15851067800904 |                     357.41018826147393 |            0.7208796193204248 |                 2.227324322196176 |     85.37174387508604 |  0.6784774746924949 |     2.227324322196176 |
  ;;|          00) original | id-kootenai-rv-complex |         55.0 |   62.3390007019043 | 1.9686000347137451 |          46.70336698998892 |         88.90660215164625 |  0.006864000111818314 |                    1789.429948892311 |   0.217382350167601 |                0.217382350167601 |   0.3057306706905365 |        80.0 | 7.0949578285217285 |  246.17483520507812 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  139.0 |  246.17483520507812 |     341.468884792845 |             0.6615480314874678 |                     117.39417366767272 |                       341.468884792845 |            0.6518279064662649 |                2.0810239183388735 |    13.491641816918275 |  0.6615480314874678 |    2.0810239183388735 |
  ;;|          00) original |           wa-irving-pk |         35.0 |  49.21500015258789 | 3.9372000694274902 |                            |                           | 0.0037440001033246517 |                    6775.139519568284 |  0.3138266174914029 |               0.3138266174914029 |   0.2867453992366791 |        80.0 |  8.169930458068848 |  302.26055908203125 |              20.0 |                     |                              1.0 |                               8000.0 |  209.0 |  302.26055908203125 |    76.95056397063323 |             0.6675724201829962 |                     120.77204765470383 |                      76.95056397063323 |            0.6018372530097712 |                 1.785671829799968 |     63.27882639688939 |  0.6675724201829962 |     1.785671829799968 |
  ;;|          00) original |        or-double-creek |         45.0 |  75.46299743652344 |  2.952899932861328 |          156.5239383311683 |        163.33184763650195 |  0.004991999827325344 |                   3764.8202847700272 | 0.21692336615184582 |              0.21692336615184582 |  0.40402624011039734 |        80.0 |  3.197040557861328 |   191.6094512939453 |              20.0 |  110.00000000000001 |                              0.3 |                               8000.0 |   37.0 |   191.6094512939453 |   218.70798646868656 |             0.7602797810250002 |                     189.87546210463535 |                     218.70798646868656 |            1.0600696321312444 |                3.3219878806721006 |     45.21654535507459 |  0.7602797810250002 |    3.3219878806721006 |
  ;;|          00) original |        wa-minnow-ridge |         75.0 |  88.58699798583984 |  5.249599933624268 |          461.6649959254512 |        387.15697803651994 |  0.013728000223636627 |                    7601.639854734984 |  0.3138266174914029 |               0.3138266174914029 |   0.4244748055934906 |        80.0 | 11.835177421569824 |   292.1236877441406 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  195.0 |   292.1236877441406 |    37.90013590769393 |             0.7509869363975907 |                      181.0772510511493 |                      37.90013590769393 |            0.8053669518991072 |                3.3542678756225617 |      19729.6577008749 |  0.9150573080563729 |    128.46592702272937 |
  ;;|          00) original |        wa-minnow-ridge |         65.0 |  88.58699798583984 |  4.593400001525879 |         1022.9023931318901 |        316.88317388600854 |  0.009983999654650688 |                    7375.983420092473 |  0.3138266174914029 |               0.3138266174914029 |   0.6745085120201111 |        80.0 | 11.000730514526367 |  295.10589599609375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  286.0 |  295.10589599609375 |                106.0 |             0.8959465002542958 |                      440.5036773969968 |                                  106.0 |            0.7667665718312663 |                7.6593576433609805 |    10056.456912399217 |  0.9070437217870287 |     87.29776372370618 |
  ;;|          00) original | id-columbus-bear-gulch |         35.0 | 36.090999603271484 | 1.6404999494552612 |                            |                           |  0.005615999922156334 |                   1625.6037422278987 |   0.217382350167601 |                0.217382350167601 |    0.554309070110321 |        80.0 |  2.771230697631836 |   227.2384490966797 |              20.0 |                     |                              1.0 |                               8000.0 |  160.0 |   227.2384490966797 |   342.54557800651753 |             0.8313034011830731 |                     281.29980650516404 |                     342.54557800651753 |            0.6377078446618295 |                 5.523864521320801 |     32.53349628263343 |  0.8313034011830731 |     5.523864521320801 |
  ;;|          00) original |        wa-minnow-ridge |         75.0 |  88.58699798583984 |  5.249599933624268 |         417.71753548072616 |        387.15697803651994 |  0.013728000223636627 |                    7583.416161812534 |  0.3138266174914029 |               0.3138266174914029 |   0.3057306706905365 |        80.0 | 11.768802642822266 |   290.0382995605469 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  273.0 |   290.0382995605469 |    100.4276903705832 |             0.7297932756141692 |                      162.8698835816312 |                      100.4276903705832 |            0.8017373838423333 |                3.0422570288415405 |    19032.413675437467 |  0.9144592371976453 |    123.97828426379115 |
  ;;|          00) original |               id-lemhi |         45.0 |   62.3390007019043 | 3.9372000694274902 |          256.7865924551327 |        251.46584509473425 |  0.004991999827325344 |                    3782.154777328894 | 0.21692336615184582 |              0.21692336615184582 |   0.5317094326019287 |        80.0 |  6.035749435424805 |       236.955078125 |              20.0 |  110.00000000000001 |                              0.3 |                               8000.0 |  307.0 |       236.955078125 |   121.81433776062238 |             0.8408165544275708 |                      298.2621322210274 |                     121.81433776062238 |            1.0686638752452582 |                 5.424935431666571 |    1827.8688350277623 |  0.8216633338126789 |      45.1141637336908 |
  ;;|          00) original |            ca-mosquito |         57.0 | 108.27300262451172 |  6.561999797821045 |           259.662201800368 |          541.068310789092 |  0.005615999922156334 |                     7991.44682050759 |  0.3138266174914029 |               0.3138266174914029 |  0.14054083824157715 |        80.0 | 3.5495967864990234 |  206.43145751953125 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  252.0 |  206.43145751953125 |    49.30511971828088 |            0.46272027856029047 |                      45.06517568659306 |                      49.30511971828088 |             1.109035280705296 |                1.7945741496063174 |     75.01106763531327 | 0.46272027856029047 |    1.7945741496063174 |
  ;;|          00) original |        wa-minnow-ridge |         45.0 |  75.46299743652344 |  2.952899932861328 |          59.15066549687619 |        163.33184763650195 |  0.004991999827325344 |                   1499.3192595641028 |   0.217382350167601 |                0.217382350167601 |   0.4452286958694458 |        80.0 |  6.101963043212891 |   311.8785095214844 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  251.0 |   311.8785095214844 |    78.32225324637498 |             0.7851579715258864 |                     216.38776377140974 |                      78.32225324637498 |            0.5022754121537308 |                  3.14564057138796 |      17.0874102564653 |  0.7851579715258864 |      3.14564057138796 |
  ;;| 01) crowning disabled |               id-lemhi |         15.0 | 36.090999603271484 | 36.090999603271484 |                            |                           |  0.003120000008493662 |                   2535.4078355599577 | 0.21012730729142196 |              0.21012730729142196 |   0.5773502588272095 |        80.0 |  2.470113515853882 |   33.54071044921875 |              20.0 |                     |                              1.5 |                               8000.0 |  343.0 |   33.54071044921875 |    165.9779198127343 |             0.8379757534271318 |                     293.03425021693107 |                      165.9779198127343 |            1.9153131410669997 |                 23.20678929166536 |     206.0602071355885 |  0.8379757534271318 |     23.20678929166536 |
  ;;| 01) crowning disabled |            ca-mosquito |         72.0 |  91.86799621582031 |  91.86799621582031 |          1240.760655439498 |         28342.89094404834 |  0.012480000033974648 |                    8437.965873504401 |  0.3138266174914029 |               0.3138266174914029 |    0.554309070110321 |        80.0 |  7.733380317687988 |   259.0696105957031 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  194.0 |   259.0696105957031 |    21.10098805795511 |             0.8386894978663468 |                      294.3342205543905 |                      21.10098805795511 |            1.2307237133577962 |                 8.121352010789211 |     358.4302251120937 |  0.8386894978663468 |     8.121352010789211 |
  ;;| 01) crowning disabled |               id-lemhi |         55.0 |   62.3390007019043 |   62.3390007019043 |          387.7699096419956 |         15843.00408983124 |  0.006864000111818314 |                   1979.5382122465764 |   0.217382350167601 |                0.217382350167601 |   0.7265425324440002 |        80.0 |  7.144917011260986 |   205.5135498046875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  226.0 |   205.5135498046875 |    44.70676613402122 |             0.8908874611520359 |                     422.94788892087286 |                      44.70676613402122 |             1.059750923663733 |                15.619020610455545 |    112.01874865660106 |  0.8908874611520359 |    15.619020610455545 |
  ;;|          00) original |           wa-irving-pk |         35.0 |   62.3390007019043 |  7.218200206756592 |                            |                           | 0.0037440001033246517 |                   1771.8672542826516 |   0.217382350167601 |                0.217382350167601 |  0.40402624011039734 |        80.0 |  8.901456832885742 |   285.1929016113281 |              20.0 |                     |                              1.0 |                               8000.0 |  139.0 |   285.1929016113281 |   338.76564984893423 |             0.6954418851227001 |                      137.8533986323509 |                     338.76564984893423 |             0.674296904529087 |                2.5351215326770156 |    16.274325404085378 |  0.6954418851227001 |    2.5351215326770156 |
  ;;| 01) crowning disabled |            ca-mosquito |         42.0 |  65.62000274658203 |  65.62000274658203 |          191.0296292306193 |        17110.084183327035 |  0.004991999827325344 |                   7651.9443888659225 |  0.3138266174914029 |               0.3138266174914029 |   0.1227845624089241 |        80.0 | 1.7958258390426636 |    61.3187255859375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  136.0 |    61.3187255859375 |   283.37160825251317 |            0.38323122711539953 |                      29.09573071876578 |                     283.37160825251317 |            1.0065842976970414 |                1.3788182199683032 |    55.184529512629645 | 0.38323122711539953 |    1.3788182199683032 |
  ;;|          00) original | id-columbus-bear-gulch |         35.0 |   62.3390007019043 |  4.921500205993652 |                            |                           | 0.0037440001033246517 |                    7435.574488626511 |  0.3138266174914029 |               0.3138266174914029 |   0.6248693466186523 |        80.0 |  8.615198135375977 |   269.1917419433594 |              20.0 |                     |                              1.0 |                               8000.0 |  311.0 |   269.1917419433594 |     123.791794728203 |             0.8805671517145895 |                      390.7392127416158 |                       123.791794728203 |             0.820855233436961 |                 7.230760232278946 |    281.21404996201886 |  0.8805671517145895 |     7.230760232278946 |
  ;;|          00) original |            or-sturgill |         45.0 |  75.46299743652344 |  2.952899932861328 |          225.9896494168774 |        163.33184763650195 |  0.004991999827325344 |                   2058.1497818712796 |   0.217382350167601 |                0.217382350167601 |   0.6248693466186523 |        80.0 |  7.133437633514404 |   209.6936798095703 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   65.0 |   209.6936798095703 |   248.62392312468486 |             0.8465992623506905 |                       309.367145949636 |                     248.62392312468486 |            0.8952112207319287 |                 8.754979897314271 |    2249.5140464653227 |  0.8488519094208564 |      45.2570912145921 |
  ;;|          00) original |               id-lemhi |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                    1158.039364465359 | 0.21098446587998754 |              0.21098446587998754 |   0.6494075655937195 |        80.0 |  5.650454044342041 |   181.2028350830078 |              20.0 |                     |                              1.0 |                               8000.0 |  194.0 |   181.2028350830078 |                 14.0 |             0.8884955995282465 |                      415.0765016387117 |                                   14.0 |             2.648014842223746 |                 60.66437380787285 |    247.03373901622584 |  0.8884955995282465 |     60.66437380787285 |
  ;;|          00) original | id-columbus-bear-gulch |         75.0 |  49.21500015258789 |  1.312399983406067 |          463.5319348303401 |         48.39462225456499 |  0.013728000223636627 |                    6675.447751388788 |  0.3138266174914029 |               0.3138266174914029 |   0.5317094326019287 |        80.0 | 3.4352309703826904 |  175.11187744140625 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  287.0 |  175.11187744140625 |   103.17262779655567 |             0.8137343770001986 |                     253.60502819027707 |                     103.17262779655567 |            0.6727659803605516 |                 3.835105760448029 |     818.8571064961816 |   0.714542200963741 |     7.811873569113832 |
  ;;| 01) crowning disabled |            or-sturgill |         45.0 |   62.3390007019043 |   62.3390007019043 |          413.6209258824617 |         15843.00408983124 |  0.004991999827325344 |                    3935.380373556184 | 0.21692336615184582 |              0.21692336615184582 |    0.700207531452179 |        80.0 |  8.851813316345215 |  192.57745361328125 |              20.0 |  110.00000000000001 |                              0.3 |                               8000.0 |   60.0 |  192.57745361328125 |   244.50013418505557 |             0.8860166333086598 |                      407.1877142885224 |                     244.50013418505557 |            1.1396586373276174 |                 8.398027913877668 |    119.48657537227378 |  0.8860166333086598 |     8.398027913877668 |
  ;;| 01) crowning disabled |        wa-minnow-ridge |         45.0 |   62.3390007019043 |   62.3390007019043 |          865.8049010307516 |         15843.00408983124 |  0.004991999827325344 |                    6795.729307154291 |  0.3138266174914029 |               0.3138266174914029 |   0.7535540461540222 |        80.0 | 2.8181886672973633 |   10.45672607421875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  270.0 |   10.45672607421875 |    92.00080300909963 |             0.9028225340480269 |                      466.5792723455056 |                      92.00080300909963 |            0.6627554863394786 |                7.0365868837414425 |     250.1132222553286 |  0.9028225340480269 |    7.0365868837414425 |
  ;;|          00) original |        wa-minnow-ridge |         65.0 |  88.58699798583984 |  5.905799865722656 |          843.3417167629027 |          461.972228189994 |  0.009983999654650688 |                    7997.156144586368 |  0.3138266174914029 |               0.3138266174914029 |    0.600860595703125 |        80.0 |  7.829322338104248 |    104.923095703125 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  267.0 |    104.923095703125 |     84.2683236007033 |             0.8327974532390007 |                      283.8636362189065 |                       84.2683236007033 |            0.9149557930162706 |                 5.824331613237345 |    11927.213997707911 |  0.8628903455694295 |    106.15152809717488 |
  ;;|          00) original | id-kootenai-rv-complex |         45.0 |   62.3390007019043 | 1.6404999494552612 |          102.0947044706988 |          67.6335388486365 |  0.004991999827325344 |                    2538.019330660596 | 0.21692336615184582 |              0.21692336615184582 |   0.6745085120201111 |        80.0 |  3.881542205810547 |      5.855712890625 |              20.0 |  110.00000000000001 |                              0.3 |                               8000.0 |  121.0 |      5.855712890625 |    299.2672239909987 |             0.8840357433888738 |                      401.0711805587281 |                      299.2672239909987 |           0.44442516640590696 |                 3.214177211183113 |    232.90482266549952 |   0.739357318510896 |     5.034829889215147 |
  ;;| 01) crowning disabled |            ca-mosquito |         49.0 | 118.11599731445312 | 118.11599731445312 |           1826.12075794787 |        41320.052237994845 | 0.0043680001981556416 |                    7825.678266508168 |  0.3138266174914029 |               0.3138266174914029 |   0.8097840547561646 |        80.0 | 1.1957429647445679 |   270.9634094238281 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  348.0 |   270.9634094238281 |    167.5062816708994 |             0.9170984625958655 |                      530.9562195616298 |                      167.5062816708994 |            1.0569663305748955 |                12.888001863395488 |     527.5287151342452 |  0.9170984625958655 |    12.888001863395488 |
  ;;|          00) original |              mt-cannon |         25.0 |  49.21500015258789 | 1.9686000347137451 |                            |                           |  0.002495999913662672 |                    385.4727442483051 | 0.20955665024630543 |              0.20955665024630543 |    0.700207531452179 |        80.0 |  7.032727241516113 |  266.02508544921875 |              20.0 |                     |                              0.9 |                               8000.0 |  289.0 |  266.02508544921875 |   106.23363388120333 |             0.8639193112436638 |                      346.9254698234746 |                     106.23363388120333 |            0.1918623395224453 |                 3.066065861232639 |     4.127863736438179 |  0.8639193112436638 |     3.066065861232639 |
  ;;| 01) crowning disabled |               id-lemhi |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                   1164.3782913121343 | 0.21098446587998754 |              0.21098446587998754 |      0.5095254778862 |        80.0 |  4.717007637023926 |    226.760498046875 |              20.0 |                     |                              1.0 |                               8000.0 |  199.0 |    226.760498046875 |    28.50050978190078 |             0.8452088653989989 |                     306.63811867469985 |                      28.50050978190078 |            2.6697550133605876 |                 40.31931444412162 |     165.0845648600507 |  0.8452088653989989 |     40.31931444412162 |
  ;;|          00) original |         mt-george-lake |         45.0 |   62.3390007019043 | 2.2967000007629395 |         2.8647801687788834 |        112.03512054935626 |  0.004991999827325344 |                     717.641758156676 |  0.2504714934367811 |               0.2504714934367811 |  0.23086819052696228 |        80.0 |  1.933518648147583 |  185.93173217773438 |              20.0 |  110.00000000000001 |                              0.3 |                               8000.0 |  345.0 |  185.93173217773438 |   161.54457186788557 |             0.5555909170610006 |                       71.3538398354317 |                     161.54457186788557 |           0.16018843818063092 |                0.2762441925879129 |    0.8275760488459066 |  0.5555909170610006 |    0.2762441925879129 |
  ;;|          00) original |            ca-mosquito |         52.0 |  95.14900207519531 |  1.312399983406067 |          1074.982813851074 |         48.39462225456499 |  0.006240000016987324 |                    7969.018876929178 |  0.3138266174914029 |               0.3138266174914029 |   0.5773502588272095 |        80.0 |  3.765909194946289 |   227.5386199951172 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  147.0 |   227.5386199951172 |   330.86022272171795 |             0.8424611980520742 |                      301.3557618772549 |                     330.86022272171795 |            1.1027698015199838 |                 7.450316995330032 |    1866.9413179503222 |  0.7332766023037259 |    19.935442166482257 |
  ;;|          00) original |            ca-mosquito |         64.0 | 104.99199676513672 |  5.905799865722656 |          566.7345371032886 |          461.972228189994 |  0.006864000111818314 |                    8477.053572013734 |  0.3138266174914029 |               0.3138266174914029 |   0.3443275988101959 |        80.0 |  8.079806327819824 |   248.1741943359375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  353.0 |   248.1741943359375 |   149.13282392059273 |             0.6684880713188155 |                     121.29458310836701 |                     149.13282392059273 |            1.2403075103358616 |                 3.692434820297093 |     8480.723287162116 |  0.8674534060937016 |     91.71445761111676 |
  ;;| 01) crowning disabled |        wa-minnow-ridge |         45.0 |  75.46299743652344 |  75.46299743652344 |          358.4513182345804 |        21100.796906307733 |  0.004991999827325344 |                    3621.799815727651 | 0.21692336615184582 |              0.21692336615184582 |    0.700207531452179 |        80.0 | 2.9839539527893066 |   231.0802001953125 |              20.0 |  110.00000000000001 |                              0.3 |                               8000.0 |  221.0 |   231.0802001953125 |                 41.0 |             0.8936708829721776 |                      432.4482828067767 |                                   41.0 |            0.9956241890076665 |                 7.908011029569142 |    103.54921081942088 |  0.8936708829721776 |     7.908011029569142 |
  ;;|          00) original |            ca-mosquito |         59.0 | 101.71099853515625 | 0.9843000173568726 |          522.2909635435213 |         31.43323063684178 |  0.006240000016987324 |                    8529.068173840626 |  0.3138266174914029 |               0.3138266174914029 |   0.2867453992366791 |        80.0 |  7.781811237335205 |  231.89810180664062 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  147.0 |  231.89810180664062 |   356.51688992740344 |             0.6406008653770938 |                     106.40852440836011 |                     356.51688992740344 |            1.2548331690039982 |                 3.382119995957021 |     3179.128245994534 |  0.8619974185593191 |     36.13463519389995 |
  ;;|          00) original |            or-sturgill |         25.0 | 36.090999603271484 | 32.810001373291016 |                            |                           |   6.23999978415668E-4 |                     568.456783729908 |  0.2390077658246982 |               0.2390077658246982 |    0.554309070110321 |        80.0 |  6.110177040100098 |   128.2122802734375 |              20.0 |                     |                              0.6 |                               8000.0 |  148.0 |   128.2122802734375 |     324.575831504814 |             0.8498178999524115 |                      315.8348752724553 |                       324.575831504814 |           0.05721728973607874 |                0.5174276744597246 |     1.171676902066898 |  0.8498178999524115 |    0.5174276744597246 |
  ;;|          00) original |          wa-bolt-creek |         85.0 |  88.58699798583984 | 0.9843000173568726 |         130.19746655137402 |         31.43323063684178 |  0.018719999119639397 |                    7209.976328257479 |  0.3138266174914029 |               0.3138266174914029 |  0.15838444232940674 |        80.0 |   8.42008113861084 |   260.8177490234375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  121.0 |   260.8177490234375 |   49.132250109745144 |            0.43635812441966093 |                      39.20975137444713 |                     49.132250109745144 |            0.6540258967956973 |                0.9973481368470756 |    19364.879954806336 |  0.8732857845948011 |     88.39107987137997 |
  ;;|          00) original | id-isabella-lower-twin |         65.0 |   62.3390007019043 | 3.2809998989105225 |          935.9161923523619 |        191.29653582205867 |  0.009983999654650688 |                    7985.762363903482 |  0.3138266174914029 |               0.3138266174914029 |    0.554309070110321 |        80.0 |  5.399317741394043 |    256.620361328125 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  248.0 |    256.620361328125 |                 68.0 |             0.8440585778929739 |                      304.4090625261595 |                                   68.0 |            0.9484729470610427 |                 6.472896525342341 |     5901.540171547109 |  0.8022075051342829 |      71.6270150645523 |
  ;;| 01) crowning disabled |            ca-mosquito |         61.0 | 104.99199676513672 | 104.99199676513672 |         1734.6513983224886 |         34628.37189050189 |  0.006240000016987324 |                    8238.720722781292 |  0.3138266174914029 |               0.3138266174914029 |    0.700207531452179 |        80.0 |  5.188572883605957 |   242.0089874267578 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  233.0 |   242.0089874267578 |                 53.0 |             0.8944082865688208 |                     435.02940754398236 |                                   53.0 |            1.1796031798145468 |                11.628682556247545 |    501.10509909060835 |  0.8944082865688208 |    11.628682556247545 |
  ;;|          00) original | id-kootenai-rv-complex |         55.0 |   62.3390007019043 | 0.9843000173568726 |          57.39865291199796 |         31.43323063684178 |  0.006864000111818314 |                   1865.5464079730793 |   0.217382350167601 |                0.217382350167601 |   0.3443275988101959 |        80.0 |  7.359054088592529 |      150.7119140625 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   29.0 |      150.7119140625 |   227.04878102543057 |              0.683151061840543 |                      130.0088269839957 |                     227.04878102543057 |            0.6934740179768425 |                2.4532354122001427 |     5489.539068336517 |  0.8536388141416702 |     97.46714332492918 |
  ;;| 01) crowning disabled |        wa-minnow-ridge |         75.0 |  75.46299743652344 |  75.46299743652344 |         232.16580953655773 |        21100.796906307733 |  0.013728000223636627 |                    7221.418852008852 |  0.3138266174914029 |               0.3138266174914029 |   0.3443275988101959 |        80.0 |  7.472068786621094 |   287.1802978515625 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   80.0 |   287.1802978515625 |   245.61668306954095 |             0.6045307849369531 |                      89.88809200688462 |                     245.61668306954095 |            0.7427615212038559 |                 1.775635475565461 |     67.06792563956958 |  0.6045307849369531 |     1.775635475565461 |
  ;;|          00) original | id-kootenai-rv-complex |         55.0 |   62.3390007019043 | 3.9372000694274902 |         1212.4688549077016 |        251.46584509473425 |  0.006864000111818314 |                     7483.46789576118 |  0.3138266174914029 |               0.3138266174914029 |    0.839099645614624 |        80.0 |    5.0785813331604 |    159.338623046875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  102.0 |    159.338623046875 |    284.3711161087375 |             0.9253716271863359 |                      576.6100981300586 |                      284.3711161087375 |            0.6712065979974903 |                 8.948408004899568 |    1993.7547005028916 |  0.7910997685385767 |    30.748700331217115 |
  ;;|          00) original | id-kootenai-rv-complex |         55.0 |   62.3390007019043 | 2.2967000007629395 |          6.060497318341122 |        112.03512054935626 |  0.006864000111818314 |                     744.979692476773 |  0.2504714934367811 |               0.2504714934367811 |   0.3443275988101959 |        80.0 |  5.577175140380859 |  236.59725952148438 |              20.0 |  110.00000000000001 |                              0.3 |                               8000.0 |  169.0 |  236.59725952148438 |  0.23958921689552426 |             0.7228143784682609 |                     157.37552623965752 |                    0.23958921689552426 |            0.1834649400802163 |                0.5629546173770308 |    1.7507529825200627 |  0.7228143784682609 |    0.5629546173770308 |
  ;;|          00) original |            ca-mosquito |         54.0 |  72.18199920654297 |  9.843000411987305 |          393.9669967645233 |         994.0060664134882 |  0.006864000111818314 |                    8187.046767620315 |  0.3138266174914029 |               0.3138266174914029 |  0.19438031315803528 |        80.0 |  5.537454128265381 |  231.25347900390625 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  213.0 |  231.25347900390625 |   42.191648699909585 |             0.5894911882447272 |                       83.7650165272179 |                     42.191648699909585 |             1.165656020993763 |                2.6577287628752346 |    113.80895962326012 |  0.5894911882447272 |    2.6577287628752346 |
  ;;|          00) original |        or-double-creek |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                   1613.0266497907908 | 0.22971884333771272 |              0.22971884333771272 |   0.9656887650489807 |        80.0 |  5.142111778259277 |  205.89207458496094 |              20.0 |                     |                              1.0 |                               8000.0 |   58.0 |  205.89207458496094 |   243.32840264694002 |             0.9250985951150147 |                      574.9817878355083 |                     243.32840264694002 |           0.14680085670262077 |                 2.527878065505799 |    15.611442533099689 |  0.9250985951150147 |     2.527878065505799 |
  ;;|          00) original | id-columbus-bear-gulch |         85.0 | 101.71099853515625 |  5.905799865722656 |          524.2505799662077 |          461.972228189994 |  0.013728000223636627 |                    7319.006138395373 |  0.3138266174914029 |               0.3138266174914029 |  0.48773258924484253 |        80.0 | 2.8223073482513428 |  260.80731201171875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   17.0 |  260.80731201171875 |   194.33260104697615 |             0.7877508066912929 |                       219.434100302106 |                     194.33260104697615 |            0.7966533948384921 |                3.9560784406277283 |     2574.127240953897 |  0.6734707217381242 |    13.815332089302808 |
  ;;|          00) original | id-columbus-bear-gulch |         75.0 |  88.58699798583984 |  5.249599933624268 |          725.9359851426885 |        387.15697803651994 |  0.013728000223636627 |                    7562.836385179956 |  0.3138266174914029 |               0.3138266174914029 |   0.5317094326019287 |        80.0 |  5.274412155151367 |   275.0106201171875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  251.0 |   275.0106201171875 |    73.13059219196558 |             0.8299638420962587 |                      279.0311144839839 |                      73.13059219196558 |            0.8470614504651965 |                 5.301413824025462 |      9291.08145183957 |  0.7979956395935981 |     59.53407165560918 |
  ;;| 01) crowning disabled |             id-ross-fk |         15.0 | 36.090999603271484 | 36.090999603271484 |                            |                           |  0.003120000008493662 |               3.1579922255442283E-13 | 0.18698337292161518 |              0.18698337292161518 |   0.3057306706905365 |        80.0 | 2.1210365295410156 |  247.45944213867188 |              20.0 |                     |                              0.4 |                               8000.0 |  276.0 |  247.45944213867188 |                  0.0 |                            0.0 |                                    0.0 |                                    0.0 |         4.593604787174521E-16 |             4.593604787174521E-16 |  4.52081175418463E-31 |                 0.0 | 4.593604787174521E-16 |
  ;;|          00) original |            or-sturgill |         45.0 |  75.46299743652344 |  5.905799865722656 |          8.045833795691916 |          461.972228189994 |  0.004991999827325344 |                   1785.0959395426805 | 0.22971884333771272 |              0.22971884333771272 |  0.24932800233364105 |        80.0 | 6.5906524658203125 |  138.96951293945312 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   16.0 |  138.96951293945312 |    231.3823169000226 |             0.5785536821357893 |                      79.56066768280502 |                      231.3823169000226 |           0.16684132569409824 |               0.34007972115511725 |    2.3242758431784982 |  0.5785536821357893 |   0.34007972115511725 |
  ;;|          00) original | id-kootenai-rv-complex |         55.0 |  88.58699798583984 |  6.561999797821045 |         20.288533512631307 |          541.068310789092 |  0.006864000111818314 |                   1640.9267349790418 | 0.22971884333771272 |              0.22971884333771272 |    0.600860595703125 |        80.0 | 7.0949578285217285 |  246.17483520507812 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   25.0 |  246.17483520507812 |     201.310379008341 |             0.8440629338979558 |                     304.41745536295986 |                       201.310379008341 |           0.12111827771565685 |                0.9328948740506593 |    5.8609399018627215 |  0.8440629338979558 |    0.9328948740506593 |
  ;;|          00) original |               id-lemhi |         35.0 |  49.21500015258789 | 11.811599731445312 |                            |                           | 0.0037440001033246517 |                   2608.5578028185246 | 0.21012730729142196 |              0.21012730729142196 |   0.6494075655937195 |        80.0 |  5.901920795440674 |  213.63204956054688 |              20.0 |                     |                              1.5 |                               8000.0 |  286.0 |  213.63204956054688 |   100.82576324156827 |             0.8616301687483768 |                      341.5459827793199 |                     100.82576324156827 |            1.9823459305938664 |                 29.53502859074141 |     269.8168731910849 |  0.8616301687483768 |     29.53502859074141 |
  ;;|          00) original |           wa-irving-pk |         65.0 |  49.21500015258789 | 0.9843000173568726 |          307.4816665928575 |         31.43323063684178 |  0.009983999654650688 |                    7726.100466052186 |  0.3138266174914029 |               0.3138266174914029 |   0.3443275988101959 |        80.0 | 1.7512824535369873 |      327.7255859375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  164.0 |      327.7255859375 |   345.43919555035177 |             0.6574708285759449 |                     115.16568858309583 |                     345.43919555035177 |            0.7684803952375502 |                 2.198046932077079 |     1197.312093242063 |  0.5717829870386788 |     17.26488662007889 |
  ;;|          00) original |            ca-mosquito |         52.0 | 104.99199676513672 |  6.561999797821045 |         1158.9357255925809 |          541.068310789092 |  0.004991999827325344 |                     8313.42139115912 |  0.3138266174914029 |               0.3138266174914029 |   0.5773502588272095 |        80.0 | 6.4066901206970215 |   269.2918395996094 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  155.0 |   269.2918395996094 |   341.86953386333215 |             0.8343176175461152 |                     286.50931237221175 |                     341.86953386333215 |            1.1984514351328195 |                 7.699413462202791 |     2759.546683966378 |  0.8316517359853719 |     37.01066717639531 |
  ;;|          00) original |               id-lemhi |         25.0 |   62.3390007019043 | 11.811599731445312 |                            |                           |  0.002495999913662672 |                    2041.208740799073 |   0.217382350167601 |                0.217382350167601 |    0.700207531452179 |        80.0 | 3.3275206089019775 |      332.9521484375 |              20.0 |                     |                              1.0 |                               8000.0 |   17.0 |      332.9521484375 |   195.31946422095916 |              0.881335692587977 |                      392.9883286226485 |                     195.31946422095916 |            1.1107313765088038 |                14.846762612118095 |     109.7974397201721 |   0.881335692587977 |    14.846762612118095 |
  ;;| 01) crowning disabled |            ca-mosquito |         22.0 |   68.9010009765625 |   68.9010009765625 |                            |                           |  0.002495999913662672 |                    2143.520098693678 | 0.21012730729142196 |              0.21012730729142196 | 0.052407778799533844 |        80.0 |  4.329098701477051 |      307.0478515625 |              20.0 |                     |                              1.5 |                               8000.0 |  199.0 |      307.0478515625 |   122.43395427444489 |             0.5110824294714507 |                      57.52532631345533 |                     122.43395427444489 |            1.5330548880408974 |                 3.119610302827593 |    23.418504130699258 |  0.5110824294714507 |     3.119610302827593 |
  ;;|          00) original |        wa-minnow-ridge |         45.0 |   62.3390007019043 | 2.2967000007629395 |         55.718467455230495 |        112.03512054935626 |  0.004991999827325344 |                   1693.6303349771106 |   0.217382350167601 |                0.217382350167601 |   0.1051042377948761 |        80.0 | 14.776552200317383 |   281.2853698730469 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  190.0 |   281.2853698730469 |    95.94566630707476 |             0.7027303878268085 |                     142.75986898488173 |                      95.94566630707476 |            0.6723705317090835 |                2.6231560115682235 |     16.09591885858519 |  0.7027303878268085 |    2.6231560115682235 |
  ;;|          00) original |        wa-minnow-ridge |         65.0 | 101.71099853515625 | 7.8744001388549805 |         466.54210484857435 |           711.25281721317 | 0.0074880002066493034 |                    7486.447987857256 |  0.3138266174914029 |               0.3138266174914029 |    0.466307669878006 |        80.0 | 1.9604164361953735 |  209.18499755859375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  266.0 |  209.18499755859375 |    84.01503394739063 |             0.7806518317398174 |                     211.23049044624744 |                      84.01503394739063 |            0.7181995019730869 |                3.4418593465967575 |     134.7744151396479 |  0.7806518317398174 |    3.4418593465967575 |
  ;;| 01) crowning disabled |               id-lemhi |         15.0 |  49.21500015258789 |  49.21500015258789 |                            |                           | 0.0018720000516623259 |                   2542.9849766464913 | 0.21012730729142196 |              0.21012730729142196 |    0.600860595703125 |        80.0 | 2.4025814533233643 |    67.1864013671875 |              20.0 |                     |                              1.5 |                               8000.0 |   21.0 |    67.1864013671875 |   203.27212368059023 |             0.8460850762979306 |                      308.3534460329929 |                     203.27212368059023 |            1.9224218478382362 |                24.941415930627823 |    222.12433681423892 |  0.8460850762979306 |    24.941415930627823 |
  ;;|          00) original |            ca-mosquito |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                   3365.6229818824945 |  0.2283338581461741 |               0.2283338581461741 |   0.1227845624089241 |        80.0 | 5.2178802490234375 |  237.71560668945312 |              20.0 |                     |                              3.0 |                               8000.0 |   84.0 |  237.71560668945312 |    55.98223734573878 |             0.7720816752380019 |                      201.8742553641729 |                      55.98223734573878 |             2.053850144115899 |                16.902783787311886 |    216.49245502420274 |  0.7720816752380019 |    16.902783787311886 |
  ;;|          00) original | id-columbus-bear-gulch |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                    1162.524671161579 | 0.22971884333771272 |              0.22971884333771272 |   0.3249197006225586 |        80.0 | 2.3968687057495117 |  200.20590209960938 |              20.0 |                     |                              1.0 |                               8000.0 |  198.0 |  200.20590209960938 |                 18.0 |             0.7431023920891578 |                      174.0187058418701 |                                   18.0 |           0.11849633131838767 |               0.48359177495614925 |    2.1524172030316038 |  0.7431023920891578 |   0.48359177495614925 |
  ;;|          00) original | id-kootenai-rv-complex |         45.0 |  49.21500015258789 | 1.6404999494552612 |          93.10587368710327 |          67.6335388486365 |  0.004991999827325344 |                   1876.4069928466165 |   0.217382350167601 |                0.217382350167601 |   0.4244748055934906 |        80.0 |  3.254014492034912 |      86.09130859375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  132.0 |      86.09130859375 |    308.8554959755593 |              0.769099573721632 |                     198.74956426210463 |                      308.8554959755593 |             0.699053934814188 |                 3.956340111556934 |    1014.8909780148139 |  0.7033361154860215 |    31.200894221408102 |
  ;;|          00) original | id-kootenai-rv-complex |         55.0 |   62.3390007019043 | 2.2967000007629395 |          274.1983516710644 |        112.03512054935626 |  0.006864000111818314 |                    6899.135368423075 |  0.3138266174914029 |               0.3138266174914029 |   0.4244748055934906 |        80.0 |  6.128656387329102 |  197.86398315429688 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  335.0 |  197.86398315429688 |    143.4503406171808 |             0.7150553166701752 |                      151.5276485871603 |                      143.4503406171808 |            0.6153015731521301 |                2.1950694189143394 |    1539.0296323918712 |  0.8242506452369354 |    26.565976138832745 |
  ;;|          00) original |        wa-minnow-ridge |         65.0 |  88.58699798583984 |  2.952899932861328 |          891.6729053134692 |        163.33184763650195 |  0.009983999654650688 |                     8491.24840565566 |  0.3138266174914029 |               0.3138266174914029 |   0.5317094326019287 |        80.0 |  2.299785614013672 |  209.57945251464844 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  210.0 |  209.57945251464844 |                 30.0 |             0.8231467577627342 |                      267.9052889427494 |                                   30.0 |            0.9645018323517208 |                 5.799787880609821 |    2749.2648714170045 |  0.6298519703837342 |    21.857578930617777 |
  ;;| 01) crowning disabled |        or-double-creek |         45.0 |  75.46299743652344 |  75.46299743652344 |          137.3357763440063 |        21100.796906307733 |  0.004991999827325344 |                    2828.406838409663 | 0.21692336615184582 |              0.21692336615184582 |   0.6248693466186523 |        80.0 |  7.594292640686035 |       259.470703125 |              20.0 |  110.00000000000001 |                              0.3 |                               8000.0 |   30.0 |       259.470703125 |   205.62195563333876 |             0.8647475744296322 |                     348.90654236357517 |                     205.62195563333876 |            0.6356153877163283 |                3.8797463664436824 |     39.67348014713597 |  0.8647475744296322 |    3.8797463664436824 |
  ;;| 01) crowning disabled | id-kootenai-rv-complex |         55.0 |   62.3390007019043 |   62.3390007019043 |         20.458959963158655 |         15843.00408983124 |  0.006864000111818314 |                    1789.429948892311 |   0.217382350167601 |                0.217382350167601 |  0.14054083824157715 |        80.0 | 7.0949578285217285 |  246.17483520507812 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   61.0 |  246.17483520507812 |    72.44857507054553 |             0.4190650868833536 |                     35.683902350708316 |                      72.44857507054553 |            0.6518279064662649 |                0.9116170368786649 |     5.910172596951634 |  0.4190650868833536 |    0.9116170368786649 |
  ;;| 01) crowning disabled |            or-sturgill |         35.0 |   62.3390007019043 |   62.3390007019043 |                            |                           | 0.0037440001033246517 |                   1976.2063364961534 |   0.217382350167601 |                0.217382350167601 |  0.23086819052696228 |        80.0 |  7.688671588897705 |  195.51626586914062 |              20.0 |                     |                              1.0 |                               8000.0 |  263.0 |  195.51626586914062 |   48.440051721386794 |             0.6630352901734371 |                     118.21851379072754 |                     48.440051721386794 |             0.844739393042372 |                 2.715562183162512 |    19.443080257450774 |  0.6630352901734371 |     2.715562183162512 |
  ;;|          00) original |        wa-minnow-ridge |         55.0 |  75.46299743652344 |  5.249599933624268 |         406.30128181475857 |        387.15697803651994 |  0.006864000111818314 |                    7900.900161216143 |  0.3138266174914029 |               0.3138266174914029 |  0.36397022008895874 |        80.0 |  4.619705677032471 |  217.43324279785156 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  181.0 |  217.43324279785156 |    7.541169031552878 |              0.723139824674347 |                     157.62668814733763 |                      7.541169031552878 |            0.7698621239797476 |                2.8402051102422874 |    2158.5937662553733 |  0.7733910794414444 |    31.765388167948565 |
  ;;|          00) original |         mt-george-lake |          0.0 |                0.0 |                0.0 |                            |                           |                   0.0 |                   1522.3337886960685 | 0.22971884333771272 |              0.22971884333771272 |    0.554309070110321 |        80.0 | 7.0223236083984375 |       276.666015625 |              20.0 |                     |                              1.0 |                               8000.0 |  355.0 |       276.666015625 |   143.54363230400742 |             0.8660180985156932 |                     351.98219645474774 |                     143.54363230400742 |           0.13961546028298605 |                1.2845575819769823 |    7.4870172572246805 |  0.8660180985156932 |    1.2845575819769823 |
  ;;|          00) original |            ca-mosquito |         66.0 | 108.27300262451172 |   6.23390007019043 |          535.7386056359675 |          500.999779032296 | 0.0074880002066493034 |                    8437.965873504401 |  0.3138266174914029 |               0.3138266174914029 |   0.3057306706905365 |        80.0 |  7.733380317687988 |   259.0696105957031 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  342.0 |   259.0696105957031 |   137.77357055286814 |             0.6563967754390739 |                       114.586171594721 |                     137.77357055286814 |            1.2307237133577962 |                3.5066568101306403 |      9014.57410704897 |  0.8610778997685832 |      86.9667430753827 |
  ;;|          00) original |        or-double-creek |         35.0 | 36.090999603271484 | 0.9843000173568726 |                            |                           |  0.005615999922156334 |                   1781.0981657331276 | 0.22971884333771272 |              0.22971884333771272 |   1.1503684520721436 |        80.0 |  1.886354684829712 |   160.3096923828125 |              20.0 |                     |                              1.0 |                               8000.0 |  289.0 |   160.3096923828125 |   108.68384107392501 |             0.9531872781686295 |                      812.0959631840343 |                     108.68384107392501 |           0.16383594710833235 |                 4.457682501281892 |    30.397814434127216 |  0.9531872781686295 |     4.457682501281892 |
  ;;| 01) crowning disabled |            ca-mosquito |         54.0 | 104.99199676513672 | 104.99199676513672 |          219.9863562613229 |         34628.37189050189 |  0.004991999827325344 |                    7715.385396267361 |  0.3138266174914029 |               0.3138266174914029 |  0.19438031315803528 |        80.0 |  1.345163345336914 |      99.34033203125 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  293.0 |      99.34033203125 |   115.81226158683111 |             0.4396899205435315 |                     39.916876564833544 |                     115.81226158683111 |              1.02519153328456 |                 1.574766645639867 |     63.54953217661897 |  0.4396899205435315 |     1.574766645639867 |
  ;;|          00) original |               id-lemhi |         35.0 |  49.21500015258789 | 3.2809998989105225 |                            |                           | 0.0037440001033246517 |                   2122.5849342504353 |   0.217382350167601 |                0.217382350167601 |   0.6745085120201111 |        80.0 | 3.0400640964508057 |          258.484375 |              20.0 |                     |                              1.0 |                               8000.0 |  335.0 |          258.484375 |   153.08026165501974 |             0.8720208881325621 |                      367.1471760838731 |                     153.08026165501974 |            1.1805779969972423 |                14.425622195145374 |    110.93604036428778 |  0.8720208881325621 |    14.425622195145374 |
  ;;|          00) original | id-kootenai-rv-complex |         55.0 |  75.46299743652344 |  2.624799966812134 |         367.25710047531066 |        136.88066227665723 |  0.006864000111818314 |                    7614.742024063769 |  0.3138266174914029 |               0.3138266174914029 |   0.4452286958694458 |        80.0 |  5.726394176483154 |   186.2341766357422 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  329.0 |   186.2341766357422 |   140.97365970678044 |              0.734765806355032 |                     166.92845475123787 |                     140.97365970678044 |            0.6871475297930618 |                2.6637477285152427 |    2263.7060428781456 |  0.8126021756137274 |     32.36669433631717 |
  ;;|          00) original |            ca-mosquito |         48.0 |  78.74400329589844 |  4.921500205993652 |         429.34714110928866 |        351.43421505077157 |  0.005615999922156334 |                     8215.46120573934 |  0.3138266174914029 |               0.3138266174914029 |   0.2679491937160492 |        80.0 |  5.025650978088379 |  244.73843383789062 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  165.0 |  244.73843383789062 |    9.253996700402752 |             0.6134698221928285 |                      93.72863938008818 |                      9.253996700402752 |            1.1730377175605453 |                 2.886388012311979 |     1778.598276940534 |  0.7891716049164961 |    29.931619358971766 |
  ;;|          00) original |        wa-minnow-ridge |         75.0 |  88.58699798583984 |  5.249599933624268 |          565.5971541528203 |        387.15697803651994 |  0.013728000223636627 |                    7804.263547656061 |  0.3138266174914029 |               0.3138266174914029 |  0.48773258924484253 |        80.0 |  6.936948776245117 |    125.705810546875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  270.0 |    125.705810546875 |    83.71837527241138 |             0.7693203550805021 |                     198.97868025281983 |                      83.71837527241138 |            0.8825509721634992 |                 4.002703072699715 |    13502.737077633625 |   0.844481567163017 |     87.44775041193552 |
  ;;|          00) original |            ca-mosquito |         51.0 |  95.14900207519531 |  5.905799865722656 |         203.91543400605528 |          461.972228189994 |  0.006240000016987324 |                     7724.36684115741 |  0.3138266174914029 |               0.3138266174914029 |  0.14054083824157715 |        80.0 | 3.3694212436676025 |   102.9224853515625 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  208.0 |   102.9224853515625 |     334.127373592471 |              0.400854171616975 |                      32.21996742646377 |                       334.127373592471 |             1.029061522821625 |                1.4580260825352067 |     58.90697339103745 |   0.400854171616975 |    1.4580260825352067 |
  ;;|          00) original |        wa-minnow-ridge |         45.0 |  49.21500015258789 | 0.9843000173568726 |          316.3366650021386 |         31.43323063684178 |  0.004991999827325344 |                    3621.799815727651 | 0.21692336615184582 |              0.21692336615184582 |   0.6494075655937195 |        80.0 | 2.9839539527893066 |   231.0802001953125 |              20.0 |  110.00000000000001 |                              0.3 |                               8000.0 |  242.0 |   231.0802001953125 |                 62.0 |             0.8806636300914066 |                      391.0203293412679 |                                   62.0 |            0.9956241890076665 |                6.9788942281331465 |    1200.4624257366504 |  0.6852210417764236 |     34.54822377693597 |
  ;;|          00) original | id-kootenai-rv-complex |         75.0 |  88.58699798583984 |  5.249599933624268 |           255.655564380163 |        387.15697803651994 |  0.013728000223636627 |                    6564.516756875821 |  0.3138266174914029 |               0.3138266174914029 |  0.40402624011039734 |        80.0 |  2.470756769180298 |  278.88616943359375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  345.0 |  278.88616943359375 |    161.3931844007783 |              0.734153995770083 |                      166.4224127325961 |                      161.3931844007783 |            0.5563343178971561 |                 2.150951283985354 |     73.85363251986958 |   0.734153995770083 |     2.150951283985354 |
  ;;| 01) crowning disabled |        wa-minnow-ridge |         35.0 |  75.46299743652344 |  75.46299743652344 |                            |                           | 0.0037440001033246517 |                   1527.7361826818903 |   0.217382350167601 |                0.217382350167601 |   0.1051042377948761 |        80.0 |  6.254113674163818 |   272.0964660644531 |              20.0 |                     |                              1.0 |                               8000.0 |   27.0 |   272.0964660644531 |   108.38332110876695 |             0.5254821671204497 |                      61.72601380554787 |                     108.38332110876695 |            0.5196523258909164 |                0.9735464703240887 |     5.388626473126502 |  0.5254821671204497 |    0.9735464703240887 |
  ;;|          00) original |            or-sturgill |         55.0 |  75.46299743652344 |  2.624799966812134 |         184.02751061369852 |        136.88066227665723 |  0.006864000111818314 |                   2052.3289453802936 |   0.217382350167601 |                0.217382350167601 |  0.48773258924484253 |        80.0 |  9.451106071472168 |   199.4600372314453 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  215.0 |   199.4600372314453 |    32.27915089147899 |             0.8210914446137448 |                      264.6819512819859 |                      32.27915089147899 |            0.8912052267902452 |                 7.149560790342472 |     10923.96035529645 |   0.888733786663068 |    163.07456970621212 |
  ;;|          00) original |            ca-mosquito |         75.0 |  78.74400329589844 | 3.2809998989105225 |         1014.7979480319211 |        191.29653582205867 |  0.013728000223636627 |                    8437.965873504401 |  0.3138266174914029 |               0.3138266174914029 |      0.5095254778862 |        80.0 |  7.733380317687988 |   259.0696105957031 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  356.0 |   259.0696105957031 |    165.8800577371735 |             0.8037248258989149 |                      239.5954730473244 |                      165.8800577371735 |            1.2307237133577962 |                 6.642321643310427 |     13771.89057821914 |  0.8610778997685832 |     97.58185679081566 |
  ;;|          00) original |               id-lemhi |         35.0 |  49.21500015258789 | 11.811599731445312 |                            |                           | 0.0037440001033246517 |                   2551.2610407309844 | 0.21012730729142196 |              0.21012730729142196 |   0.8692867159843445 |        80.0 |  4.076582908630371 |   241.9470977783203 |              20.0 |                     |                              1.5 |                               8000.0 |  287.0 |   241.9470977783203 |    105.7377686683746 |             0.9119141017392729 |                      505.7392617663179 |                      105.7377686683746 |            1.9299718138390096 |                  49.4781656839822 |      442.078844632587 |  0.9119141017392729 |      49.4781656839822 |
  ;;|          00) original |        wa-minnow-ridge |         45.0 |  75.46299743652344 |  2.952899932861328 |         30.001054556617614 |        163.33184763650195 |  0.004991999827325344 |                   1842.4756109481632 |   0.217382350167601 |                0.217382350167601 |  0.21255655586719513 |        80.0 | 10.444052696228027 |   96.82037353515625 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  266.0 |   96.82037353515625 |    296.7317921922768 |            0.47486744980915685 |                      47.97394387716279 |                      296.7317921922768 |            0.8069754065682824 |                1.2983098592924294 |     8.666687399528852 | 0.47486744980915685 |    1.2983098592924294 |
  ;;|          00) original |           wa-irving-pk |         75.0 |   62.3390007019043 | 4.2652997970581055 |          35.74199007287372 |         283.5450846339534 |  0.013728000223636627 |                    1773.500206411531 |   0.217382350167601 |                0.217382350167601 |  0.24932800233364105 |        80.0 |  3.916884183883667 |   73.16604614257812 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  175.0 |   73.16604614257812 |    343.6132913097015 |             0.6036747036258078 |                      89.52838668735816 |                      343.6132913097015 |            0.6459899271522007 |                1.6069082063491944 |    10.325125552306037 |  0.6036747036258078 |    1.6069082063491944 |
  ;;|          00) original |            mt-billiard |         75.0 |  88.58699798583984 |  5.249599933624268 |         192.41824577223886 |        387.15697803651994 |  0.013728000223636627 |                     7275.84660348438 |  0.3138266174914029 |               0.3138266174914029 |  0.24932800233364105 |        80.0 | 3.5367655754089355 |   304.3478088378906 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  127.0 |   304.3478088378906 |    308.0498192147479 |             0.4970876647703658 |                      53.67001239805913 |                      308.0498192147479 |            0.8304098777851545 |                1.4606319717328367 |      55.5856722611727 |  0.4970876647703658 |    1.4606319717328367 |
  ;;|          00) original |           wa-irving-pk |         75.0 |   62.3390007019043 | 1.9686000347137451 |         227.69251129611067 |         88.90660215164625 |  0.013728000223636627 |                    7069.997141930174 |  0.3138266174914029 |               0.3138266174914029 |  0.17632697522640228 |        80.0 | 12.934005737304688 |  292.22088623046875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  218.0 |  292.22088623046875 |     95.1335598543709 |             0.6447748378374039 |                     108.50842439100829 |                       95.1335598543709 |            0.6504435290715462 |                1.7787200266594234 |    10369.955426402847 |  0.9240989300233594 |     93.24882185758042 |
  ;;|          00) original |            or-sturgill |         45.0 |   62.3390007019043 | 1.6404999494552612 |         207.25999318358225 |          67.6335388486365 |  0.004991999827325344 |                    3856.855274870749 | 0.21692336615184582 |              0.21692336615184582 |  0.38386404514312744 |        80.0 | 10.325777053833008 |   202.3260955810547 |              20.0 |  110.00000000000001 |                              0.3 |                               8000.0 |  212.0 |   202.3260955810547 |                 32.0 |             0.8000803717231171 |                     234.77148843843284 |                                   32.0 |            1.1031102810126254 |                 4.293818310573165 |     7882.760872332602 |  0.8996831209216393 |     193.6314219680523 |
  ;;|          00) original |            ca-mosquito |         56.0 |  85.30599975585938 |  4.921500205993652 |          616.3960885224348 |        351.43421505077157 | 0.0074880002066493034 |                    7984.896113827261 |  0.3138266174914029 |               0.3138266174914029 |  0.38386404514312744 |        80.0 |  4.461589813232422 |   300.1851806640625 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  348.0 |   300.1851806640625 |   160.92612886142695 |             0.7331694755698752 |                     165.61207270812227 |                     160.92612886142695 |            1.1074343055160798 |                 4.263523782776732 |    1916.8743453642844 |  0.7667275263839848 |    21.665827541760766 |
  ;;|          00) original |           wa-irving-pk |         65.0 |  75.46299743652344 | 4.2652997970581055 |          339.4286063900698 |         283.5450846339534 |  0.009983999654650688 |                    7961.329686871307 |  0.3138266174914029 |               0.3138266174914029 |  0.38386404514312744 |        80.0 |  7.307188034057617 |  123.37698364257812 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  244.0 |  123.37698364257812 |   45.839962882569296 |             0.6886874227554532 |                      133.4781624851841 |                     45.839962882569296 |            0.7330564501373678 |                2.3547289222485954 |    11777.650517839309 |   0.852559433857372 |     123.2306100315809 |
  ;;| 01) crowning disabled |            or-sturgill |         55.0 |  75.46299743652344 |  75.46299743652344 |          5.492232441885167 |        21100.796906307733 |  0.006864000111818314 |                    1704.145296526402 | 0.22971884333771272 |              0.22971884333771272 | 0.052407778799533844 |        80.0 |  6.110177040100098 |   128.2122802734375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  254.0 |   128.2122802734375 |   312.99967260438393 |             0.4794447947531673 |                      49.10667222637928 |                     312.99967260438393 |           0.15882958166330807 |               0.24317197936698726 |    1.5865929515757737 |  0.4794447947531673 |   0.24317197936698726 |
  ;;|          00) original |        or-double-creek |         45.0 |  75.46299743652344 |  2.952899932861328 |          419.1809733303556 |        163.33184763650195 |  0.004991999827325344 |                   3944.4165691590956 | 0.21692336615184582 |              0.21692336615184582 |   0.7265425324440002 |        80.0 | 11.315101623535156 |   211.5686492919922 |              20.0 |  110.00000000000001 |                              0.3 |                               8000.0 |   33.0 |   211.5686492919922 |   213.20229858535816 |              0.886778135537986 |                      409.5827565526677 |                     213.20229858535816 |            1.1439856319693753 |                 8.491419838243562 |    10892.724933704523 |  0.9101933166348578 |      223.187428041654 |
  ;;|          00) original |               id-lemhi |         35.0 |   62.3390007019043 |  4.921500205993652 |                            |                           | 0.0037440001033246517 |                      9137.3656196719 |  0.3138266174914029 |               0.3138266174914029 |    0.600860595703125 |        80.0 | 3.2649519443511963 |  170.20989990234375 |              20.0 |                     |                              1.0 |                               8000.0 |  260.0 |  170.20989990234375 |    75.62780704555757 |             0.8516985989990064 |                     319.71476428381294 |                      75.62780704555757 |            1.3939173864038987 |                 9.994275582476499 |    477.65117344828394 |  0.8516985989990064 |     9.994275582476499 |
  ;;|          00) original | id-columbus-bear-gulch |         85.0 |  75.46299743652344 |  3.609100103378296 |          734.6136999095363 |         220.6968690160058 |  0.018719999119639397 |                    7249.038420111354 |  0.3138266174914029 |               0.3138266174914029 |    0.600860595703125 |        80.0 | 2.3010318279266357 |   261.4784851074219 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  352.0 |   261.4784851074219 |    170.3583226636997 |             0.8513003536957541 |                      318.8868204728594 |                      170.3583226636997 |            0.7826725400829578 |                 5.597018053194112 |    2096.8184730017183 |  0.6299678296476493 |    10.508122991707408 |
  ;;|          00) original |            ca-mosquito |         59.0 | 118.11599731445312 |  7.218200206756592 |         286.50312609046506 |         624.2250106714278 |  0.006240000016987324 |                     8215.46120573934 |  0.3138266174914029 |               0.3138266174914029 |   0.1051042377948761 |        80.0 |  5.025650978088379 |  244.73843383789062 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  280.0 |  244.73843383789062 |    74.48163367608561 |              0.468952337497752 |                     46.540133553864116 |                      74.48163367608561 |            1.1730377175605453 |                1.9260852337361352 |     82.76485841949025 |   0.468952337497752 |    1.9260852337361352 |
  ;;|          00) original |            ca-mosquito |         70.0 |  91.86799621582031 |  4.593400001525879 |         205.28253743403218 |        316.88317388600854 |  0.011855999939143658 |                    7834.001680969677 |  0.3138266174914029 |               0.3138266174914029 | 0.052407778799533844 |        80.0 |  4.459424018859863 |  113.29302978515625 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  285.0 |  113.29302978515625 |    294.3662786447155 |             0.3794759239719558 |                     28.457617258585564 |                      294.3662786447155 |            1.0639315238497946 |                  1.44725957927082 |      59.3019014436744 |  0.3794759239719558 |      1.44725957927082 |
  ;;|          00) original |           wa-irving-pk |         65.0 |   62.3390007019043 |  2.624799966812134 |         268.01702139342865 |        136.88066227665723 |  0.009983999654650688 |                    7017.074883608683 |  0.3138266174914029 |               0.3138266174914029 |  0.24932800233364105 |        80.0 | 12.407780647277832 |  288.67547607421875 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  228.0 |  288.67547607421875 |    84.81091581850106 |             0.6942366058279423 |                     137.06084238333466 |                      84.81091581850106 |            0.6427372817776936 |                2.1095234191737995 |     6372.999537365499 |  0.9199620850695541 |     79.19804841812626 |
  ;;|          00) original |            ca-mosquito |         71.0 |   82.0250015258789 | 3.9372000694274902 |         303.71064597995075 |        251.46584509473425 |  0.011855999939143658 |                    8309.230904367794 |  0.3138266174914029 |               0.3138266174914029 | 0.034920770674943924 |        80.0 |  6.808093070983887 |   269.1046142578125 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   -1.0 |   269.1046142578125 |    90.84338599494447 |              0.479665721488335 |                      49.16186356178367 |                      90.84338599494447 |             1.198317119879811 |                 2.018725522461544 |      9441.38358481548 |  0.8415077014723206 |      75.7741186731061 |
  ;;|          00) original |            ca-mosquito |         66.0 | 118.11599731445312 |  6.890100002288818 |          1077.896712212608 |         582.1515630707672 | 0.0074880002066493034 |                     8290.74331450815 |  0.3138266174914029 |               0.3138266174914029 |      0.5095254778862 |        80.0 |  7.331422328948975 |    269.997314453125 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  304.0 |    269.997314453125 |   119.53256671216337 |             0.8232590863168772 |                      268.0831474494294 |                     119.53256671216337 |            1.1933573912970477 |                   7.1806170147323 |     3639.504450496999 |   0.853065305355132 |    29.970139918365415 |
  ;;|          00) original |        wa-minnow-ridge |         65.0 |  88.58699798583984 |  5.905799865722656 |           339.915535518098 |          461.972228189994 |  0.009983999654650688 |                    7771.588429017369 |  0.3138266174914029 |               0.3138266174914029 |   0.3057306706905365 |        80.0 |  3.856501817703247 |  344.26409912109375 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   36.0 |  344.26409912109375 |   207.11112309875728 |             0.6553309938863302 |                     114.01417558743789 |                     207.11112309875728 |            0.8510621415441114 |                2.4156794622955546 |     98.19460456029164 |  0.6553309938863302 |    2.4156794622955546 |
  ;;| 01) crowning disabled |               id-lemhi |         35.0 |   62.3390007019043 |   62.3390007019043 |                            |                           | 0.0037440001033246517 |                   2099.9712931549607 |   0.217382350167601 |                0.217382350167601 |    0.839099645614624 |        80.0 | 3.5224287509918213 |    9.46441650390625 |              20.0 |                     |                              1.0 |                               8000.0 |  347.0 |    9.46441650390625 |   167.55250912629575 |             0.9113935434309764 |                     503.33241244435413 |                     167.55250912629575 |            1.1611705463056652 |                   21.626903690724 |     164.5435009615636 |  0.9113935434309764 |       21.626903690724 |
  ;;|          00) original |           wa-irving-pk |         65.0 |  49.21500015258789 | 0.9843000173568726 |         202.85851457016727 |         31.43323063684178 |  0.009983999654650688 |                    7144.152486145403 |  0.3138266174914029 |               0.3138266174914029 |  0.08748866617679596 |        80.0 |  9.692375183105469 |   287.7334899902344 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  255.0 |   287.7334899902344 |   104.75022123519167 |             0.5997845516919595 |                      87.91117324959625 |                     104.75022123519167 |            0.6660043729484402 |                1.5682691932716974 |     5471.459641518729 |  0.8919306656178762 |      84.3062501888714 |
  ;;|          00) original | id-kootenai-rv-complex |         55.0 |  49.21500015258789 | 0.9843000173568726 |         102.25164622277526 |         31.43323063684178 |  0.006864000111818314 |                   1753.8277011347138 |   0.217382350167601 |                0.217382350167601 |      0.5095254778862 |        80.0 |  2.277345895767212 |  237.04441833496094 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |  147.0 |  237.04441833496094 |    328.6689200826374 |             0.8089060911362324 |                     246.70272738173298 |                      328.6689200826374 |             0.632509961014887 |                 4.648651246901098 |     859.2030193694175 |   0.627754871068983 |     18.79589399457464 |
  ;;|          00) original |        wa-minnow-ridge |         45.0 |   62.3390007019043 | 2.2967000007629395 |         37.147984238494715 |        112.03512054935626 |  0.004991999827325344 |                    1645.068788506412 |   0.217382350167601 |                0.217382350167601 |   0.3057306706905365 |        80.0 | 3.1006157398223877 |    296.388916015625 |              20.0 |  110.00000000000001 |                              1.0 |                               8000.0 |   82.0 |    296.388916015625 |    257.0770417827139 |             0.6506164944253245 |                     111.51989285545208 |                      257.0770417827139 |            0.5928132642714912 |                 1.800506825596331 |    10.731288338884175 |  0.6506164944253245 |     1.800506825596331 |
  (print-inputs-quantiles-tables
   [[:canopy-base-height-matrix "ft" zero?]
    [:canopy-height-matrix      "ft" zero?]]
   (->> replay-results
        (take 16)
        (map
         (fn [replay-res]
           [(-> replay-res :pyrcst_fire_name)
            (-> replay-res ::sim-outputs first val ::simulations/inputs)]))))

  ;;|      :pyrcst_fire_name |                :input-name |               :min |        :quartile-1 |            :median |        :quartile-3 |
  ;;|------------------------+----------------------------+--------------------+--------------------+--------------------+--------------------|
  ;;|              ca-summit | :canopy-base-height-matrix | 0.6561999917030334 | 2.2967000007629395 |  4.593400001525879 |  8.530599594116211 |
  ;;|              ca-summit |      :canopy-height-matrix |  9.843000411987305 |  52.49599838256836 |  75.46299743652344 |  91.86799621582031 |
  ;;|            or-sturgill | :canopy-base-height-matrix | 0.9843000173568726 | 1.6404999494552612 |  2.952899932861328 |  4.593400001525879 |
  ;;|            or-sturgill |      :canopy-height-matrix |  9.843000411987305 |  49.21500015258789 |   62.3390007019043 |  75.46299743652344 |
  ;;|          mt-government | :canopy-base-height-matrix | 0.9843000173568726 | 1.6404999494552612 |  3.609100103378296 |  4.593400001525879 |
  ;;|          mt-government |      :canopy-height-matrix |  9.843000411987305 |   62.3390007019043 |   62.3390007019043 |  75.46299743652344 |
  ;;|               id-lemhi | :canopy-base-height-matrix | 0.9843000173568726 | 1.9686000347137451 | 3.2809998989105225 |  4.921500205993652 |
  ;;|               id-lemhi |      :canopy-height-matrix |  9.843000411987305 |  49.21500015258789 |   62.3390007019043 |   62.3390007019043 |
  ;;|           wa-irving-pk | :canopy-base-height-matrix | 0.9843000173568726 | 1.6404999494552612 | 4.2652997970581055 |   6.23390007019043 |
  ;;|           wa-irving-pk |      :canopy-height-matrix |  9.843000411987305 |   62.3390007019043 |  75.46299743652344 |  88.58699798583984 |
  ;;| id-isabella-lower-twin | :canopy-base-height-matrix | 0.9843000173568726 | 2.2967000007629395 | 3.9372000694274902 |  4.921500205993652 |
  ;;| id-isabella-lower-twin |      :canopy-height-matrix |  9.843000411987305 |   62.3390007019043 |   62.3390007019043 |  75.46299743652344 |
  ;;|              ca-summit | :canopy-base-height-matrix | 0.6561999917030334 | 2.2967000007629395 |  4.593400001525879 |  8.530599594116211 |
  ;;|              ca-summit |      :canopy-height-matrix |  9.843000411987305 |  52.49599838256836 |  75.46299743652344 |  91.86799621582031 |
  ;;|          wa-goat-rocks | :canopy-base-height-matrix | 0.9843000173568726 |  3.609100103378296 |  5.249599933624268 |  6.890100002288818 |
  ;;|          wa-goat-rocks |      :canopy-height-matrix |  9.843000411987305 |   62.3390007019043 |  75.46299743652344 |  88.58699798583984 |
  ;;| id-isabella-lower-twin | :canopy-base-height-matrix | 0.9843000173568726 | 2.2967000007629395 | 3.9372000694274902 |  4.921500205993652 |
  ;;| id-isabella-lower-twin |      :canopy-height-matrix |  9.843000411987305 |   62.3390007019043 |   62.3390007019043 |  75.46299743652344 |
  ;;|              ca-summit | :canopy-base-height-matrix | 0.6561999917030334 | 2.2967000007629395 |  4.593400001525879 |  8.858699798583984 |
  ;;|              ca-summit |      :canopy-height-matrix |  9.843000411987305 |  52.49599838256836 |  75.46299743652344 |  91.86799621582031 |
  ;;|        or-double-creek | :canopy-base-height-matrix | 0.9843000173568726 | 1.6404999494552612 |  2.952899932861328 |  4.593400001525879 |
  ;;|        or-double-creek |      :canopy-height-matrix |  9.843000411987305 |  49.21500015258789 |   62.3390007019043 |  75.46299743652344 |
  ;;|            mt-billiard | :canopy-base-height-matrix | 0.9843000173568726 | 2.2967000007629395 | 3.9372000694274902 |  5.249599933624268 |
  ;;|            mt-billiard |      :canopy-height-matrix |  9.843000411987305 |   62.3390007019043 |  75.46299743652344 |  75.46299743652344 |
  ;;|                 ca-red | :canopy-base-height-matrix | 0.6561999917030334 | 1.9686000347137451 |  4.593400001525879 |  8.530599594116211 |
  ;;|                 ca-red |      :canopy-height-matrix |  9.843000411987305 | 55.777000427246094 |  75.46299743652344 |  91.86799621582031 |
  ;;|              mt-cannon | :canopy-base-height-matrix | 0.9843000173568726 | 1.6404999494552612 | 2.2967000007629395 | 3.9372000694274902 |
  ;;|              mt-cannon |      :canopy-height-matrix |  9.843000411987305 |  49.21500015258789 |   62.3390007019043 |   62.3390007019043 |
  ;;|          wa-goat-rocks | :canopy-base-height-matrix | 0.9843000173568726 |  3.609100103378296 |  5.249599933624268 |  6.890100002288818 |
  ;;|          wa-goat-rocks |      :canopy-height-matrix |  9.843000411987305 |   62.3390007019043 |  75.46299743652344 |  88.58699798583984 |
  ;;|             id-ross-fk | :canopy-base-height-matrix | 0.9843000173568726 | 1.9686000347137451 |  2.624799966812134 |  4.593400001525879 |
  ;;|             id-ross-fk |      :canopy-height-matrix |  9.843000411987305 | 36.090999603271484 |  49.21500015258789 |   62.3390007019043 |

  (->> replay-results
       (map
        (fn [replay-res]
          [(-> replay-res :pyrcst_fire_name)
           (-> replay-res ::sim-outputs first val ::simulations/inputs :spotting)])))

  (def replay-results nil)

  *e)

(defn explore-variations
  [snaps variation-name->info]
  (->> (replayable-successive-t0t1-pairs snaps)
       (sort-by snapshot-pair-hash)
       (partition-all 10) ;; grouping into batches, processed in parallel
       (mapcat
        (fn replay-batch [t0+t1-snaps]
          (let [replay-results        (replay-snapshot-pairs variation-name->info t0+t1-snaps)
                summary-table-entries (replayed-results-table-entries replay-results)]
            (pprint-table-from-kv-lists summary-table-entries)
            summary-table-entries)))
       (vec)))

(defn index-explored-variations
  [kv-lists]
  (let [variation-name->summary-maps (->> kv-lists
                                          (map (fn kv-list->map [k+vs] (into {} k+vs)))
                                          (group-by #(get % "Variation")))]
    variation-name->summary-maps))

(defn weight-favoring
  "A weighting function that will put more weight on duration dt as it gets close to preferred-dt."
  [preferred-dt dt]
  ;; Gamma PDF with k=3 and mode at preferred-dt, re-scaled so that f(preferred-dt)=1.
  ;; There's no deep rationale for using the Gamma PDF - it just roughly has the right shape.
  (let [mode       preferred-dt
        k          3
        k-1        (- k 1.0)
        theta      (/ mode k-1)
        u          (/ (double dt) theta)]
    (* (Math/pow (* u
                    Math/E
                    (/ 1.0 k-1))
                 k-1)
       (Math/exp (- u)))))

(defn aggregate-explored-variations
  [variation-name->summary-maps]
  (letfn [(unformat-count [s] (-> s (str/split #"\s") (first) (read-string)))
          (weight [{dt-hr :delta-t-hr :as _m}]
            (weight-favoring 24.0 dt-hr))
          (weighted-avg [weight-fn score-fn coll]
            (/ (->> coll (map (fn [x] (* (weight-fn x) (score-fn x)))) (apply + 0.0))
               (->> coll (map weight-fn) (apply + 0.0))))
          (failed-ignition? [m]
            (zero? (+ (:sim-inter-obs-n-cells m) (:sim-minus-obs-n-cells m))))]
    (->> variation-name->summary-maps
         (map (fn [[variation-name sumry-maps]]
                (let [ms
                      (->> sumry-maps
                           (map (fn back-to-kws [sumry-map]
                                  (merge (select-keys sumry-map ["Variation" "t1"])
                                         {:pyrcst_fire_name (get sumry-map "Fire name")}
                                         {:observed-burn-n-cells (get sumry-map "n cells really burned")
                                          :sim-inter-obs-n-cells (-> sumry-map (get "sim ∩ real") (unformat-count))
                                          :obs-minus-sim-n-cells (-> sumry-map (get "real - sim") (unformat-count))
                                          :sim-minus-obs-n-cells (-> sumry-map (get "sim - real") (unformat-count))
                                          :delta-t-hr            (when-some [[_ hrs] (re-matches #".*\(\+\s*(\d+)hr\)" (get sumry-map "t1"))]
                                                                   (-> hrs (Long/parseLong 10) (double)))})))
                           (vec))]
                  {"Variation"          variation-name
                   "n replays"          (count ms)
                   "n failed ignitions" (->> ms (filter failed-ignition?) count)
                   "real - sim %-w-avg" (weighted-avg
                                         weight
                                         (fn [m]
                                           (let [real-n-cells (:observed-burn-n-cells m)]
                                             (if (zero? real-n-cells)
                                               0.0
                                               (-> (:obs-minus-sim-n-cells m)
                                                   (/ real-n-cells)
                                                   (* 100.0)))))
                                         (remove failed-ignition? ms))
                   "sim - real %-w-avg" (weighted-avg
                                         weight
                                         (fn [m]
                                           (let [real-n-cells (:observed-burn-n-cells m)]
                                             (if (zero? real-n-cells)
                                               0.0
                                               (-> (:sim-minus-obs-n-cells m)
                                                   (/ real-n-cells)
                                                   (* 100.0)))))
                                         (remove failed-ignition? ms))
                   "mixed error"        (weighted-avg
                                         weight
                                         (fn [m]
                                           (let [real-n-cells (:observed-burn-n-cells m)]
                                             (if (zero? real-n-cells)
                                               0.0
                                               (+
                                                (* 1.0 (-> (:sim-minus-obs-n-cells m) (/ real-n-cells)))
                                                (* 1.0 (-> (:obs-minus-sim-n-cells m) (/ real-n-cells)))))))
                                         ms)})))
         (sort-by #(get % "Variation")))))

(comment

  (def fut_explore-variations
    (future
     (let [variation-name->info  (into [["baseline" {}]]
                                       (for [eaf [1.0 0.5 1.5 0.8 1.2]]
                                         [(format "01) crowning disabled, EAF=%s" (str eaf))
                                          {::transform-inputs-fn (fn [inputs] (-> inputs
                                                                                  (assoc :canopy-base-height-matrix (:canopy-height-matrix inputs))
                                                                                  (update :ellipse-adjustment-factor-samples
                                                                                          (fn [eaf-samples] (mapv (constantly eaf) eaf-samples)))))}]))]
       (explore-variations snaps))))

  (def variation-name->summary-maps (index-explored-variations @fut_explore-variations))

  (pprint/print-table (aggregate-explored-variations aggregate-explored-variations))
  ;;|                      Variation | n replays | n failed ignitions | real - sim %-w-avg | sim - real %-w-avg |        mixed error |
  ;;|--------------------------------+-----------+--------------------+--------------------+--------------------+--------------------|
  ;;| 01) crowning disabled, EAF=0.5 |       187 |                 58 |  38.43956966815156 |  90.82745015856521 | 1.1644699233103408 |
  ;;| 01) crowning disabled, EAF=0.8 |       187 |                 58 |  40.19882577624674 | 62.378055824764914 | 0.9784421793703703 |
  ;;| 01) crowning disabled, EAF=1.0 |       187 |                 58 |  41.06493197803133 |  50.96176395706256 | 0.9049083811943723 |
  ;;| 01) crowning disabled, EAF=1.2 |       187 |                 58 |  41.77630030878872 |  42.20147817521467 | 0.8488081863612352 |
  ;;| 01) crowning disabled, EAF=1.5 |       187 |                 58 | 42.697194089929376 |  33.75083916273461 | 0.7963265728909918 |
  ;;|                       baseline |       187 |                 58 |  35.72564690954984 | 310.89965530770434 | 2.6794366334053863 |

  ;; IMPROVEMENT: quantiles of baseline - treatment errors.

  ;; IMPROVEMENT new variations to try:
  ;; - Elmfire eccentricity formulas

  *e)

;; ------------------------------------------------------------------------------
;; Images

(defn matrix-bounding-box                                   ;; TODO when support is empty?
  [elem-pred m]
  (let [dtype :double
        row-indices (t/compute-tensor (d/shape m)
                                      (fn [i _j] i)
                                      dtype)
        col-indices (t/compute-tensor (d/shape m)
                                      (fn [_i j] j)
                                      dtype)
        support-row-indices (d/emap
                             (fn [v i]
                               (if (elem-pred v) i Double/NaN))
                             dtype
                             m
                             row-indices)
        support-col-indices (d/emap
                             (fn [v j]
                               (if (elem-pred v) j Double/NaN))
                             dtype
                             m
                             col-indices)]
    [[(long (dfn/reduce-min support-row-indices))
      (long (dfn/reduce-min support-col-indices))]
     [(long (dfn/reduce-max support-row-indices))
      (long (dfn/reduce-max support-col-indices))]]))

(defn clip-matrix-to-bounding-box
  [m [[i0 j0] [i1 j1]]]
  (t/select m (range i0 (inc i1)) (range j0 (inc j1))))


(defn save-matrix-as-png-at-file
  [color-ramp nodata-value matrix file]
  (let [ret (io/file file)
        pixels-per-cell (max 1
                             (let [min-desired-width-px 800
                                   n-cols (-> (d/shape matrix) (nth 1) (long))]
                               (Math/round (double (/ (double min-desired-width-px) n-cols)))))]
    (io/make-parents ret)
    (save-matrix-as-png color-ramp pixels-per-cell nodata-value matrix (-> ret (.toPath) (.toString)))
    ret))


(defn venn-diagram-color-numbers
  [obs? sim? ign?]
  (if ign?
    4.0
    (if obs?
      (if sim? 2.5 1.0)
      (if sim? 5.0 0.0))))

(def <venn-diagram-colors-explanation>
  [:p "The colored map shows the areas that were ignited (yellow), over-predicted (red), under-predicted (blue) and correctly predicted (green)"])


(defn variation-hash
  "Creates a pseudo-id for a replayed variation."
  ^long [replay-res variation-name]
  (hash [(select-keys replay-res [:pyrcst_fire_name
                                  :pyrcst_fire_subnumber])
         (:pyrcst_snapshot_inst (::t0-fire replay-res))
         (:pyrcst_snapshot_inst (::t1-fire replay-res))
         variation-name]))

(defn generate-images-for-replay!
  [img-dir replay-res]
  (let [observed-burn-area   (::observed-burn-area replay-res)
        observed-burn-matrix (:matrix observed-burn-area)
        stop-inst            (-> replay-res ::t1-fire :pyrcst_snapshot_inst)]
    (reduce
     (fn [replay-res [path v]] (assoc-in replay-res path v))
     replay-res
     (for [[variation-name sim-output] (::sim-outputs replay-res)
           :let [sim-result               (::simulations/result sim-output)
                 sim-inputs               (::simulations/inputs sim-output)
                 sim-burn-area            (if (nil? sim-result)
                                            (:ignition-matrix sim-inputs)
                                            (simulated-burned-area-matrix stop-inst sim-inputs sim-result))
                 burn-venn-diagram-matrix (d/emap
                                           (fn [obs sim ign]
                                             (venn-diagram-color-numbers (pos? obs) (pos? sim) (= 1.0 ign)))
                                           :double
                                           observed-burn-matrix
                                           sim-burn-area
                                           (:ignition-matrix sim-inputs))
                 bbox                     (matrix-bounding-box pos? burn-venn-diagram-matrix)
                 variation-hash (variation-hash replay-res variation-name)
                 subdir (io/file img-dir (str variation-hash))
                 img-name->file           {::fuel-models       (save-matrix-as-png-at-file :gray
                                                                                           -1.0
                                                                                           (-> (:fuel-model-matrix sim-inputs)
                                                                                               (clip-matrix-to-bounding-box bbox)
                                                                                               (d/elemwise-cast :double))
                                                                                           (io/file subdir "fuel-models.png"))
                                           ::burn-venn-diagram (save-matrix-as-png-at-file :color 0.0
                                                                                           (-> burn-venn-diagram-matrix
                                                                                               (clip-matrix-to-bounding-box bbox))
                                                                                           (io/file subdir "burn-venn-diagram.png"))
                                           ::toa-hr            (when-some [burn-time-matrix (:burn-time-matrix sim-result)]
                                                                 (let [nodata-value -1.0]
                                                                   (save-matrix-as-png-at-file :gray nodata-value
                                                                                               (-> burn-time-matrix
                                                                                                   (clip-matrix-to-bounding-box bbox)
                                                                                                   (->> (d/emap (fn compute-toa-hr [toa-min]
                                                                                                                  (cond-> toa-min (not= toa-min nodata-value) (/ 60.0)))
                                                                                                                nil)))
                                                                                               (io/file subdir "toa-hr.png"))))}]
           [img-k img-file] img-name->file]
       (let [path (conj [::sim-outputs variation-name ::generated-images] img-k)]
         [path img-file])))))

(defn <html-viz-page>
  [webviz-dir replay-results++]
  [:html
   [:body
    [:div
     [:p
      "This page is intended as a benchmark of GridFire misprediction: "
      "it shows the results of replaying PyreCast snapshots against more "
      "recent versions of themselves."]
     [:p "This analysis currently has " [:strong "some weaknesses:"]]
     [:ol
      [:li
       ;; IMPROVEMENT some under-predicted areas are far from the ignition-region:
       ;; in this case, under-predicting them is not GridFire's fault,
       ;; so we don't want those in the misprediction metrics.
       ;; Idea: buffering + graph components. Keep only connected components
       ;; of the really-burned area that intersect with the buffered ignition region.
       ;; Library: loom.
       "Some of the ignition regions computed upstream of GridFire are wrong, "
       "causing an underprediction which is no fault of GridFire. "
       "TODO: eliminate the influence of those underpredicted regions on misprediction metrics."]
      [:li "In particular, sometimes the ignition region is empty."]
      [:li "The (t0, t1) snapshot pairs are not always optimal. Too close increases the noise caused by poorly-estimated perimeters."]]]
    [:div <venn-diagram-colors-explanation>]
    (->> replay-results++
         (map
          (fn [replay-res]
            [:div
             (let [t0-inst (-> replay-res ::t0-fire :pyrcst_snapshot_inst)
                   t1-inst (-> replay-res ::t1-fire :pyrcst_snapshot_inst)]
               [:h2
                (:pyrcst_fire_name replay-res)
                " from "
                (-> t0-inst (pr-str))
                " to "
                (format-hours-between t0-inst t1-inst)])
             (->> replay-res ::sim-outputs
                  (map
                   (fn [[variation-name sim-output]]
                     (letfn [(<img> [img-k]
                               (when-some [img-file (-> sim-output ::generated-images (get img-k))]
                                 [:img {:src   (subs (-> img-file (str))
                                                     (-> webviz-dir (str) (count) (inc)))
                                        :style "width: 400px;"}]))]
                       [:div {:style "padding-left: 100px;"}
                        [:h3 {:id (str (variation-hash replay-res variation-name))}
                         variation-name
                         [:a {:href  (str "#" (variation-hash replay-res variation-name))
                              :style "font-size: 8px; margin-left: 5px;"}
                          "link"]]
                        [:div
                         (<img> ::fuel-models)
                         (<img> ::burn-venn-diagram)
                         (<img> ::toa-hr)]])))
                  (doall))]))
         (doall))]])

(comment

  (def webviz-dir (io/file "../gridfire-replay-web"))
  (def img-dir (io/file webviz-dir "img"))

  *e)

(comment

  (def replay-res (nth replay-results2 2))
  (def sim-output (-> replay-res ::sim-outputs (get "00) original")))

  (-> sim-output keys sort)

  (-> sim-output ::simulations/inputs keys sort)

  (def webviz-dir (io/file "../gridfire-replay-web"))


  (-> replay-results2
      (->> (pmap (fn [replay-res] (generate-images-for-replay! img-dir replay-res))))
      (vec)
      (as-> replay-results++
            (spit (io/file img-dir webviz-dir "replay-results2-viz.html")
                  (html/html5
                   (<html-viz-page> replay-results++)))))


  ;; TODO :burn-time-matrix not starting at zero? (min= 21)
  (=
   #inst"2022-09-14T21:21:00.000-00:00"
   (-> replay-res ::t0-fire :pyrcst_snapshot_inst)
   (-> sim-output ::simulations/inputs :ignition-start-timestamps first))
  ;;=> true
  ;; 21 minutes is also the min of :burn-time-matrix...


  *e)

(comment


  ;; IMPROVEMENT sometimes it looks like the t1 active-fire fills holes inside the t0 active-fire region
  ;; (see e.g "wa-mcallister-creek from #inst "2022-09-27T10:49:00.000-00:00" to +22hr").
  ;; This should probably not be counted as under-prediction.

  (def fut_created-html
    (future
     (spit (io/file webviz-dir "replay-results-viz.html")
           (html/html5
            (<html-viz-page>
             webviz-dir
             (->> (replayable-successive-t0t1-pairs snaps)
                  (sort-by snapshot-pair-hash)
                  (partition-all 16)                        ;; grouping into batches, processed in parallel
                  (mapcat
                   (fn process-batch [t0+t1-snaps]
                     (->> (replay-snapshot-pairs variation-name->info t0+t1-snaps)
                          (pmap (fn [replay-res]
                                  (try
                                    (generate-images-for-replay! img-dir replay-res)
                                    (catch Exception err
                                      (pprint/pprint
                                       (ex-info
                                        (str "error for replay: " (pr-str (:pyrcst_fire_name replay-res)))
                                        {}
                                        err))
                                      nil))))
                          (remove nil?)
                          (doall))))))))))


  *e)


