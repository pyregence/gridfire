#!/usr/bin/env bb

;; FIXME: document babashka (bb) and gdalsrsinfo as installation dependencies for running this script
;; FIXME: use babashka's pod protocol to integrate this script with gridfire.server

(require '[clojure.edn        :as edn]
         '[clojure.java.io    :as io]
         '[clojure.java.shell :refer [sh]]
         '[clojure.pprint     :refer [pprint]]
         '[clojure.string     :as str]
         '[clojure.tools.cli  :refer [parse-opts]])

;;=============================================================================
;; Units conversion functions
;;=============================================================================

(defn m->ft
  "Convert meters to feet."
  ^double
  [^double m]
  (* m 3.281))

(defn sec->min
  "Convert seconds to minutes."
  ^double
  [^double seconds]
  (* seconds 0.016666666666666666))

(defn kW-m->Btu-ft-s
  "Convert kilowatt per meter to BTU per feet per second."
  ^double
  [^double kW-m]
  (* kW-m 0.28887942532730604))

;;=============================================================================
;; File access functions
;;=============================================================================

(defn relative-path?
  [path]
  (re-matches #"^((\.){1,2}\/)+.*" path))

(defn file-path
  ([output-dir file-or-directory]
   (-> (if (relative-path? file-or-directory)
         (io/file output-dir file-or-directory)
         (io/file file-or-directory))
       (.toPath)
       (.normalize)
       (.toString)))
  ([output-dir directory tif-file-prefix]
   (let [file-name (if (relative-path? directory)
                     (io/file output-dir directory (str tif-file-prefix ".tif"))
                     (io/file directory (str tif-file-prefix ".tif")))]
     (-> file-name
         (.toPath)
         (.normalize)
         (.toString)))))

;;=============================================================================
;; Write gridfire.edn
;;=============================================================================

(defn write-config [{:keys [output-dir]} config-params]
  (let [output-file-path (file-path output-dir "./gridfire.edn")]
    (println "Creating config file:" output-file-path)
    (with-open [writer (io/writer output-file-path)]
      (pprint config-params writer))))

;;=============================================================================
;; Merge Override Config
;;=============================================================================

(defn merge-override-config [override-config-file-path config-params]
  (if override-config-file-path
    (->> (slurp override-config-file-path)
         (edn/read-string)
         (reduce-kv (fn [acc k v]
                      (if (nil? v)
                        (dissoc! acc k)
                        (assoc! acc k v)))
                    (transient config-params))
         (persistent!))
    config-params))

;;=============================================================================
;; LANDFIRE
;;=============================================================================

(defn process-landfire-layers
  [{:keys [output-dir]}
   {:strs [ASP_FILENAME CBH_FILENAME CC_FILENAME CH_FILENAME CBD_FILENAME
           FBFM_FILENAME SLP_FILENAME DEM_FILENAME FUELS_AND_TOPOGRAPHY_DIRECTORY]}
   config]
  (assoc config
         :landfire-layers
         {:aspect             {:type   :geotiff
                               :source (file-path output-dir FUELS_AND_TOPOGRAPHY_DIRECTORY ASP_FILENAME)}
          :canopy-base-height {:type       :geotiff
                               :source     (file-path output-dir FUELS_AND_TOPOGRAPHY_DIRECTORY CBH_FILENAME)
                               :units      :metric
                               :multiplier 0.1}
          :canopy-cover       {:type   :geotiff
                               :source (file-path output-dir FUELS_AND_TOPOGRAPHY_DIRECTORY CC_FILENAME)}
          :canopy-height      {:type       :geotiff
                               :source     (file-path output-dir FUELS_AND_TOPOGRAPHY_DIRECTORY CH_FILENAME)
                               :units      :metric
                               :multiplier 0.1}
          :crown-bulk-density {:type       :geotiff
                               :source     (file-path output-dir FUELS_AND_TOPOGRAPHY_DIRECTORY CBD_FILENAME)
                               :units      :metric
                               :multiplier 0.01}
          :elevation          {:type   :geotiff
                               :source (file-path output-dir FUELS_AND_TOPOGRAPHY_DIRECTORY DEM_FILENAME)
                               :units  :metric}
          :fuel-model         {:type   :geotiff
                               :source (file-path output-dir FUELS_AND_TOPOGRAPHY_DIRECTORY FBFM_FILENAME)}
          :slope              {:type   :geotiff
                               :source (file-path output-dir FUELS_AND_TOPOGRAPHY_DIRECTORY SLP_FILENAME)}}))

;;=============================================================================
;; Ignition
;;=============================================================================

(defn process-ignition
  [{:keys [output-dir]}
   {:strs [PHI_FILENAME FUELS_AND_TOPOGRAPHY_DIRECTORY RANDOM_IGNITIONS
           USE_IGNITION_MASK EDGEBUFFER IGNITION_MASK_FILENAME]}
   config]
  (if RANDOM_IGNITIONS
    (assoc config
           :random-ignition
           (cond-> {}
             USE_IGNITION_MASK
             (assoc :ignition-mask {:type   :geotiff
                                    :source (file-path output-dir FUELS_AND_TOPOGRAPHY_DIRECTORY IGNITION_MASK_FILENAME)})

             EDGEBUFFER
             (assoc :edge-buffer (m->ft EDGEBUFFER))))
    (assoc config
           :ignition-layer
           {:type        :geotiff
            :source      (file-path output-dir FUELS_AND_TOPOGRAPHY_DIRECTORY PHI_FILENAME)
            :burn-values {:burned   -1.0
                          :unburned 1.0}})))

;;=============================================================================
;; Weather
;;=============================================================================

;; FIXME: Since tmpf.tif and rh.tif aren't provided in elmfire.data, where are these files on disk?
(defn process-weather
  [{:keys [output-dir]}
   {:strs [WS_FILENAME WD_FILENAME WEATHER_DIRECTORY]} config]
  (assoc config
         :temperature         {:type   :geotiff
                               :source (file-path output-dir WEATHER_DIRECTORY "tmpf")}
         :relative-humidity   {:type   :geotiff
                               :source (file-path output-dir WEATHER_DIRECTORY "rh")}
         :wind-speed-20ft     {:type   :geotiff
                               :source (file-path output-dir WEATHER_DIRECTORY WS_FILENAME)}
         :wind-from-direction {:type   :geotiff
                               :source (file-path output-dir WEATHER_DIRECTORY WD_FILENAME)}))

;;=============================================================================
;; Output
;;=============================================================================

(defn process-output
  [{:keys [output-dir]}
   {:strs [OUTPUTS_DIRECTORY DUMP_BURN_PROBABILITY_AT_DTDUMP DTDUMP]} config]
  (cond-> (assoc config
                 :output-directory        (file-path output-dir OUTPUTS_DIRECTORY)
                 :outfile-suffix          ""
                 :output-landfire-inputs? false
                 :output-geotiffs?        false
                 :output-pngs?            false
                 :output-binary?          true
                 :output-csvs?            true)
    DUMP_BURN_PROBABILITY_AT_DTDUMP (assoc :burn-probability (sec->min DTDUMP))))

;;=============================================================================
;; Perturbations
;;=============================================================================

(def unused-perturbations #{:crown-bulk-density :canopy-base-height})

(def layers-in-metric #{:crown-bulk-density :canopy-base-height :canopy-height})

(def layers-in-ratio #{:fuel-moisture-dead-1hr
                       :fuel-moisture-dead-10hr
                       :fuel-moisture-dead-100hr
                       :fuel-moisture-live-herbaceous
                       :fuel-moisture-live-woody})

(def elmfire->gridfire
  "A mapping of ELMFIRE string names to GridFire keywords"
  {"CBH"    :canopy-base-height
   "CC"     :canopy-cover
   "CH"     :canopy-height
   "CBD"    :crown-bulk-density
   "WS"     :wind-speed-20ft
   "WD"     :wind-from-direction
   "M1"     :fuel-moisture-dead-1hr
   "M10"    :fuel-moisture-dead-10hr
   "M100"   :fuel-moisture-dead-100hr
   "MLH"    :fuel-moisture-live-herbaceous
   "MLW"    :fuel-moisture-live-woody
   "GLOBAL" :global
   "PIXEL"  :pixel})

(defn perturbation-info
  [config index key]
  (cond-> {:spatial-type (->> (str "SPATIAL_PERTURBATION-" index)
                              (get config)
                              (get elmfire->gridfire))
           :range        [(get config (str "PDF_LOWER_LIMIT-" index))
                          (get config (str "PDF_UPPER_LIMIT-" index))]}
    (layers-in-metric key) (assoc :units :metric)
    (layers-in-ratio key)  (assoc :units :ratio)))

(defn perturbation-key
  [config index]
  (->> (str "RASTER_TO_PERTURB-" index)
       (get config)
       (get elmfire->gridfire)))

(defn extract-perturbations
  [{:strs [^long NUM_RASTERS_TO_PERTURB] :as config}]
  (when (and NUM_RASTERS_TO_PERTURB (pos? NUM_RASTERS_TO_PERTURB))
    (into {}
          (keep (fn [index]
                  (when-let [key (perturbation-key config index)]
                    (when-not (unused-perturbations key)
                      [key (perturbation-info config index key)]))))
          (range 1 (inc NUM_RASTERS_TO_PERTURB)))))

(defn process-perturbations
  [data config]
  (if-let [perturbations (extract-perturbations data)]
    (assoc config :perturbations perturbations)
    config))

;;=============================================================================
;; Spotting
;;=============================================================================

(defn extract-fuel-range [s]
  (->> s
       (re-find #"(\d+):(\d+)")
       (rest)
       (mapv #(Integer/parseInt %))))

;; FIXME: Is this logic (and return format) right?
(defn extract-surface-spotting-percents
  [data]
  (if-let [SURFACE_FIRE_SPOTTING_PERCENT (get data "SURFACE_FIRE_SPOTTING_PERCENT(:)")]
    [[[1 204] SURFACE_FIRE_SPOTTING_PERCENT]]
    (transduce (filter #(str/includes? % "SURFACE_FIRE_SPOTTING_PERCENT"))
               (completing (fn [acc k] (conj acc (extract-fuel-range k) (get data k))))
               []
               (keys data))))

;; FIXME: Is this logic (and return format) right?
(defn extract-global-surface-spotting-percents
  [{:strs
    [^double GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN
     ^double GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX
     ^double GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT
     ENABLE_SPOTTING] :as data}]
  (if (or GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN)
    (if ENABLE_SPOTTING
      [[[1 204] [(* 0.01 GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN) (* 0.01 GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX)]]]
      [[[1 204] (* 0.01 GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT)]])
    (extract-surface-spotting-percents data)))

;; FIXME: Is this logic (and return format) right?
(defn extract-num-firebrands
  [{:strs [NEMBERS NEMBERS_MIN NEMBERS_MIN_LO NEMBERS_MIN_HI NEMBERS_MAX
           NEMBERS_MAX_LO NEMBERS_MAX_HI ENABLE_SPOTTING]}]
  (if ENABLE_SPOTTING
    {:lo (cond
           (and NEMBERS_MIN_LO (= NEMBERS_MIN_LO NEMBERS_MIN_HI)) NEMBERS_MIN_LO
           NEMBERS_MIN_LO                                         [NEMBERS_MIN_LO NEMBERS_MIN_HI]
           :else                                                  NEMBERS_MIN)
     :hi (cond (and NEMBERS_MAX_LO (= NEMBERS_MAX_LO NEMBERS_MAX_HI)) NEMBERS_MAX_LO
               NEMBERS_MAX_LO                                         [NEMBERS_MAX_LO NEMBERS_MAX_HI]
               :else                                                  NEMBERS_MAX)}
    NEMBERS))

;; FIXME: Is this logic (and return format) right?
(defn extract-crown-fire-spotting-percent
  [{:strs [^double CROWN_FIRE_SPOTTING_PERCENT_MIN
           ^double CROWN_FIRE_SPOTTING_PERCENT_MAX
           ^double CROWN_FIRE_SPOTTING_PERCENT
           ENABLE_SPOTTING]}]
  (if ENABLE_SPOTTING
    [(* 0.01 CROWN_FIRE_SPOTTING_PERCENT_MIN) (* 0.01 CROWN_FIRE_SPOTTING_PERCENT_MAX)]
    (* 0.01 CROWN_FIRE_SPOTTING_PERCENT)))

(defn extract-normalized-distance-variance
  [{:strs [NORMALIZED_SPOTTING_DIST_VARIANCE_MIN
           NORMALIZED_SPOTTING_DIST_VARIANCE_MAX
           NORMALIZED_SPOTTING_DIST_VARIANCE]}]
  (or NORMALIZED_SPOTTING_DIST_VARIANCE
      {:lo NORMALIZED_SPOTTING_DIST_VARIANCE_MIN
       :hi NORMALIZED_SPOTTING_DIST_VARIANCE_MAX}))

(defn extract-flin-exp
  [{:strs [SPOT_FLIN_EXP_LO SPOT_FLIN_EXP_HI SPOT_FLIN_EXP]}]
  (or SPOT_FLIN_EXP
      {:lo SPOT_FLIN_EXP_LO
       :hi SPOT_FLIN_EXP_HI}))

(defn extract-ws-exp
  [{:strs [SPOT_WS_EXP_LO SPOT_WS_EXP_HI SPOT_WS_EXP]}]
  (or SPOT_WS_EXP
      {:lo SPOT_WS_EXP_LO
       :hi SPOT_WS_EXP_HI}))

(defn extract-mean-distance
  [{:strs [MEAN_SPOTTING_DIST_MIN MEAN_SPOTTING_DIST_MAX MEAN_SPOTTING_DIST]}]
  (or MEAN_SPOTTING_DIST
      {:lo MEAN_SPOTTING_DIST_MIN
       :hi MEAN_SPOTTING_DIST_MAX}))

(defn process-spotting
  [{:strs [ENABLE_SPOTTING ENABLE_SURFACE_FIRE_SPOTTING CRITICAL_SPOTTING_FIRELINE_INTENSITY] :as data} config]
  (cond-> config
    ENABLE_SPOTTING
    (assoc :spotting
           {:mean-distance                (extract-mean-distance data)
            :ws-exp                       (extract-ws-exp data)
            :flin-exp                     (extract-flin-exp data)
            :normalized-distance-variance (extract-normalized-distance-variance data)
            :crown-fire-spotting-percent  (extract-crown-fire-spotting-percent data)
            :num-firebrands               (extract-num-firebrands data)
            :decay-constant               0.005})

    (and ENABLE_SPOTTING ENABLE_SURFACE_FIRE_SPOTTING)
    (assoc-in [:spotting :surface-fire-spotting]
              {:spotting-percent             (extract-global-surface-spotting-percents data)
               :critical-fire-line-intensity (kW-m->Btu-ft-s CRITICAL_SPOTTING_FIRELINE_INTENSITY)})))

;;=============================================================================
;; Fuel moisture layers
;;=============================================================================

;; FIXME: Since mlw.tif and mlh.tif aren't provided in elmfire.data, where are these files on disk?
(defn process-fuel-moisture
  [{:keys [output-dir]}
   {:strs [WEATHER_DIRECTORY M1_FILENAME M10_FILENAME M100_FILENAME
           USE_CONSTANT_LW USE_CONSTANT_LH LW_MOISTURE_CONTENT LH_MOISTURE_CONTENT]}
   config]
  (assoc config
         :fuel-moisture
         {:dead {:1hr   {:type   :geotiff
                         :source (file-path output-dir WEATHER_DIRECTORY M1_FILENAME)}
                 :10hr  {:type   :geotiff
                         :source (file-path output-dir WEATHER_DIRECTORY M10_FILENAME)}
                 :100hr {:type   :geotiff
                         :source (file-path output-dir WEATHER_DIRECTORY M100_FILENAME)}}
          :live {:woody      (if USE_CONSTANT_LW
                               (* 0.01 ^double LW_MOISTURE_CONTENT)
                               {:type   :geotiff
                                :source (file-path output-dir WEATHER_DIRECTORY "mlw")})
                 :herbaceous (if USE_CONSTANT_LH
                               (* 0.01 ^double LH_MOISTURE_CONTENT)
                               {:type   :geotiff
                                :source (file-path output-dir WEATHER_DIRECTORY "mlh")})}}))

;;=============================================================================
;; Pyrome specific calibration
;;=============================================================================

(defn process-pyrome-specific-calibration
  [{:strs [ADJUSTMENT_FACTORS_BY_PYROME CALIBRATION_CONSTANTS_BY_PYROME ADJUSTMENT_FACTORS_FILENAME
           CALIBRATION_CONSTANTS_FILENAME]}
   config]
  (cond-> config
    (true? ADJUSTMENT_FACTORS_BY_PYROME)    (assoc :pyrome-spread-rate-adjustment-csv
                                                   ADJUSTMENT_FACTORS_FILENAME)
    (true? CALIBRATION_CONSTANTS_BY_PYROME) (assoc :pyrome-calibration-csv
                                                   CALIBRATION_CONSTANTS_FILENAME)))

;;=============================================================================
;; Build gridfire.edn
;;=============================================================================

(defn build-edn
  [options
   {:strs [COMPUTATIONAL_DOMAIN_CELLSIZE A_SRS SIMULATION_TSTOP SEED FOLIAR_MOISTURE_CONTENT] :as data}]
  (->> {:cell-size                       (m->ft COMPUTATIONAL_DOMAIN_CELLSIZE)
        :srid                            (or A_SRS "EPSG:32610")
        :max-runtime                     (sec->min SIMULATION_TSTOP)
        :simulations                     10 ; FIXME: use NUM_ENSEMBLE_MEMBERS or override.edn
        :random-seed                     SEED
        :foliar-moisture                 FOLIAR_MOISTURE_CONTENT
        :ellipse-adjustment-factor       1.0
        :parallel-strategy               :between-fires
        :fractional-distance-combination :sum} ; FIXME: unused parameter
       (process-landfire-layers options data)
       (process-ignition options data)
       (process-weather options data)
       (process-output options data)
       (process-perturbations data)
       (process-spotting data)
       (process-fuel-moisture options data)
       (process-pyrome-specific-calibration data)))

;;=============================================================================
;; Parse elmfire.data
;;=============================================================================

(defn get-srid
  [proj-string]
  (let [{:keys [err out]} (sh "gdalsrsinfo" "-e" proj-string "-o" "epsg" "--single-line")]
    (when (empty? err)
      (->> out
           (str/split-lines)
           (filter #(str/includes? % "EPSG"))
           (first)))))

(def regex-for-array-item #"^[A-Z0-9\_]+\(\d+\)")

(defn convert-key [s]
  (when (string? s)
    (let [s-trimmed (str/trim s)]
     (if (re-matches regex-for-array-item s-trimmed)
       (str/join "-" (str/split s-trimmed #"[\(\)]"))
       s-trimmed))))

(defn convert-val [s]
  (when (string? s)
    (let [s-trimmed  (str/trim s)
          char-count (count s-trimmed)]
      (cond
        (re-matches #"^-?[0-9]\d*\.(\d+)?$" s-trimmed) (Double/parseDouble s-trimmed)
        (re-matches #"^-?\d+$" s-trimmed)              (Integer/parseInt s-trimmed)
        (re-matches #".TRUE." s-trimmed)               true
        (re-matches #".FALSE." s-trimmed)              false
        (re-matches #"'[0-9a-zA-Z_.//]*'" s-trimmed)   (subs s-trimmed 1 (dec char-count))
        (str/includes? s-trimmed "proj")               (get-srid (subs s-trimmed 1 (dec char-count)))
        :else                                          nil))))

(defn parse-elmfire [s]
  (transduce (comp
              (filter #(str/includes? % " = "))
              (map #(str/split % #" = ")))
             (completing
              (fn [acc [k v]]
                (assoc acc
                       (convert-key k)
                       (convert-val v))))
             (sorted-map)
             (str/split s #"\n")))

;;=============================================================================
;; Main
;;=============================================================================

(defn convert-config! [{:keys [elmfire-data override-config] :as options}]
  (println "Converting configuration file to one that GridFire accepts.")
  (->> elmfire-data
       (slurp)
       (parse-elmfire)
       (build-edn options)
       (merge-override-config override-config)
       (write-config options)))

(def cli-options
  [["-e" "--elmfire-data FILE" "Path to an elmfire.data file"
    :validate [#(.exists  (io/file %)) "The provided --elmfire-data does not exist."
               #(.canRead (io/file %)) "The provided --elmfire-data is not readable."]]

   ["-o" "--override-config OVERRIDE" "Path to override.edn file"
    :validate [#(.exists  (io/file %)) "The provided --override-config does not exist."
               #(.canRead (io/file %)) "The provided --override-config is not readable."]]])

(def program-banner
  (str "elm_to_grid.clj: Generate a gridfire.edn file from an elmfire.data file.\n"
       "Copyright © 2020-2022 Spatial Informatics Group, LLC.\n"))

(defn main [args]
  (println program-banner)
  (let [{:keys [options summary errors]} (parse-opts args cli-options)]
    ;; {:options   The options map, keyed by :id, mapped to the parsed value
    ;;  :summary   A string containing a minimal options summary
    ;;  :errors    A vector of error message strings thrown during parsing; nil when no errors exist
    (cond
      ;; Errors encountered during input parsing
      (seq errors)
      (do
        (run! println errors)
        (println (str "\nUsage:\n" summary)))

      ;; Valid --elmfire-data argument provided, so perform conversion
      (:elmfire-data options)
      (convert-config! (assoc options :output-dir (.getParent (io/file (:elmfire-data options)))))

      ;; Incorrect CLI invocation
      :else
      (do
        (println "You must provide a valid --elmfire-data file path to initiate conversion.")
        (println (str "\nUsage:\n" summary))))

    ;; Exit cleanly
    (System/exit 0)))

(main *command-line-args*)
