(ns gridfire.config
  (:require [clojure.java.io     :as io]
            [clojure.pprint      :as pprint]
            [clojure.string      :as str]
            [gridfire.conversion :as convert]
            [triangulum.logging  :refer [log-str]]))

;;=============================================================================
;; Utilities
;;=============================================================================

(def ^:dynamic *elmfire-file-path* "")

(defn file-path
  ([file-or-directory]
   (-> (io/file *elmfire-file-path* file-or-directory)
       (.toPath)
       (.normalize)
       (.toString)))
  ([directory tif-file-prefix]
   (-> (io/file *elmfire-file-path* directory (str tif-file-prefix ".tif"))
       (.toPath)
       (.normalize)
       (.toString))))

;;=============================================================================
;; Write gridfire.edn
;;=============================================================================

(defn write-config [config-params]
  (let [output-file-path (file-path "gridfire.edn")]
    (log-str "Creating config file: " output-file-path)
    (with-open [writer (io/writer output-file-path)]
      (pprint/pprint config-params writer))))

;;=============================================================================
;; LANDFIRE
;;=============================================================================

(defn process-landfire-layers
  [{:strs [ASP_FILENAME CBH_FILENAME CC_FILENAME CH_FILENAME CBD_FILENAME
           FBFM_FILENAME SLP_FILENAME DEM_FILENAME FUELS_AND_TOPOGRAPHY_DIRECTORY]}
   _
   config]
  (assoc config
         :landfire-layers
         {:aspect             {:type       :geotiff
                               :source     (file-path FUELS_AND_TOPOGRAPHY_DIRECTORY ASP_FILENAME)}
          :canopy-base-height {:type       :geotiff
                               :source     (file-path FUELS_AND_TOPOGRAPHY_DIRECTORY CBH_FILENAME)
                               :units      :metric
                               :multiplier 0.1}
          :canopy-cover       {:type       :geotiff
                               :source     (file-path FUELS_AND_TOPOGRAPHY_DIRECTORY CC_FILENAME)}
          :canopy-height      {:type       :geotiff
                               :source     (file-path FUELS_AND_TOPOGRAPHY_DIRECTORY CH_FILENAME)
                               :units      :metric
                               :multiplier 0.1}
          :crown-bulk-density {:type       :geotiff
                               :source     (file-path FUELS_AND_TOPOGRAPHY_DIRECTORY CBD_FILENAME)
                               :units      :metric
                               :multiplier 0.01}
          :elevation          {:type       :geotiff
                               :source     (file-path FUELS_AND_TOPOGRAPHY_DIRECTORY DEM_FILENAME)
                               :units      :metric}
          :fuel-model         {:type       :geotiff
                               :source     (file-path FUELS_AND_TOPOGRAPHY_DIRECTORY FBFM_FILENAME)}
          :slope              {:type       :geotiff
                               :source     (file-path FUELS_AND_TOPOGRAPHY_DIRECTORY SLP_FILENAME)}}))

;;=============================================================================
;; Ignition
;;=============================================================================

(defn process-ignition
  [{:strs [PHI_FILENAME FUELS_AND_TOPOGRAPHY_DIRECTORY RANDOM_IGNITIONS
           USE_IGNITION_MASK EDGEBUFFER IGNITION_MASK_FILENAME]}
   _
   config]
  (if RANDOM_IGNITIONS
    (let [ignition-mask-file-path (file-path FUELS_AND_TOPOGRAPHY_DIRECTORY IGNITION_MASK_FILENAME)]
      (assoc config
             :random-ignition
             {:ignition-mask (when (and USE_IGNITION_MASK (.exists (io/file ignition-mask-file-path)))
                               {:type   :geotiff
                                :source ignition-mask-file-path})
              :edge-buffer   (when EDGEBUFFER
                               (convert/m->ft EDGEBUFFER))}))
    (assoc config
           :ignition-layer
           {:type        :geotiff
            :source      (file-path FUELS_AND_TOPOGRAPHY_DIRECTORY PHI_FILENAME)
            :burn-values {:burned   -1.0
                          :unburned 1.0}})))

;;=============================================================================
;; Weather
;;=============================================================================

;; FIXME: Since tmpf.tif and rh.tif aren't provided in elmfire.data, where are these files on disk?
(defn process-weather
  [{:strs [WS_FILENAME WD_FILENAME WEATHER_DIRECTORY]}
   _
   config]
  (assoc config
         :temperature         {:type   :geotiff
                               :source (file-path WEATHER_DIRECTORY "tmpf")}
         :relative-humidity   {:type   :geotiff
                               :source (file-path WEATHER_DIRECTORY "rh")}
         :wind-speed-20ft     {:type   :geotiff
                               :source (file-path WEATHER_DIRECTORY WS_FILENAME)}
         :wind-from-direction {:type   :geotiff
                               :source (file-path WEATHER_DIRECTORY WD_FILENAME)}))

;;=============================================================================
;; Output
;;=============================================================================

(defn process-output
  [{:strs [OUTPUTS_DIRECTORY DUMP_BURN_PROBABILITY_AT_DTDUMP DTDUMP]}
   _
   config]
  (cond-> (assoc config
                 :output-directory        (file-path OUTPUTS_DIRECTORY)
                 :outfile-suffix          ""
                 :output-landfire-inputs? false
                 :output-geotiffs?        false
                 :output-pngs?            false
                 :output-binary?          true
                 :output-csvs?            true)
    DUMP_BURN_PROBABILITY_AT_DTDUMP (assoc :burn-probability (convert/sec->min DTDUMP))))

;;=============================================================================
;; Perturbations
;;=============================================================================

(def unused-perturbations #{:crown-bulk-density :canopy-base-height})

(def layers-in-metric #{:crown-bulk-density :canopy-base-height :canopy-height})

(def elmfire->gridfire
  "A mapping of ELMFIRE string names to GridFire keywords"
  {"CBH"    :canopy-base-height
   "CC"     :canopy-cover
   "CH"     :canopy-height
   "CBD"    :crown-bulk-density
   "WS"     :wind-speed-20ft
   "WD"     :wind-direction
   "GLOBAL" :global
   "PIXEL"  :pixel})

(defn perturbation-info
  [config index key]
  (cond-> {:spatial-type (->> (str "SPATIAL_PERTURBATION-" index)
                              (get config)
                              (get elmfire->gridfire))
           :range        [(get config (str "PDF_LOWER_LIMIT-" index))
                          (get config (str "PDF_UPPER_LIMIT-" index))]}
    (layers-in-metric key) (assoc :units :metric)))

(defn perturbation-key
  [config index]
  (->> (str "RASTER_TO_PERTURB-" index)
       (get config)
       (get elmfire->gridfire)))

(defn extract-perturbations
  [{:strs [NUM_RASTERS_TO_PERTURB] :as config}]
  (when (and NUM_RASTERS_TO_PERTURB (pos? NUM_RASTERS_TO_PERTURB))
    (into {}
          (keep (fn [index]
                  (when-let [key (perturbation-key config index)]
                    (when-not (unused-perturbations key)
                      [key (perturbation-info config index key)]))))
          (range 1 (inc NUM_RASTERS_TO_PERTURB)))))

(defn process-perturbations
  [data _ config]
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
    [GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN
     GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX
     GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT
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
    {:lo (if NEMBERS_MIN_LO [NEMBERS_MIN_LO NEMBERS_MIN_HI] NEMBERS_MIN)
     :hi (if NEMBERS_MAX_LO [NEMBERS_MAX_LO NEMBERS_MAX_HI] NEMBERS_MAX)}
    NEMBERS))

;; FIXME: Is this logic (and return format) right?
(defn extract-crown-fire-spotting-percent
  [{:strs [CROWN_FIRE_SPOTTING_PERCENT_MIN CROWN_FIRE_SPOTTING_PERCENT_MAX
           CROWN_FIRE_SPOTTING_PERCENT ENABLE_SPOTTING]}]
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
  [{:strs [ENABLE_SPOTTING ENABLE_SURFACE_FIRE_SPOTTING CRITICAL_SPOTTING_FIRELINE_INTENSITY] :as data} _ config]
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
               :critical-fire-line-intensity (convert/kW-m->Btu-ft-s CRITICAL_SPOTTING_FIRELINE_INTENSITY)})))

;;=============================================================================
;; Fuel moisture layers
;;=============================================================================

;; FIXME: Since mlw.tif and mlh.tif aren't provided in elmfire.data, where are these files on disk?
(defn process-fuel-moisture-layers
  [{:strs [M1_FILENAME M10_FILENAME M100_FILENAME WEATHER_DIRECTORY]} _ config]
  (assoc config
         :fuel-moisture-layers
         {:dead {:1hr   {:type   :geotiff
                         :source (file-path WEATHER_DIRECTORY M1_FILENAME)}
                 :10hr  {:type   :geotiff
                         :source (file-path WEATHER_DIRECTORY M10_FILENAME)}
                 :100hr {:type   :geotiff
                         :source (file-path WEATHER_DIRECTORY M100_FILENAME)}}
          :live {:woody      {:type   :geotiff
                              :source (file-path WEATHER_DIRECTORY "mlw")}
                 :herbaceous {:type   :geotiff
                              :source (file-path WEATHER_DIRECTORY "mlh")}}}))

;;=============================================================================
;; Build gridfire.edn
;;=============================================================================

(defn build-edn
  [{:strs [COMPUTATIONAL_DOMAIN_CELLSIZE A_SRS SIMULATION_TSTOP SEED FOLIAR_MOISTURE_CONTENT] :as data} options]
  (->> {:cell-size                       (convert/m->ft COMPUTATIONAL_DOMAIN_CELLSIZE)
        :srid                            (or A_SRS "EPSG:32610")
        :max-runtime                     (convert/sec->min SIMULATION_TSTOP)
        :simulations                     10 ; FIXME: use NUM_ENSEMBLE_MEMBERS or gridfire_base.edn
        :random-seed                     SEED
        :foliar-moisture                 FOLIAR_MOISTURE_CONTENT
        :ellipse-adjustment-factor       1.0
        :parallel-strategy               :between-fires
        :fractional-distance-combination :sum}
       (process-landfire-layers data options)
       (process-ignition data options)
       (process-weather data options)
       (process-output data options)
       (process-perturbations data options)
       (process-spotting data options)
       (process-fuel-moisture-layers data options)))

;;=============================================================================
;; Parse elmfire.data
;;=============================================================================

(def regex-for-array-item #"^[A-Z0-9\_]+\(\d+\)")

(defn convert-key [s]
  (if (re-matches regex-for-array-item s)
    (str/join "-" (str/split s #"[\(-\)]"))
    s))

(defn convert-val [s]
  (cond
    (re-matches #"^-?[0-9]\d*\.(\d+)?$" s) (Double/parseDouble s)
    (re-matches #"^\d+$" s)                (Integer/parseInt s)
    (re-matches #".TRUE." s)               true
    (re-matches #".FALSE." s)              false
    (re-matches #"'[0-9a-zA-Z_.//]*'" s)   (subs s 1 (dec (count s)))
    :else                                  nil))

(defn parse-elmfire [s]
  (transduce (comp
              (filter #(str/includes? % "="))
              (map #(str/split % #"=")))
             (completing
              (fn [acc [k v]]
                (assoc acc
                       (convert-key (str/trim k))
                       (convert-val (str/trim v)))))
             (sorted-map)
             (str/split s #"\n")))

;;=============================================================================
;; Main
;;=============================================================================

(defn convert-config! [{:keys [elmfire-data] :as options}]
  (log-str "Converting configuration file to one that GridFire accepts.")
  (binding [*elmfire-file-path* (.getParent (io/file elmfire-data))]
    (-> elmfire-data
        (slurp)
        (parse-elmfire)
        (build-edn options)
        (write-config))))
