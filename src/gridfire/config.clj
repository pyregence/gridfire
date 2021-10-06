(ns gridfire.config
  (:require [clojure.java.io     :as io]
            [clojure.pprint      :as pprint]
            [clojure.string      :as str]
            [gridfire.conversion :as convert]
            [triangulum.logging  :refer [log-str]]))

;;-----------------------------------------------------------------------------
;; Util
;;-----------------------------------------------------------------------------

(def ^:dynamic elmfire-file-path "")

(defn file-path [directory file-name]
  (let [directory (subs directory 1 (count directory))]
    (str elmfire-file-path directory "/" file-name ".tif")))

(def regex-for-array-item #"^[A-Z0-9\_]+\(\d+\)")

(defn convert-val [s]
  (cond
    (re-matches #"^-?[0-9]\d*\.(\d+)?$" s) (Double/parseDouble s)
    (re-matches #"^-?\d+$" s)              (Integer/parseInt s)
    (re-matches #".TRUE." s)               true
    (re-matches #".FALSE." s)              false
    (re-matches #"'[0-9a-zA-Z_.//]*'" s)   (subs s 1 (dec (count s)))
    (str/includes? "proj" s)               s
    :else                                  nil))

(defn convert-key [s]
  (if (re-matches regex-for-array-item s)
    (str/join "-" (str/split s #"[\(\)]"))
    s))

(defn parse [s]
  (->> (str/split s #"\n")
       (filter #(str/includes? % "="))
       (mapcat #(str/split % #" = "))
       (map str/trim)
       (apply hash-map)
       (reduce-kv (fn [m k v]
                    (assoc m (convert-key k) (convert-val v)))
                  {})))

(defn sec->min
  [seconds]
  (int (/ seconds 60)))

;;-----------------------------------------------------------------------------
;; Landfire
;;-----------------------------------------------------------------------------

(defn process-landfire-layers
  [{:strs [ASP_FILENAME CBH_FILENAME CC_FILENAME CH_FILENAME CBD_FILENAME
           FBFM_FILENAME SLP_FILENAME DEM_FILENAME FUELS_AND_TOPOGRAPHY_DIRECTORY]}
   _
   config]
  (let [dir FUELS_AND_TOPOGRAPHY_DIRECTORY]
    (merge config
           {:landfire-layers {:aspect             {:type   :geotiff
                                                   :source (file-path dir ASP_FILENAME)}
                              :canopy-base-height {:type       :geotiff
                                                   :source     (file-path dir CBH_FILENAME)
                                                   :units      :metric
                                                   :multiplier 0.1}
                              :canopy-cover       {:type   :geotiff
                                                   :source (file-path dir CC_FILENAME)}
                              :canopy-height      {:type       :geotiff
                                                   :source     (file-path dir CH_FILENAME)
                                                   :units      :metric
                                                   :multiplier 0.1}
                              :crown-bulk-density {:type       :geotiff
                                                   :source     (file-path dir CBD_FILENAME)
                                                   :units      :metric
                                                   :multiplier 0.01}
                              :elevation          {:type   :geotiff
                                                   :source (file-path dir DEM_FILENAME)
                                                   :units  :metric}
                              :fuel-model         {:type   :geotiff
                                                   :source (file-path dir FBFM_FILENAME)}
                              :slope              {:type   :geotiff
                                                   :source (file-path dir SLP_FILENAME)}}})))

;;-----------------------------------------------------------------------------
;; Ignition
;;-----------------------------------------------------------------------------

(defn process-ignition
  [{:strs [PHI_FILENAME FUELS_AND_TOPOGRAPHY_DIRECTORY RANDOM_IGNITIONS
           USE_IGNITION_MASK EDGEBUFFER IGNITION_MASK_FILENAME]}
   _
   config]
  (let [dir            FUELS_AND_TOPOGRAPHY_DIRECTORY
        ignition-mask? (.exists (io/file (file-path dir IGNITION_MASK_FILENAME)))]
    (merge config
           (if RANDOM_IGNITIONS
             {:random-ignition {:ignition-mask (when (and USE_IGNITION_MASK ignition-mask?)
                                                 {:type   :geotiff
                                                  :source (file-path dir IGNITION_MASK_FILENAME)})
                                :edge-buffer   (when EDGEBUFFER
                                                 (convert/m->ft EDGEBUFFER))}}
             {:ignition-layer {:type        :geotiff
                               :source      (file-path dir PHI_FILENAME)
                               :burn-values {:burned   -1.0
                                             :unburned 1.0}}}))))


;;-----------------------------------------------------------------------------
;; Weather
;;-----------------------------------------------------------------------------

(defn process-weather
  [{:strs [TMP_FILENAME RH_FILENAME WS_FILENAME
           WD_FILENAME WEATHER_DIRECTORY]}
   _
   config]
  (let [dir    WEATHER_DIRECTORY
        layers {:temperature         {:type   :geotiff
                                      :source (file-path dir "tmpf")}
                :relative-humidity   {:type   :geotiff
                                      :source (file-path dir "rh")}
                :wind-speed-20ft     {:type   :geotiff
                                      :source (file-path dir WS_FILENAME)}
                :wind-from-direction {:type   :geotiff
                                      :source (file-path dir WD_FILENAME)}}]
    (merge config
           layers)))


;;-----------------------------------------------------------------------------
;; Output
;;-----------------------------------------------------------------------------

(defn process-output
  [{:strs [CALCULATE_BURN_PROBABILITY DTDUMP DUMP_BURN_PROBABILITY_AT_DTDUMP
           OUTPUTS_DIRECTORY]}
   {:keys [verbose]} config]
  (let [burn-probability (when DUMP_BURN_PROBABILITY_AT_DTDUMP
                           {:burn-probability (sec->min DTDUMP)})]
    (merge config
           burn-probability
           {:outfile-suffix          ""
            :output-landfire-inputs? false
            :output-geotiffs?        false
            :output-binary?          true
            :output-directory        (str elmfire-file-path (subs OUTPUTS_DIRECTORY 1))
            :output-pngs?            false
            :output-csvs?            true})))

;;-----------------------------------------------------------------------------
;; Perturbations
;;-----------------------------------------------------------------------------

(def elmfire->gridfire
  "A mapping of Elmfire string names to Gridfire keywords"
  {"CBH"    :canopy-base-height
   "CC"     :canopy-cover
   "CH"     :canopy-height
   "CBD"    :crown-bulk-density
   "WS"     :wind-speed-20ft
   "WD"     :wind-direction
   "GLOBAL" :global
   "PIXEL"  :pixel})

(def layers-in-metric #{:crown-bulk-density :canopy-base-height :canopy-height :elevation})

(defn perturbation-info
  [config index]
  {:spatial-type (->> (str/join "-" ["SPATIAL_PERTURBATION" index])
                      (get config)
                      (get elmfire->gridfire))
   :range        [(get config (str/join "-" ["PDF_LOWER_LIMIT" index]))
                  (get config (str/join "-" ["PDF_UPPER_LIMIT" index]))]})

(defn perturbation-key [config index]
  (->> (str/join "-" ["RASTER_TO_PERTURB" index])
       (get config)
       (get elmfire->gridfire)))

(defn extract-perturbations
  [{:strs [NUM_RASTERS_TO_PERTURB] :as config}]
  (when (and NUM_RASTERS_TO_PERTURB (pos? NUM_RASTERS_TO_PERTURB))
    (into {}
          (map (fn [index]
                 (when-let [key (perturbation-key config index)]
                   [key (perturbation-info config index)]))
               (range 1 (inc NUM_RASTERS_TO_PERTURB))))))

(defn add-units
  [config]
  (into config
        (map (fn [[layer spec]]
               (if (layers-in-metric layer)
                 [layer (assoc spec :units :metric)]
                 [layer spec])))
        config))

(defn remove-unused-perturbations
  [config]
  (dissoc config :crown-bulk-density :canopy-base-height))

(defn process-perturbations
  [data _ config]
  (let [perturbations (-> (extract-perturbations data)
                          (add-units)
                          remove-unused-perturbations)]
    (if (seq perturbations)
      (merge config
             {:perturbations perturbations})
      config)))

;;-----------------------------------------------------------------------------
;; Spotting
;;-----------------------------------------------------------------------------


(defn- extract-fuel-range [s]
  (mapv #(Integer/parseInt %) (str/split (re-find #"\d+:\d+" s) #":")))

(defn extract-surface-spotting-percents
  [{:keys [data]}]
  (if-let [SURFACE_FIRE_SPOTTING_PERCENT (get data "SURFACE_FIRE_SPOTTING_PERCENT(:)")]
    [[[1 204] SURFACE_FIRE_SPOTTING_PERCENT]]
    (let [kys (filter #(str/includes? (name %) "SURFACE_FIRE_SPOTTING_PERCENT") (keys data))]
      (reduce-kv (fn [acc k v]
                   (conj acc (extract-fuel-range k) v))
                 []
                 (select-keys data kys)))))

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

(defn extract-crown-fire-spotting-percent
  [{:strs [CROWN_FIRE_SPOTTING_PERCENT_MIN CROWN_FIRE_SPOTTING_PERCENT_MAX
           CROWN_FIRE_SPOTTING_PERCENT ENABLE_SPOTTING]}]
  (if ENABLE_SPOTTING
    [(* 0.01 CROWN_FIRE_SPOTTING_PERCENT_MIN) (* 0.01 CROWN_FIRE_SPOTTING_PERCENT_MAX)]
    (* 0.01 CROWN_FIRE_SPOTTING_PERCENT)))

(defn extract-num-firebrands
  [{:strs [NEMBERS NEMBERS_MIN NEMBERS_MIN_LO NEMBERS_MIN_HI NEMBERS_MAX
           NEMBERS_MAX_LO NEMBERS_MAX_HI ENABLE_SPOTTING]}]

  (if ENABLE_SPOTTING
    {:lo (if NEMBERS_MIN_LO [NEMBERS_MIN_LO NEMBERS_MIN_HI] NEMBERS_MIN)
     :hi (if NEMBERS_MAX_LO [NEMBERS_MAX_LO NEMBERS_MAX_HI] NEMBERS_MAX)}
    NEMBERS))

(defn extract-mean-distance
  [{:strs [MEAN_SPOTTING_DIST_MIN MEAN_SPOTTING_DIST_MAX MEAN_SPOTTING_DIST]}]
  (if MEAN_SPOTTING_DIST
    MEAN_SPOTTING_DIST
    {:lo MEAN_SPOTTING_DIST_MIN
     :hi MEAN_SPOTTING_DIST_MAX}))

(defn extract-flin-exp
  [{:strs [SPOT_FLIN_EXP_LO SPOT_FLIN_EXP_HI SPOT_FLIN_EXP]}]
  (if SPOT_FLIN_EXP
    SPOT_FLIN_EXP
    {:lo SPOT_FLIN_EXP_LO
     :hi SPOT_FLIN_EXP_HI}))

(defn extract-ws-exp
  [{:strs [SPOT_WS_EXP_LO SPOT_WS_EXP_HI SPOT_WS_EXP]}]
  (if SPOT_WS_EXP
    SPOT_WS_EXP
    {:lo SPOT_WS_EXP_LO
     :hi SPOT_WS_EXP_HI}))

(defn extract-normalized-distance-variance
  [{:strs [NORMALIZED_SPOTTING_DIST_VARIANCE_MIN NORMALIZED_SPOTTING_DIST_VARIANCE_MAX NORMALIZED_SPOTTING_DIST_VARIANCE]}]
  (if NORMALIZED_SPOTTING_DIST_VARIANCE
    NORMALIZED_SPOTTING_DIST_VARIANCE
    {:lo NORMALIZED_SPOTTING_DIST_VARIANCE_MIN
     :hi NORMALIZED_SPOTTING_DIST_VARIANCE_MAX}))

(defn process-spotting
  [{:strs [ENABLE_SPOTTING ENABLE_SURFACE_FIRE_SPOTTING CRITICAL_SPOTTING_FIRELINE_INTENSITY] :as data} _ config]
  (if ENABLE_SPOTTING
    (let [spotting-config (cond-> {:spotting {:mean-distance                (extract-mean-distance data)
                                              :ws-exp                       (extract-ws-exp data)
                                              :flin-exp                     (extract-flin-exp data)
                                              :normalized-distance-variance (extract-normalized-distance-variance data)
                                              :crown-fire-spotting-percent  (extract-crown-fire-spotting-percent data)
                                              :num-firebrands               (extract-num-firebrands data)
                                              :decay-constant               0.005}}
                            ENABLE_SURFACE_FIRE_SPOTTING
                            (assoc-in [:spotting :surface-fire-spotting]
                                      {:spotting-percent             (extract-global-surface-spotting-percents data)
                                       :critical-fire-line-intensity (convert/kW-m->Btu-ft-s CRITICAL_SPOTTING_FIRELINE_INTENSITY)}))]
      (merge config spotting-config))
    config))

;;-----------------------------------------------------------------------------
;; Fuel moisture layers
;;-----------------------------------------------------------------------------

(defn process-fuel-moisture-layers
  [{:strs [M1_FILENAME M10_FILENAME M100_FILENAME WEATHER_DIRECTORY
           USE_CONSTANT_LH LH_MOISTURE_CONTENT USE_CONSTANT_LW
           LW_MOISTURE_CONTENT]} _ config]
  (let [dir                  WEATHER_DIRECTORY
        fuel-moisture-layers {:fuel-moisture-layers
                              {:dead {:1hr   {:type   :geotiff
                                              :source (file-path dir M1_FILENAME)}
                                      :10hr  {:type   :geotiff
                                              :source (file-path dir M10_FILENAME)}
                                      :100hr {:type   :geotiff
                                              :source (file-path dir M100_FILENAME)}}
                               :live {:woody      (if USE_CONSTANT_LW
                                                    LW_MOISTURE_CONTENT
                                                    {:type   :geotiff
                                                     :source (file-path dir "mlw")})
                                      :herbaceous (if USE_CONSTANT_LH
                                                    LH_MOISTURE_CONTENT
                                                    {:type   :geotiff
                                                     :source (file-path dir "mlh")})}}}]
    (merge config fuel-moisture-layers)))

;;-----------------------------------------------------------------------------
;; Main
;;-----------------------------------------------------------------------------

(defn build-edn
  [{:strs
    [COMPUTATIONAL_DOMAIN_CELLSIZE SIMULATION_TSTOP NUM_ENSEMBLE_MEMBERS
     A_SRS FOLIAR_MOISTURE_CONTENT SEED] :as d}
   options]
  (let [data (into (sorted-map ) d)]
    (->> {:cell-size                       (convert/m->ft COMPUTATIONAL_DOMAIN_CELLSIZE)
          :srid                            (or A_SRS "EPSG:32610")
          :max-runtime                     (sec->min SIMULATION_TSTOP)
          :simulations                     10
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
         (process-fuel-moisture-layers data options))))

(defn write-config [config-params]
  (let [file-name "gridfire.edn"]
    (log-str "Created Config file: " elmfire-file-path "/" file-name)
    (spit (str/join "/" [elmfire-file-path file-name])
          (with-out-str (pprint/pprint config-params)))))

(defn convert-config! [{:keys [elmfire-data] :as options}]
  (log-str "Converting configuration file to one that Gridfire accepts.")
  (binding [elmfire-file-path (str/replace elmfire-data #"/[\w-]+.data$" "")]
    (let [elmfire-data (parse (slurp elmfire-data))
          gridfire-config (build-edn elmfire-data options)]
      (write-config gridfire-config))
    (shutdown-agents)))
