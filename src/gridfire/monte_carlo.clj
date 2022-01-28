;; [[file:../../org/GridFire.org::monte-carlo-simulation][monte-carlo-simulation]]
(ns gridfire.monte-carlo
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.java.jdbc :as jdbc]
            [tech.v3.tensor :as t]
            [tech.v3.tensor.operators :as top]
            [clojure.core.reducers :as r]
            [gridfire.surface-fire :refer [degrees-to-radians]]
            [gridfire.fire-spread :refer [random-cell run-fire-spread]]
            [gridfire.postgis-bridge :refer [postgis-raster-to-matrix]]
            [tech.v3.datatype :as d]
            [tech.v3.datatype.functional :as dfn]))

(defn postprocess-simulation-results
  [wrf-cell-id lon lat cell-offset-in-neighborhood output-directory results-table]
  (let [num-fires (count results-table)]
    (with-open [out-file (io/writer (io/file output-directory (str "all-fires-" wrf-cell-id ".csv")))]
      (csv/write-csv out-file
                     (cons ["wrf_cell_id" "lon" "lat" "landfire_y" "landfire_x" "offwig_percentile"
                            "ws_20ft_mph" "wdir" "mparam" "lw_moisture" "eaf" "fire_size_ac" "flame_length_mean"
                            ;; "flame_length_stddev" "fire_volume" "fire_shape"]
                            "fire_volume" "fire_shape"]
                           (mapv (fn [{:keys [ignition-site weather-sample wind-speed-20ft wind-from-direction
                                              equilibrium-moisture lw-moisture eaf fire-size flame-length-mean
                                              ;; flame-length-stddev fire-volume fire-shape]}]
                                              fire-volume fire-shape]}]
                                   (let [local-site     (mop/- ignition-site cell-offset-in-neighborhood)
                                         wrf-percentile (- 100.0 (/ weather-sample 36.0))]
                                     [wrf-cell-id
                                      lon
                                      lat
                                      (local-site 0)
                                      (local-site 1)
                                      wrf-percentile
                                      wind-speed-20ft
                                      wind-from-direction
                                      equilibrium-moisture
                                      lw-moisture
                                      eaf
                                      fire-size
                                      flame-length-mean
                                      ;; flame-length-stddev
                                      fire-volume
                                      fire-shape]))
                                 (sort-by :ignition-site results-table)))))
    (format "%s,%s,%s,%.2f,%.2f,%.2f,%.2f\n"
            wrf-cell-id lon lat
            (/ (transduce (map :fire-size)         + 0.0 results-table) num-fires)
            (/ (transduce (map :flame-length-mean) + 0.0 results-table) num-fires)
            (/ (transduce (map :fire-volume)       + 0.0 results-table) num-fires)
            (/ (transduce (map :fire-shape)        + 0.0 results-table) num-fires))))

(defn cells-to-acres
  [cell-size num-cells]
  (let [acres-per-cell (/ (* cell-size cell-size) 43560.0)]
    (* acres-per-cell num-cells)))

(defn compute-fire-behavior-metrics!
  [weather-readings lw-moisture burn-duration cell-size landfire-layers
   ellipse-adjustment-factor ignition-site weather-sample]
  (let [weather-reading      (get weather-readings weather-sample)
        wind-speed-20ft      (weather-reading :ws)     ;; mph
        wind-from-direction  (mod (+ 15 (weather-reading :wd)) 360) ;; degrees (+15 for WRF->AEA warping)
        equilibrium-moisture (weather-reading :mparam) ;; % (0-100)
        fuel-moisture        {:dead {:1hr        (* (+ equilibrium-moisture 0.2) 0.01)
                                     :10hr       (* (+ equilibrium-moisture 1.5) 0.01)
                                     :100hr      (* (+ equilibrium-moisture 2.5) 0.01)}
                              :live {:herbaceous 0.30
                                     :woody      (* lw-moisture 0.01)}}
        foliar-moisture      0.90
        fire-results         (run-fire-spread burn-duration cell-size landfire-layers
                                              wind-speed-20ft wind-from-direction
                                              fuel-moisture foliar-moisture
                                              ellipse-adjustment-factor ignition-site)]
    (if fire-results
      (let [flame-lengths       (filterv pos? (t/tensor->buffer (:flame-length-matrix fire-results)))
            burned-cells        (count flame-lengths)
            fire-size           (cells-to-acres cell-size burned-cells)
            flame-length-mean   (/ (dfn/sum flame-lengths) burned-cells)
            ;; flame-length-stddev (->> flame-lengths
            ;;                          (d/emap #(Math/pow (- flame-length-mean %) 2.0) nil)
            ;;                          (dfn/sum)
            ;;                          (#(/ % burned-cells))
            ;;                          (Math/sqrt))]
            ]
        {:ignition-site        ignition-site
         :weather-sample       weather-sample
         :wind-speed-20ft      wind-speed-20ft
         :wind-from-direction  wind-from-direction
         :equilibrium-moisture equilibrium-moisture
         :lw-moisture          lw-moisture
         :eaf                  ellipse-adjustment-factor
         :fire-size            fire-size
         :flame-length-mean    flame-length-mean
         ;; :flame-length-stddev  flame-length-stddev
         :fire-volume          (* fire-size flame-length-mean)
         :fire-shape           (/ fire-size flame-length-mean)})
      {:ignition-site        ignition-site
       :weather-sample       weather-sample
       :wind-speed-20ft      wind-speed-20ft
       :wind-from-direction  wind-from-direction
       :equilibrium-moisture equilibrium-moisture
       :lw-moisture          lw-moisture
       :eaf                  ellipse-adjustment-factor
       :fire-size            0.0
       :flame-length-mean    0.0
       ;; :flame-length-stddev  0.0
       :fire-volume          0.0
       :fire-shape           0.0})))

(defn run-monte-carlo-fire-spread
  "Returns a vector of maps with the following fields:
   {:ignition-site :weather-sample :wind-speed-20ft :wind-from-direction :equilibrium-moisture
    :eaf :fire-size :flame-length-mean :flame-length-stddev :fire-volume :fire-shape}
   Inputs include:
   - landfire-layers  (map of core.matrix 2D double arrays)
                      {:elevation          m
                       :slope              degrees
                       :aspect             degrees from north
                       :fuel-model         category
                       :canopy-height      m
                       :canopy-base-height m
                       :crown-bulk-density kg/m^3
                       :canopy-cover       % (0-100)}
   - cell-size        cell size of matrices in landfire-layers (ft)
   - ignition-sites   (vector of [i j] points)
   - weather-readings (vector of weather records)
                      [{:ws mph :wd degrees :mparam %} ...]
   - lw-moisture      live woody fuel moisture % (0-100+)
   - samples-per-site (no-arg fn that produces a sequence of indices into weather-readings)
   - burn-duration    maximum time to allow each fire to spread (mins)
   - ellipse-adjustment-factor (< 1.0 = more circular, > 1.0 = more elliptical)"
  [landfire-layers cell-size ignition-sites weather-readings lw-moisture max-wrf-sample-index
   burn-duration ellipse-adjustment-factor]
  (let [landfire-layers (assoc landfire-layers
                               :elevation          (d/clone (d/emap #(* % 3.28) nil (landfire-layers :elevation))) ; m -> ft
                               :slope              (d/clone (d/emap #(Math/tan (degrees-to-radians %)) nil (landfire-layers :slope))) ; degrees -> %
                               :canopy-height      (d/clone (d/emap #(* % 3.28) nil (landfire-layers :canopy-height))) ; m -> ft
                               :canopy-base-height (d/clone (d/emap #(* % 3.28) nil (landfire-layers :canopy-base-height))) ; m -> ft
                               :crown-bulk-density (d/clone (d/emap #(* % 0.0624) nil (landfire-layers :crown-bulk-density))))] ; kg/m^3 -> lb/ft^3
    (mapv (fn [ignition-site]
            (let [weather-sample (rand-int max-wrf-sample-index)]
              (compute-fire-behavior-metrics! weather-readings lw-moisture burn-duration cell-size
                                              landfire-layers ellipse-adjustment-factor ignition-site weather-sample)))
          ignition-sites)))

(defn fetch-wrf-cell-ids
  "Returns a vector of all unique wrf_cell_id strings."
  [db-spec]
  (let [query (str "SELECT j_i AS wrf_cell_id, lon, lat, lw_moisture"
                   "  FROM weather.wrf_points_ca"
                   "  ORDER BY lw_moisture DESC")]
    (jdbc/with-db-transaction [conn db-spec]
      (vec (jdbc/query conn [query])))))

(defn fetch-extreme-weather-readings
  "Returns a vector of maps for each of the top 2% weather readings by
   FFWI with these units:
   {:rank   1-73 (1 = 100th percentile, 73 = 98th percentile)
    :ws     mph (* 0.87 to adjust from 10m winds to 20ft winds)
    :wd     degrees from north
    :mparam 10 * % (0-1000)}"
  [db-spec wrf-cell-id]
  (let [query (str "SELECT rank, 0.87*ows_mph AS ws, wd_deg AS wd, mparam::int AS mparam"
                   "  FROM weather.toptwo_full_daily"
                   "  WHERE j_i_wrf_cacut='" wrf-cell-id "'"
                   "  ORDER BY rank")]
     (jdbc/with-db-transaction [conn db-spec]
       (vec (jdbc/query conn [query])))))

(defn fetch-midrange-weather-readings
  "Returns a vector of maps for each of the 74-76% weather readings by
   FFWI with these units:
   {:rank   1-73 (1 = 100th percentile, 73 = 98th percentile)
    :ws     mph (* 0.87 to adjust from 10m winds to 20ft winds)
    :wd     degrees from north
    :mparam 10 * % (0-1000)}"
  [db-spec wrf-cell-id]
  (let [query (str "SELECT rank, 0.87*ows_mph AS ws, wd_deg AS wd, mparam::int AS mparam"
                   "  FROM weather.midtwo_full_daily"
                   "  WHERE j_i_wrf_cacut='" wrf-cell-id "'"
                   "  ORDER BY rank")]
     (jdbc/with-db-transaction [conn db-spec]
       (vec (jdbc/query conn [query])))))

(defn fetch-landfire-data
  "Returns a map of LANDFIRE rasters as core.matrix 2D double arrays:
   {:elevation          m
    :slope              degrees
    :aspect             degrees
    :fuel-model         category
    :canopy-height      m
    :canopy-base-height m
    :crown-bulk-density kg/m^3
    :canopy-cover       % (0-100)}"
  [db-spec wrf-cell-id]
  (let [landfire-data
        {:elevation          (:matrix (postgis-raster-to-matrix db-spec (str "landfire.dem_wrf_tiles                WHERE j_i='" wrf-cell-id "'")))
         :slope              (:matrix (postgis-raster-to-matrix db-spec (str "landfire.slp_wrf_tiles                WHERE j_i='" wrf-cell-id "'")))
         :aspect             (:matrix (postgis-raster-to-matrix db-spec (str "landfire.asp_wrf_tiles                WHERE j_i='" wrf-cell-id "'")))
         :fuel-model         (:matrix (postgis-raster-to-matrix db-spec (str "fuel_model.fmod_iet_veg2015_wrf_tiles WHERE j_i='" wrf-cell-id "'")))
         ;; :fuel-model         (:matrix (postgis-raster-to-matrix db-spec (str "fuel_model.fmod_reax_v2005_wrf_tiles  WHERE j_i='" wrf-cell-id "'")))
         :canopy-height      (:matrix (postgis-raster-to-matrix db-spec (str "landfire.ch_wrf_tiles                 WHERE j_i='" wrf-cell-id "'")))
         :canopy-base-height (:matrix (postgis-raster-to-matrix db-spec (str "landfire.cbh_wrf_tiles                WHERE j_i='" wrf-cell-id "'")))
         :crown-bulk-density (:matrix (postgis-raster-to-matrix db-spec (str "landfire.cbd_wrf_tiles                WHERE j_i='" wrf-cell-id "'")))
         :canopy-cover       (:matrix (postgis-raster-to-matrix db-spec (str "landfire.cc_wrf_tiles                 WHERE j_i='" wrf-cell-id "'")))}]
    (if (not-any? nil? (vals landfire-data))
      landfire-data)))

(defn read-wrf-cells-list [clj-file start end]
  (-> (slurp clj-file)
      (read-string)
      (subvec start end)))

(defn launch-calfire-monte-carlo-simulation
  [db-spec output-directory wrf-cells-file start end fold-bin-size]
  ;; 1. Read in a list of wrf-cell-ids to process [{:wrf_cell_id :lon :lat :lw_moisture}...]
  ;; 2. Iterate through the wrf-cell-ids sequentially
  ;;    1. Load the live woody fuel moisture
  ;;    2. Load the top 2% extreme FFWI weather dataset as a vector of maps
  ;;    3. Load the LANDFIRE data
  ;;    4. Randomly select 1000 distinct LANDFIRE ignition-sites as a sequence of [i j] points
  ;;    5. Run run-monte-carlo-fire-spread for this WRF cell
  ;;    6. Write results-table to disk as a CSV
  (let [landfire-cell-size          98.425 ;; ft
        calfire-burn-duration       60.0   ;; mins
        max-wrf-sample-index        73
        ellipse-adjustment-factor   1.0
        cell-offset-in-neighborhood [84 83]
        num-ignitions               1000]
    (->> (read-wrf-cells-list wrf-cells-file start end)
         (r/filter (fn [{:keys [wrf_cell_id]}]
                     (not (.exists (io/file output-directory (str "all-fires-" wrf_cell_id ".csv"))))))
         (r/map (fn [{:keys [wrf_cell_id lon lat lw_moisture]}]
                  (try (let [ignition-sites   (into []
                                                    (comp (distinct) (take num-ignitions))
                                                    (repeatedly #(mop/+ cell-offset-in-neighborhood (random-cell 84 83))))
                             weather-readings (fetch-midrange-weather-readings db-spec wrf_cell_id)]
                         (when-let [landfire-data (fetch-landfire-data db-spec wrf_cell_id)]
                           (->> (run-monte-carlo-fire-spread landfire-data landfire-cell-size ignition-sites weather-readings lw_moisture
                                                             max-wrf-sample-index calfire-burn-duration ellipse-adjustment-factor)
                                (postprocess-simulation-results wrf_cell_id lon lat cell-offset-in-neighborhood output-directory))))
                       (catch Exception e (println "Exception in" wrf_cell_id "->" e)))))
         (r/remove nil?)
         (r/fold fold-bin-size r/cat r/append!)
         (cons "wrf_cell_id,lon,lat,fire_size,flame_length,fire_volume,fire_shape")
         (spit (io/file output-directory "all-fires-summary.csv")))))

(comment
  (spit "/data/CALFIRE_MAP1_RUN6/inputs/wrf_cells_to_process.clj"
        (fetch-wrf-cell-ids {:classname   "org.postgresql.Driver"
                             :subprotocol "postgresql"
                             :subname     "//iwap03:5432/calfire"
                             :user        "gridfire"}))

  (spit "/data/CALFIRE_MAP1_RUN6/inputs/wrf_cells_to_process.clj"
        (filterv (fn [{:keys [wrf_cell_id]}]
                   (not (.exists (io/file "/data/CALFIRE_MAP1_RUN6/outputs" (str "all-fires-" wrf_cell_id ".csv")))))
                 (fetch-wrf-cell-ids {:classname   "org.postgresql.Driver"
                                      :subprotocol "postgresql"
                                      :subname     "//iwap03:5432/calfire"
                                      :user        "gridfire"})))

  (spit "/data/IWAP_GRIDFIRE_RUNS/inputs/wrf_cells_to_process.clj"
        (fetch-wrf-cell-ids {:classname   "org.postgresql.Driver"
                             :subprotocol "postgresql"
                             :subname     "//iwap03:5432/calfire"
                             :user        "gridfire"}))

  (spit "/data/IWAP_GRIDFIRE_RUNS/inputs/wrf_cells_to_process.clj"
        (filterv (fn [{:keys [wrf_cell_id]}]
                   (not (.exists (io/file "/data/IWAP_GRIDFIRE_RUNS/outputs" (str "all-fires-" wrf_cell_id ".csv")))))
                 (fetch-wrf-cell-ids {:classname   "org.postgresql.Driver"
                                      :subprotocol "postgresql"
                                      :subname     "//iwap03:5432/calfire"
                                      :user        "gridfire"})))

  ;; iwap02
  (launch-calfire-monte-carlo-simulation
   {:classname   "org.postgresql.Driver"
    :subprotocol "postgresql"
    :subname     "//iwap03:5432/calfire"
    :user        "gridfire"}
   "/data/IWAP_GRIDFIRE_RUNS/outputs"
   "/data/IWAP_GRIDFIRE_RUNS/inputs/wrf_cells_to_process.clj"
   0 6000 30)

  ;; iwap03
  (launch-calfire-monte-carlo-simulation
   {:classname   "org.postgresql.Driver"
    :subprotocol "postgresql"
    :subname     "//localhost:5432/calfire"
    :user        "gridfire"}
   "/data/IWAP_GRIDFIRE_RUNS/outputs"
   "/data/IWAP_GRIDFIRE_RUNS/inputs/wrf_cells_to_process.clj"
   6000 18000 100)

  ;; iwap04
  (launch-calfire-monte-carlo-simulation
   {:classname   "org.postgresql.Driver"
    :subprotocol "postgresql"
    :subname     "//iwap03:5432/calfire"
    :user        "gridfire"}
   "/data/IWAP_GRIDFIRE_RUNS/outputs"
   "/data/IWAP_GRIDFIRE_RUNS/inputs/wrf_cells_to_process.clj"
   18000 30000 100)

  ;; iwap05
  (launch-calfire-monte-carlo-simulation
   {:classname   "org.postgresql.Driver"
    :subprotocol "postgresql"
    :subname     "//iwap03:5432/calfire"
    :user        "gridfire"}
   "/data/IWAP_GRIDFIRE_RUNS/outputs"
   "/data/IWAP_GRIDFIRE_RUNS/inputs/wrf_cells_to_process.clj"
   30000 41423 100))
;; monte-carlo-simulation ends here
