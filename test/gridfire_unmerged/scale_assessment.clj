;; (ns gridfire-unmerged.scale-assessment
;;   (:require [clojure.java.io :as io]
;;             [tech.v3.tensor :as t]
;;             [tech.v3.tensor.operators :as top]
;;             [clojure.core.reducers :as r]
;;             [clojure.data.csv :as csv]
;;             [incanter.core :as incanter]
;;             [incanter.charts :as charts]
;;             [incanter.stats :as stats]
;;             [matrix-viz.core :refer [save-matrix-as-png]]
;;             [gridfire.postgis-bridge :refer [postgis-raster-to-matrix]]
;;             [gridfire.fire-spread :refer [random-cell select-random-ignition-site burnable? burnable-neighbors?]]
;;             [gridfire.monte-carlo :refer [run-monte-carlo-fire-spread]]))

;; (def db-spec {:classname   "org.postgresql.Driver"
;;               :subprotocol "postgresql"
;;               :subname     "//localhost:5432/calfire"
;;               :user        "gjohnson"})

;; (def landfire-layers-by-tile
;;   {:tile205 {:elevation     (postgis-raster-to-matrix db-spec "validation.elevation_tile205"     nil nil)
;;              :slope         (postgis-raster-to-matrix db-spec "validation.slope_tile205"         nil nil)
;;              :aspect        (postgis-raster-to-matrix db-spec "validation.aspect_tile205"        nil nil)
;;              :fuel-model    (postgis-raster-to-matrix db-spec "validation.fuel_model_tile205"    nil nil)
;;              :canopy-height (postgis-raster-to-matrix db-spec "validation.canopy_height_tile205" nil nil)
;;              :canopy-cover  (postgis-raster-to-matrix db-spec "validation.canopy_cover_tile205"  nil nil)}
;;    :tile210 {:elevation     (postgis-raster-to-matrix db-spec "validation.elevation_tile210"     nil nil)
;;              :slope         (postgis-raster-to-matrix db-spec "validation.slope_tile210"         nil nil)
;;              :aspect        (postgis-raster-to-matrix db-spec "validation.aspect_tile210"        nil nil)
;;              :fuel-model    (postgis-raster-to-matrix db-spec "validation.fuel_model_tile210"    nil nil)
;;              :canopy-height (postgis-raster-to-matrix db-spec "validation.canopy_height_tile210" nil nil)
;;              :canopy-cover  (postgis-raster-to-matrix db-spec "validation.canopy_cover_tile210"  nil nil)}
;;    :tile281 {:elevation     (postgis-raster-to-matrix db-spec "validation.elevation_tile281"     nil nil)
;;              :slope         (postgis-raster-to-matrix db-spec "validation.slope_tile281"         nil nil)
;;              :aspect        (postgis-raster-to-matrix db-spec "validation.aspect_tile281"        nil nil)
;;              :fuel-model    (postgis-raster-to-matrix db-spec "validation.fuel_model_tile281"    nil nil)
;;              :canopy-height (postgis-raster-to-matrix db-spec "validation.canopy_height_tile281" nil nil)
;;              :canopy-cover  (postgis-raster-to-matrix db-spec "validation.canopy_cover_tile281"  nil nil)}
;;    :tile310 {:elevation     (postgis-raster-to-matrix db-spec "validation.elevation_tile310"     nil nil)
;;              :slope         (postgis-raster-to-matrix db-spec "validation.slope_tile310"         nil nil)
;;              :aspect        (postgis-raster-to-matrix db-spec "validation.aspect_tile310"        nil nil)
;;              :fuel-model    (postgis-raster-to-matrix db-spec "validation.fuel_model_tile310"    nil nil)
;;              :canopy-height (postgis-raster-to-matrix db-spec "validation.canopy_height_tile310" nil nil)
;;              :canopy-cover  (postgis-raster-to-matrix db-spec "validation.canopy_cover_tile310"  nil nil)}
;;    :tile564 {:elevation     (postgis-raster-to-matrix db-spec "validation.elevation_tile564"     nil nil)
;;              :slope         (postgis-raster-to-matrix db-spec "validation.slope_tile564"         nil nil)
;;              :aspect        (postgis-raster-to-matrix db-spec "validation.aspect_tile564"        nil nil)
;;              :fuel-model    (postgis-raster-to-matrix db-spec "validation.fuel_model_tile564"    nil nil)
;;              :canopy-height (postgis-raster-to-matrix db-spec "validation.canopy_height_tile564" nil nil)
;;              :canopy-cover  (postgis-raster-to-matrix db-spec "validation.canopy_cover_tile564"  nil nil)}
;;    :tile643 {:elevation     (postgis-raster-to-matrix db-spec "validation.elevation_tile643"     nil nil)
;;              :slope         (postgis-raster-to-matrix db-spec "validation.slope_tile643"         nil nil)
;;              :aspect        (postgis-raster-to-matrix db-spec "validation.aspect_tile643"        nil nil)
;;              :fuel-model    (postgis-raster-to-matrix db-spec "validation.fuel_model_tile643"    nil nil)
;;              :canopy-height (postgis-raster-to-matrix db-spec "validation.canopy_height_tile643" nil nil)
;;              :canopy-cover  (postgis-raster-to-matrix db-spec "validation.canopy_cover_tile643"  nil nil)}})

;; (defn load-extreme-weather-readings
;;   [csv-filename]
;;   (mapv (fn [[x y date hour percentile temp rh ws gust wd ffwi ffwig]]
;;           {:x          (read-string x)
;;            :y          (read-string y)
;;            :date       date
;;            :hour       (read-string hour)
;;            :percentile (read-string percentile)
;;            :temp       (read-string temp)
;;            :rh         (read-string rh)
;;            :ws         (read-string ws)
;;            :gust       (read-string gust)
;;            :wd         (read-string wd)
;;            :ffwi       (read-string ffwi)
;;            :ffwig      (read-string ffwig)})
;;         (with-open [in-file (io/reader csv-filename)]
;;           (doall (rest (csv/read-csv in-file))))))

;; (def extreme-weather-readings
;;   {:tile205 (load-extreme-weather-readings "resources/tile205_extreme_ffwig_percentiles.csv")
;;    :tile210 (load-extreme-weather-readings "resources/tile210_extreme_ffwig_percentiles.csv")
;;    :tile281 (load-extreme-weather-readings "resources/tile281_extreme_ffwig_percentiles.csv")
;;    :tile310 (load-extreme-weather-readings "resources/tile310_extreme_ffwig_percentiles.csv")
;;    :tile564 (load-extreme-weather-readings "resources/tile564_extreme_ffwig_percentiles.csv")
;;    :tile643 (load-extreme-weather-readings "resources/tile643_extreme_ffwig_percentiles.csv")})

;; (def global-cell-size 98.425) ; 30m
;; (def global-burn-duration 60.0) ; mins

;; (defn lattice-cell
;;   [total-rows total-cols lattice-rows lattice-cols lattice-number]
;;   (let [di (double (/ total-rows lattice-rows))
;;         dj (double (/ total-cols lattice-cols))
;;         i0 (long (/ di 2.0))
;;         j0 (long (/ dj 2.0))]
;;     [(+ i0 (long (* di (quot lattice-number lattice-rows))))
;;      (+ j0 (long (* dj (rem lattice-number lattice-rows))))]))

;; (defn select-lattice-ignition-site
;;   [fuel-model-matrix lattice-rows lattice-cols lattice-number]
;;   (let [num-rows        (-> (t/tensor->dimensions fuel-model-matrix) :shape first)
;;         num-cols        (-> (t/tensor->dimensions fuel-model-matrix) :shape second)
;;         ignition-matrix (doto (m/zero-matrix num-rows num-cols) (mop/+= 1.0))]
;;     (if (and (burnable? ignition-matrix fuel-model-matrix ignition-site)
;;              (burnable-neighbors? ignition-matrix fuel-model-matrix num-rows num-cols ignition-site))
;;       ignition-site)))

;; (defn scenario-sites
;;   [fuel-model-matrix scenario]
;;   (case (int scenario)
;;     1 (repeatedly 92 #(select-random-ignition-site fuel-model-matrix))
;;     2 (filterv identity (r/map #(select-lattice-ignition-site fuel-model-matrix 7 7 %) (range 49)))
;;     3 (filterv identity (r/map #(select-lattice-ignition-site fuel-model-matrix 7 7 %) (range 49)))
;;     4 (filterv identity (r/map #(select-lattice-ignition-site fuel-model-matrix 14 14 %) (range 196)))
;;     5 (filterv identity (r/map #(select-lattice-ignition-site fuel-model-matrix 14 14 %) (range 196)))
;;     6 (let [ignition-matrix (doto (m/zero-matrix 67 67) (mop/+= 1.0))]
;;         (filterv (fn [ignition-site]
;;                    (and (burnable? ignition-matrix fuel-model-matrix ignition-site)
;;                         (burnable-neighbors? ignition-matrix fuel-model-matrix 67 67 ignition-site)))
;;                  (for [i (range 67) j (range 67)] [i j])))
;;     7 (let [num-rows (-> (t/tensor->dimensions fuel-model-matrix) :shape first)
;;             num-cols (-> (t/tensor->dimensions fuel-model-matrix) :shape second)]
;;         (take 1000 (distinct (repeatedly #(random-cell num-rows num-cols)))))))
;;             num-cols (-> (t/tensor->dimensions fuel-model-matrix) :shape second)]
;; (def scenario-samples-per-site
;;   {1 #(vector (rand-int 92))
;;    2 (constantly [0 45 90])
;;    3 (constantly [0 8 16 24 32 40 48 56 64 72 80 88])
;;    4 (constantly [0 45 90])
;;    5 (constantly [0 8 16 24 32 40 48 56 64 72 80 88])
;;    6 #(range 92)
;;    7 #(vector (rand-int 92))})

;; (defn run-scenario-on-tile
;;   [scenario tile]
;;   (let [landfire-layers  (landfire-layers-by-tile tile)
;;         cell-size        global-cell-size
;;         ignition-sites   (let [fm  (landfire-layers :fuel-model)
;;                                fm' (m/submatrix fm 66 67 66 67)]
;;                            (mapv #(mop/+ [66 66] %) (scenario-sites fm' scenario)))
;;         weather-readings (mapv #(assoc % :mparam 0.04) (extreme-weather-readings tile))
;;         samples-per-site (scenario-samples-per-site scenario)
;;         burn-duration    global-burn-duration
;;         ellipse-adjustment-factor 1.0]
;;     (run-monte-carlo-fire-spread landfire-layers cell-size ignition-sites
;;                                  weather-readings samples-per-site burn-duration
;;                                  ellipse-adjustment-factor)))

;; (def wrf-cells
;;   {:tile205 [205  6 14]
;;    :tile210 [210 19 20]
;;    :tile281 [281  8 19]
;;    :tile310 [310 18 12]
;;    :tile564 [564  6  2]
;;    :tile643 [643  5  3]})

;; (defn postprocess-simulation-results
;;   [scenario tile {:keys [results-table burn-prob-matrix]}]
;;   (let [wrf-cell    (wrf-cells tile)
;;         plot-titles ["Scenario 1 - Random (Ignition,WRF) Sample Pairs"
;;                      "Scenario 2 - Coarse Lattice, Few WRF Samples"
;;                      "Scenario 3 - Coarse Lattice, Many WRF Samples"
;;                      "Scenario 4 - Fine Lattice, Few WRF Samples"
;;                      "Scenario 5 - Fine Lattice, Many WRF Samples"
;;                      "Scenario 6 - All 30m Cells, All WRF Samples"
;;                      "Scenario 7 - Random (Ignition,WRF) Sample Pairs"]]
;;     (with-open [out-file (io/writer (str "org/pics/scale_assessment/" (name tile) "_data-table-scenario-" scenario ".csv"))]
;;       (csv/write-csv out-file
;;                      (cons ["wrf_tile" "wrf_x" "wrf_y" "landfire_x" "landfire_y" "ffwig_percentile"
;;                             "ws_20ft_mph" "wdir" "mparam" "eaf" "fire_size_ac" "flame_length_mean"
;;                             "flame_length_stddev" "fire_volume" "fire_shape"]
;;                            (mapv (fn [{:keys [ignition-site weather-sample wind-speed-20ft wind-from-direction
;;                                               equilibrium-moisture eaf fire-size flame-length-mean
;;                                               flame-length-stddev fire-volume fire-shape]}]
;;                                    (let [local-site     (mop/- ignition-site [66 66])
;;                                          wrf-percentile (- 100.0 (/ weather-sample 36.53))]
;;                                      [(wrf-cell 0)
;;                                       (wrf-cell 1)
;;                                       (wrf-cell 2)
;;                                       (local-site 0)
;;                                       (local-site 1)
;;                                       wrf-percentile
;;                                       wind-speed-20ft
;;                                       wind-from-direction
;;                                       equilibrium-moisture
;;                                       eaf
;;                                       fire-size
;;                                       flame-length-mean
;;                                       flame-length-stddev
;;                                       fire-volume
;;                                       fire-shape]))
;;                                  results-table))))
;;     (incanter/save
;;      (charts/scatter-plot (mapv :fire-size results-table)
;;                           (mapv :flame-length-mean results-table)
;;                           :group-by (mapv #(Math/floor (- 5.0 (* 2.0 (/ (:weather-sample %) 36.53)))) results-table)
;;                           :title (plot-titles (dec scenario))
;;                           :x-label "Fire Size (ac)"
;;                           :y-label "Flame Length Mean (ft)"
;;                           :series-label "Percentile 0=97.5, 1=98, 2=98.5, 3=99, 4=99.5, 5=100"
;;                           :legend true)
;;      (str "org/pics/scale_assessment/" (name tile) "_scatterplot-scenario-" scenario ".png"))
;;     (spit (str "org/pics/scale_assessment/" (name tile) "_burn-prob-matrix-scenario-" scenario ".clj")
;;           (mapv vec (m/rows burn-prob-matrix)))
;;     (save-matrix-as-png :color 4 -1.0 burn-prob-matrix
;;                         (str "org/pics/scale_assessment/" (name tile) "_burn-prob-scenario-" scenario ".png"))))

;; (defn read-csv-simple
;;   [filename]
;;   (with-open [in-file (io/reader filename)]
;;     (let [rows (csv/read-csv in-file)]
;;       {:header (first rows)
;;        :rows   (doall (rest rows))})))

;; (defn write-csv-simple
;;   [filename csv]
;;   (with-open [out-file (io/writer filename)]
;;     (csv/write-csv out-file (cons (:header csv) (:rows csv)))))

;; (defn merge-csv-files-simple
;;   [csv-directory infile-pattern outfile]
;;   (let [csv-files (->> (io/file csv-directory)
;;                        (file-seq)
;;                        (filter #(re-matches infile-pattern (.getName ^java.io.File %)))
;;                        (map #(read-csv-simple %)))]
;;     (write-csv-simple (io/file csv-directory outfile)
;;                       {:header (:header (first csv-files))
;;                        :rows   (mapcat :rows csv-files)})))

;; ;; (merge-csv-files-simple "/data/statewide_results" #".*.csv$" "all-fires-combined.csv")

;; (defn merge-csv-files
;;   [csv-directory infile-pattern scenario-pattern outfile]
;;   (let [csv-files (->> (io/file csv-directory)
;;                        (file-seq)
;;                        (filter #(re-matches infile-pattern (.getName %)))
;;                        (map #(re-matches scenario-pattern (.getName %)))
;;                        (mapv (fn [[filename scenario]] (assoc (read-csv-simple (io/file csv-directory filename)) :scenario scenario))))]
;;     (write-csv-simple (io/file csv-directory outfile)
;;                       {:header (cons "scenario" (:header (first csv-files)))
;;                        :rows   (mapcat (fn [{:keys [scenario rows]}] (mapv #(cons scenario %) rows)) csv-files)})))

;; (defn interpolate-fire-records
;;   [csv-mins csv-maxs {:keys [percentile ws wd :as wrf-sample]}]
;;   (mapv (fn [csv-min csv-max]
;;           (let [min-ws      (csv-min 6)
;;                 max-ws      (csv-max 6)
;;                 min-fsz     (csv-min 8)
;;                 max-fsz     (csv-max 8)
;;                 min-flm     (csv-min 9)
;;                 max-flm     (csv-max 9)
;;                 min-fls     (csv-min 10)
;;                 max-fls     (csv-max 10)
;;                 max-percent (max 0.0 (min 1.0 (/ (- ws min-ws) (- max-ws min-ws))))
;;                 min-percent (- 1.0 max-percent)
;;                 rand-op1    (rand-nth [+ -])
;;                 rand-op2    (rand-nth [+ -])
;;                 rand-op3    (rand-nth [+ -])]
;;             [(csv-min 0) ; wrf_tile
;;              (csv-min 1) ; wrf_x
;;              (csv-min 2) ; wrf_y
;;              (csv-min 3) ; landfire_x
;;              (csv-min 4) ; landfire_y
;;              percentile  ; ffwig_percentile
;;              ws ; ws_20ft_mph
;;              wd ; wdir
;;              (max 0.0
;;                   (rand-op1
;;                    (+ (* min-percent min-fsz)
;;                       (* max-percent max-fsz))
;;                    (rand (* 0.1 max-fsz)))) ; fire_size_ac
;;              (max 0.0
;;                   (rand-op2
;;                    (+ (* min-percent min-flm)
;;                       (* max-percent max-flm))
;;                    (rand (* 0.1 max-flm)))) ; flame_length_mean
;;              (max 0.0
;;                   (rand-op3
;;                    (+ (* min-percent min-fls)
;;                       (* max-percent max-fls))
;;                    (rand (* 0.1 max-fls))))])) ; flame_length_stddev
;;         csv-mins
;;         csv-maxs))

;; (defn bridge-csv-samples
;;   [csv-min-dir csv-max-dir output-dir]
;;   (doseq [scenario [6] tile [:tile205 :tile210 :tile281 :tile310 :tile564 :tile643]]
;;     (let [csv-min-file (io/file csv-min-dir (str (name tile) "_data-table-scenario-" scenario ".csv"))
;;           csv-max-file (io/file csv-max-dir (str (name tile) "_data-table-scenario-" scenario ".csv"))
;;           csv-mins     (if (.exists csv-min-file)
;;                          (with-open [csv-min-reader (io/reader csv-min-file)]
;;                            (mapv #(mapv read-string %) (rest (csv/read-csv csv-min-reader)))))
;;           csv-maxs     (if (.exists csv-max-file)
;;                          (with-open [csv-max-reader (io/reader csv-max-file)]
;;                            (mapv #(mapv read-string %) (rest (csv/read-csv csv-max-reader)))))]
;;       (with-open [out-file (io/writer (io/file output-dir (str (name tile) "_data-table-scenario-" scenario ".csv")))]
;;         (csv/write-csv out-file
;;                        (cons ["wrf_tile" "wrf_x" "wrf_y" "landfire_x" "landfire_y" "ffwig_percentile" "ws_20ft_mph"
;;                               "wdir" "fire_size_ac" "flame_length_mean" "flame_length_stddev"]
;;                              (concat csv-maxs
;;                                      (apply concat
;;                                             (for [i (range 1 91)]
;;                                               (let [wrf-sample  (get-in extreme-weather-readings [tile i])]
;;                                                   (interpolate-fire-records csv-mins csv-maxs wrf-sample))))
;;                                      csv-mins)))))))

;; (defn plot-csv-results
;;   [csv-directory tile scenario]
;;   (let [plot-titles ["Scenario 1 - Random (Ignition,WRF) Sample Pairs"
;;                      "Scenario 2 - Coarse Lattice, Few WRF Samples"
;;                      "Scenario 3 - Coarse Lattice, Many WRF Samples"
;;                      "Scenario 4 - Fine Lattice, Few WRF Samples"
;;                      "Scenario 5 - Fine Lattice, Many WRF Samples"
;;                      "Scenario 6 - All 30m Cells, All WRF Samples"
;;                      "Scenario 7 - Random (Ignition,WRF) Sample Pairs"]
;;         results-table (with-open [in-file (io/reader (io/file csv-directory (str (name tile) "_data-table-scenario-" scenario ".csv")))]
;;                         (mapv (fn [record] {:percentile        (read-string (record 5))
;;                                             :fire-size         (read-string (record 8))
;;                                             :flame-length-mean (read-string (record 9))})
;;                               (rest (csv/read-csv in-file))))]
;;     (incanter/save
;;      (charts/scatter-plot (mapv :fire-size results-table)
;;                           (mapv :flame-length-mean results-table)
;;                           :group-by (mapv #(Math/floor (- 5.0 (* 2.0 (- 100.0 (:percentile %))))) results-table)
;;                           :title (plot-titles (dec scenario))
;;                           :x-label "Fire Size (ac)"
;;                           :y-label "Flame Length Mean (ft)"
;;                           :series-label "Percentile 0=97.5, 1=98, 2=98.5, 3=99, 4=99.5, 5=100"
;;                           :legend true)
;;      (io/file csv-directory (str (name tile) "_scatterplot-scenario-" scenario ".png")))))

;; (defn generate-cumulative-csv-results
;;   [csv-directory tile scenario]
;;   (let [results-table (with-open [in-file (io/reader (io/file csv-directory (str (name tile) "_data-table-scenario-" scenario ".csv")))]
;;                         (mapv (fn [record] {:percentile        (read-string (record 5))
;;                                             :fire-size         (read-string (record 8))
;;                                             :flame-length-mean (read-string (record 9))
;;                                             :fire-volume       (read-string (record 11))
;;                                             :fire-shape        (read-string (record 12))})
;;                               (rest (csv/read-csv in-file))))]
;;     (with-open [out-file (io/writer (io/file csv-directory (format "%s_cumulative-stats-scenario-%d.csv" (name tile) scenario)))]
;;       (csv/write-csv out-file
;;                      (cons ["samples" "fire_size_mean" "fire_size_stddev" "fire_size_skewness"
;;                             "flame_length_mean" "flame_length_stddev" "flame_length_skewness"
;;                             "fire_volume_mean" "fire_volume_stddev" "fire_volume_skewness"
;;                             "fire_shape_mean" "fire_shape_stddev" "fire_shape_skewness"]
;;                            (for [bin-size (range 100 1001 100)]
;;                              (let [results-table (take bin-size results-table)
;;                                    fire-sizes    (filterv pos? (mapv :fire-size         results-table))
;;                                    flame-lengths (filterv pos? (mapv :flame-length-mean results-table))
;;                                    fire-volumes  (filterv pos? (mapv :fire-volume       results-table))
;;                                    fire-shapes   (filterv pos? (mapv :fire-shape        results-table))
;;                                    get-stats     (juxt stats/mean stats/sd stats/skewness)]
;;                                (cons bin-size (mapcat get-stats [fire-sizes flame-lengths fire-volumes fire-shapes])))))))
;;     (doseq [bin-size (range 100 1001 100)]
;;       (let [results-table (take bin-size results-table)]
;;         (incanter/save
;;          (charts/scatter-plot (mapv :fire-size results-table)
;;                               (mapv :flame-length-mean results-table)
;;                               :group-by (mapv #(Math/floor (- 5.0 (* 2.0 (- 100.0 (:percentile %))))) results-table)
;;                               :title (str "Scenario 7-" bin-size " Fire Size vs Flame Length")
;;                               :x-label "Fire Size (ac)"
;;                               :y-label "Flame Length Mean (ft)"
;;                               :series-label "Percentile 0=97.5, 1=98, 2=98.5, 3=99, 4=99.5, 5=100"
;;                               :legend true)
;;          (io/file csv-directory (format "%s_scatterplot-scenario-%d-%04d.png" (name tile) scenario bin-size)))
;;         (incanter/save
;;          (charts/histogram (filterv pos? (mapv :fire-size results-table))
;;                            :nbins 20
;;                            :density true
;;                            :title (str "Scenario 7-" bin-size " Fire Size")
;;                            :x-label "Fire Size (ac)")
;;          (io/file csv-directory (format "%s_fire-size-scenario-%d-%04d.png" (name tile) scenario bin-size)))
;;         (incanter/save
;;          (charts/histogram (filterv pos? (mapv :flame-length-mean results-table))
;;                            :nbins 20
;;                            :density true
;;                            :title (str "Scenario 7-" bin-size " Flame Length Mean")
;;                            :x-label "Flame Length Mean (ft)")
;;          (io/file csv-directory (format "%s_flame-length-scenario-%d-%04d.png" (name tile) scenario bin-size)))
;;         (incanter/save
;;          (charts/histogram (filterv pos? (mapv :fire-volume results-table))
;;                            :nbins 20
;;                            :density true
;;                            :title (str "Scenario 7-" bin-size " Fire Volume")
;;                            :x-label "Fire Volume (ac*ft)")
;;          (io/file csv-directory (format "%s_fire-volume-scenario-%d-%04d.png" (name tile) scenario bin-size)))
;;         (incanter/save
;;          (charts/histogram (filterv pos? (mapv :fire-shape results-table))
;;                            :nbins 20
;;                            :density true
;;                            :title (str "Scenario 7-" bin-size " Fire Shape")
;;                            :x-label "Fire Shape (ac/ft)")
;;          (io/file csv-directory (format "%s_fire-shape-scenario-%d-%04d.png" (name tile) scenario bin-size)))))))

;; ;; Example commands for the 36 (scenario,tile) pairs
;; (comment
;;   (time (def scenario1-tile205 (run-scenario-on-tile 1 :tile205)))
;;   (time (def scenario2-tile205 (run-scenario-on-tile 2 :tile205)))
;;   (time (def scenario3-tile205 (run-scenario-on-tile 3 :tile205)))
;;   (time (def scenario4-tile205 (run-scenario-on-tile 4 :tile205)))
;;   (time (def scenario5-tile205 (run-scenario-on-tile 5 :tile205)))
;;   (time (def scenario6-tile205 (run-scenario-on-tile 6 :tile205)))
;;   (time (def scenario7-tile205 (run-scenario-on-tile 7 :tile205)))

;;   (postprocess-simulation-results 1 :tile205 scenario1-tile205)
;;   (postprocess-simulation-results 2 :tile205 scenario2-tile205)
;;   (postprocess-simulation-results 3 :tile205 scenario3-tile205)
;;   (postprocess-simulation-results 4 :tile205 scenario4-tile205)
;;   (postprocess-simulation-results 5 :tile205 scenario5-tile205)
;;   (postprocess-simulation-results 6 :tile205 scenario6-tile205)
;;   (postprocess-simulation-results 7 :tile205 scenario7-tile205)

;;   (time (def scenario1-tile210 (run-scenario-on-tile 1 :tile210)))
;;   (time (def scenario2-tile210 (run-scenario-on-tile 2 :tile210)))
;;   (time (def scenario3-tile210 (run-scenario-on-tile 3 :tile210)))
;;   (time (def scenario4-tile210 (run-scenario-on-tile 4 :tile210)))
;;   (time (def scenario5-tile210 (run-scenario-on-tile 5 :tile210)))
;;   (time (def scenario6-tile210 (run-scenario-on-tile 6 :tile210)))
;;   (time (def scenario7-tile210 (run-scenario-on-tile 7 :tile210)))

;;   (postprocess-simulation-results 1 :tile210 scenario1-tile210)
;;   (postprocess-simulation-results 2 :tile210 scenario2-tile210)
;;   (postprocess-simulation-results 3 :tile210 scenario3-tile210)
;;   (postprocess-simulation-results 4 :tile210 scenario4-tile210)
;;   (postprocess-simulation-results 5 :tile210 scenario5-tile210)
;;   (postprocess-simulation-results 6 :tile210 scenario6-tile210)
;;   (postprocess-simulation-results 7 :tile210 scenario7-tile210)

;;   (time (def scenario1-tile281 (run-scenario-on-tile 1 :tile281)))
;;   (time (def scenario2-tile281 (run-scenario-on-tile 2 :tile281)))
;;   (time (def scenario3-tile281 (run-scenario-on-tile 3 :tile281)))
;;   (time (def scenario4-tile281 (run-scenario-on-tile 4 :tile281)))
;;   (time (def scenario5-tile281 (run-scenario-on-tile 5 :tile281)))
;;   (time (def scenario6-tile281 (run-scenario-on-tile 6 :tile281)))
;;   (time (def scenario7-tile281 (run-scenario-on-tile 7 :tile281)))

;;   (postprocess-simulation-results 1 :tile281 scenario1-tile281)
;;   (postprocess-simulation-results 2 :tile281 scenario2-tile281)
;;   (postprocess-simulation-results 3 :tile281 scenario3-tile281)
;;   (postprocess-simulation-results 4 :tile281 scenario4-tile281)
;;   (postprocess-simulation-results 5 :tile281 scenario5-tile281)
;;   (postprocess-simulation-results 6 :tile281 scenario6-tile281)
;;   (postprocess-simulation-results 7 :tile281 scenario7-tile281)

;;   (time (def scenario1-tile310 (run-scenario-on-tile 1 :tile310)))
;;   (time (def scenario2-tile310 (run-scenario-on-tile 2 :tile310)))
;;   (time (def scenario3-tile310 (run-scenario-on-tile 3 :tile310)))
;;   (time (def scenario4-tile310 (run-scenario-on-tile 4 :tile310)))
;;   (time (def scenario5-tile310 (run-scenario-on-tile 5 :tile310)))
;;   (time (def scenario6-tile310 (run-scenario-on-tile 6 :tile310)))
;;   (time (def scenario7-tile310 (run-scenario-on-tile 7 :tile310)))

;;   (postprocess-simulation-results 1 :tile310 scenario1-tile310)
;;   (postprocess-simulation-results 2 :tile310 scenario2-tile310)
;;   (postprocess-simulation-results 3 :tile310 scenario3-tile310)
;;   (postprocess-simulation-results 4 :tile310 scenario4-tile310)
;;   (postprocess-simulation-results 5 :tile310 scenario5-tile310)
;;   (postprocess-simulation-results 6 :tile310 scenario6-tile310)
;;   (postprocess-simulation-results 7 :tile310 scenario7-tile310)

;;   (time (def scenario1-tile564 (run-scenario-on-tile 1 :tile564)))
;;   (time (def scenario2-tile564 (run-scenario-on-tile 2 :tile564)))
;;   (time (def scenario3-tile564 (run-scenario-on-tile 3 :tile564)))
;;   (time (def scenario4-tile564 (run-scenario-on-tile 4 :tile564)))
;;   (time (def scenario5-tile564 (run-scenario-on-tile 5 :tile564)))
;;   (time (def scenario6-tile564 (run-scenario-on-tile 6 :tile564)))
;;   (time (def scenario7-tile564 (run-scenario-on-tile 7 :tile564)))

;;   (postprocess-simulation-results 1 :tile564 scenario1-tile564)
;;   (postprocess-simulation-results 2 :tile564 scenario2-tile564)
;;   (postprocess-simulation-results 3 :tile564 scenario3-tile564)
;;   (postprocess-simulation-results 4 :tile564 scenario4-tile564)
;;   (postprocess-simulation-results 5 :tile564 scenario5-tile564)
;;   (postprocess-simulation-results 6 :tile564 scenario6-tile564)
;;   (postprocess-simulation-results 7 :tile564 scenario7-tile564)

;;   (time (def scenario1-tile643 (run-scenario-on-tile 1 :tile643)))
;;   (time (def scenario2-tile643 (run-scenario-on-tile 2 :tile643)))
;;   (time (def scenario3-tile643 (run-scenario-on-tile 3 :tile643)))
;;   (time (def scenario4-tile643 (run-scenario-on-tile 4 :tile643)))
;;   (time (def scenario5-tile643 (run-scenario-on-tile 5 :tile643)))
;;   (time (def scenario6-tile643 (run-scenario-on-tile 6 :tile643)))
;;   (time (def scenario7-tile643 (run-scenario-on-tile 7 :tile643)))

;;   (postprocess-simulation-results 1 :tile643 scenario1-tile643)
;;   (postprocess-simulation-results 2 :tile643 scenario2-tile643)
;;   (postprocess-simulation-results 3 :tile643 scenario3-tile643)
;;   (postprocess-simulation-results 4 :tile643 scenario4-tile643)
;;   (postprocess-simulation-results 5 :tile643 scenario5-tile643)
;;   (postprocess-simulation-results 6 :tile643 scenario6-tile643)
;;   (postprocess-simulation-results 7 :tile643 scenario7-tile643)

;;   (do
;;     (generate-cumulative-csv-results "org/pics/scale_assessment" :tile205 7)
;;     (generate-cumulative-csv-results "org/pics/scale_assessment" :tile210 7)
;;     (generate-cumulative-csv-results "org/pics/scale_assessment" :tile281 7)
;;     (generate-cumulative-csv-results "org/pics/scale_assessment" :tile310 7)
;;     (generate-cumulative-csv-results "org/pics/scale_assessment" :tile564 7)
;;     (generate-cumulative-csv-results "org/pics/scale_assessment" :tile643 7))

;;   (merge-csv-files "org/pics/scale_assessment/sequential_runs" #"tile.+data-table-scenario.+\.csv" #"tile.+-(\d+)\.csv" "data-table.csv"))
