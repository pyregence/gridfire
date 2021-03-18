;; [[file:../../org/GridFire.org::command-line-interface][command-line-interface]]
(ns gridfire.cli
  (:gen-class)
  (:require [clojure.core.matrix   :as m]
            [clojure.data.csv      :as csv]
            [clojure.edn           :as edn]
            [clojure.java.io       :as io]
            [clojure.spec.alpha    :as s]
            [clojure.string        :as str]
            [gridfire.crown-fire   :refer [m->ft]]
            [gridfire.common       :refer [calc-emc]]
            [gridfire.fetch        :as fetch]
            [gridfire.fire-spread  :refer [get-neighbors in-bounds? run-fire-spread]]
            [gridfire.perturbation :as perturbation]
            [gridfire.spec.config  :as spec]
            [gridfire.utils.random :refer [my-rand-int my-rand-nth]]
            [magellan.core         :refer [make-envelope
                                           matrix-to-raster
                                           register-new-crs-definitions-from-properties-file!
                                           write-raster]]
            [matrix-viz.core       :refer [save-matrix-as-png]])
  (:import java.util.Random))

(m/set-current-implementation :vectorz)

(register-new-crs-definitions-from-properties-file! "CUSTOM"
                                                    (io/resource "custom_projections.properties"))

(defn sample-from-list
  [rand-generator n xs]
  (repeatedly n #(my-rand-nth rand-generator xs)))

(defn sample-from-range
  [rand-generator n [min max]]
  (let [range (- max min)]
    (repeatedly n #(+ min (my-rand-int rand-generator range)))))

(defn draw-samples
  [rand-generator n x]
  (into []
        (cond (list? x)   (sample-from-list rand-generator n x)
              (vector? x) (sample-from-range rand-generator n x)
              :else       (repeat n x))))

(defn cells-to-acres
  [cell-size num-cells]
  (let [acres-per-cell (/ (* cell-size cell-size) 43560.0)]
    (* acres-per-cell num-cells)))

(defn summarize-fire-spread-results
  [fire-spread-results cell-size]
  (let [flame-lengths              (filterv pos? (m/eseq (:flame-length-matrix fire-spread-results)))
        fire-line-intensities      (filterv pos? (m/eseq (:fire-line-intensity-matrix fire-spread-results)))
        burned-cells               (count flame-lengths)
        fire-size                  (cells-to-acres cell-size burned-cells)
        flame-length-mean          (/ (m/esum flame-lengths) burned-cells)
        fire-line-intensity-mean   (/ (m/esum fire-line-intensities) burned-cells)
        flame-length-stddev        (->> flame-lengths
                                        (m/emap #(Math/pow (- flame-length-mean %) 2.0))
                                        (m/esum)
                                        (#(/ % burned-cells))
                                        (Math/sqrt))
        fire-line-intensity-stddev (->> fire-line-intensities
                                        (m/emap #(Math/pow (- fire-line-intensity-mean %) 2.0))
                                        (m/esum)
                                        (#(/ % burned-cells))
                                        (Math/sqrt))]
    {:fire-size                  fire-size
     :flame-length-mean          flame-length-mean
     :flame-length-stddev        flame-length-stddev
     :fire-line-intensity-mean   fire-line-intensity-mean
     :fire-line-intensity-stddev fire-line-intensity-stddev}))

(defn calc-ffwi
  "Computes the Fosberg Fire Weather Index value from rh (relative
   humidity in %), temp (temperature in F), wsp (wind speed in mph),
   and a constant x (gust multiplier).
   ------------------------------------------------------------------
   Note: ffwi can be computed with (calc-ffwi rh temp wsp 1.0)
         ffwi-max can be computed with (calc-ffwi minrh maxtemp wsp 1.75)
   Geek points: Uses Cramer's rule: (+ d (* x (+ c (* x (+ b (* x a))))))
                for an efficient cubic calculation on tmp."
  [rh temp wsp x]
  (let [m   (calc-emc rh temp)
        eta (+ 1 (* m (+ -2 (* m (+ 1.5 (* m -0.5))))))]
    (/ (* eta (Math/sqrt (+ 1 (Math/pow (* x wsp) 2))))
       0.3002)))

(defn kebab->snake [s]
  (str/replace s #"-" "_"))

(defn min->hour [t]
  (int (quot t 60)))

(defn previous-active-perimeter?
  [[i j :as here] matrix]
  (let [num-rows (m/row-count matrix)
        num-cols (m/column-count matrix)]
   (and
    (= (m/mget matrix i j) -1.0)
    (->> (get-neighbors here)
         (filter #(in-bounds? num-rows num-cols %))
         (map #(apply m/mget matrix %))
         (some pos?)))))

(defn to-color-map-values [burn-time-matrix current-clock]
  (m/emap-indexed (fn [here burn-time]
                    (let [delta-hours (->> (- current-clock burn-time)
                                           min->hour)]
                      (cond
                        (previous-active-perimeter? here burn-time-matrix) 201
                        (= burn-time -1.0)                                 200
                        (< 0 delta-hours 5)                                delta-hours
                        (>= delta-hours 5)                                 5
                        :else                                              0)))
                  burn-time-matrix))

(defn layer-matrix
  [{:keys [burn-time-matrix] :as fire-spread-results} layer current-clock]
  (if (= layer :burn-history)
    (to-color-map-values burn-time-matrix current-clock)
    (let [kw (->> (str (name layer) "-matrix")
                  keyword)]
      (get fire-spread-results kw))))

(defn layer-snapshot [burn-time-matrix layer-matrix  t]
  (m/emap (fn [layer-value burn-time]
            (if (<= burn-time t)
              layer-value
              0))
          layer-matrix
          burn-time-matrix))

(defn output-simulation-layers-timestep!
  [{:keys [output-layers output-geotiffs? output-pngs?]}
   {:keys [global-clock burn-time-matrix] :as fire-spread-results}
   envelope
   simulation-id]
  (doseq [[layer timestep] output-layers
          output-time      (range 0 (inc global-clock) timestep)]
    (let [filtered-matrix (layer-snapshot burn-time-matrix
                                          (layer-matrix fire-spread-results layer output-time)
                                          output-time)
          layer-name      (-> (name layer)
                              kebab->snake)]
      (when output-geotiffs?
        (-> (matrix-to-raster layer-name filtered-matrix envelope)
            (write-raster (str/join "_" [layer-name
                                         simulation-id
                                         (str "t" output-time ".tif")]))))
      (when output-pngs?
        (save-matrix-as-png :color 4 -1.0
                            filtered-matrix
                            (str/join "_" [layer-name
                                           simulation-id
                                           (str "t" output-time ".png")]))))))

(defn output-simulation-layers!
  [{:keys [output-geotiffs? outfile-suffix output-pngs?]}
   fire-spread-results
   envelope
   simulation-id]
  (doseq [[name layer] [["fire_spread"         :fire-spread-matrix]
                        ["flame_length"        :flame-length-matrix]
                        ["fire_line_intensity" :fire-line-intensity-matrix]]]
    (when output-geotiffs?
      (-> (matrix-to-raster name (fire-spread-results layer) envelope)
          (write-raster (str name outfile-suffix "_" simulation-id ".tif"))))
    (when output-pngs?
      (save-matrix-as-png :color 4 -1.0
                          (fire-spread-results layer)
                          (str name outfile-suffix "_" simulation-id ".png")))))

(defn process-burn-count!
  [{:keys [fire-spread-matrix burn-time-matrix global-clock]}
   burn-count-matrix
   timestep]
  (doseq [clock (range 0 (inc global-clock) timestep)]
    (let [filtered-fire-spread (m/emap (fn [layer-value burn-time]
                                         (if (<= burn-time clock)
                                           layer-value
                                           0))
                                       fire-spread-matrix
                                       burn-time-matrix)
          band                 (int (quot clock timestep))]
      (m/add! (nth (seq burn-count-matrix) band) filtered-fire-spread))))

(defn output-burn-probability-layer!
  [{:keys [output-geotiffs? output-pngs? output-burn-probability simulations]} envelope burn-count-matrix]
  (when-let [timestep output-burn-probability]
    (doseq [[band matrix] (map-indexed vector burn-count-matrix)]
      (let [output-name        "burn_probability"
            output-time        (* band timestep)
            probability-matrix (m/emap #(/ % simulations) matrix)]
        (do
          (when output-geotiffs?
           (-> (matrix-to-raster output-name probability-matrix envelope)
               (write-raster (str/join "_" [output-name (str "t" output-time ".tif")]))))
          (when output-pngs?
            (save-matrix-as-png :color 4 -1.0
                                probability-matrix
                                (str/join "_" [output-name (str "t" output-time ".png")]))))))))

(defn initialize-burn-count-matrix
  [{:keys [output-burn-probability]} max-runtime num-rows num-cols]
  (when output-burn-probability
    (let [num-bands (inc (quot (apply max max-runtime) output-burn-probability))]
      (m/zero-array [num-bands num-rows num-cols ]))))

(defn run-simulations
  [{:keys
    [cell-size output-csvs? simulations output-layers output-burn-probability] :as config}
   landfire-rasters envelope ignition-row ignition-col max-runtime temperature
   relative-humidity wind-speed-20ft wind-from-direction foliar-moisture
   ellipse-adjustment-factor ignition-layer multiplier-lookup perturbations
   burn-count-matrix]
  (mapv
   (fn [i]
     (let [matrix-or-i           (fn [obj i] (:matrix obj (obj i)))
           initial-ignition-site (or ignition-layer
                                     [(ignition-row i) (ignition-col i)])
           input-variations      {:max-runtime               (max-runtime i)
                                  :temperature               (matrix-or-i temperature i)
                                  :relative-humidity         (matrix-or-i relative-humidity i)
                                  :wind-speed-20ft           (matrix-or-i wind-speed-20ft i)
                                  :wind-from-direction       (matrix-or-i wind-from-direction i)
                                  :foliar-moisture           (* 0.01 (foliar-moisture i))
                                  :ellipse-adjustment-factor (ellipse-adjustment-factor i)}
           fire-spread-results   (run-fire-spread
                                  (merge
                                   input-variations
                                   {:cell-size             cell-size
                                    :landfire-rasters      landfire-rasters
                                    :num-rows              (m/row-count (:fuel-model landfire-rasters))
                                    :num-cols              (m/column-count (:fuel-model landfire-rasters))
                                    :multiplier-lookup     multiplier-lookup
                                    :initial-ignition-site initial-ignition-site
                                    :perturbations         (when perturbations
                                                             (perturbations i))}))]
       (when fire-spread-results
         (if output-layers
           (output-simulation-layers-timestep! config fire-spread-results envelope i)
           (output-simulation-layers! config fire-spread-results envelope i))
         (when-let [timestep output-burn-probability]
           (process-burn-count! fire-spread-results burn-count-matrix timestep)))
       (when output-csvs?
         (merge
          input-variations
          {:ignition-row    (ignition-row i)
           :ignition-col    (ignition-col i)
           :foliar-moisture (foliar-moisture i)
           :exit-condition  (:exit-condition fire-spread-results :no-fire-spread)}
          (if fire-spread-results
            (summarize-fire-spread-results fire-spread-results cell-size)
            {:fire-size                  0.0
             :flame-length-mean          0.0
             :flame-length-stddev        0.0
             :fire-line-intensity-mean   0.0
             :fire-line-intensity-stddev 0.0})))))
   (range simulations)))

(defn write-csv-outputs
  [output-csvs? output-filename results-table]
  (when output-csvs?
    (with-open [out-file (io/writer output-filename)]
      (->> results-table
           (sort-by #(vector (:ignition-row %) (:ignition-col %)))
           (mapv (fn [{:keys [ignition-row ignition-col max-runtime temperature relative-humidity wind-speed-20ft
                              wind-from-direction foliar-moisture ellipse-adjustment-factor fire-size flame-length-mean
                              flame-length-stddev fire-line-intensity-mean fire-line-intensity-stddev]}]
                   [ignition-row
                    ignition-col
                    max-runtime
                    temperature
                    relative-humidity
                    wind-speed-20ft
                    wind-from-direction
                    foliar-moisture
                    ellipse-adjustment-factor
                    fire-size
                    flame-length-mean
                    flame-length-stddev
                    fire-line-intensity-mean
                    fire-line-intensity-stddev]))
           (cons ["ignition-row" "ignition-col" "max-runtime" "temperature" "relative-humidity" "wind-speed-20ft"
                  "wind-from-direction" "foliar-moisture" "ellipse-adjustment-factor" "fire-size" "flame-length-mean"
                  "flame-length-stddev" "fire-line-intensity-mean" "fire-line-intensity-stddev"])
           (csv/write-csv out-file)))))

(defn get-envelope
  [config landfire-layers]
  (let [{:keys [upperleftx upperlefty width height scalex scaley]} (landfire-layers :elevation)]
    (make-envelope (:srid config)
                   upperleftx
                   (+ upperlefty (* height scaley))
                   (* width scalex)
                   (* -1.0 height scaley))))

(defn get-weather [config rand-generator weather-type weather-layers]
  (if (contains? weather-layers weather-type)
    (weather-type weather-layers)
    (draw-samples rand-generator (:simulations config) (config weather-type))))

(defn create-multiplier-lookup
  [{:keys [cell-size]} weather-layers]
  (reduce-kv (fn [acc k {:keys [scalex]}]
               (assoc acc k (int (quot (m->ft scalex) cell-size))))
             {}
             weather-layers))

(defn -main
  [& config-files]
  (doseq [config-file config-files]
    (let [config (edn/read-string (slurp config-file))]
      (if (s/valid? ::spec/config config)
        (let [landfire-layers   (fetch/landfire-layers config)
              landfire-rasters  (into {}
                                      (map (fn [[layer info]] [layer (first (:matrix info))]))
                                      landfire-layers)
              ignition-layer    (fetch/ignition-layer config)
              weather-layers    (fetch/weather-layers config)
              multiplier-lookup (create-multiplier-lookup config weather-layers)
              envelope          (get-envelope config landfire-layers)
              simulations       (:simulations config)
              rand-generator    (if-let [seed (:random-seed config)]
                                  (Random. seed)
                                  (Random.))
              max-runtimes      (draw-samples rand-generator simulations (:max-runtime config))
              num-rows          (m/row-count (:fuel-model landfire-rasters))
              num-cols          (m/column-count (:fuel-model landfire-rasters))
              burn-count-matrix (initialize-burn-count-matrix config max-runtimes num-rows num-cols)]
          (when (:output-landfire-inputs? config)
            (doseq [[layer matrix] landfire-rasters]
              (-> (matrix-to-raster (name layer) matrix envelope)
                  (write-raster (str (name layer) (:outfile-suffix config) ".tif")))))
          (->> (run-simulations
                config
                landfire-rasters
                envelope
                (draw-samples rand-generator simulations (:ignition-row config))
                (draw-samples rand-generator simulations (:ignition-col config))
                max-runtimes
                (get-weather config rand-generator :temperature weather-layers)
                (get-weather config rand-generator :relative-humidity weather-layers)
                (get-weather config rand-generator :wind-speed-20ft weather-layers)
                (get-weather config rand-generator :wind-from-direction weather-layers)
                (draw-samples rand-generator simulations (:foliar-moisture config))
                (draw-samples rand-generator simulations (:ellipse-adjustment-factor config))
                ignition-layer
                multiplier-lookup
                (perturbation/draw-samples rand-generator simulations (:perturbations config))
                burn-count-matrix)
               (write-csv-outputs
                (:output-csvs? config)
                (str "summary_stats" (:outfile-suffix config) ".csv")))
          (output-burn-probability-layer! config envelope burn-count-matrix))
        (s/explain ::spec/config config)))))
;; command-line-interface ends here
