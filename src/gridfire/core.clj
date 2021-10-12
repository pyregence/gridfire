;; [[file:../../org/GridFire.org::gridfire-core][gridfire-core]]
(ns gridfire.core
  (:gen-class)
  (:require [clojure.core.matrix      :as m]
            [clojure.core.reducers    :as r]
            [clojure.data.csv         :as csv]
            [clojure.java.io          :as io]
            [clojure.spec.alpha       :as s]
            [clojure.string           :as str]
            [gridfire.binary-output   :as binary]
            [gridfire.common          :refer [calc-emc get-neighbors in-bounds?]]
            [gridfire.crown-fire      :refer [m->ft]]
            [gridfire.fetch           :as fetch]
            [gridfire.fire-spread     :refer [rothermel-fast-wrapper run-fire-spread]]
            [gridfire.perturbation    :as perturbation]
            [gridfire.random-ignition :as random-ignition]
            [gridfire.spec.config     :as spec]
            [gridfire.utils.random    :refer [draw-samples]]
            [magellan.core            :refer [make-envelope
                                              matrix-to-raster
                                              register-new-crs-definitions-from-properties-file!
                                              write-raster]]
            [matrix-viz.core          :refer [save-matrix-as-png]]
            [taoensso.tufte           :as tufte]
            [triangulum.logging       :refer [log log-str]])
  (:import java.util.Random))

(m/set-current-implementation :vectorz)

(register-new-crs-definitions-from-properties-file! "CUSTOM"
                                                    (io/resource "custom_projections.properties"))

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
        crown-fire-size            (cells-to-acres cell-size (:crown-fire-count fire-spread-results))
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
     :crown-fire-size            crown-fire-size
     :flame-length-mean          flame-length-mean
     :flame-length-stddev        flame-length-stddev
     :fire-line-intensity-mean   fire-line-intensity-mean
     :fire-line-intensity-stddev fire-line-intensity-stddev
     :spot-count                 (:spot-count fire-spread-results)}))

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

(defn snake->kebab [s]
  (str/replace s #"_" "-"))

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

(defn layer-snapshot [burn-time-matrix layer-matrix t]
  (m/emap (fn [layer-value burn-time]
            (if (<= burn-time t)
              layer-value
              0))
          layer-matrix
          burn-time-matrix))

(defn output-filename [name outfile-suffix simulation-id output-time ext]
  (as-> [name outfile-suffix simulation-id (when output-time (str "t" output-time))] $
    (remove str/blank? $)
    (str/join "_" $)
    (str $ ext)))

(defn output-geotiff
  ([config matrix name envelope]
   (output-geotiff config matrix name envelope nil nil))

  ([config matrix name envelope simulation-id]
   (output-geotiff config matrix name envelope simulation-id nil))

  ([{:keys [output-directory outfile-suffix] :as config}
    matrix name envelope simulation-id output-time]
   (let [file-name (output-filename name
                                    outfile-suffix
                                    (str simulation-id)
                                    output-time
                                    ".tif")]
     (-> (matrix-to-raster name matrix envelope)
         (write-raster (if output-directory
                         (str/join "/" [output-directory file-name])
                         file-name))))))

(defn output-png
  ([config matrix name envelope]
   (output-png config matrix name envelope nil nil))

  ([config matrix name envelope simulation-id]
   (output-png config matrix name envelope simulation-id nil))

  ([{:keys [output-directory outfile-suffix]}
    matrix name envelope simulation-id output-time]
   (let [file-name (output-filename name
                                    outfile-suffix
                                    (str simulation-id)
                                    output-time
                                    ".png")]
     (save-matrix-as-png :color 4 -1.0
                         matrix
                         (if output-directory
                           (str/join "/" [output-directory file-name])
                           (file-name))))))

(def layer-name->matrix
  [["fire_spread"         :fire-spread-matrix]
   ["flame_length"        :flame-length-matrix]
   ["fire_line_intensity" :fire-line-intensity-matrix]
   ["burn_history"        :burn-time-matrix]
   ["spread_rate"         :spread-rate-matrix]
   ["fire_type"           :fire-type-matrix]])

(defn filter-output-layers [output-layers]
  (let [layers-to-filter (set (map (comp kebab->snake name) (keys output-layers)))]
    (filter (fn [[name _]] (contains? layers-to-filter name)) layer-name->matrix)))

(defn process-output-layers-timestepped
  [{:keys [simulation-id] :as config}
   {:keys [global-clock burn-time-matrix] :as fire-spread-results}
   name layer timestep envelope]
  (doseq [output-time (range 0 (inc global-clock) timestep)]
    (let [matrix          (if (= layer "burn_history")
                            (to-color-map-values layer output-time)
                            (fire-spread-results layer))
          filtered-matrix (layer-snapshot burn-time-matrix matrix output-time)]
      (output-geotiff config filtered-matrix name envelope simulation-id output-time)
      (output-png config filtered-matrix name envelope simulation-id output-time))))

(defn process-output-layers!
  [{:keys [output-layers ouput-geotiffs? output-pngs?] :as config}
   {:keys [global-clock burn-time-matrix] :as fire-spread-results}
   envelope
   simulation-id]
  (let [layers (if output-layers
                 (filter-output-layers output-layers)
                 layer-name->matrix)]
    (doseq [[name layer] layers]
      (let [kw       (keyword (snake->kebab name))
            timestep (get output-layers kw)]
        (if (int? timestep)
          (process-output-layers-timestepped config
                                             fire-spread-results
                                             name
                                             layer
                                             timestep
                                             envelope)
          (let [matrix (if (= layer "burn_history")
                         (to-color-map-values layer global-clock)
                         (fire-spread-results layer))]
            (when ouput-geotiffs?
             (output-geotiff config matrix name envelope simulation-id))
            (when output-pngs?
             (output-png config matrix name envelope simulation-id))))))))

(defn process-burn-count!
  [{:keys [fire-spread-matrix burn-time-matrix global-clock]}
   burn-count-matrix
   timestep]
  (if (int? timestep)
    (doseq [clock (range 0 (inc global-clock) timestep)]
      (let [filtered-fire-spread (m/emap (fn [layer-value burn-time]
                                           (if (<= burn-time clock)
                                             layer-value
                                             0))
                                         fire-spread-matrix
                                         burn-time-matrix)
            band                 (int (quot clock timestep))]
        (m/add! (nth (seq burn-count-matrix) band) filtered-fire-spread)))
    (m/add! burn-count-matrix fire-spread-matrix)))

(defn process-aggregate-output-layers!
  [{:keys [output-burn-probability burn-count-matrix flame-length-sum-matrix
           flame-length-max-matrix]} fire-spread-results]
  (when-let [timestep output-burn-probability]
    (process-burn-count! fire-spread-results burn-count-matrix timestep))
  (when flame-length-sum-matrix
    (m/add! flame-length-sum-matrix (:flame-length-matrix fire-spread-results)))
  (when flame-length-max-matrix
    (m/emap! #(max %1 %2) flame-length-max-matrix (:flame-length-matrix fire-spread-results))))

(defn initialize-burn-count-matrix
  [{:keys [output-burn-probability output-burn-count max-runtimes num-rows num-cols]}]
  (when (or output-burn-count output-burn-probability)
    (if (int? output-burn-probability)
      (let [num-bands (inc (quot (apply max max-runtimes) output-burn-probability))]
        (m/zero-array [num-bands num-rows num-cols]))
      (m/zero-array [num-rows num-cols]))))

(defn process-binary-output!
  [{:keys [output-binary? output-directory]}
   {:keys [burn-time-matrix flame-length-matrix spread-rate-matrix fire-type-matrix]}
   simulation]
  (when output-binary?
    (let [output-name (format "toa_0001_%05d.bin" (inc simulation))
          output-path (if output-directory
                        (.getPath (io/file output-directory output-name))
                        output-name)]
      (binary/write-matrices-as-binary output-path
                                       [:float :float :float :int]
                                       [(m/emap #(if (pos? %) (* 60 %) %) burn-time-matrix)
                                        flame-length-matrix
                                        spread-rate-matrix
                                        fire-type-matrix]))))

(defn get-envelope
  [config landfire-layers]
  (let [{:keys [upperleftx upperlefty width height scalex scaley]} (landfire-layers :elevation)]
    (make-envelope (:srid config)
                   upperleftx
                   (+ upperlefty (* height scaley))
                   (* width scalex)
                   (* -1.0 height scaley))))

(defn cell-size-multiplier
  [cell-size {:keys [scalex]}]
  (int (quot (m->ft scalex) cell-size)))  ; FIXME: Should we be using /?

;; FIXME: This would be simpler is we flattened fuel-moisture-layers into a single-level map
(defn create-multiplier-lookup
  [{:keys [cell-size weather-layers fuel-moisture-layers]}]
  (let [layers (merge weather-layers fuel-moisture-layers)]
    (reduce (fn [acc ks] (let [layer (get-in layers ks)]
                           (if (map? layer)
                             (assoc-in acc ks (cell-size-multiplier cell-size layer))
                             acc)))
            {}
            [[:temperature]
             [:relative-humidity]
             [:wind-speed-20ft]
             [:wind-from-direction]
             [:dead :1hr]
             [:dead :10hr]
             [:dead :100hr]
             [:live :herbaceous]
             [:live :woody]])))

(defn get-weather [config rand-generator weather-type weather-layers]
  (if (contains? weather-layers weather-type)
    (weather-type weather-layers)
    (draw-samples rand-generator (:simulations config) (config weather-type))))

;; FIXME: Rename :landfire-rasters to :landfire-matrices everywhere (do we have to pass this parameter around?)
(defn add-input-layers
  [config]
  (let [landfire-layers (fetch/landfire-layers config)]
    (assoc config
           :envelope             (get-envelope config landfire-layers)
           :landfire-rasters    (into {}
                                      (map (fn [[layer-name layer-info]] [layer-name (first (:matrix layer-info))]))
                                      landfire-layers)
           :ignition-layer       (fetch/ignition-layer config)
           :ignition-mask-layer  (fetch/ignition-mask-layer config)
           :weather-layers       (fetch/weather-layers config)
           :fuel-moisture-layers (fetch/fuel-moisture-layers config))))

(defn add-misc-params
  [{:keys [random-seed landfire-rasters] :as inputs}]
  (assoc inputs
         :num-rows          (m/row-count (:fuel-model landfire-rasters))
         :num-cols          (m/column-count (:fuel-model landfire-rasters))
         :multiplier-lookup (create-multiplier-lookup inputs)
         :rand-gen          (if random-seed
                              (Random. random-seed)
                              (Random.))))

(defn add-ignitions-csv
  [{:keys [ignitions-csv] :as inputs}]
  (if ignitions-csv
    (let [ignitions (with-open [reader (io/reader ignitions-csv)]
                      (doall (rest (csv/read-csv reader))))]
      (assoc inputs
             :ignition-rows        (mapv #(Integer/parseInt (get % 0)) ignitions)
             :ignition-cols        (mapv #(Integer/parseInt (get % 1)) ignitions)
             :ignition-start-times (mapv #(Double/parseDouble (get % 2)) ignitions)
             :max-runtimes         (mapv #(Double/parseDouble (get % 3)) ignitions)
             :simulations          (count ignitions)))
    inputs))

;; FIXME: Try using draw-sample within run-simulation instead of draw-samples here.
(defn add-sampled-params
  [{:keys [rand-gen simulations max-runtime ignition-row ignition-col
           foliar-moisture ellipse-adjustment-factor perturbations ignition-rows
           ignition-cols max-runtimes]
    :as inputs}]
  (assoc inputs
         :max-runtimes               (or max-runtimes (draw-samples rand-gen simulations max-runtime))
         :ignition-rows              (or ignition-rows (draw-samples rand-gen simulations ignition-row))
         :ignition-cols              (or ignition-cols (draw-samples rand-gen simulations ignition-col))
         :foliar-moistures           (draw-samples rand-gen simulations foliar-moisture)
         :ellipse-adjustment-factors (draw-samples rand-gen simulations ellipse-adjustment-factor)
         :perturbations              (perturbation/draw-samples rand-gen simulations perturbations)));FIXME: shadowed

;; FIXME: Try using draw-sample within run-simulation instead of get-weather here.
(defn add-weather-params
  [{:keys [rand-gen weather-layers] :as inputs}]
  (assoc inputs
         :temperatures         (get-weather inputs rand-gen :temperature weather-layers)
         :relative-humidities  (get-weather inputs rand-gen :relative-humidity weather-layers)
         :wind-speeds-20ft     (get-weather inputs rand-gen :wind-speed-20ft weather-layers)
         :wind-from-directions (get-weather inputs rand-gen :wind-from-direction weather-layers)))

(defn add-ignitable-sites
  [{:keys [ignition-mask-layer num-rows num-cols] :as inputs}]
  (let [ignition-mask-indices (some->> ignition-mask-layer
                                       :matrix
                                       first
                                       m/non-zero-indices
                                       (map-indexed (fn [i v] (when (pos? (count v)) [i v])))
                                       (filterv identity))
        ignitable-sites (if ignition-mask-indices
                          (for [[row cols] ignition-mask-indices
                                col        cols
                                :when      (random-ignition/valid-ignition-site? inputs row col)]
                            [row col])
                          (for [row   (range num-rows)
                                col   (range num-cols)
                                :when (random-ignition/valid-ignition-site? inputs row col)]
                            [row col]))]
    (assoc inputs :ignitable-sites ignitable-sites)))

(defn initialize-aggregate-matrices
  [{:keys [num-rows num-cols output-flame-length-sum
           output-flame-length-max] :as inputs}]
  {:burn-count-matrix       (initialize-burn-count-matrix inputs)
   :flame-length-sum-matrix (when output-flame-length-sum (m/zero-array [num-rows num-cols]))
   :flame-length-max-matrix (when output-flame-length-max (m/zero-array [num-rows num-cols]))})

(defn add-aggregate-matrices
  [inputs]
  (merge inputs (initialize-aggregate-matrices inputs)))

(defn load-inputs
  [config]
  (-> config
      (add-input-layers)
      (add-misc-params)
      (add-ignitions-csv)
      (add-sampled-params)
      (add-weather-params)
      (add-ignitable-sites)
      (add-aggregate-matrices)))

;; FIXME: Replace input-variations expression with add-sampled-params
;;        and add-weather-params (and remove them from load-inputs).
;;        This will require making these function return single
;;        samples instead of sequences of samples. Also combine the
;;        initial-ignition-site calculation into input-variations or
;;        move it to run-fire-spread.
(defn run-simulation!
  [{:keys [output-csvs? envelope ignition-layer cell-size
           max-runtimes ignition-rows ignition-cols foliar-moistures ellipse-adjustment-factors perturbations
           temperatures relative-humidities wind-speeds-20ft wind-from-directions
           random-seed ignition-start-times] :as inputs}
   i]
  (tufte/profile
   {:id :run-simulation}
   (let [matrix-or-i           (fn [obj i] (:matrix obj (obj i)))
         initial-ignition-site (or ignition-layer
                                   (when (and (ignition-rows i) (ignition-cols i))
                                     [(ignition-rows i) (ignition-cols i)]))
         input-variations      {:rand-gen                  (if random-seed (Random. (+ random-seed i)) (Random.))
                                :max-runtime               (max-runtimes i)
                                :foliar-moisture           (* 0.01 (foliar-moistures i))
                                :ellipse-adjustment-factor (ellipse-adjustment-factors i)
                                :perturbations             (when perturbations (perturbations i))
                                :temperature               (matrix-or-i temperatures i)
                                :relative-humidity         (matrix-or-i relative-humidities i)
                                :wind-speed-20ft           (matrix-or-i wind-speeds-20ft i)
                                :wind-from-direction       (matrix-or-i wind-from-directions i)
                                :ignition-start-time       (get ignition-start-times i 0.0)}
         fire-spread-results   (tufte/p :run-fire-spread
                                        (run-fire-spread
                                         (merge inputs
                                                input-variations
                                                {:initial-ignition-site initial-ignition-site})))]
     (when fire-spread-results
       (process-output-layers! inputs fire-spread-results envelope i)
       (process-aggregate-output-layers! inputs fire-spread-results)
       (process-binary-output! inputs fire-spread-results i))
     (when output-csvs?
       (merge
        input-variations
        {:simulation       (inc i)
         :ignition-row     (ignition-rows i)
         :ignition-col     (ignition-cols i)
         :foliar-moisture  (foliar-moistures i)
         :global-clock     (:global-clock fire-spread-results)
         :exit-condition   (:exit-condition fire-spread-results :no-fire-spread)
         :crown-fire-count (:crown-fire-count fire-spread-results)}
        (if fire-spread-results
          (tufte/p
           :summarize-fire-spread-results
           (summarize-fire-spread-results fire-spread-results cell-size))
          {:fire-size                  0.0
           :flame-length-mean          0.0
           :flame-length-stddev        0.0
           :fire-line-intensity-mean   0.0
           :fire-line-intensity-stddev 0.0}))))))

(defn run-simulations!
  [{:keys [simulations parallel-strategy] :as inputs}]
  (log-str "Running simulations")
  (let [stats-accumulator       (do
                                  (tufte/remove-handler! :accumulating)
                                  (tufte/add-accumulating-handler! {:handler-id :accumulating}))
        parallel-bin-size       (max 1 (quot simulations (.availableProcessors (Runtime/getRuntime))))
        reducer-fn              (if (= parallel-strategy :between-fires)
                                  #(into [] (r/fold parallel-bin-size r/cat r/append! %))
                                  #(into [] %))
        summary-stats           (with-redefs [rothermel-fast-wrapper (memoize rothermel-fast-wrapper)]
                                  (->> (vec (range simulations))
                                       (r/map (partial run-simulation! inputs))
                                       (r/remove nil?)
                                       (reducer-fn)))]
    (Thread/sleep 1000)
    (log (tufte/format-grouped-pstats @stats-accumulator
                                      {:format-pstats-opts {:columns [:n-calls :min :max :mean :mad :clock :total]}})
         :truncate? false)
    {:burn-count-matrix       (:burn-count-matrix inputs)
     :flame-length-sum-matrix (:flame-length-sum-matrix inputs)
     :flame-length-max-matrix (:flame-length-max-matrix inputs)
     :summary-stats           summary-stats}))

;;-----------------------------------------------------------------------------
;; Outputs ;TODO move section to it's own ns
;;-----------------------------------------------------------------------------

(defn write-landfire-layers!
  [{:keys [output-landfire-inputs? outfile-suffix landfire-rasters envelope]}]
  (when output-landfire-inputs?
    (doseq [[layer matrix] landfire-rasters]
      (-> (matrix-to-raster (name layer) matrix envelope)
          (write-raster (str (name layer) outfile-suffix ".tif"))))))

(defn write-burn-probability-layer!
  [{:keys [output-burn-probability simulations envelope ouptut-png?] :as inputs} {:keys [burn-count-matrix]}]
  (when-let [timestep output-burn-probability]
    (let [output-name "burn_probability"]
      (if (int? timestep)
        (doseq [[band matrix] (map-indexed vector burn-count-matrix)]
          (let [output-time        (* band timestep)
                probability-matrix (m/emap #(/ % simulations) matrix)]
            (output-geotiff inputs probability-matrix output-name envelope nil output-time)
            (output-png inputs probability-matrix output-name envelope nil output-time)))
        (let [probability-matrix (m/emap #(/ % simulations) burn-count-matrix)]
          (output-geotiff inputs probability-matrix output-name envelope)
          (when ouptut-png?
           (output-png inputs probability-matrix output-name envelope)))))))

(defn write-flame-length-sum-layer!
  [{:keys [envelope output-flame-length-sum] :as inputs}
   {:keys [flame-length-sum-matrix]}]
  (when output-flame-length-sum
    (output-geotiff inputs flame-length-sum-matrix "flame_length_sum" envelope)))

(defn write-flame-length-max-layer!
  [{:keys [envelope output-flame-length-max] :as inputs}
   {:keys [flame-length-max-matrix]}]
  (when output-flame-length-max
    (output-geotiff inputs flame-length-max-matrix "flame_length_max" envelope)))

(defn write-burn-count-layer!
  [{:keys [envelope output-burn-count] :as inputs}
   {:keys [burn-count-matrix]}]
  (when output-burn-count
    (output-geotiff inputs burn-count-matrix "burn_count" envelope)))

(defn write-aggregate-layers!
  [inputs outputs]
  (write-burn-probability-layer! inputs outputs)
  (write-flame-length-sum-layer! inputs outputs)
  (write-flame-length-max-layer! inputs outputs)
  (write-burn-count-layer! inputs outputs))

(defn write-csv-outputs!
  [{:keys [output-csvs? output-directory outfile-suffix]} {:keys [summary-stats]}]
  (when output-csvs?
    (let [output-filename (str "summary_stats" outfile-suffix ".csv")]
      (with-open [out-file (io/writer (if output-directory
                                        (str/join "/" [output-directory output-filename])
                                        output-filename))]
        (->> summary-stats
             (sort-by #(vector (:ignition-row %) (:ignition-col %)))
             (mapv (fn [{:keys [ignition-row ignition-col max-runtime temperature relative-humidity
                                wind-speed-20ft wind-from-direction foliar-moisture ellipse-adjustment-factor
                                fire-size flame-length-mean flame-length-stddev fire-line-intensity-mean
                                fire-line-intensity-stddev simulation crown-fire-size spot-count]}]
                     [simulation
                      ignition-row
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
                      fire-line-intensity-stddev
                      crown-fire-size
                      spot-count]))
             (cons ["simulation" "ignition-row" "ignition-col" "max-runtime" "temperature" "relative-humidity"
                    "wind-speed-20ft" "wind-from-direction" "foliar-moisture" "ellipse-adjustment-factor"
                    "fire-size" "flame-length-mean" "flame-length-stddev" "fire-line-intensity-mean"
                    "fire-line-intensity-stddev" "crown-fire-size" "spot-count"])
             (csv/write-csv out-file))))))

(defn process-config-file!
  [config]
  (if-not (s/valid? ::spec/config config)
    (s/explain ::spec/config config)
    (let [inputs (load-inputs config)]
      (if (seq (:ignitable-sites inputs))
        (let [outputs (run-simulations! inputs)]
          (write-landfire-layers! inputs)
          (write-aggregate-layers! inputs outputs)
          (write-csv-outputs! inputs outputs))
        (log-str "Could not run simulation. No valid ignition sites")))))
;; gridfire-core ends here
