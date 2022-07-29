(ns gridfire.inputs
  (:require [clojure.data.csv        :as csv]
            [clojure.java.io         :as io]
            [clojure.string          :as str]
            [gridfire.common         :refer [burnable-fuel-model?]]
            [gridfire.conversion     :refer [conversion-table min->ms ms->min percent->dec]]
            [gridfire.fetch          :as fetch]
            [gridfire.postgis-bridge :refer [with-db-connection-pool]]
            [gridfire.utils.random   :refer [draw-samples my-shuffle my-rand-nth]]
            [gridfire.utils.server   :refer [throw-message]]
            [tech.v3.tensor          :as t])
  (:import java.util.Date
           java.util.Random))

(set! *unchecked-math* :warn-on-boxed)

(defn add-input-layers
  [config]
  (with-db-connection-pool (:db-spec config)
    (let [aspect-layer                        (future (fetch/landfire-layer config :aspect))
          canopy-base-height-layer            (future (fetch/landfire-layer config :canopy-base-height))
          canopy-cover-layer                  (future (fetch/landfire-layer config :canopy-cover))
          canopy-height-layer                 (future (fetch/landfire-layer config :canopy-height))
          crown-bulk-density-layer            (future (fetch/landfire-layer config :crown-bulk-density))
          elevation-layer                     (future (fetch/landfire-layer config :elevation))
          fuel-model-layer                    (future (fetch/landfire-layer config :fuel-model)) ; Use its envelope
          slope-layer                         (future (fetch/landfire-layer config :slope))
          ignition-layer                      (future (fetch/ignition-layer config))
          ignition-mask-layer                 (future (fetch/ignition-mask-layer config))
          temperature-layer                   (future (fetch/weather-layer config :temperature))
          relative-humidity-layer             (future (fetch/weather-layer config :relative-humidity))
          wind-speed-20ft-layer               (future (fetch/weather-layer config :wind-speed-20ft))
          wind-from-direction-layer           (future (fetch/weather-layer config :wind-from-direction))
          fuel-moisture-dead-1hr-layer        (future (fetch/fuel-moisture-layer config :dead :1hr))
          fuel-moisture-dead-10hr-layer       (future (fetch/fuel-moisture-layer config :dead :10hr))
          fuel-moisture-dead-100hr-layer      (future (fetch/fuel-moisture-layer config :dead :100hr))
          fuel-moisture-live-herbaceous-layer (future (fetch/fuel-moisture-layer config :live :herbaceous))
          fuel-moisture-live-woody-layer      (future (fetch/fuel-moisture-layer config :live :woody))]
      (assoc config
             :envelope                             (fetch/layer->envelope @fuel-model-layer (:srid config))
             :aspect-matrix                        (:matrix @aspect-layer)
             :canopy-base-height-matrix            (:matrix @canopy-base-height-layer)
             :canopy-cover-matrix                  (:matrix @canopy-cover-layer)
             :canopy-height-matrix                 (:matrix @canopy-height-layer)
             :crown-bulk-density-matrix            (:matrix @crown-bulk-density-layer)
             :elevation-matrix                     (:matrix @elevation-layer)
             :fuel-model-matrix                    (:matrix @fuel-model-layer)
             :slope-matrix                         (:matrix @slope-layer)
             :ignition-matrix                      (:matrix @ignition-layer)
             :ignition-mask-matrix                 (:matrix @ignition-mask-layer)
             :temperature-matrix                   (:matrix @temperature-layer)
             :relative-humidity-matrix             (:matrix @relative-humidity-layer)
             :wind-speed-20ft-matrix               (:matrix @wind-speed-20ft-layer)
             :wind-from-direction-matrix           (:matrix @wind-from-direction-layer)
             :fuel-moisture-dead-1hr-matrix        (:matrix @fuel-moisture-dead-1hr-layer)
             :fuel-moisture-dead-10hr-matrix       (:matrix @fuel-moisture-dead-10hr-layer)
             :fuel-moisture-dead-100hr-matrix      (:matrix @fuel-moisture-dead-100hr-layer)
             :fuel-moisture-live-herbaceous-matrix (:matrix @fuel-moisture-live-herbaceous-layer)
             :fuel-moisture-live-woody-matrix      (:matrix @fuel-moisture-live-woody-layer)))))

(defn- multi-band? [matrix]
  (> ^long (:n-dims (t/tensor->dimensions matrix)) 2))

;; TODO Document: using higher resolution layers than fuel model will choose upper left corner cell of the layer from the higher resolution grid within each fuel model grid cell. Recommend to use layers at or below resolution of fuel model matrix if you want to avoid loss of information.
(defn calc-multiplier
  [inputs ^long fuel-model-matrix-height matrix-kw]
  (when-let [matrix (get inputs matrix-kw)]
    (let [height-dimension    (if (multi-band? matrix) 1 0)
          ^long matrix-height (-> (t/tensor->dimensions matrix) :shape (get height-dimension))]
      (when (not= matrix-height fuel-model-matrix-height)
        (double (/ matrix-height fuel-model-matrix-height))))))

;; TODO Document fuel-model as the resolution of the computational space. Cell size must also match fuel model.
(defn add-misc-params
  [{:keys [max-runtime random-seed fuel-model-matrix] :as inputs}]
  (let [[num-rows num-cols] (:shape (t/tensor->dimensions fuel-model-matrix))]
    (assoc inputs
           :num-rows                                       (long num-rows)
           :num-cols                                       (long num-cols)
           :max-runtime                                    (double max-runtime)
           :rand-gen                                       (if random-seed (Random. random-seed) (Random.))
           :aspect-index-multiplier                        (calc-multiplier inputs num-rows :aspect-matrix)
           :canopy-base-height-index-multiplier            (calc-multiplier inputs num-rows :canopy-base-height-matrix)
           :canopy-cover-index-multiplier                  (calc-multiplier inputs num-rows :canopy-cover-matrix)
           :canopy-height-index-multiplier                 (calc-multiplier inputs num-rows :canopy-height-matrix)
           :crown-bulk-density-index-multiplier            (calc-multiplier inputs num-rows :crown-bulk-density-matrix)
           :elevation-index-multiplier                     (calc-multiplier inputs num-rows :elevation-matrix)
           :slope-index-multiplier                         (calc-multiplier inputs num-rows :slope-matrix)
           :temperature-index-multiplier                   (calc-multiplier inputs num-rows :temperature-matrix)
           :relative-humidity-index-multiplier             (calc-multiplier inputs num-rows :relative-humidity-matrix)
           :wind-speed-20ft-index-multiplier               (calc-multiplier inputs num-rows :wind-speed-20ft-matrix)
           :wind-from-direction-index-multiplier           (calc-multiplier inputs num-rows :wind-from-direction-matrix)
           :fuel-moisture-dead-1hr-index-multiplier        (calc-multiplier inputs num-rows :fuel-moisture-dead-1hr-matrix)
           :fuel-moisture-dead-10hr-index-multiplier       (calc-multiplier inputs num-rows :fuel-moisture-dead-10hr-matrix)
           :fuel-moisture-dead-100hr-index-multiplier      (calc-multiplier inputs num-rows :fuel-moisture-dead-100hr-matrix)
           :fuel-moisture-live-herbaceous-index-multiplier (calc-multiplier inputs num-rows :fuel-moisture-live-herbaceous-matrix)
           :fuel-moisture-live-woody-index-multiplier      (calc-multiplier inputs num-rows :fuel-moisture-live-woody-matrix))))

(defn add-ignition-csv
  [{:keys [ignition-csv] :as inputs}]
  (if ignition-csv
    (let [ignitions (with-open [reader (io/reader ignition-csv)]
                      (doall (rest (csv/read-csv reader))))]
      (assoc inputs
             :ignition-rows        (mapv #(Long/parseLong (get % 0)) ignitions)
             :ignition-cols        (mapv #(Long/parseLong (get % 1)) ignitions)
             :ignition-start-times (mapv #(Double/parseDouble (get % 2)) ignitions)
             :max-runtime-samples  (mapv #(Double/parseDouble (get % 3)) ignitions)
             :simulations          (count ignitions)))
    inputs))

(defn add-sampled-params
  [{:keys
    [rand-gen simulations max-runtime max-runtime-samples foliar-moisture ellipse-adjustment-factor]
    :as inputs}]
  (assoc inputs
         :max-runtime-samples               (or max-runtime-samples (draw-samples rand-gen simulations max-runtime))
         :foliar-moisture-samples           (mapv percent->dec (draw-samples rand-gen simulations foliar-moisture))
         :ellipse-adjustment-factor-samples (draw-samples rand-gen simulations (or ellipse-adjustment-factor 1.0))))

(defn- convert-ranges
  [config]
  (into config
        (map (fn [[layer {:keys [units range] :as spec}]]
               (if-let [converter (get-in conversion-table [layer units])]
                 [layer (assoc spec :range (mapv converter range))]
                 [layer spec])))
        config))

(defn add-perturbation-params
  [{:keys [perturbations] :as inputs}]
  (if perturbations
    (assoc inputs :perturbations (convert-ranges perturbations))
    inputs))

(defn get-weather
  [{:keys [rand-gen simulations] :as inputs} weather-type]
  (let [matrix-kw (-> weather-type
                      name
                      (str "-matrix")
                      keyword)]
    (when (and (inputs weather-type)
               (not (inputs matrix-kw)))
      (draw-samples rand-gen simulations (inputs weather-type)))))

(defn add-weather-params
  [inputs]
  (assoc inputs
         :temperature-samples         (get-weather inputs :temperature)
         :relative-humidity-samples   (get-weather inputs :relative-humidity)
         :wind-speed-20ft-samples     (get-weather inputs :wind-speed-20ft)
         :wind-from-direction-samples (get-weather inputs :wind-from-direction)))

(defn get-fuel-moisture
  [{:keys [fuel-moisture simulations rand-gen] :as inputs} category size]
  (let [matrix-kw (keyword (str/join "-" ["fuel-moisture"
                                          (name category)
                                          (name size)
                                          "matrix"]))]
    (when (and (get-in fuel-moisture [category size])
               (not (inputs matrix-kw)))
      (draw-samples rand-gen simulations (get-in fuel-moisture [category size])))))

(defn add-fuel-moisture-params
  [inputs]
  (assoc inputs
         :fuel-moisture-dead-1hr-samples        (get-fuel-moisture inputs :dead :1hr)
         :fuel-moisture-dead-10hr-samples       (get-fuel-moisture inputs :dead :10hr)
         :fuel-moisture-dead-100hr-samples      (get-fuel-moisture inputs :dead :100hr)
         :fuel-moisture-live-herbaceous-samples (get-fuel-moisture inputs :live :herbaceous)
         :fuel-moisture-live-woody-samples      (get-fuel-moisture inputs :live :woody)))

(defn- filter-ignitions
  [ignition-param buffer-size limit num-items]
  (filterv
   #(<= buffer-size % limit)
   (cond
     (vector? ignition-param) (range (first ignition-param) (inc ^long (second ignition-param)))
     (list? ignition-param)   ignition-param
     (number? ignition-param) (list ignition-param)
     :else                    (range 0 num-items))))

(defn- fill-ignition-sites
  [rand-gen ignition-sites ^long simulations]
  (let [num-sites-available (count ignition-sites)]
    (loop [num-sites-needed     (- simulations num-sites-available)
           final-ignition-sites ignition-sites]
      (if (pos? num-sites-needed)
        (let [num-additional-sites (min num-sites-needed num-sites-available)
              additional-sites     (-> (my-shuffle rand-gen ignition-sites)
                                       (subvec 0 num-additional-sites))]
          (recur (- num-sites-needed num-additional-sites)
                 (into final-ignition-sites additional-sites)))
        final-ignition-sites))))

(defn- sample-ignition-sites-shuffle
  [{:keys [rand-gen ^long simulations]} ignitable-cell? ignition-rows ignition-cols]
  (let [ignitable-sites (my-shuffle rand-gen
                                    (for [row   ignition-rows
                                          col   ignition-cols
                                          :when (ignitable-cell? row col)]
                                      [row col]))]
    (subvec ignitable-sites 0 (min simulations (count ignitable-sites)))))

(defn sample-ignition-sites-darts
  [{:keys [rand-gen simulations]} ignitable-cell? ignition-rows ignition-cols]
  (let [total-cells (* (count ignition-rows) (count ignition-cols))]
    (loop [[i j :as cell]    (vector (my-rand-nth rand-gen ignition-rows) (my-rand-nth rand-gen ignition-cols))
           ignitable-cells   #{}
           unignitable-cells #{}]
      (if (or (= (count ignitable-cells) simulations)
              (= (+ (count ignitable-cells) (count unignitable-cells))
                 total-cells))
        (vec ignitable-cells)
        (if (ignitable-cell? i j)
          (recur (vector (my-rand-nth rand-gen ignition-rows) (my-rand-nth rand-gen ignition-cols))
                 (conj ignitable-cells cell)
                 unignitable-cells)
          (recur (vector (my-rand-nth rand-gen ignition-rows) (my-rand-nth rand-gen ignition-cols))
                 ignitable-cells
                 (conj unignitable-cells cell)))))))

(defn select-ignition-algorithm
  [{:keys [^long num-rows ^long num-cols]} ignitable-cell? ignition-rows ignition-cols]
  (let [ratio-threshold (max 1 (int (* 0.0025 num-rows num-cols)))] ; the inflection point from our benchmarks
    (if (= ratio-threshold
           (reduce +
                   (take ratio-threshold
                         (for [row   ignition-rows
                               col   ignition-cols
                               :when (ignitable-cell? row col)]
                           1))))
      :use-darts
      :use-shuffle)))

(defn add-random-ignition-sites
  [{:keys
    [^long num-rows ^long num-cols ignition-row ignition-col simulations ^double cell-size random-ignition
     rand-gen ignition-matrix ignition-csv config-file-path ignition-mask-matrix
     fuel-model-matrix] :as inputs}]
  (if (or ignition-matrix ignition-csv)
    inputs
    (let [ignitable-cell?   (if ignition-mask-matrix
                              (fn [row col]
                                (and (pos? ^double (t/mget ignition-mask-matrix row col))
                                     (burnable-fuel-model? (t/mget fuel-model-matrix row col))))
                              (fn [row col]
                                (burnable-fuel-model? (t/mget fuel-model-matrix row col))))
          ^long buffer-size (if-let [^double edge-buffer (:edge-buffer random-ignition)]
                              (int (Math/ceil (/ edge-buffer cell-size)))
                              0)
          ignition-rows     (filter-ignitions ignition-row buffer-size (- num-rows buffer-size 1) num-rows)
          ignition-cols     (filter-ignitions ignition-col buffer-size (- num-cols buffer-size 1) num-cols)
          ignition-sites    (if (= :use-darts (select-ignition-algorithm inputs ignitable-cell? ignition-rows ignition-cols))
                              (sample-ignition-sites-darts inputs ignitable-cell? ignition-rows ignition-cols)
                              (sample-ignition-sites-shuffle inputs ignitable-cell? ignition-rows ignition-cols))]
      (if (seq ignition-sites)
        (let [ignition-sites* (fill-ignition-sites rand-gen ignition-sites simulations)]
          (assoc inputs
                 :ignition-rows (mapv first ignition-sites*)
                 :ignition-cols (mapv second ignition-sites*)))
        (throw-message (format "Invalid config file [%s]: No valid ignition sites." config-file-path))))))

(defn initialize-burn-count-matrix
  [output-burn-probability max-runtime-samples ^long num-rows ^long num-cols]
  (if (number? output-burn-probability)
    (let [num-bands (long (Math/ceil (/ ^double (reduce max max-runtime-samples) ^double output-burn-probability)))]
      (t/new-tensor [num-bands num-rows num-cols]))
    (t/new-tensor [num-rows num-cols])))

(defn add-aggregate-matrices
  [{:keys
    [max-runtime-samples num-rows num-cols output-burn-count? output-burn-probability output-flame-length-sum?
     output-flame-length-max? output-spot-count?] :as inputs}]
  (assoc inputs
         :burn-count-matrix       (when (or output-burn-count? output-burn-probability)
                                    (initialize-burn-count-matrix output-burn-probability max-runtime-samples num-rows num-cols))
         :flame-length-sum-matrix (when output-flame-length-sum? (t/new-tensor [num-rows num-cols]))
         :flame-length-max-matrix (when output-flame-length-max? (t/new-tensor [num-rows num-cols]))
         :spot-count-matrix       (when output-spot-count? (t/new-tensor [num-rows num-cols]))))

(defn add-burn-period-params
  [{:keys [burn-period] :as inputs}]
  (let [{:keys [start end]} burn-period]
    (-> inputs
        (assoc :burn-period-start (or start "00:00"))
        (assoc :burn-period-end   (or end   "24:00")))))

(defn add-ignition-start-times
  [{:keys [ignition-start-times ignition-start-timestamp weather-start-timestamp simulations] :as inputs}]
  (if (and (nil? ignition-start-times) ignition-start-timestamp weather-start-timestamp)
    (let [ignition-start-time (ms->min (- (double (inst-ms ignition-start-timestamp))
                                          (double (inst-ms weather-start-timestamp))))]
      (assoc inputs :ignition-start-times (vec (repeat simulations ignition-start-time))))
    inputs))

(defn add-ignition-start-timestamps
  [{:keys [ignition-start-times simulations ignition-start-timestamp weather-start-timestamp] :as inputs}]
  (let [weather-start-ms                 (min->ms weather-start-timestamp)
        compute-ignition-start-timestamp (fn [ignition-start-time]
                                           (Date. (+ weather-start-ms (min->ms ignition-start-time))))
        ignition-start-timestamps        (cond
                                           ignition-start-timestamp (vec (repeat simulations ignition-start-timestamp))
                                           ignition-start-times     (mapv compute-ignition-start-timestamp ignition-start-times)
                                           :else                    (vec (repeat simulations #inst "1970-01-01T00-00:00")))] ; adding no-op value for required parameter
    (-> inputs
        (assoc :ignition-start-timestamps ignition-start-timestamps)
        (dissoc :ignition-start-timestamp))))
