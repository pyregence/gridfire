(ns gridfire.inputs
  (:require [clojure.data.csv      :as csv]
            [clojure.java.io       :as io]
            [clojure.string        :as str]
            [gridfire.common       :refer [burnable-fuel-model?]]
            [gridfire.conversion   :refer [conversion-table percent->dec]]
            [gridfire.fetch        :as fetch]
            [gridfire.utils.random :refer [draw-samples my-shuffle my-rand-nth]]
            [gridfire.utils.server :refer [throw-message]]
            [tech.v3.tensor        :as t])
  (:import java.util.Random))

(defn add-input-layers
  [config]
  (let [future-or-matrix                     (fn [f] (if (future? f) @f f))
        envelope                             @(fetch/landfire-envelope config :aspect)
        aspect-matrix                        @(fetch/landfire-matrix config :aspect)
        canopy-base-height-matrix            @(fetch/landfire-matrix config :canopy-base-height)
        canopy-cover-matrix                  @(fetch/landfire-matrix config :canopy-cover)
        canopy-height-matrix                 @(fetch/landfire-matrix config :canopy-height)
        crown-bulk-density-matrix            @(fetch/landfire-matrix config :crown-bulk-density)
        elevation-matrix                     @(fetch/landfire-matrix config :elevation)
        fuel-model-matrix                    @(fetch/landfire-matrix config :fuel-model)
        slope-matrix                         @(fetch/landfire-matrix config :slope)
        ignition-matrix                      @(fetch/ignition-matrix config)
        ignition-mask-matrix                 @(fetch/ignition-mask-matrix config)
        temperature-matrix                   @(fetch/weather-matrix config :temperature)
        relative-humidity-matrix             @(fetch/weather-matrix config :relative-humidity)
        wind-speed-20ft-matrix               @(fetch/weather-matrix config :wind-speed-20ft)
        wind-from-direction-matrix           @(fetch/weather-matrix config :wind-from-direction)
        fuel-moisture-dead-1hr-matrix        @(fetch/fuel-moisture-matrix config :dead :1hr)
        fuel-moisture-dead-10hr-matrix       @(fetch/fuel-moisture-matrix config :dead :10hr)
        fuel-moisture-dead-100hr-matrix      @(fetch/fuel-moisture-matrix config :dead :100hr)
        fuel-moisture-live-herbaceous-matrix @(fetch/fuel-moisture-matrix config :live :herbaceous)
        fuel-moisture-live-woody-matrix      @(fetch/fuel-moisture-matrix config :live :woody)]
    (assoc config
           :envelope                             (future-or-matrix envelope)
           :aspect-matrix                        (future-or-matrix aspect-matrix)
           :canopy-base-height-matrix            (future-or-matrix canopy-base-height-matrix)
           :canopy-cover-matrix                  (future-or-matrix canopy-cover-matrix)
           :canopy-height-matrix                 (future-or-matrix canopy-height-matrix)
           :crown-bulk-density-matrix            (future-or-matrix crown-bulk-density-matrix)
           :elevation-matrix                     (future-or-matrix elevation-matrix)
           :fuel-model-matrix                    (future-or-matrix fuel-model-matrix)
           :slope-matrix                         (future-or-matrix slope-matrix)
           :ignition-matrix                      (future-or-matrix ignition-matrix)
           :ignition-mask-matrix                 (future-or-matrix ignition-mask-matrix)
           :temperature-matrix                   (future-or-matrix temperature-matrix)
           :relative-humidity-matrix             (future-or-matrix relative-humidity-matrix)
           :wind-speed-20ft-matrix               (future-or-matrix wind-speed-20ft-matrix)
           :wind-from-direction-matrix           (future-or-matrix wind-from-direction-matrix)
           :fuel-moisture-dead-1hr-matrix        (future-or-matrix fuel-moisture-dead-1hr-matrix)
           :fuel-moisture-dead-10hr-matrix       (future-or-matrix fuel-moisture-dead-10hr-matrix)
           :fuel-moisture-dead-100hr-matrix      (future-or-matrix fuel-moisture-dead-100hr-matrix)
           :fuel-moisture-live-herbaceous-matrix (future-or-matrix fuel-moisture-live-herbaceous-matrix)
           :fuel-moisture-live-woody-matrix      (future-or-matrix fuel-moisture-live-woody-matrix))))

(defn- multi-band? [matrix]
  (> (:n-dims (t/tensor->dimensions matrix)) 2))

;;TODO Document: using higher resolution layers than fuel model will choose upper left corner cell of the layer from the higher resolution grid within each fuel model grid cell. Recommend to use layers at or below resolution of fuel model matrix if you want to avoid loss of information.
(defn calc-multiplier
  [inputs fuel-model-matrix-height matrix-kw]
  (when-let [matrix (get inputs matrix-kw)]
    (let [height-dimension (if (multi-band? matrix) 1 0)
          matrix-height    (-> (t/tensor->dimensions matrix) :shape (get height-dimension))]
      (when (not= matrix-height fuel-model-matrix-height)
        (double (/ matrix-height fuel-model-matrix-height))))))

;;TODO Document fuel-model as the resolution of the computational space. Cell size must also match fuel model.
(defn add-misc-params
  [{:keys [random-seed fuel-model-matrix] :as inputs}]
  (let [[num-rows num-cols] (:shape (t/tensor->dimensions fuel-model-matrix))]
    (assoc inputs
           :num-rows                                       num-rows
           :num-cols                                       num-cols
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

;; FIXME: Try using draw-sample within run-simulation instead of get-weather here.
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
     (vector? ignition-param) (range (first ignition-param) (inc (second ignition-param)))
     (list? ignition-param)   ignition-param
     (number? ignition-param) (list ignition-param)
     :else                    (range 0 num-items))))

(defn- fill-ignition-sites
  [rand-gen ignition-sites simulations]
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
  [{:keys [rand-gen simulations]} ignitable-cell? ignition-rows ignition-cols]
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
  [{:keys [num-rows num-cols]} ignitable-cell? ignition-rows ignition-cols]
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
    [num-rows num-cols ignition-row ignition-col simulations cell-size random-ignition
     rand-gen ignition-matrix ignition-csv config-file-path ignition-mask-matrix
     fuel-model-matrix] :as inputs}]
  (if (or ignition-matrix ignition-csv)
    inputs
    (let [ignitable-cell? (if ignition-mask-matrix
                            (fn [row col]
                              (and (pos? (t/mget ignition-mask-matrix row col))
                                   (burnable-fuel-model? (t/mget fuel-model-matrix row col))))
                            (fn [row col]
                              (burnable-fuel-model? (t/mget fuel-model-matrix row col))))
          buffer-size     (if-let [edge-buffer (:edge-buffer random-ignition)]
                            (int (Math/ceil (/ edge-buffer cell-size)))
                            0)
          ignition-rows   (filter-ignitions ignition-row buffer-size (- num-rows buffer-size 1) num-rows)
          ignition-cols   (filter-ignitions ignition-col buffer-size (- num-cols buffer-size 1) num-cols)
          ignition-sites  (if (= :use-darts (select-ignition-algorithm inputs ignitable-cell? ignition-rows ignition-cols))
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
    (let [num-bands (long (Math/ceil (/ (reduce max max-runtime-samples) output-burn-probability)))]
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
