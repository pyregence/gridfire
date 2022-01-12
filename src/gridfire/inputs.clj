(ns gridfire.inputs
  (:require [clojure.core.matrix      :as m]
            [clojure.data.csv         :as csv]
            [clojure.java.io          :as io]
            [gridfire.common          :refer [burnable-fuel-model?]]
            [gridfire.conversion      :refer [m->ft]]
            [gridfire.fetch           :as fetch]
            [gridfire.perturbation    :as perturbation]
            [gridfire.utils.random    :refer [draw-samples my-shuffle]]
            [clojure.string :as str])
  (:import java.util.Random))

(m/set-current-implementation :vectorz)

(defn add-input-layers
  [config]
  (assoc config
         :envelope                             (fetch/landfire-envelope config :aspect)
         :aspect-matrix                        (fetch/landfire-matrix config :aspect)
         :canopy-base-height-matrix            (fetch/landfire-matrix config :canopy-base-height)
         :canopy-cover-matrix                  (fetch/landfire-matrix config :canopy-cover)
         :canopy-height-matrix                 (fetch/landfire-matrix config :canopy-height)
         :crown-bulk-density-matrix            (fetch/landfire-matrix config :crown-bulk-density)
         :elevation-matrix                     (fetch/landfire-matrix config :elevation)
         :fuel-model-matrix                    (fetch/landfire-matrix config :fuel-model)
         :slope-matrix                         (fetch/landfire-matrix config :slope)
         :ignition-matrix                      (fetch/ignition-matrix config)
         :ignition-mask-matrix                 (fetch/ignition-mask-matrix config)
         :temperature-matrix                   (fetch/weather-matrix config :temperature)
         :relative-humidity-matrix             (fetch/weather-matrix config :relative-humidity)
         :wind-speed-20ft-matrix               (fetch/weather-matrix config :wind-speed-20ft)
         :wind-from-direction-matrix           (fetch/weather-matrix config :wind-from-direction)
         :fuel-moisture-1hr-matrix             (fetch/fuel-moisture-matrix config :dead :1hr)
         :fuel-moisture-10hr-matrix            (fetch/fuel-moisture-matrix config :dead :10hr)
         :fuel-moisture-100hr-matrix           (fetch/fuel-moisture-matrix config :dead :100hr)
         :fuel-moisture-live-herbaceous-matrix (fetch/fuel-moisture-matrix config :live :herbaceous)
         :fuel-moisture-live-woody-matrix      (fetch/fuel-moisture-matrix config :live :woody)))

(defn cell-size-multiplier
  ^double [^double cell-size {:keys [^double scalex]}]
  (/ (m->ft scalex) cell-size)) ; FIXME: scalex isn't in meters for all SRIDs

(defn create-multiplier-lookup
  [{:keys [cell-size weather-layers fuel-moisture]}]
  (let [layers (merge weather-layers fuel-moisture)]
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

(defn add-misc-params
  [{:keys [random-seed fuel-model-matrix] :as inputs}]
  (assoc inputs
         :num-rows          (m/row-count fuel-model-matrix)
         :num-cols          (m/column-count fuel-model-matrix)
         :multiplier-lookup (create-multiplier-lookup inputs)
         :rand-gen          (if random-seed
                              (Random. random-seed)
                              (Random.))))

(defn add-ignition-csv
  [{:keys [ignition-csv] :as inputs}]
  (if ignition-csv
    (let [ignitions (with-open [reader (io/reader ignition-csv)]
                      (doall (rest (csv/read-csv reader))))]
      (assoc inputs
             :ignition-rows        (mapv #(Long/parseLong (get % 0)) ignitions)
             :ignition-cols        (mapv #(Long/parseLong (get % 1)) ignitions)
             :ignition-start-times (mapv #(Double/parseDouble (get % 2)) ignitions)
             :max-runtimes         (mapv #(Double/parseDouble (get % 3)) ignitions)
             :simulations          (count ignitions)))
    inputs))

(defn add-sampled-params
  [{:keys
    [rand-gen simulations max-runtime max-runtimes foliar-moisture ellipse-adjustment-factor perturbations]
    :as inputs}]
  (assoc inputs
         :max-runtimes               (or max-runtimes (draw-samples rand-gen simulations max-runtime))
         :foliar-moistures           (draw-samples rand-gen simulations foliar-moisture)
         :ellipse-adjustment-factors (draw-samples rand-gen simulations (or ellipse-adjustment-factor 1.0))
         :perturbations              (perturbation/draw-samples rand-gen simulations perturbations)))

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
    (when (and fuel-moisture
               (not (inputs matrix-kw)))
      (draw-samples rand-gen simulations (get-in fuel-moisture [category size])))))

(defn add-fuel-moisture-params
  [inputs]
  (assoc inputs
         :fuel-moisture-dead-1hr-samples        (get-fuel-moisture inputs :dead :1hr)
         :fuel-moisture-dead-10hr-samples       (get-fuel-moisture inputs :dead :10hr)
         :fuel-moisture-dead-100hrsamples       (get-fuel-moisture inputs :dead :100hr)
         :fuel-moisture-live-herbaceous-samples (get-fuel-moisture inputs :live :herbaceous)
         :fuel-moisture-live-woodysamples       (get-fuel-moisture inputs :live :woody)))

(defn- filter-ignitions
  [ignition-param buffer-size limit num-items]
  (filter
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

(defn- sample-ignition-sites
  [{:keys [rand-gen fuel-model-matrix ignition-mask-matrix simulations]}
   ignition-rows
   ignition-cols]
  (let [ignitable-cell? (if ignition-mask-matrix
                          (fn [row col]
                            (and (pos? (m/mget ignition-mask-matrix row col))
                                 (burnable-fuel-model? (m/mget fuel-model-matrix row col))))
                          (fn [row col]
                            (burnable-fuel-model? (m/mget fuel-model-matrix row col))))
        ignitable-sites (my-shuffle rand-gen
                                    (for [row   ignition-rows
                                          col   ignition-cols
                                          :when (ignitable-cell? row col)]
                                      [row col]))]
    (subvec ignitable-sites 0 (min simulations (count ignitable-sites)))))

(defn add-random-ignition-sites
  [{:keys
    [num-rows num-cols ignition-row ignition-col simulations cell-size random-ignition
     rand-gen ignition-matrix ignition-csv] :as inputs}]
  (if (or ignition-matrix ignition-csv)
    inputs
    (let [buffer-size    (if-let [edge-buffer (:edge-buffer random-ignition)]
                           (int (Math/ceil (/ edge-buffer cell-size)))
                           0)
          ignition-rows  (filter-ignitions ignition-row buffer-size (- num-rows buffer-size 1) num-rows)
          ignition-cols  (filter-ignitions ignition-col buffer-size (- num-cols buffer-size 1) num-cols)
          ignition-sites (sample-ignition-sites inputs ignition-rows ignition-cols)]
      (if (seq ignition-sites)
        (let [ignition-sites* (fill-ignition-sites rand-gen ignition-sites simulations)]
          (assoc inputs
                 :ignition-rows (mapv first ignition-sites*)
                 :ignition-cols (mapv second ignition-sites*)))
        inputs))))

(defn initialize-burn-count-matrix
  [output-burn-probability max-runtimes ^long num-rows ^long num-cols]
  (if (number? output-burn-probability)
    (let [num-bands (long (Math/ceil (/ (reduce max max-runtimes) output-burn-probability)))]
      (m/zero-array [num-bands num-rows num-cols]))
    (m/zero-array [num-rows num-cols])))

(defn add-aggregate-matrices
  [{:keys
    [max-runtimes num-rows num-cols output-burn-count? output-burn-probability output-flame-length-sum?
     output-flame-length-max? output-spot-count?] :as inputs}]
  (assoc inputs
         :burn-count-matrix       (when (or output-burn-count? output-burn-probability)
                                    (initialize-burn-count-matrix output-burn-probability max-runtimes num-rows num-cols))
         :flame-length-sum-matrix (when output-flame-length-sum? (m/zero-array [num-rows num-cols]))
         :flame-length-max-matrix (when output-flame-length-max? (m/zero-array [num-rows num-cols]))
         :spot-count-matrix       (when output-spot-count? (m/zero-array [num-rows num-cols]))))
