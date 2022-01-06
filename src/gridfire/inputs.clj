(ns gridfire.inputs
  (:require [clojure.core.matrix      :as m]
            [clojure.data.csv         :as csv]
            [clojure.java.io          :as io]
            [gridfire.conversion      :refer [m->ft]]
            [gridfire.fetch           :as fetch]
            [gridfire.perturbation    :as perturbation]
            [gridfire.random-ignition :as random-ignition]
            [gridfire.utils.random    :refer [draw-samples]]
            [clojure.string :as str])
  (:import java.util.Random))

(m/set-current-implementation :vectorz)

(set! *unchecked-math* :warn-on-boxed)

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
  [{:keys [rand-gen simulations max-runtime ignition-row ignition-col
           foliar-moisture ellipse-adjustment-factor perturbations ignition-rows
           ignition-cols max-runtimes]
    :as   inputs}]
  (assoc inputs
         :max-runtimes               (or max-runtimes (draw-samples rand-gen simulations max-runtime))
         :ignition-rows              (or ignition-rows (draw-samples rand-gen simulations ignition-row))
         :ignition-cols              (or ignition-cols (draw-samples rand-gen simulations ignition-col))
         :foliar-moistures           (draw-samples rand-gen simulations foliar-moisture)
         :ellipse-adjustment-factors (draw-samples rand-gen simulations ellipse-adjustment-factor)
         :perturbations              (perturbation/draw-samples rand-gen simulations perturbations)))

(defn get-weather
  [inputs rand-generator weather-type]
  (let [matrix-kw (-> weather-type
                      name
                      (str "-matrix")
                      keyword)]
    (if (contains? inputs matrix-kw)
      (matrix-kw inputs)
      (draw-samples rand-generator (:simulations inputs) (inputs weather-type)))))

;; FIXME: Try using draw-sample within run-simulation instead of get-weather here.
(defn add-weather-params
  [{:keys [rand-gen] :as inputs}]
  (assoc inputs
         :temperatures         (get-weather inputs rand-gen :temperature)
         :relative-humidities  (get-weather inputs rand-gen :relative-humidity)
         :wind-speeds-20ft     (get-weather inputs rand-gen :wind-speed-20ft)
         :wind-from-directions (get-weather inputs rand-gen :wind-from-direction)))

(defn get-fuel-moisture
  [{:keys [fuel-moisture] :as inputs} rand-generator category size]
  (let [matrix-kw (keyword (str/join "-" ["fuel-moisture"
                                          (name category)
                                          (name size)
                                          "matrix"]))]
    (if (matrix-kw inputs)
      (matrix-kw inputs)
      (draw-samples rand-generator (:simulations inputs) (get-in fuel-moisture [category size])))))

(defn add-fuel-moisture-params
  [{:keys [rand-gen] :as inputs}]
  (assoc inputs
         :fuel-moistures-dead-1hr        (get-fuel-moisture inputs rand-gen :dead :1hr)
         :fuel-moistures-dead-10hr       (get-fuel-moisture inputs rand-gen :dead :10hr)
         :fuel-moistures-dead-100hr      (get-fuel-moisture inputs rand-gen :dead :100hr)
         :fuel-moistures-live-herbaceous (get-fuel-moisture inputs rand-gen :live :herbaceous)
         :fuel-moistures-live-woody      (get-fuel-moisture inputs rand-gen :live :woody)))

(defn add-ignitable-sites
  [{:keys [ignition-mask-matrix num-rows num-cols] :as inputs}]
  (let [ignition-mask-indices (some->> ignition-mask-matrix
                                       m/non-zero-indices
                                       (map-indexed (fn [i v] (when (pos? (count v)) [i v])))
                                       (filterv identity))
        ignitable-sites       (if ignition-mask-indices
                                (for [[row cols] ignition-mask-indices
                                      col        cols
                                      :when      (random-ignition/valid-ignition-site? inputs row col)]
                                  [row col])
                                (for [row   (range num-rows)
                                      col   (range num-cols)
                                      :when (random-ignition/valid-ignition-site? inputs row col)]
                                  [row col]))]
    (assoc inputs :ignitable-sites ignitable-sites)))

(defn initialize-burn-count-matrix
  [{:keys [^double output-burn-probability output-burn-count? ^doubles max-runtimes num-rows num-cols]}]
  (when (or output-burn-count? output-burn-probability)
    (if (int? output-burn-probability)
      (let [num-bands (inc (long (quot (double (apply max max-runtimes)) output-burn-probability)))]
        (m/zero-array [num-bands num-rows num-cols]))
      (m/zero-array [num-rows num-cols]))))

(defn initialize-aggregate-matrices
  [{:keys
    [num-rows num-cols output-flame-length-sum?
     output-flame-length-max? output-spot-count?] :as inputs}]
  {:burn-count-matrix       (initialize-burn-count-matrix inputs)
   :flame-length-sum-matrix (when output-flame-length-sum? (m/zero-array [num-rows num-cols]))
   :flame-length-max-matrix (when output-flame-length-max? (m/zero-array [num-rows num-cols]))
   :spot-count-matrix       (when output-spot-count? (m/zero-array [num-rows num-cols]))})

(defn add-aggregate-matrices
  [inputs]
  (merge inputs (initialize-aggregate-matrices inputs)))
