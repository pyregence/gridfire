(ns gridfire.inputs
  (:require [clojure.core.matrix      :as m]
            [clojure.data.csv         :as csv]
            [clojure.java.io          :as io]
            [gridfire.conversion      :refer [m->ft]]
            [gridfire.fetch           :as fetch]
            [gridfire.perturbation    :as perturbation]
            [gridfire.random-ignition :as random-ignition]
            [gridfire.utils.random    :refer [draw-samples]]
            [magellan.core            :refer [make-envelope register-new-crs-definitions-from-properties-file!]])
  (:import java.util.Random))

(m/set-current-implementation :vectorz)

(register-new-crs-definitions-from-properties-file! "CUSTOM" (io/resource "custom_projections.properties"))

(set! *unchecked-math* :warn-on-boxed)

(defn get-envelope
  [config landfire-layers]
  (let [{:keys [^double upperleftx
                ^double upperlefty
                ^double width
                ^double height
                ^double scalex
                ^double scaley]} (landfire-layers :elevation)]
    (make-envelope (:srid config)
                   upperleftx
                   (+ upperlefty (* height scaley))
                   (* width scalex)
                   (* -1.0 height scaley))))

(defn add-input-layers
  [config]
  (let [landfire-layers (fetch/landfire-layers config)]
    (assoc config
           :envelope             (get-envelope config landfire-layers)
           :landfire-rasters     (into {}
                                       (map (fn [[layer-name layer-info]] [layer-name (first (:matrix layer-info))]))
                                       landfire-layers)
           :ignition-layer       (fetch/ignition-layer config)
           :ignition-mask-layer  (fetch/ignition-mask-layer config)
           :weather-layers       (fetch/weather-layers config)
           :fuel-moisture        (fetch/fuel-moisture config))))

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
  [{:keys [random-seed landfire-rasters] :as inputs}]
  (let [fuel-model (:fuel-model landfire-rasters)]
    (assoc inputs
           :num-rows          (m/row-count fuel-model)
           :num-cols          (m/column-count fuel-model)
           :multiplier-lookup (create-multiplier-lookup inputs)
           :rand-gen          (if random-seed
                                (Random. random-seed)
                                (Random.)))))

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
    :as inputs}]
  (assoc inputs
         :max-runtimes               (or max-runtimes (draw-samples rand-gen simulations max-runtime))
         :ignition-rows              (or ignition-rows (draw-samples rand-gen simulations ignition-row))
         :ignition-cols              (or ignition-cols (draw-samples rand-gen simulations ignition-col))
         :foliar-moistures           (draw-samples rand-gen simulations foliar-moisture)
         :ellipse-adjustment-factors (draw-samples rand-gen simulations ellipse-adjustment-factor)
         :perturbations              (perturbation/draw-samples rand-gen simulations perturbations)))

(defn get-weather
  [config rand-generator weather-type weather-layers]
  (if (contains? weather-layers weather-type)
    (weather-type weather-layers)
    (draw-samples rand-generator (:simulations config) (config weather-type))))

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

(defn initialize-burn-count-matrix
  [{:keys [^double output-burn-probability output-burn-count? ^doubles max-runtimes num-rows num-cols]}]
  (when (or output-burn-count? output-burn-probability)
    (if (int? output-burn-probability)
      (let [num-bands (inc (long (quot (double (apply max max-runtimes)) output-burn-probability)))]
        (m/zero-array [num-bands num-rows num-cols]))
      (m/zero-array [num-rows num-cols]))))

(defn initialize-aggregate-matrices
  [{:keys [num-rows num-cols output-flame-length-sum?
           output-flame-length-max? output-spot-count?] :as inputs}]
  {:burn-count-matrix       (initialize-burn-count-matrix inputs)
   :flame-length-sum-matrix (when output-flame-length-sum? (m/zero-array [num-rows num-cols]))
   :flame-length-max-matrix (when output-flame-length-max? (m/zero-array [num-rows num-cols]))
   :spot-count-matrix       (when output-spot-count? (m/zero-array [num-rows num-cols]))})

(defn add-aggregate-matrices
  [inputs]
  (merge inputs (initialize-aggregate-matrices inputs)))
