;; [[file:../../org/GridFire.org::command-line-interface][command-line-interface]]
(ns gridfire.cli
  (:gen-class)
  (:require [clojure.core.matrix  :as m]
            [clojure.data.csv     :as csv]
            [clojure.edn          :as edn]
            [clojure.java.io      :as io]
            [clojure.spec.alpha   :as s]
            [gridfire.common      :refer [calc-emc]]
            [gridfire.fetch       :as fetch]
            [gridfire.fire-spread :refer [run-fire-spread]]
            [gridfire.validation  :as validation]
            [magellan.core        :refer [make-envelope
                                          matrix-to-raster
                                          register-new-crs-definitions-from-properties-file!
                                          write-raster]]
            [matrix-viz.core      :refer [save-matrix-as-png]])
  (:import java.util.Random))

(m/set-current-implementation :vectorz)

(register-new-crs-definitions-from-properties-file! "CUSTOM"
                                                    (io/resource "custom_projections.properties"))

(defn my-rand
  ([^Random rand-generator] (.nextDouble rand-generator))
  ([^Random rand-generator n] (* n (my-rand rand-generator))))

(defn my-rand-int
  [rand-generator n]
  (int (my-rand rand-generator n)))

(defn my-rand-nth
  [rand-generator coll]
  (nth coll (my-rand-int rand-generator (count coll))))

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

(defn run-simulations
  [simulations landfire-rasters envelope cell-size ignition-row
   ignition-col max-runtime temperature relative-humidity wind-speed-20ft
   wind-from-direction foliar-moisture ellipse-adjustment-factor
   outfile-suffix output-geotiffs? output-pngs? output-csvs? ignition-layer]
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
                                    :initial-ignition-site initial-ignition-site}))]
       (when fire-spread-results
         (doseq [[layer-name layer] [["fire_spread"         :fire-spread-matrix]
                                     ["flame_length"        :flame-length-matrix]
                                     ["fire_line_intensity" :fire-line-intensity-matrix]]]
           (when output-geotiffs?
             (-> (matrix-to-raster layer-name (fire-spread-results layer) envelope)
                 (write-raster (str layer-name outfile-suffix "_" i ".tif"))))
           (when output-pngs?
             (save-matrix-as-png :color 4 -1.0
                                 (fire-spread-results layer)
                                 (str name outfile-suffix "_" i ".png")))))
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

(defn get-weather [{:keys [simulations] :as config} rand-generator weather-type]
  (let [weather-spec (weather-type config)]
    (if (map? weather-spec)
      (fetch/weather config weather-spec)
      (draw-samples rand-generator simulations weather-spec))))

(defn -main
  [& config-files]
  (doseq [config-file config-files]
    (let [config           (edn/read-string (slurp config-file))]
      (if (s/valid? ::validation/config config)
        (let [landfire-layers  (fetch/landfire-layers config)
              landfire-rasters (into {}
                                     (map (fn [[layer info]] [layer (first (:matrix info))]))
                                     landfire-layers)
              ignition-layer   (fetch/ignition-layer config)
              envelope         (get-envelope config landfire-layers)
              simulations      (:simulations config)
              rand-generator   (if-let [seed (:random-seed config)]
                                 (Random. seed)
                                 (Random.))]
          (when (:output-landfire-inputs? config)
            (doseq [[layer matrix] landfire-rasters]
              (-> (matrix-to-raster (name layer) matrix envelope)
                  (write-raster (str (name layer) (:outfile-suffix config) ".tif")))))
          (->> (run-simulations
                simulations
                landfire-rasters
                envelope
                (:cell-size config)
                (draw-samples rand-generator simulations (:ignition-row config))
                (draw-samples rand-generator simulations (:ignition-col config))
                (draw-samples rand-generator simulations (:max-runtime config))
                (get-weather config rand-generator :temperature)
                (get-weather config rand-generator :relative-humidity)
                (get-weather config rand-generator :wind-speed-20ft)
                (get-weather config rand-generator :wind-from-direction)
                (draw-samples rand-generator simulations (:foliar-moisture config))
                (draw-samples rand-generator simulations (:ellipse-adjustment-factor config))
                (:outfile-suffix config)
                (:output-geotiffs? config)
                (:output-pngs? config)
                (:output-csvs? config)
                ignition-layer)
               (write-csv-outputs
                (:output-csvs? config)
                (str "summary_stats" (:outfile-suffix config) ".csv"))))
        (s/explain ::validation/config config)))))
;; command-line-interface ends here
