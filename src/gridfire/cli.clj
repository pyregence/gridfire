;; [[file:../../org/GridFire.org::command-line-interface][command-line-interface]]
(ns gridfire.cli
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.core.matrix :as m]
            [gridfire.postgis-bridge :refer [postgis-raster-to-matrix]]
            [gridfire.surface-fire :refer [degrees-to-radians]]
            [gridfire.fire-spread :refer [run-fire-spread]]
            [matrix-viz.core :refer [save-matrix-as-png]]
            [magellan.core :refer [register-new-crs-definitions-from-properties-file!
                                   make-envelope matrix-to-raster write-raster
                                   read-raster]])
  (:import (java.util Random)))

(m/set-current-implementation :vectorz)

(register-new-crs-definitions-from-properties-file! "CUSTOM"
                                                    (io/resource "custom_projections.properties"))

(def layers-kw
  [:aspect
   :canopy-base-height
   :canopy-cover
   :canopy-height
   :crown-bulk-density
   :elevation
   :fuel-model
   :slope])

(defn convert-metrics
  "Converting from meters to feet"
  [landfire-layers]
  (-> landfire-layers
      (update-in [:elevation :matrix]
                 (fn [matrix] (m/emap #(* % 3.28) matrix))) ; m -> ft
      (update-in [:slope :matrix]
                 (fn [matrix] (m/emap #(Math/tan (degrees-to-radians %)) matrix))) ; degrees -> %
      (update-in [:canopy-height :matrix]
                 (fn [matrix] (m/emap #(* % 3.28) matrix))) ; m -> ft
      (update-in [:canopy-base-height :matrix]
                 (fn [matrix] (m/emap #(* % 3.28) matrix))) ; m -> ft
      (update-in [:crown-bulk-density :matrix]
                 (fn [matrix] (m/emap #(* % 0.0624) matrix))))) ; kg/m^3 -> lb/ft^3

(defn- geotiff-raster-to-matrix
  "Reads a raster from a file using the magellan.core library. Returns the
   post-processed raster values as a Clojure matrix using the core.matrix API
   along with all of the georeferencing information associated with this tile in a
   hash-map with the following form:
  {:coverage   coverage
   :image      image
   :crs        crs
   :projection (CRS/getMapProjection crs)
   :envelope   (.getEnvelope coverage)
   :grid       grid
   :width      (.getWidth image)
   :height     (.getHeight image)
   :bands      (vec (.getSampleDimensions coverage))
   :envelope
   :numbands 1,
   :matrix #vectorz/matrix Large matrix with shape: [534,486]}"
  [file-path]
  (let [{:keys [matrix bands] :as raster} (read-raster file-path)]
    (merge raster
           {;; :srid 900916, ;; not used?
            ;; :skewx 0.0, ;; not used?
            ;; :skewy 0.0, ;; not used?
            :numbands (count bands)
            :matrix   (->> matrix (m/emap #(or % -1.0)) m/matrix)})))

(defmulti fetch-landfire-layers
  "Returns a map of LANDFIRE rasters (represented as maps) with the following units:
   {:elevation          feet
    :slope              vertical feet/horizontal feet
    :aspect             degrees clockwise from north
    :fuel-model         fuel model numbers 1-256
    :canopy-height      feet
    :canopy-base-height feet
    :crown-bulk-density lb/ft^3
    :canopy-cover       % (0-100)}"
  (fn [config]
    (:fetch-layer-method config)))

(defmethod fetch-landfire-layers :postgis
  [{:keys [db-spec layer-tables] :as config}]
  (let [landfire-layers (reduce (fn [amap layer]
                                  (let [table (layer-tables layer)]
                                    (assoc amap layer
                                           (postgis-raster-to-matrix db-spec table))))
                                {}
                                layers-kw)]
    (convert-metrics landfire-layers)))

(defmethod fetch-landfire-layers :geotiff
  [{:keys [layer-files] :as config}]
  (let [landfire-layers (reduce (fn [amap layer]
                                  (let [raster (layer-files layer)]
                                    (assoc amap layer
                                           (geotiff-raster-to-matrix raster))))
                                {}
                                layers-kw)]
    (convert-metrics landfire-layers)))

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

(defn calc-emc
  "Computes the Equilibrium Moisture Content (EMC) from rh (relative
   humidity in %) and temp (temperature in F)."
  [rh temp]
  (/ (cond (< rh 10)  (+ 0.03229 (* 0.281073 rh) (* -0.000578 rh temp))
           (< rh 50)  (+ 2.22749 (* 0.160107 rh) (* -0.01478 temp))
           :otherwise (+ 21.0606 (* 0.005565 rh rh) (* -0.00035 rh temp) (* -0.483199 rh)))
     30))

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
   outfile-suffix output-geotiffs? output-pngs? output-csvs?]
  (mapv
    (fn [i]
      (let [equilibrium-moisture (calc-emc (relative-humidity i) (temperature i))
            fuel-moisture        {:dead {:1hr   (+ equilibrium-moisture 0.002)
                                         :10hr  (+ equilibrium-moisture 0.015)
                                         :100hr (+ equilibrium-moisture 0.025)}
                                  :live {:herbaceous (* equilibrium-moisture 2.0)
                                         :woody      (* equilibrium-moisture 0.5)}}]
        (if-let [fire-spread-results (run-fire-spread (max-runtime i)
                                                      cell-size
                                                      landfire-rasters
                                                      (wind-speed-20ft i)
                                                      (wind-from-direction i)
                                                      fuel-moisture
                                                      (* 0.01 (foliar-moisture i))
                                                      (ellipse-adjustment-factor i)
                                                      [(ignition-row i)
                                                       (ignition-col i)])]
          (do
            (doseq [[name layer] [["fire_spread"         :fire-spread-matrix]
                                  ["flame_length"        :flame-length-matrix]
                                  ["fire_line_intensity" :fire-line-intensity-matrix]]]
              (when output-geotiffs?
                (-> (matrix-to-raster name (fire-spread-results layer) envelope)
                    (write-raster (str name outfile-suffix "_" i ".tif"))))
              (when output-pngs?
                (save-matrix-as-png :color 4 -1.0
                                    (fire-spread-results layer)
                                    (str name outfile-suffix "_" i ".png"))))
            (when output-csvs?
              (merge
                {:ignition-row              (ignition-row i)
                 :ignition-col              (ignition-col i)
                 :max-runtime               (max-runtime i)
                 :temperature               (temperature i)
                 :relative-humidity         (relative-humidity i)
                 :wind-speed-20ft           (wind-speed-20ft i)
                 :wind-from-direction       (wind-from-direction i)
                 :foliar-moisture           (foliar-moisture i)
                 :ellipse-adjustment-factor (ellipse-adjustment-factor i)}
                (summarize-fire-spread-results fire-spread-results cell-size))))
          (when output-csvs?
            {:ignition-row               (ignition-row i)
             :ignition-col               (ignition-col i)
             :max-runtime                (max-runtime i)
             :temperature                (temperature i)
             :relative-humidity          (relative-humidity i)
             :wind-speed-20ft            (wind-speed-20ft i)
             :wind-from-direction        (wind-from-direction i)
             :foliar-moisture            (foliar-moisture i)
             :ellipse-adjustment-factor  (ellipse-adjustment-factor i)
             :fire-size                  0.0
             :flame-length-mean          0.0
             :flame-length-stddev        0.0
             :fire-line-intensity-mean   0.0
             :fire-line-intensity-stddev 0.0}))))
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
  (if (= :postgis (:fetch-layer-method config))
    (let [{:keys [upperleftx upperlefty width height scalex scaley]}
          (landfire-layers :elevation)]
      (make-envelope (:srid config)
                     upperleftx
                     (+ upperlefty (* height scaley))
                     (* width scalex)
                     (* -1.0 height scaley)))
    (-> landfire-layers :elevation :envelope2D)))

(defn -main
  [& config-files]
  (doseq [config-file config-files]
    (let [config           (edn/read-string (slurp config-file))
          landfire-layers  (fetch-landfire-layers config)
          landfire-rasters (into {}
                                 (map (fn [[layer info]] [layer (:matrix info)]))
                                 landfire-layers)
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
             (draw-samples rand-generator simulations (:temperature config))
             (draw-samples rand-generator simulations (:relative-humidity config))
             (draw-samples rand-generator simulations (:wind-speed-20ft config))
             (draw-samples rand-generator simulations (:wind-from-direction config))
             (draw-samples rand-generator simulations (:foliar-moisture config))
             (draw-samples rand-generator simulations (:ellipse-adjustment-factor config))
             (:outfile-suffix config)
             (:output-geotiffs? config)
             (:output-pngs? config)
             (:output-csvs? config))
           (write-csv-outputs
             (:output-csvs? config)
             (str "summary_stats" (:outfile-suffix config) ".csv"))))))

;; command-line-interface ends here
