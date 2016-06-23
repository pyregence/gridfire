(ns gridfire.cli
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.core.matrix :as m]
            [gridfire.postgis-bridge :refer [postgis-raster-to-matrix]]
            [gridfire.surface-fire :refer [degrees-to-radians]]
            [gridfire.fire-spread :refer [run-fire-spread]]
            [matrix-viz.core :refer [save-matrix-as-png]]
            [magellan.core :refer [register-new-crs-definitions-from-properties-file!
                                   make-envelope matrix-to-raster write-raster]]))

(m/set-current-implementation :vectorz)

(register-new-crs-definitions-from-properties-file!
 "CUSTOM" "custom_projections.properties")

(defn fetch-landfire-layers
  "Returns a map of LANDFIRE rasters (represented as maps) with the following units:
   {:elevation          feet
    :slope              vertical feet/horizontal feet
    :aspect             degrees clockwise from north
    :fuel-model         fuel model numbers 1-256
    :canopy-height      feet
    :canopy-base-height feet
    :crown-bulk-density lb/ft^3
    :canopy-cover       % (0-100)}"
  [db-spec layer->table]
  (let [landfire-layers (reduce (fn [amap layer]
                                  (let [table (layer->table layer)]
                                    (assoc amap layer
                                           (postgis-raster-to-matrix db-spec table))))
                                {}
                                [:elevation
                                 :slope
                                 :aspect
                                 :fuel-model
                                 :canopy-height
                                 :canopy-base-height
                                 :crown-bulk-density
                                 :canopy-cover])]
    (-> landfire-layers
        (update-in [:elevation :matrix]
                   (fn [matrix] (m/emap #(* % 3.28) matrix))) ; m -> ft
        (update-in [:slope :matrix]
                   (fn [matrix] (m/emap #(Math/tan (degrees-to-radians %)) matrix)))
                   ; degrees -> %
        (update-in [:canopy-height :matrix]
                   (fn [matrix] (m/emap #(* % 3.28) matrix))) ; m -> ft
        (update-in [:canopy-base-height :matrix]
                   (fn [matrix] (m/emap #(* % 3.28) matrix))) ; m -> ft
        (update-in [:crown-bulk-density :matrix]
                   (fn [matrix] (m/emap #(* % 0.0624) matrix)))))) ; kg/m^3 -> lb/ft^3

(defn sample-from-list
  [n x]
  (let [xs (if (list? x) x (list x))]
    (into [] (repeatedly n #(rand-nth xs)))))

(defn sample-from-range
  [n {:keys [min-row max-row min-col max-col]}]
  (let [row-range (- max-row min-row)
        col-range (- max-col min-col)]
    (into []
          (comp (distinct) (take n))
          (repeatedly #(vector (+ min-row (rand-int row-range))
                               (+ min-col (rand-int col-range)))))))

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

(defn run-simulations
  [simulations landfire-rasters envelope cell-size ignition-site
   max-runtime wind-speed-20ft wind-from-direction fuel-moisture
   foliar-moisture ellipse-adjustment-factor outfile-suffix
   output-geotiffs? output-pngs? output-csvs?]
  (mapv
   (fn [i]
     (if-let [fire-spread-results (run-fire-spread (max-runtime i)
                                                   cell-size
                                                   landfire-rasters
                                                   (wind-speed-20ft i)
                                                   (wind-from-direction i)
                                                   (fuel-moisture i)
                                                   (foliar-moisture i)
                                                   (ellipse-adjustment-factor i)
                                                   (ignition-site i))]
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
            {:ignition-site             (ignition-site i)
             :max-runtime               (max-runtime i)
             :wind-speed-20ft           (wind-speed-20ft i)
             :wind-from-direction       (wind-from-direction i)
             :fuel-moisture             (fuel-moisture i)
             :foliar-moisture           (foliar-moisture i)
             :ellipse-adjustment-factor (ellipse-adjustment-factor i)}
            (summarize-fire-spread-results fire-spread-results cell-size))))
       (when output-csvs?
         {:ignition-site              (ignition-site i)
          :max-runtime                (max-runtime i)
          :wind-speed-20ft            (wind-speed-20ft i)
          :wind-from-direction        (wind-from-direction i)
          :fuel-moisture              (fuel-moisture i)
          :foliar-moisture            (foliar-moisture i)
          :ellipse-adjustment-factor  (ellipse-adjustment-factor i)
          :fire-size                  0.0
          :flame-length-mean          0.0
          :flame-length-stddev        0.0
          :fire-line-intensity-mean   0.0
          :fire-line-intensity-stddev 0.0})))
   (range simulations)))

(defn -main
  [& config-files]
  (doseq [config-file config-files]
    (let [config           (edn/read-string (slurp config-file))
          landfire-layers  (fetch-landfire-layers (:db-spec config)
                                                  (:landfire-layers config))
          landfire-rasters (into {}
                                 (map (fn [[layer info]] [layer (:matrix info)]))
                                 landfire-layers)
          envelope         (let [{:keys [upperleftx upperlefty width height scalex scaley]}
                                 (landfire-layers :elevation)]
                             (make-envelope (:srid config)
                                            upperleftx
                                            (+ upperlefty (* height scaley))
                                            (* width scalex)
                                            (* -1.0 height scaley)))
          simulations      (:simulations config)]
      (when (:output-landfire-inputs? config)
        (doseq [[layer matrix] landfire-rasters]
          (-> (matrix-to-raster (name layer) matrix envelope)
              (write-raster (str (name layer) (:outfile-suffix config) ".tif")))))
      (run-simulations
       simulations
       landfire-rasters
       envelope
       (:cell-size config)
       (if (map? (:ignition-site config))
         (sample-from-range simulations (:ignition-site config))
         (sample-from-list simulations (:ignition-site config)))
       (sample-from-list simulations (:max-runtime config))
       (sample-from-list simulations (:wind-speed-20ft config))
       (sample-from-list simulations (:wind-from-direction config))
       (sample-from-list simulations (:fuel-moisture config))
       (sample-from-list simulations (:foliar-moisture config))
       (sample-from-list simulations (:ellipse-adjustment-factor config))
       (:outfile-suffix config)
       (:output-geotiffs? config)
       (:output-pngs? config)
       (:output-csvs? config)))))
