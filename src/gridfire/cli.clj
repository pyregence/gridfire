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

(defn n-cycle
  [n x]
  (let [xs (if (list? x) x (list x))]
    (into [] (repeatedly n #(rand-nth xs)))))

(defn -main
  [& config-files]
  (doseq [config-file config-files]
    (let [config              (edn/read-string (slurp config-file))
          landfire-layers     (fetch-landfire-layers (:db-spec config)
                                                     (:landfire-layers config))
          landfire-rasters    (into {}
                                    (map (fn [[layer info]] [layer (:matrix info)]))
                                    landfire-layers)
          envelope            (let [{:keys [upperleftx upperlefty width height scalex scaley]}
                                    (landfire-layers :elevation)]
                                (make-envelope (:srid config)
                                               upperleftx
                                               (+ upperlefty (* height scaley))
                                               (* width scalex)
                                               (* -1.0 height scaley)))
          simulations         (:simulations config)
          ignition-site       (n-cycle simulations (:ignition-site config))
          max-runtime         (n-cycle simulations (:max-runtime config))
          wind-speed-20ft     (n-cycle simulations (:wind-speed-20ft config))
          wind-from-direction (n-cycle simulations (:wind-from-direction config))
          fuel-moisture       (n-cycle simulations (:fuel-moisture config))
          foliar-moisture     (n-cycle simulations (:foliar-moisture config))
          ellipse-adjustment-factor (n-cycle simulations (:ellipse-adjustment-factor config))]
      (when (:output-landfire-inputs? config)
        (doseq [[layer info] landfire-layers]
          (-> (matrix-to-raster (name layer) (:matrix info) envelope)
              (write-raster (str (name layer) (:outfile-suffix config) ".tif")))))
      (dotimes [i simulations]
        (let [fire-spread-results (run-fire-spread (max-runtime i)
                                                   (:cell-size config)
                                                   landfire-rasters
                                                   (wind-speed-20ft i)
                                                   (wind-from-direction i)
                                                   (fuel-moisture i)
                                                   (foliar-moisture i)
                                                   (ellipse-adjustment-factor i)
                                                   (ignition-site i))]
          (println (str "Simulation " i
                        " Completed After " (:global-clock fire-spread-results) " Minutes"
                        " Because " (if (seq (:ignited-cells fire-spread-results))
                                      "The Max Runtime Was Exceeded"
                                      "All Burnable Cells Were Ignited")))
          (doseq [[name layer] [["fire_spread"         :fire-spread-matrix]
                                ["flame_length"        :flame-length-matrix]
                                ["fire_line_intensity" :fire-line-intensity-matrix]]]
            (when (:output-geotiffs? config)
              (-> (matrix-to-raster name (fire-spread-results layer) envelope)
                  (write-raster (str name (:outfile-suffix config) "_" i ".tif"))))
            (when (:output-pngs? config)
              (save-matrix-as-png :color 4 -1.0
                                  (fire-spread-results layer)
                                  (str name (:outfile-suffix config) "_" i ".png")))))))))
