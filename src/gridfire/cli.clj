(ns gridfire.cli
  (:gen-class)
  (:require [clojure.core.matrix :as m]
            [matrix-viz.core :refer [save-matrix-as-png]]
            [gridfire.postgis-bridge :refer [postgis-raster-to-matrix]]
            [gridfire.surface-fire :refer [degrees-to-radians]]
            [gridfire.fire-spread :refer [run-fire-spread]]))

(m/set-current-implementation :vectorz)

(defn fetch-landfire-layers
  "Returns a map of LANDFIRE rasters as core.matrix 2D double arrays:
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
                                           (postgis-raster-to-matrix db-spec table nil nil))))
                                {}
                                [:elevation
                                 :slope
                                 :aspect
                                 :fuel-model
                                 :canopy-height
                                 :canopy-base-height
                                 :crown-bulk-density
                                 :canopy-cover])]
    (assoc landfire-layers
           :elevation          (m/emap #(* % 3.28)
                                       (landfire-layers :elevation)) ; m -> ft
           :slope              (m/emap #(Math/tan (degrees-to-radians %))
                                       (landfire-layers :slope)) ; degrees -> %
           :canopy-height      (m/emap #(* % 3.28)
                                       (landfire-layers :canopy-height)) ; m -> ft
           :canopy-base-height (m/emap #(* % 3.28)
                                       (landfire-layers :canopy-base-height)) ; m -> ft
           :crown-bulk-density (m/emap #(* % 0.0624)
                                       (landfire-layers :crown-bulk-density)))))
                                       ; kg/m^3 -> lb/ft^3

(defn -main
  "Outputs:
  {:global-clock               global-clock
   :initial-ignition-site      initial-ignition-site
   :ignited-cells              ignited-cells
   :fire-spread-matrix         (m/emap #(- 1.0 %) ignition-matrix)
   :flame-length-matrix        flame-length-matrix
   :fire-line-intensity-matrix fire-line-intensity-matrix}"
  [config-file]
  (let [config              (read-string (slurp config-file))
        landfire-layers     (fetch-landfire-layers (:db-spec config)
                                                   (:landfire-layers config))
        fire-spread-results (run-fire-spread (:max-runtime               config)
                                             (:cell-size                 config)
                                             landfire-layers
                                             (:wind-speed-20ft           config)
                                             (:wind-from-direction       config)
                                             (:fuel-moisture             config)
                                             (:foliar-moisture           config)
                                             (:ellipse-adjustment-factor config)
                                             (:ignition-site             config))]
    (save-matrix-as-png :color 4 -1.0
                        (:fire-spread-matrix fire-spread-results)
                        (:fire-spread-outfile config))
    (save-matrix-as-png :color 4 -1.0
                        (:flame-length-matrix fire-spread-results)
                        (:flame-length-outfile config))
    (save-matrix-as-png :color 4 -1.0
                        (:fire-line-intensity-matrix fire-spread-results)
                        (:fire-line-intensity-outfile config))
    (println "Global Clock:" (:global-clock fire-spread-results))
    (println "Ignited Cells:" (count (:ignited-cells fire-spread-results)))))
