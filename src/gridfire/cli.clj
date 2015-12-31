(ns gridfire.cli
  (:gen-class)
  (:require [clojure.core.matrix :as m]
            [gridfire.postgis-bridge :refer [postgis-raster-to-matrix]]
            [gridfire.surface-fire :refer [degrees-to-radians]]
            [gridfire.fire-spread :refer [run-fire-spread]]
            [magellan.core :refer [register-new-crs-definitions-from-properties-file!
                                   srid-to-crs make-envelope
                                   matrix-to-raster write-raster]]))

(m/set-current-implementation :vectorz)

(register-new-crs-definitions-from-properties-file!
 "CALFIRE" "custom_projections.properties")

(defn fetch-landfire-layers
  "Returns a map of LANDFIRE rasters with the following units:
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
        (update-in [:elevation          :matrix] (fn [matrix] (m/emap #(* % 3.28) matrix))) ; m -> ft
        (update-in [:slope              :matrix] (fn [matrix] (m/emap #(Math/tan (degrees-to-radians %)) matrix))) ; degrees -> %
        (update-in [:canopy-height      :matrix] (fn [matrix] (m/emap #(* % 3.28) matrix))) ; m -> ft
        (update-in [:canopy-base-height :matrix] (fn [matrix] (m/emap #(* % 3.28) matrix))) ; m -> ft
        (update-in [:crown-bulk-density :matrix] (fn [matrix] (m/emap #(* % 0.0624) matrix)))))) ; kg/m^3 -> lb/ft^3

(defn -main
  [config-file]
  (let [config              (read-string (slurp config-file))
        landfire-layers     (fetch-landfire-layers (:db-spec config)
                                                   (:landfire-layers config))
        landfire-rasters    (into {}
                                  (map (fn [[layer info]] [layer (:matrix info)]))
                                  landfire-layers)
        fire-spread-results (run-fire-spread (:max-runtime               config)
                                             (:cell-size                 config)
                                             landfire-rasters
                                             (:wind-speed-20ft           config)
                                             (:wind-from-direction       config)
                                             (:fuel-moisture             config)
                                             (:foliar-moisture           config)
                                             (:ellipse-adjustment-factor config)
                                             (:ignition-site             config))
        envelope            (make-envelope (srid-to-crs (:srid config))
                                           (landfire-layers :elevation :upperleftx)
                                           (landfire-layers :elevation :upperlefty)
                                           (* (landfire-layers :elevation :width)
                                              (landfire-layers :elevation :scalex))
                                           (* (landfire-layers :elevation :height)
                                              (landfire-layers :elevation :scaley)))]
    (-> (matrix-to-raster "fire-spread-matrix"
                          (:fire-spread-matrix fire-spread-results)
                          envelope)
        (write-raster (:fire-spread-outfile config)))
    (-> (matrix-to-raster "flame-length-matrix"
                          (:flame-length-matrix fire-spread-results)
                          envelope)
        (write-raster (:flame-length-outfile config)))
    (-> (matrix-to-raster "fire-line-intensity-matrix"
                          (:fire-line-intensity-matrix fire-spread-results)
                          envelope)
        (write-raster (:fire-line-intensity-outfile fire-spread-results)))
    (println "Global Clock:" (:global-clock fire-spread-results))
    (println "Ignited Cells:" (count (:ignited-cells fire-spread-results)))))
