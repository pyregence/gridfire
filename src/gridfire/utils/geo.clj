(ns gridfire.utils.geo
  (:import (org.geotools.referencing CRS)
           (org.opengis.geometry Envelope)))

(defn- envelope-lower-corner-lat+lon
  [^Envelope envelope]
  (let [crs-4326  (CRS/decode "EPSG:4326")                  ; NOTE I checked that this call is fast. (Val, 01 Nov 2022)
        lc        (-> envelope
                      ;; Transforming to a CRS that will give us latitude/longitude coordinates.
                      (CRS/transform crs-4326)
                      (.getLowerCorner))
        [lat lon] (.getCoordinate lc)]
    [lat lon]))

(defn resolve-simulation-lat+lon
  "Returns a [latitude longitude] (in degrees) pair for some point in the :envelope of the given GridFire inputs map.

  The returned latitude and longitude are in angular degrees."
  [inputs]
  ;; NOTE why use the Lower Corner? It doesn't matter much which point we use, (Val, 01 Nov 2022)
  ;; because we're not after high accuracy.
  ;; We could use the Median point, but that would require
  ;; a GeoTools upgrade.
  (envelope-lower-corner-lat+lon (:envelope inputs)))
