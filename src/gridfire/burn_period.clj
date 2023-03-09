;; [[file:../../org/GridFire.org::gridfire.burn-period][gridfire.burn-period]]
(ns gridfire.burn-period
  (:require [gridfire.burn-period.sunrise-sunset :as burnp-sun]
            [gridfire.utils.geo                  :refer [resolve-simulation-lat+lon]]))

(defn from-sunrise-sunset
  [inputs ignition-start-timestamp]
  (burnp-sun/infer-burn-period (merge (select-keys inputs [:burn-period-frac
                                                           :burn-period-length])
                                      (let [[lat lon] (resolve-simulation-lat+lon inputs)]
                                        {::burnp-sun/lat-deg lat
                                         ::burnp-sun/lng-deg lon})
                                      {:ignition-start-timestamp ignition-start-timestamp})))
;; gridfire.burn-period ends here
