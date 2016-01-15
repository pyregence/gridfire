(ns gridfire.crown-fire
  (:require [gridfire.surface-fire :refer [byram-fire-line-intensity byram-flame-length]]))

(defn ft->m [ft] (* 0.3048 ft))

(defn kW-m->Btu-ft-s [kW-m] (* 0.288894658272 kW-m))

(defn van-wagner-crown-fire-initiation?
  "- canopy-cover (0-100 %)
   - canopy-base-height (ft)
   - foliar-moisture (lb moisture/lb ovendry weight)
   - fire-line-intensity (Btu/ft*s)"
  [canopy-cover canopy-base-height foliar-moisture fire-line-intensity]
  (and (> canopy-cover 40.0)
       (-> (+ 460.0 (* 2600.0 foliar-moisture)) ;; heat-of-ignition = kJ/kg
           (* 0.01 (ft->m canopy-base-height))
           (Math/pow 1.5) ;; critical-intensity = kW/m
           (kW-m->Btu-ft-s)
           (< fire-line-intensity))))

(defn mph->km-hr [mph] (* 1.609344 mph))

(defn lb-ft3->kg-m3 [lb-ft3] (* 16.01846 lb-ft3))

(defn m->ft [m] (* 3.281 m))

(defn cruz-crown-fire-spread
  "Returns spread-rate in ft/min given:
   - wind-speed-20ft (mph)
   - crown-bulk-density (lb/ft^3)
   - estimated-fine-fuel-moisture (-> M_f :dead :1hr) (0-1)"
  [wind-speed-20ft crown-bulk-density estimated-fine-fuel-moisture]
  (let [wind-speed-10m               (/ (mph->km-hr wind-speed-20ft) 0.87) ;; km/hr
        crown-bulk-density           (lb-ft3->kg-m3 crown-bulk-density) ;; kg/m^3
        estimated-fine-fuel-moisture (* 100.0 estimated-fine-fuel-moisture)
        active-spread-rate           (* 11.02
                                        (Math/pow wind-speed-10m 0.90)
                                        (Math/pow crown-bulk-density 0.19)
                                        (Math/exp (* -0.17 estimated-fine-fuel-moisture))) ;; m/min
        critical-spread-rate         (/ 3.0 crown-bulk-density) ;; m/min
        criteria-for-active-crowning (/ active-spread-rate critical-spread-rate)]
    (m->ft
     (if (>= active-spread-rate critical-spread-rate)
       active-spread-rate
       (* active-spread-rate (Math/exp (- criteria-for-active-crowning)))))))

(defn crown-fire-line-intensity
  "(ft/min * lb/ft^3 * ft * Btu/lb)/60 = (Btu/ft*min)/60 = Btu/ft*s"
  [crown-spread-rate crown-bulk-density canopy-height canopy-base-height heat-of-combustion]
  (/ (* crown-spread-rate
        crown-bulk-density
        (- canopy-height canopy-base-height)
        heat-of-combustion)
     60.0))

(defn crown-flame-length
  [reaction-intensity flame-depth crown-spread-rate crown-bulk-density
   canopy-height canopy-base-height heat-of-combustion]
  (byram-flame-length
   (+ (byram-fire-line-intensity reaction-intensity flame-depth)
      (crown-fire-line-intensity crown-spread-rate
                                 crown-bulk-density
                                 canopy-height
                                 canopy-base-height
                                 heat-of-combustion))))
