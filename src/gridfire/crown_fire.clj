(ns gridfire.crown-fire
  (:require [gridfire.surface-fire :refer [byram-fire-line-intensity byram-flame-length]]))

(defn van-wagner-crown-fire-initiation
  "- canopy-cover (0-100 %)
   - canopy-base-height (ft)
   - crown-bulk-density (lb/ft^3)
   - foliar-moisture (lb moisture/lb ovendry weight)
   - spread-rate (ft/min)
   - fire-line-intensity (Btu/ft*s)"
  [canopy-cover canopy-base-height crown-bulk-density foliar-moisture spread-rate fire-line-intensity]
  (if (and (pos? canopy-cover)
           (pos? canopy-base-height)
           (pos? crown-bulk-density))
    (let [heat-of-ignition (+ 197.8 (* 1118.0 foliar-moisture)) ;; Btu/lb
          critical-intensity (Math/pow (* 0.002048
                                          canopy-base-height
                                          heat-of-ignition)
                                       1.5) ;; Btu/ft*s
          critical-spread-rate (/ 0.61445 crown-bulk-density)] ;; ft/min
      (if (and (> canopy-cover 40.0)
               (> fire-line-intensity critical-intensity))
        ;; crown fire initiation occurs
        (if (> spread-rate critical-spread-rate)
          :active-crown-fire
          :passive-crown-fire)
        ;; no crown fire initiation
        :surface-fire))
    ;; no trees to set on fire
    :surface-fire))

(defn cruz-active-crown-fire-spread
  "Returns spread-rate in ft/min given:
   - wind-speed-20ft (mph)
   - crown-bulk-density (lb/ft^3)
   - estimated-fine-fuel-moisture (-> M_f :dead :1hr) (0-1)"
  [wind-speed-20ft crown-bulk-density estimated-fine-fuel-moisture]
  (let [wind-speed-10m (/ (* 1.609344 wind-speed-20ft) 0.87) ;; km/hr
        crown-bulk-density (* 16.01846 crown-bulk-density) ;; kg/m^3
        estimated-fine-fuel-moisture (* 100.0 estimated-fine-fuel-moisture)]
    (* 36.155
       (Math/pow wind-speed-10m 0.90)
       (Math/pow crown-bulk-density 0.19)
       (Math/exp (* -0.17 estimated-fine-fuel-moisture)))))

(defn cruz-passive-crown-fire-spread
  "Returns spread-rate in ft/min given:
   - wind-speed-20ft (mph)
   - crown-bulk-density (lb/ft^3)
   - estimated-fine-fuel-moisture (-> M_f :dead :1hr) (0-1)"
  [wind-speed-20ft crown-bulk-density estimated-fine-fuel-moisture]
  (let [active-crown-spread-rate (cruz-active-crown-fire-spread wind-speed-20ft
                                                                crown-bulk-density
                                                                estimated-fine-fuel-moisture)]
    (* active-crown-spread-rate
       (Math/exp (/ (* active-crown-spread-rate crown-bulk-density) -0.61445)))))

;; heat of combustion is h from the fuel models (generally 8000 Btu/lb)
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
