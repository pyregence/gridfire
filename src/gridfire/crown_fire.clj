;; [[file:../../org/GridFire.org::van-wagner-crown-fire-initiation][van-wagner-crown-fire-initiation]]
(ns gridfire.crown-fire
  (:require [gridfire.conversion :as convert]))

(defn van-wagner-crown-fire-line-intensity
  "Ouputs the fire-line intensity (kW/m) using:
   - canopy-base-height (m)
   - foliar-moisture (%)"
  ^double
  [^double canopy-base-height ^double foliar-moisture]
  (-> (+ 460.0 (* 26.0 foliar-moisture))
      (* 0.01 canopy-base-height)
      (Math/pow 1.5))) ;; critical-intensity = kW/m

(defn van-wagner-crown-fire-initiation-metric?
  "- canopy-cover (0-100 %)
   - canopy-base-height (m)
   - foliar-moisture (0-100 %) (lb moisture/lb ovendry weight)
   - fire-line-intensity (kW/m)"
  [^double canopy-cover ^double canopy-base-height ^double foliar-moisture ^double fire-line-intensity]
  (and (> canopy-cover 40.0)
       (> fire-line-intensity 0)
       (> canopy-base-height 0)
       (> fire-line-intensity (van-wagner-crown-fire-line-intensity canopy-base-height foliar-moisture))))

(defn van-wagner-crown-fire-initiation?
  "- canopy-cover (0-100 %)
   - canopy-base-height (ft)
   - foliar-moisture (lb moisture/lb ovendry weight)
   - fire-line-intensity (Btu/ft*s)"
  [^double canopy-cover ^double canopy-base-height ^double foliar-moisture ^double fire-line-intensity]
  (van-wagner-crown-fire-initiation-metric? canopy-cover
                                            (convert/ft->m canopy-base-height)
                                            (convert/dec->percent foliar-moisture)
                                            (convert/Btu-ft-s->kW-m fire-line-intensity)))
;; van-wagner-crown-fire-initiation ends here

;; [[file:../../org/GridFire.org::cruz-crown-fire-spread][cruz-crown-fire-spread]]

(defn cruz-active-crown-fire-spread
  "Returns active spread-rate in m/min given:
   - wind-speed-10m (km/h)
   - crown-bulk-density (kg/m^3)
   - estimated-fine-fuel-moisture (0-100 %)"
  ^double
  [^double wind-speed-10m ^double crown-bulk-density ^double estimated-fine-fuel-moisture]
  (* 11.02
     (Math/pow wind-speed-10m 0.90)
     (Math/pow crown-bulk-density 0.19)
     (Math/exp (* -0.17 estimated-fine-fuel-moisture))))

(defn cruz-passive-crown-fire-spread
  "Returns the passive spread-rate in m/min given:
   - wind-speed-10m (km/h)
   - crown-bulk-density (kg/m^3)
   - estimated-fine-fuel-moisture (0-100 %)"
  ^double
  [^double wind-speed-10m ^double crown-bulk-density ^double estimated-fine-fuel-moisture]
  (let [active-spread-rate           (cruz-active-crown-fire-spread wind-speed-10m
                                                                    crown-bulk-density
                                                                    estimated-fine-fuel-moisture)
        criteria-for-active-crowning (/ active-spread-rate (/ 3.0 crown-bulk-density))]
    (* active-spread-rate (Math/exp (- criteria-for-active-crowning)))))

(defn cruz-crown-fire-spread-metric
  "Returns spread-rate in m/min given:
   - wind-speed-10m (km/hr)
   - crown-bulk-density (kg/m^3)
   - estimated-fine-fuel-moisture (-> M_f :dead :1hr) (0-100+ %)"
  [^double wind-speed-10m ^double crown-bulk-density ^double estimated-fine-fuel-moisture]
  (let [active-spread-rate           (cruz-active-crown-fire-spread wind-speed-10m
                                                                    crown-bulk-density
                                                                    estimated-fine-fuel-moisture)
        critical-spread-rate         (/ 3.0 crown-bulk-density)] ;; m/min
    (if (> active-spread-rate critical-spread-rate)
      [:active-crown active-spread-rate]
      [:passive-crown (cruz-passive-crown-fire-spread wind-speed-10m crown-bulk-density estimated-fine-fuel-moisture)])))

(defn cruz-crown-fire-spread
  "Returns spread-rate in ft/min given:
   - wind-speed-20ft (mph)
   - crown-bulk-density (lb/ft^3)
   - estimated-fine-fuel-moisture (-> M_f :dead :1hr) (0-1)"
  [^double wind-speed-20ft ^double crown-bulk-density ^double estimated-fine-fuel-moisture]
  (let [[fire-type fire-rate] (cruz-crown-fire-spread-metric (convert/U-20ft->U-10m wind-speed-20ft)
                                                             (convert/lb-ft3->kg-m3 crown-bulk-density)
                                                             (convert/dec->percent estimated-fine-fuel-moisture))]
    [fire-type (convert/m->ft fire-rate)]))
;; cruz-crown-fire-spread ends here

;; [[file:../../org/GridFire.org::crown-fire-line-intensity][crown-fire-line-intensity]]
;; heat of combustion is h from the fuel models (generally 8000 Btu/lb)
(defn crown-fire-line-intensity
  "Returns Fire Line intensity in Btu/ft/s OR kW/m, given:
   - crown spread rate (ft/min OR m/min)
   - crown bulk density (lb/ft^3 OR kg/m^3)
   - canopy height/canopy base height (ft OR m)
   - heat of combustion (Btu/lb^2 OR kJ/kg)

  (ft/min * lb/ft^3 * ft * Btu/lb)/60 = (Btu/ft*min)/60 = Btu/ft*s
  OR
  (m/min * kg/m^3 * m * kJ/kg)/60 = (kJ/m*min)/60 = kJ/m*s = kW/m
  "
  [crown-spread-rate crown-bulk-density canopy-height canopy-base-height heat-of-combustion]
  ^double
  (/ (* ^double crown-spread-rate
        ^double crown-bulk-density
        (- ^double canopy-height ^double canopy-base-height)
        ^double heat-of-combustion)
     60.0))

(defn crown-fire-line-intensity-elmfire ;; kW/m
  [surface-fire-line-intensity crown-spread-rate crown-bulk-density
   canopy-height canopy-base-height]
  (let [heat-of-combustion 18000] ;; kJ/m^2
    (+ ^double surface-fire-line-intensity ;; kW/m
       (/ (* 0.3048 ;; m/ft
             ^double crown-spread-rate ;; ft/min
             ^double crown-bulk-density ;; kg/m^3
             (- ^double canopy-height ^double canopy-base-height) ;; m
             heat-of-combustion) ;; kJ/kg
          60.0)))) ;; s/min
;; crown-fire-line-intensity ends here

;; [[file:../../org/GridFire.org::crown-eccentricity][crown-eccentricity]]
(defn crown-fire-eccentricity
  "mph"
  ^double
  [^double wind-speed-20ft ^double ellipse-adjustment-factor]
  (let [length-width-ratio (+ 1.0 (* 0.125
                                     wind-speed-20ft
                                     ellipse-adjustment-factor))]
    (/ (Math/sqrt (- (Math/pow length-width-ratio 2.0) 1.0))
       length-width-ratio)))

(defn elmfire-length-to-width-ratio
  "true/false mph int>0 ft/min
   Crown L/W = min(1.0 + 0.125*U20_mph, L/W_max)
   Surface L/W = 0.936*e^(0.2566*Ueff_mph) + 0.461*e^(-0.1548*Ueff_mph) - 0.397"
  ^double
  [crown-fire? ^double wind-speed-20ft ^double max-length-to-width-ratio ^double effective-wind-speed]
  (if crown-fire?
    (min (+ 1.0 (* 0.125 wind-speed-20ft)) max-length-to-width-ratio)
    (min (+ (* 0.936 (Math/exp (/ (* 0.2566 effective-wind-speed 60.0) 5280.0)))
            (* 0.461 (Math/exp (/ (* -0.1548 effective-wind-speed 60.0) 5280.0)))
            -0.397)
         8.0)))
;; crown-eccentricity ends here
