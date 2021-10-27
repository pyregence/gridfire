(ns gridfire.conversion
  (:require [clojure.core.matrix :as m]))

(m/set-current-implementation :vectorz)

;; TODO: Make sure no functions in this namespace are redefined elsewhere.
(set! *unchecked-math* :warn-on-boxed)

(defn F->K
  "Convert fahrenheit to kelvin."
  ^double
  [^double degrees]
  (-> degrees
      (+ 459.67)
      (* 0.5555555555555556)))

(defn K->F
  "Convert kelvin to fahrenheit."
  ^double
  [^double degrees]
  (-> degrees
      (* 1.8)
      (- 459.67)))

(defn F->C
  "Convert fahrenheit to celsius."
  ^double
  [^double degrees]
  (-> degrees
      (- 32.0)
      (* 0.5555555555555556)))

(defn C->F
  "Convert celsius to fahrenheit."
  ^double
  [^double degrees]
  (-> degrees
      (* 1.8)
      (+ 32.0)))

(defn deg->rad
  "Convert degrees to radians."
  ^double
  [^double degrees]
  (* degrees 0.017453292519943295)) ; (/ Math/PI 180.0) = 0.017453292519943295

(defn rad->deg
  "Convert radians to degrees."
  ^double
  [^double radians]
  (* radians 57.29577951308232)) ; (/ 180.0 Math/PI) = 57.29577951308232

(defn m->ft
  "Convert meters to feet."
  ^double
  [^double m]
  (* m 3.281))

(defn ft->m
  "Convert feet to meters."
  ^double
  [^double ft]
  (* ft 0.30478512648582745))

(defn mph->mps
  "Convert miles per hour to meters per second."
  ^double
  [^double mph]
  (* mph 0.44701818551254696))

(defn mps->mph
  "Convert meters per second to miles per hour."
  ^double
  [^double mps]
  (* mps 2.237045454545455))

(defn Btu-ft-s->kW-m
  "Convert BTU per feet per second to kilowatt per meter."
  ^double
  [^double Btu-ft-s]
  (* Btu-ft-s 3.46165186))

(defn kW-m->Btu-ft-s
  "Convert kilowatt per meter to BTU per feet per second."
  ^double
  [^double kW-m]
  (* kW-m 0.28887942532730604))

(defn kg-m3->lb-ft3
  "Convert kilogram per cubic meter to pound per cubic foot."
  ^double
  [^double kg-m3]
  (* kg-m3 0.0624))

(defn lb-ft3->kg-m3
  "Convert pound per cubic foot to kilogram per cubic meter."
  ^double
  [^double lb-ft3]
  (* lb-ft3 16.025641025641026))

(defn percent->dec
  "Convert percent to decimal."
  ^double
  [^double percent]
  (* percent 0.01))

(defn dec->percent
  "Convert decimal to percent."
  ^double
  [^double decimal]
  (* decimal 100.0))

(defn sec->min
  "Convert seconds to minutes."
  ^double
  [^double seconds]
  (* seconds 0.016666666666666666))

(defn min->sec
  "Convert minutes to seconds."
  ^double
  [^double minutes]
  (* minutes 60.0))

(def conversion-table
  {:elevation          {:metric m->ft}
   :slope              {nil deg->rad}
   :canopy-height      {:metric m->ft}
   :canopy-base-height {:metric m->ft}
   :crown-bulk-density {:metric kg-m3->lb-ft3}
   :wind-speed-20ft    {:metric mps->mph}
   :temperature        {:metric C->F
                        :absolute K->F}})

(defn valid-multiplier?
  [x]
  (and (number? x) (not= x 1) (not= x 1.0)))

(defn get-units-converter
  [layer-name units ^double multiplier]
  (if-let [converter (get-in conversion-table [layer-name units])]
    (if (valid-multiplier? multiplier)
      (fn ^double [^double x] (converter (* x multiplier)))
      converter)
    (if (valid-multiplier? multiplier)
      (fn ^double [^double x] (* x multiplier))
      nil)))

(defn to-imperial!
  [layer {:keys [units multiplier]} layer-name]
  (if-let [converter (get-units-converter layer-name units multiplier)]
    (update layer :matrix #(m/emap! converter %))
    layer))
