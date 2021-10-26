(ns gridfire.conversion
  (:require [clojure.core.matrix :as m]))

;; TODO: Use definline and unchecked arithmetic
;; TODO: Make sure no functions in this namespace are redefined elsewhere.

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
  {:elevation          m->ft
   :slope              deg->rad
   :canopy-height      m->ft
   :canopy-base-height m->ft
   :crown-bulk-density kg-m3->lb-ft3
   :wind-speed-20ft    mps->mph
   :temperature        {:metric   C->F
                        :absolute K->F}})

;; FIXME: This looks really inefficient. Speed me up!
(defmulti to-imperial (fn [_ _ layer-name] layer-name))

(defmethod to-imperial :elevation
  [layer {:keys [units ^double multiplier]} layer-name]
  (if-let [xforms (seq (remove nil? [(when (= units :metric) (conversion-table layer-name))
                                     (when-not (contains? #{1 1.0 nil} multiplier) #(* ^double % multiplier))]))]
    (update layer :matrix (fn [matrix] (m/emap (apply comp xforms) matrix)))
    layer))

(defmethod to-imperial :slope
  [layer {:keys [^double multiplier]} layer-name]
  (if-let [xforms (seq (remove nil? [(conversion-table layer-name)
                                     (when-not (contains? #{1 1.0 nil} multiplier) #(* ^double % multiplier))]))]
    (update layer :matrix (fn [matrix] (m/emap (apply comp xforms) matrix)))
    layer))

(defmethod to-imperial :canopy-height
  [layer {:keys [units ^double multiplier]} layer-name]
  (if-let [xforms (seq (remove nil? [(when (= units :metric) (conversion-table layer-name))
                                     (when-not (contains? #{1 1.0 nil} multiplier) #(* ^double % multiplier))]))]
    (update layer :matrix (fn [matrix] (m/emap (apply comp xforms) matrix)))
    layer))

(defmethod to-imperial :canopy-base-height
  [layer {:keys [units ^double multiplier]} layer-name]
  (if-let [xforms (seq (remove nil? [(when (= units :metric) (conversion-table layer-name))
                                     (when-not (contains? #{1 1.0 nil} multiplier) #(* ^double % multiplier))]))]
    (update layer :matrix (fn [matrix] (m/emap (apply comp xforms) matrix)))
    layer))

(defmethod to-imperial :crown-bulk-density
  [layer {:keys [units ^double multiplier]} layer-name]
  (if-let [xforms (seq (remove nil? [(when (= units :metric) (conversion-table layer-name))
                                     (when-not (contains? #{1 1.0 nil} multiplier) #(* ^double % multiplier))]))]
    (update layer :matrix (fn [matrix] (m/emap (apply comp xforms) matrix)))
    layer))

(defmethod to-imperial :wind-speed-20ft
  [layer {:keys [units ^double multiplier]} layer-name]
  (if-let [xforms (seq (remove nil? [(when (= units :metric) (conversion-table layer-name))
                                     (when-not (contains? #{1 1.0 nil} multiplier) #(* ^double % multiplier))]))]
    (update layer :matrix (fn [matrix] (m/emap (apply comp xforms) matrix)))
    layer))

(defmethod to-imperial :temperature
  [layer {:keys [units ^double multiplier]} layer-name]
  (if-let [xforms (seq (remove nil? [(cond
                                       (= units :metric)   (get-in conversion-table [layer-name units])
                                       (= units :absolute) (get-in conversion-table [layer-name units]))
                                     (when-not (contains? #{1 1.0 nil} multiplier) #(* ^double % multiplier))]))]
    (update layer :matrix (fn [matrix] (m/emap (apply comp xforms) matrix)))
    layer))

(defmethod to-imperial :default
  [layer _ _]
  layer)
