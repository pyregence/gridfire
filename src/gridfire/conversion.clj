(ns gridfire.conversion
  (:require [clojure.core.matrix :as m]))

(defn F->K
  "Convert farenheight to kelvin."
  [degrees]
  (->> (+ degrees 459.67)
       (* (/ 5.0 9.0))))

(defn K->F
  "Convert kelvin to farenheight."
  [degrees]
  (->> (- degrees 273.15)
       (* (/ 9.0 5.0))
       (+ 32.0)))

(defn F->C
  "Convert farenheight to celcius."
  [degrees]
  (->> (- degrees 32.0)
       (* (/ 5.0 9.0))))

(defn C->F
  "Convert celsius to farenheight."
  [degrees]
  (->> (* degrees (/ 9.0 5.0))
       (+ 32.0)))

(defn deg->rad
  "Convert degrees to radians."
  [d]
  (* d (/ Math/PI 180)))

(defn rad->deg
  "Convert radians to degrees."
  [d]
  (* d (/ 180 Math/PI)))

(defn m->ft
  "Convert meters to feet."
  [m]
  (* m 3.281))

(defn mph->mps
  "Convert miles per hour to meters per second."
  [s]
  (* s 0.447))

(defn Btu-ft-s->kW-m
  "Convert BTU per feet per second to kilowatt per meter."
  [Btu-ft-s]
  (/ Btu-ft-s 0.288894658272))

(defn percent->dec
  [p]
  (* p 0.001))

(defn dec->percent
  [d]
  (* d 100))

(defn sec->min
  "Convert seconds to minutes."
  [s]
  (/ s 60))

(def conversion-table
  {:elevation          m->ft
   :slope              deg->rad
   :canopy-height      m->ft
   :canopy-base-height m->ft
   :crown-bulk-density #(* % 0.0624) ; kg/m^3 -> lb/ft^3
   :wind-speed-20ft    #(* % 2.237)  ; m/s -> mph
   :temperature        {:metric   C->F
                        :absolute K->F}})

(defmulti to-imperial (fn [_ _ layer-name] layer-name))

(defmethod to-imperial :elevation
  [layer {:keys [units multiplier]} layer-name]
  (if-let [xforms (seq (remove nil? [(when (= units :metric) (conversion-table layer-name))
                                     (when-not (contains? #{1 1.0 nil} multiplier) #(* % multiplier))]))]
    (update layer :matrix (fn [matrix] (m/emap (apply comp xforms) matrix)))
    layer))

(defmethod to-imperial :slope
  [layer {:keys [multiplier]} layer-name]
  (if-let [xforms (seq (remove nil? [(conversion-table layer-name)
                                     (when-not (contains? #{1 1.0 nil} multiplier) #(* % multiplier))]))]
    (update layer :matrix (fn [matrix] (m/emap (apply comp xforms) matrix)))
    layer))

(defmethod to-imperial :canopy-height
  [layer {:keys [units multiplier]} layer-name]
  (if-let [xforms (seq (remove nil? [(when (= units :metric) (conversion-table layer-name))
                                     (when-not (contains? #{1 1.0 nil} multiplier) #(* % multiplier))]))]
    (update layer :matrix (fn [matrix] (m/emap (apply comp xforms) matrix)))
    layer))

(defmethod to-imperial :canopy-base-height
  [layer {:keys [units multiplier]} layer-name]
  (if-let [xforms (seq (remove nil? [(when (= units :metric) (conversion-table layer-name))
                                     (when-not (contains? #{1 1.0 nil} multiplier) #(* % multiplier))]))]
    (update layer :matrix (fn [matrix] (m/emap (apply comp xforms) matrix)))
    layer))

(defmethod to-imperial :crown-bulk-density
  [layer {:keys [units multiplier]} layer-name]
  (if-let [xforms (seq (remove nil? [(when (= units :metric) (conversion-table layer-name))
                                     (when-not (contains? #{1 1.0 nil} multiplier) #(* % multiplier))]))]
    (update layer :matrix (fn [matrix] (m/emap (apply comp xforms) matrix)))
    layer))

(defmethod to-imperial :wind-speed-20ft
  [layer {:keys [units multiplier]} layer-name]
  (if-let [xforms (seq (remove nil? [(when (= units :metric) (conversion-table layer-name))
                                     (when-not (contains? #{1 1.0 nil} multiplier) #(* % multiplier))]))]
    (update layer :matrix (fn [matrix] (m/emap (apply comp xforms) matrix)))
    layer))

(defmethod to-imperial :temperature
  [layer {:keys [units multiplier]} layer-name]
  (if-let [xforms (seq (remove nil? [(cond
                                       (= units :metric)   (get-in conversion-table [layer-name units])
                                       (= units :absolute) (get-in conversion-table [layer-name units]))
                                     (when-not (contains? #{1 1.0 nil} multiplier) #(* % multiplier))]))]
    (update layer :matrix (fn [matrix] (m/emap (apply comp xforms) matrix)))
    layer))

(defmethod to-imperial :default
  [layer _ _]
  layer)
