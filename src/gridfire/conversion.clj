(ns gridfire.conversion
  (:require [clojure.string      :as str]
            [tech.v3.datatype    :as d]
            [tech.v3.tensor :as t])
  (:import java.text.SimpleDateFormat
           java.util.TimeZone))

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
  (Math/toRadians degrees))

(defn rad->deg
  "Convert radians to degrees."
  ^double
  [^double radians]
  (Math/toDegrees radians))

(defn deg->ratio
  "Convert degrees to ratio."
  ^double
  [^double degrees]
  (-> degrees Math/toRadians Math/tan))

(defn ratio->deg
  "Convert ratio to degrees."
  ^double
  [^double ratio]
  (-> ratio Math/atan Math/toDegrees))

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

(defn mph->km-hr
  "Convert miles per hour to kilometers per hour."
  ^double
  [^double mph]
  (* mph 1.609344))

(defn mph->fpm
  "Convert miles per hour to feet per minute."
  ^double
  [^double mph]
  (* mph 88.0))

(defn km-hr->mph
  "Convert kilometers per hour to miles per hour."
  ^double
  [^double km-hr]
  (* km-hr 0.621371192237334))

(defn wind-speed-20ft->wind-speed-10m
  "Convert wind speed at 20ft to wind speed at 10m."
  ^double
  [^double wind-speed-20ft]
  (/ wind-speed-20ft 0.87))

(defn wind-speed-10m->wind-speed-20ft
  "Convert wind speed at 10m to wind speed at 20ft."
  ^double
  [^double wind-speed-10m]
  (* 0.87 wind-speed-10m))

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

(defn Btu-lb->kJ-kg
  "Convert BTU per lb to kilojoule per kilogram."
  ^double
  [^double Btu-lb]
  (* Btu-lb 2.3259999996185))

(defn kJ-kg->Btu-lb
  "Convert kilojoule per kilogram to BTU per lb."
  ^double
  [^double kJ-kg]
  (/ kJ-kg 2.3259999996185))

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

(defn hour->min
  "Converts hours to minutes."
  ^double
  [^long hours]
  (* hours 60.0))

(defn min->hour
  "Converts minutes to hours. (rounds down)"
  ^long
  [^double minutes]
  (long (quot minutes 60.0)))

(defn convert-date-string
  "Convert a date string between two formats."
  [date-str from-format to-format]
  (let [in-format  (doto (SimpleDateFormat. from-format)
                     (.setTimeZone (TimeZone/getTimeZone "UTC")))
        out-format (doto (SimpleDateFormat. to-format)
                     (.setTimeZone (TimeZone/getTimeZone "UTC")))]
    (->> date-str
         (.parse in-format)
         (.format out-format))))

;; TODO remove when code is in triangulum
(defn camel->kebab
  "Converts camelString to kebab-string."
  [camel-string]
  (as-> camel-string s
    (str/split s #"(?<=[a-z])(?=[A-Z])")
    (map str/lower-case s)
    (str/join "-" s)))

;; TODO remove when code is in triangulum
(defn kebab->camel
  "Converts kebab-string to camelString."
  [kebab-string]
  (let [words (-> kebab-string
                  (str/lower-case)
                  (str/replace #"^[^a-z_$]|[^\w-]" "")
                  (str/split #"-"))]
    (->> (map str/capitalize (rest words))
         (cons (first words))
         (str/join ""))))

(defn snake->kebab
  "Converts snake_string to kebab-string."
  [snake-string]
  (str/replace snake-string #"_" "-"))

(defn kebab->snake
  "Converts kebab-string to snake_string."
  [kebab-string]
  (str/replace kebab-string #"-" "_"))

(def conversion-table
  {:elevation          {:metric m->ft}
   :slope              {nil deg->ratio}
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
  [layer {:keys [units multiplier] :or {multiplier 1.0}} layer-name]
  (if-let [converter (get-units-converter layer-name units multiplier)]
    (update layer :matrix #(d/copy! (d/emap converter nil %) %))
    layer))
