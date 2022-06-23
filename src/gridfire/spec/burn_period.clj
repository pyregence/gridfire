(ns gridfire.spec.burn-period
  (:require [clojure.spec.alpha :as s])
  (:import java.time.LocalTime))

(def burn-period-regex #"^([01]\d|2[0-3]):?([0-5]\d)$")

(defn valid-burn-period-range?
  [{:keys [start end]}]
  (.isBefore (LocalTime/parse start) (LocalTime/parse end)))

(s/def ::HH:MM (s/and string? #(re-matches burn-period-regex %)))

(s/def ::start ::HH:MM)

(s/def ::end ::HH:MM)

(s/def ::weather-data-start-timestamp inst?)

(s/def ::burn-period
  (s/and (s/keys  :req-un [::start ::end ::weather-data-start-timestamp])
         valid-burn-period-range?))

;; Test
#_(s/explain ::burn-period {:start                        "08:00"
                            :end                          "20:00"
                            :weather-data-start-timestamp #inst "1970-01-01T00:00:00"})
