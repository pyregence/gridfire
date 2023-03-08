;; FIXME LP coverage
(ns gridfire.spec.burn-period
  (:require [clojure.spec.alpha   :as s]
            [gridfire.spec.common :as common]))

(def burn-period-regex #"^([01]\d|2[0-3]):?([0-5]\d)$")

(s/def ::HH:MM (s/and string? #(re-matches burn-period-regex %)))

(s/def ::start ::HH:MM)

(s/def ::end ::HH:MM)

(s/def ::burn-period
  (s/keys :req-un [::start ::end]))

(s/def ::burn-period-frac ::common/ratio)

(s/def ::burn-period-length float?)

;; Test
#_(s/explain ::burn-period {:start "08:00"
                            :end   "20:00"})
