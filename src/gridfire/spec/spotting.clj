(ns gridfire.spec.spotting
  (:require [clojure.spec.alpha :as s]))

(s/def ::ordered-range (fn [[lo hi]] (<= lo hi)))

(s/def ::crown-fire-spotting-percent (s/or
                                      :scalar (s/and float?
                                                     #(<= 0.0 % 1.0))
                                      :range (s/and (s/tuple float? float?)
                                                    ::ordered-range)))

(s/def ::scalar-or-range
  (s/nonconforming
   (s/or :scalar (s/nonconforming
                  (s/or :int int?
                        :float float?))
         :range (s/and (s/nonconforming
                        (s/or :int-tuple (s/tuple int? int?)
                              :float-tuple (s/tuple float? float?)))
                       ::ordered-range))))

(s/def ::lo ::scalar-or-range)

(s/def ::hi ::scalar-or-range)

(defn- increasing? [s]
  (if (seq s)
    (let [[f & rest] s]
      (if (and rest (> f (first rest)))
        false
        (recur rest)))
    true))

(s/def ::valid-hi-lo-ranges
  (fn [{:keys [lo hi]}]
    (increasing? (flatten (conj [] lo hi)))))

(s/def ::scalar-or-map
  (s/or :scalar (s/nonconforming
                 (s/or :float float?
                       :int int?))
        :map (s/and (s/keys :req-un [::lo ::hi])
                    ::valid-hi-lo-ranges)))

(s/def ::num-firebrands ::scalar-or-map)

(s/def ::mean-distance ::scalar-or-map)

(s/def ::flin-exp ::scalar-or-map)

(s/def ::ws-exp ::scalar-or-map)

(s/def ::normalized-distance-variance ::scalar-or-map)

(s/def ::valid-fuel-range (fn [[lo hi]] (<= 1 lo hi 204)))

(s/def ::fuel-number-range (s/and (s/tuple int? int?)
                                  ::valid-fuel-range
                                  ::ordered-range))

(s/def ::fuel-percent-pair (s/tuple ::fuel-number-range (s/or :scalar float?
                                                              :range (s/tuple float? float?))))

(s/def ::spotting-percent
  (s/coll-of ::fuel-percent-pair :kind vector?))

(s/def ::critical-fire-line-intensity float?)

(s/def ::surface-fire-spotting
  (s/keys :req-un [::spotting-percent ::critical-fire-line-intensity]))

(s/def ::spotting
  (s/keys :req-un [::mean-distance
                   ::flin-exp
                   ::ws-exp
                   ::normalized-distance-variance
                   ::crown-fire-spotting-percent
                   ::num-firebrands]
          :opt-un [::surface-fire-spotting]))
