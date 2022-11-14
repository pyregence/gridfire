(ns gridfire.spec.suppression
  (:require [clojure.spec.alpha   :as s]
            [gridfire.spec.common :as common]))

(s/def ::suppression-dt                                number?)
(s/def ::suppression-coefficient                       number?)
(s/def ::sdi-layer                                     ::common/layer-coords)
(s/def ::sdi-sensitivity-to-difficulty                 ::common/number-sample)
(s/def ::sdi-containment-overwhelming-area-growth-rate ::common/number-sample)
(s/def ::sdi-reference-suppression-speed               ::common/number-sample)

;; explicity-samples
(s/def ::explicit-samples (s/coll-of number? :kind vector?))

(s/def ::suppression-dt-samples                                ::explicit-samples)
(s/def ::suppression-coefficient-samples                       ::explicit-samples)
(s/def ::sdi-sensitivity-to-difficult-samples                  ::explicit-samples)
(s/def ::sdi-containment-overwhelming-area-growth-rate-samples ::explicit-samples)
(s/def ::sdi-reference-suppression-speed-samples               ::explicit-samples)

(s/def ::mutually-exclusive-keys
  (fn [{:keys [suppression-coefficient sdi-layer]}]
    (or (and suppression-coefficient (nil? sdi-layer))
        (and (nil? suppression-coefficient) sdi-layer))))
