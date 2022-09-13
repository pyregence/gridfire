(ns gridfire.spec.suppression
  (:require [clojure.spec.alpha   :as s]
            [gridfire.spec.common :as common]))

(s/def ::suppression-dt                                number?)
(s/def ::suppression-curve-sharpness                   number?)
(s/def ::suppression-difficulty-index-layer            ::common/layer-coords)
(s/def ::sdi-reference-suppression-speed               (and number? pos?))
(s/def ::sdi-containment-overwhelming-area-growth-rate (and number? pos?))
(s/def ::sdi-sensitivity-to-difficulty                 (and number? pos?))

(s/def ::mutually-exclusive-keys
  (fn [{:keys [suppression-curve-sharpness
               suppression-difficulty-index-layer
               sdi-sensitivity-to-difficulty
               sdi-containment-overwhelming-area-growth-rate
               sdi-reference-suppression-speed]}]
    (or (and suppression-curve-sharpness (every? nil? [suppression-difficulty-index-layer
                                                       sdi-sensitivity-to-difficulty
                                                       sdi-containment-overwhelming-area-growth-rate
                                                       sdi-reference-suppression-speed]))
        (and (nil? suppression-curve-sharpness) (every? some? [suppression-difficulty-index-layer
                                                               sdi-sensitivity-to-difficulty
                                                               sdi-containment-overwhelming-area-growth-rate
                                                               sdi-reference-suppression-speed])))))
