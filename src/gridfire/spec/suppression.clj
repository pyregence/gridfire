(ns gridfire.spec.suppression
  (:require [clojure.spec.alpha   :as s]
            [gridfire.spec.common :as common]))

(defn- positive-number? [x]
  (and (number? x) (pos? x)))

(s/def ::suppression-dt                                number?)
(s/def ::suppression-coefficient                       number?)
(s/def ::sdi-layer                                     ::common/layer-coords)
(s/def ::sdi-sensitivity-to-difficulty                 positive-number?)
(s/def ::sdi-containment-overwhelming-area-growth-rate positive-number?)
(s/def ::sdi-reference-suppression-speed               positive-number?)

(s/def ::mutually-exclusive-keys
  (fn [{:keys [suppression-coefficient
               sdi-layer
               sdi-sensitivity-to-difficulty
               sdi-containment-overwhelming-area-growth-rate
               sdi-reference-suppression-speed]}]
    (or (and suppression-coefficient (every? nil? [sdi-layer
                                                   sdi-sensitivity-to-difficulty
                                                   sdi-containment-overwhelming-area-growth-rate
                                                   sdi-reference-suppression-speed]))
        (and (nil? suppression-coefficient) (every? some? [sdi-layer
                                                           sdi-sensitivity-to-difficulty
                                                           sdi-containment-overwhelming-area-growth-rate
                                                           sdi-reference-suppression-speed])))))
