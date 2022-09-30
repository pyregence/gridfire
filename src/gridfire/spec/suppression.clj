(ns gridfire.spec.suppression
  (:require [clojure.spec.alpha   :as s]
            [gridfire.spec.common :as common]))

(defn- positive-number? [x]
  (and (number? x) (pos? x)))

(s/def ::suppression-dt                                number?)
(s/def ::suppression-curve-sharpness                   number?)
(s/def ::sdi-layer                                     ::common/layer-coords)
(s/def ::sdi-sensitivity-to-difficulty                 positive-number?)
(s/def ::sdi-containment-overwhelming-area-growth-rate positive-number?)
(s/def ::sdi-reference-suppression-speed               positive-number?)

(s/def ::mutually-exclusive-keys
  (fn [{:keys [suppression-curve-sharpness sdi-layer]}]
    (or (and suppression-curve-sharpness sdi-layer)
        (and (nil? suppression-curve-sharpness) sdi-layer))))
