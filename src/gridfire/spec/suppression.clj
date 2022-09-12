(ns gridfire.spec.suppression
  (:require [clojure.spec.alpha   :as s]
            [gridfire.spec.common :as common]))

(s/def ::suppression-dt                                                      number?)
(s/def ::suppression-curve-sharpness                           number?)
(s/def ::suppression-difficulty-index-layer                                  ::common/layer-coords)
(s/def ::suppression-difficulty-index-calibration-coefficient                (and number? pos?))
(s/def ::suppression-difficulty-index-area-growth-rate-during-no-containment (and number? pos?))
(s/def ::suppression-difficulty-index-max-containment-per-day                (and number? pos?))

(s/def ::mutually-exclusive-keys
  (fn [{:keys [suppression-curve-sharpness
               suppression-difficulty-index-layer
               suppression-difficulty-index-calibration-coefficient
               suppression-difficulty-index-area-growth-rate-during-no-containment
               suppression-difficulty-index-max-containment-per-day]}]
    (or (and suppression-curve-sharpness (every? nil? [suppression-difficulty-index-layer
                                                                     suppression-difficulty-index-calibration-coefficient
                                                                     suppression-difficulty-index-area-growth-rate-during-no-containment
                                                                     suppression-difficulty-index-max-containment-per-day]))
        (and (nil? suppression-curve-sharpness) (every? some? [suppression-difficulty-index-layer
                                                                             suppression-difficulty-index-calibration-coefficient
                                                                             suppression-difficulty-index-area-growth-rate-during-no-containment
                                                                             suppression-difficulty-index-max-containment-per-day])))))
