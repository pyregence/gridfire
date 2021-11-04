(ns gridfire.spec.perturbations
  (:require [clojure.spec.alpha   :as s]
            [gridfire.spec.common :as common]))

(s/def ::spatial-type #{:global :pixel})

(s/def ::range ::common/number-range)

(s/def ::frequency integer?)

(s/def ::perturbation
  (s/keys :req-un [::spatial-type ::range]
          :opt-un [::frequency]))

(s/def ::canopy-base-height ::perturbation)
(s/def ::canopy-cover       ::perturbation)
(s/def ::canopy-height      ::perturbation)
(s/def ::crown-bulk-density ::perturbation)
(s/def ::temperature        ::perturbation)
(s/def ::relative-humidity  ::perturbation)
(s/def ::wind-speed-20ft    ::perturbation)
(s/def ::wind-direction     ::perturbation)
