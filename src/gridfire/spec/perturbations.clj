(ns gridfire.spec.perturbations
  (:require [clojure.spec.alpha   :as s]
            [gridfire.spec.common :as common]))

(s/def ::spatial-type #{:global :pixel :smoothed-supergrid})

(s/def ::range ::common/number-range)

;; FIXME: Define a ::units field since it is added by elm_to_grid.clj
(s/def ::perturbation
  (s/keys :req-un [::spatial-type ::range]
          :opt-un [:gridfire.perturbation.smoothed-supergrid/supergrid-size]))

(s/def ::canopy-base-height            ::perturbation)
(s/def ::canopy-cover                  ::perturbation)
(s/def ::canopy-height                 ::perturbation)
(s/def ::crown-bulk-density            ::perturbation)
(s/def ::temperature                   ::perturbation)
(s/def ::relative-humidity             ::perturbation)
(s/def ::wind-speed-20ft               ::perturbation)
(s/def ::wind-direction                ::perturbation)
(s/def ::fuel-moisture-dead-1hr        ::perturbation)
(s/def ::fuel-moisture-dead-10hr       ::perturbation)
(s/def ::fuel-moisture-dead-100hr      ::perturbation)
(s/def ::fuel-moisture-live-herbaceous ::perturbation)
(s/def ::fuel-moisture-live-woody      ::perturbation)
(s/def ::foliar-moisture               ::perturbation)

(s/def :gridfire.perturbation.smoothed-supergrid/supergrid-size
  (s/tuple [:gridfire.perturbation.smoothed-supergrid/supergrid-size-b
            :gridfire.perturbation.smoothed-supergrid/supergrid-size-i
            :gridfire.perturbation.smoothed-supergrid/supergrid-size-j]))

(s/def :gridfire.perturbation.smoothed-supergrid/supergrid-size-b pos-int?)
(s/def :gridfire.perturbation.smoothed-supergrid/supergrid-size-i pos-int?)
(s/def :gridfire.perturbation.smoothed-supergrid/supergrid-size-j pos-int?)
