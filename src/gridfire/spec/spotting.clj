(ns gridfire.spec.spotting
  (:require [clojure.spec.alpha :as s]))

(s/def ::ambient-gas-density float?)
(s/def ::specific-heat-gas float?)
(s/def ::num-firebrands int?)

(s/def ::spotting
  (s/keys :req-un [::ambient-gas-density
                   ::specific-heat-gas
                   ::num-firebrands]))
