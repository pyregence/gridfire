(ns gridfire.spec.memoization
  (:require [clojure.spec.alpha :as s]))

(s/def ::surface-fire-min (s/or :no-memoization nil? :memoization-strategy #{:within-sims :across-sims}))

(s/def ::memoization
  (s/keys :opt-un [::surface-fire-min]))
