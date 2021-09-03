(ns gridfire.spec.optimization
  (:require [clojure.spec.alpha :as s]))

(s/def ::parallel-strategy #{:within-fires :between-fires})
