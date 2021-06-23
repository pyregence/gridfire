(ns gridfire.spec.server
  (:require [clojure.spec.alpha :as spec]
            [gridfire.utils.server :refer [hostname? port? time? fire-name?]]))

(spec/def ::response-host                   hostname?)
(spec/def ::response-port                   port?)
(spec/def ::fire-name                       fire-name?)
(spec/def ::ignition-time                   time?)
(spec/def ::gridfire-server-request         (spec/keys :req-un [::fire-name
                                                                ::ignition-time]
                                                       :req-opt [::response-host
                                                                 ::response-port]))

(spec/def ::gridfire-server-response-minimal (spec/keys :req-un [::response-host
                                                                 ::response-port]))
