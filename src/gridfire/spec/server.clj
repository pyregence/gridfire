(ns gridfire.spec.server
  (:require [clojure.spec.alpha :as spec]
            [gridfire.utils.server :refer [hostname? port? fire-name? time?]]
            [clojure.string :as str]))

(spec/def ::response-host hostname?)
(spec/def ::response-port port?)
(spec/def ::fire-name     fire-name?)
(spec/def ::ignition-time time?)
(spec/def ::type          #{:active-fire}) ; TODO: Add more types as needed

(spec/def ::gridfire-server-request
  (spec/keys
   :req-un [::fire-name ::ignition-time]
   :opt-un [::response-host
            ::response-port
            ::type
            ::before-gridfire-run-cmds
            ::after-gridfire-run-cmds]))

(spec/def ::before-gridfire-run-cmds (spec/coll-of ::cmd-map))
(spec/def ::after-gridfire-run-cmds (spec/coll-of ::cmd-map))

(spec/def ::cmd-map
  (spec/keys
    :req-un [::shell-cmd-args]
    :opt-un [::gf-send-notif-before]))

(defn valid-shell-arg?
  [s]
  (string? s))

(spec/def ::shell-cmd-args
  (spec/cat
    :shell-program-name ::shell-program-name
    :rest-of-cmd-args (spec/* valid-shell-arg?)))

(defn shell-program-name?
  [s]
  (and
    (string? s)
    (not (str/blank? s))))
(spec/def ::shell-program-name shell-program-name?)

(spec/def ::gridfire-server-response-minimal
  (spec/keys
   :req-un [::response-host ::response-port]))

(spec/def ::gf-send-notif-before string?)
