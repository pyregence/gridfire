;; [[file:../../../org/GridFire.org::gridfire.server.run-config][gridfire.server.run-config]]
(ns gridfire.server.run-config
  "Sequential processing of run-config requests through an in-memory job queue."
  (:require [clojure.core.async        :as async]
            [gridfire.core             :as gridfire]
            [gridfire.magellan-bridge  :refer [register-custom-projections!]]
            [gridfire.server.protocols :as server-protocols]
            [manifold.deferred         :as mfd])
  (:import (java.util.concurrent BlockingQueue LinkedBlockingDeque)))

;; NOTE why make this async job handler, when currently the only calling code is a synchronous server? Several reasons:
;; 1. even with several clients connected to the sync server, this ensures that only 1 config is run at a time;
;; 2. we might evolve to provide new access points into a running GridFire server, for example an async HTTP server.

(defn- schedule-command
  [^BlockingQueue queue config-path =notifications-channel=]
  (let [completion-dfr (mfd/deferred)]
    (.put queue [config-path =notifications-channel= completion-dfr])
    completion-dfr))

(defn- n-queued
  [^BlockingQueue queue]
  (.size queue))

(defrecord RunConfigHandler
  [queue halt-callback]
  server-protocols/JobHandler
  (schedule-command [_this command =notifications-channel=]
   (schedule-command queue command =notifications-channel=))
  (n-queued [_this]
   (n-queued queue))
  (halt [_this] (halt-callback)))

(defn- run-gridfire-config!
  [config-path =notifications-channel= completion-dfr]
  (try
    (let [_       (async/>!! =notifications-channel= (format "Starting to run-config: %s" (pr-str config-path)))
          config  (gridfire/load-config-or-throw! config-path)
          _       (async/>!! =notifications-channel= "Parsed and validated the config. Loading inputs...")
          inputs  (gridfire/load-inputs! config)
          _       (async/>!! =notifications-channel= "Running simulations...")
          outputs (gridfire/run-simulations! inputs)]
      (async/>!! =notifications-channel= "Writing outputs...")
      (gridfire/write-outputs! outputs)
      (async/>!! =notifications-channel= "... done.")
      (mfd/success! completion-dfr
                    ;; This map might get enriched in the future. (Val, 11 Jan 2023)
                    {}))
    (catch Exception err
      (mfd/error! completion-dfr err))))

(defn start-run-config-handler!
  "Starts a logical process which will process run-config commands sequentially.

  Returns a `gridfire.server.protocols/RunConfigHandler`, for which:
  - a `command` is a GridFire config path (a String);
  - =notifications-channel= will receive human-readable String messages."
  []
  (register-custom-projections!)
  (let [queue (LinkedBlockingDeque.)                        ; Using LinkedBlockingDeque as a BlockingQueue implementation because it has no size limit.
        ;; NOTE why use a queue instead of a core.async channel? Because we want to be able
        ;; to query the number of elements awaiting processing.
        *keep-handling (atom true)]
    (async/thread
     (while @*keep-handling
       (let [[config-path =notifications-channel= completion-dfr] (.take queue)]
         (run-gridfire-config! config-path =notifications-channel= completion-dfr))))
    (->RunConfigHandler queue (fn halt [] (reset! *keep-handling false)))))
;; gridfire.server.run-config ends here
