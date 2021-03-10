(ns gridfire.server
  (:require [gridfire.simple-sockets :as sockets]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.core.async :refer [timeout <!! chan >! <! go]]
            [clojure.data.json :as json]))

(def cli-options
  [["-p" "--port PORT" "Port number"
    :default 31337
    :parse-fn #(if (int? %) % (Integer/parseInt %))
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]])

(defonce job-queue (chan 10))

;; TODO This process sho8uld, after receiving response from provisioning server needs to:
;; unzip tar file in incoming and put into data folder
;; run gridfire.config/write-config to convert elmfire.data -> gridfire.edn
;; run gridfire simulation with gridfire.edn
(defn process-requests! []
  (go (loop [{:keys [response-host response-port] :as message} (<! job-queue)]
        (<! (timeout 500))
        (println "Message:" message)
        ;;TODO uncoment when ready to send response.
        #_(sockets/send-to-server! response-host
                                 (if (int? response-port) response-port (Integer/parseInt response-port))
                                 (json/write-str {:message "success"}))
        (recur (<! job-queue)))))

(defn handler [msg]
  (let [request (json/read-str msg :key-fn keyword)]
    (go (>! job-queue request))))

(defn start-server! [& args]
  (let [{:keys [options summary errors]} (parse-opts args cli-options)]
    (if (or (seq errors) (empty? options))
      (do
        (when (seq errors)
          (run! println errors)
          (newline))
        (println (str "Usage:\n" summary)))
      (let [port (:port options)]
        (println (format "Running server on port %s" port))
        (sockets/start-server! port handler)
        (process-requests!)))))

(def -main start-server!)
