(ns gridfire.server
  (:require [clojure.core.async           :refer [>! alts! chan go]]
            [clojure.data.json            :as json]
            [clojure.java.shell           :as sh]
            [clojure.spec.alpha           :as spec]
            [clojure.string               :as str]
            [gridfire.active-fire-watcher :as active-fire-watcher]
            [gridfire.config              :as config]
            [gridfire.conversion          :refer [convert-date-string camel->kebab kebab->camel]]
            [gridfire.core                :as gridfire]
            [gridfire.simple-sockets      :as sockets]
            [gridfire.spec.server         :as spec-server]
            [gridfire.utils.server        :refer [nil-on-error]]
            [triangulum.logging           :refer [log log-str set-log-path!]]
            [triangulum.utils             :refer [parse-as-sh-cmd]]))

;;=============================================================================
;; Shell Commands
;;=============================================================================

(def path-env (System/getenv "PATH"))

(defn- sh-wrapper [dir env verbose & commands]
  (sh/with-sh-dir dir
    (sh/with-sh-env (merge {:PATH path-env} env)
      (reduce (fn [acc cmd]
                (let [{:keys [out err]} (apply sh/sh (parse-as-sh-cmd cmd))]
                  (str acc (when verbose out) err)))
              ""
              commands))))

;;=============================================================================
;; Server and Handler Functions
;;=============================================================================

;; RESUME HERE: Need to review: send-response! unzip-tar
;; copy-post-process-script post-process-script build-geosync-request

(def date-from-format "yyyy-MM-dd HH:mm zzz")
(def date-to-format   "yyyyMMdd_HHmmss")

(defn- build-file-name [fire-name ignition-time]
  (str/join "_" [fire-name (convert-date-string ignition-time date-from-format date-to-format) "001"]))

(defn- unzip-tar
  "Unzips tar file and returns file path to the extracted folder"
  [{:keys [data-dir incoming-dir active-fire-dir]} {:keys [fire-name ignition-time type]}]
  (let [file-name (build-file-name fire-name ignition-time)]
    (log-str "Unzipping input deck: " file-name)
    (sh-wrapper (if (= type :active-fire) active-fire-dir incoming-dir)
                {}
                false
                (format "tar -xf %s -C %s --one-top-level" (str file-name ".tar") data-dir))
    (str/join "/" [data-dir file-name])))

(defn- copy-post-process-script [from-dir to-dir]
  (let [output-dir (str to-dir "/outputs")]
    (log-str "copying post process script into:" output-dir)
    (sh-wrapper from-dir
                {}
                false
                (format "cp resources/elmfire_post.sh %s" output-dir)
                (format "cp resources/make_tifs.sh %s" output-dir)
                (format "cp resources/build_geoserver_directory.sh %s" output-dir)
                (format "cp resources/upload_tarball.sh %s" output-dir)
                (format "cp resources/cleanup.sh %s" output-dir))))

(defn- post-process-script [respond-with! dir]
  (log-str "Running post process script")
  (let [commands [["./elmfire_post.sh" "Running elmfire_post processing"]
                  ["./make_tifs.sh" "Creating Geotiffs"]
                  ["./build_geoserver_directory.sh"]
                  ["./upload_tarball.sh"]
                  ["./cleanup.sh"]]]
    (doseq [[cmd response-msg] commands]
      (when response-msg
        (respond-with! 2 response-msg))
      (-> (sh-wrapper dir {} true cmd)
          (#(log % :truncate? false :newline? false))))))

(defn- build-geosync-request [{:keys [fire-name ignition-time]} {:keys [host port]}]
  (let [timestamp (convert-date-string ignition-time date-from-format date-to-format)]
    (json/write-str
     {"action"             "add"
      "dataDir"            (format "/var/www/html/fire_spread_forecast/%s/%s" fire-name timestamp)
      "geoserverWorkspace" (format "fire-spread-forecast_%s_%s" fire-name timestamp)
      "responseHost"       host
      "responsePort"       port})))

(defn- send-geosync-request!
  [{:keys [response-host response-port] :as geosync-server-config} request config]
  (when (spec/valid? ::spec-server/gridfire-server-response-minimal geosync-server-config)
    (sockets/send-to-server! response-host
                             response-port
                             (build-geosync-request request config))))

(defn- build-gridfire-response [request {:keys [host port]} status status-msg]
  (json/write-str (merge request
                         {:status        status
                          :message       status-msg
                          :response-host host
                          :response-port port})
                  :key-fn (comp kebab->camel name)))

(defn- send-gridfire-response!
  [{:keys [response-host response-port] :as request} config status status-msg]
  (when (spec/valid? ::spec-server/gridfire-server-response-minimal request)
    (sockets/send-to-server! response-host
                             response-port
                             (build-gridfire-response request config status status-msg))))

;;=============================================================================
;; Job Queue Management
;;=============================================================================

(defonce job-queue-size      (atom 0))
(defonce stand-by-queue-size (atom 0))

(defonce job-queue      (chan 10 (map (fn [x]
                                        (swap! job-queue-size inc)
                                        (delay (swap! job-queue-size dec) x)))))

(defonce stand-by-queue (chan 10 (map (fn [x]
                                        (swap! stand-by-queue-size inc)
                                        (delay (swap! stand-by-queue-size dec) x)))))

;; FIXME: Pass these values in through gridfire.cli rather than hardcoding them.
(def geosync-server-config {:response-host "data.pyregence.org" :response-port 31337})

;; FIXME: Should this be in a go block? Check if gridfire.cli terminates early from shutdown-agents.
(defn- process-requests! [config]
  (go (loop [request @(first (alts! [job-queue stand-by-queue] :priority true))]
        (log-str "Processing Request: " request)
        (let [respond-with        (partial send-gridfire-response! request config)
              [status status-msg] (try
                                    (let [input-deck-path (unzip-tar config request)]
                                      (config/convert-config!
                                       {:elmfire-data-file (str input-deck-path "/elmfire.data")})
                                      (respond-with 2 "Running Simulation")
                                      (gridfire/process-config-file! (str input-deck-path "/gridfire.edn"))
                                      (copy-post-process-script (:software-dir config) input-deck-path)
                                      (post-process-script respond-with (str input-deck-path "/outputs"))
                                      [0 "Successful Run! Results uploaded to GeoServer!"])
                                    (catch Exception e
                                      [1 (str "Processing Error: " (ex-message e))]))]
          (log-str "-> " status-msg)
          (if (= (:type request) :active-fire)
            (do
              (log-str "Sending GeoSync Request.")
              (send-geosync-request! geosync-server-config request config))
            (respond-with status status-msg)))
        (recur @(first (alts! [job-queue stand-by-queue] :priority true))))))

(defn- handler [config request-msg]
  (go
    (log-str "Request: " request-msg)
    (if-let [request (nil-on-error (json/read-str request-msg :key-fn (comp keyword camel->kebab)))]
      (when-let [[status status-msg] (try
                                       (if (spec/valid? ::spec-server/gridfire-server-request request)
                                         (do (>! job-queue request)
                                             [2 (format "Added to Job Queue. You are number %d in line."
                                                        @job-queue-size)])
                                         [1 (str "Invalid Request: "
                                                 (spec/explain-str ::spec-server/gridfire-server-request request))])
                                       (catch AssertionError _
                                         [1 "Job Queue Limit Exceeded! Dropping Request!"])
                                       (catch Exception e
                                         [1 (str "Validation Error: " (ex-message e))]))]
        (log-str "-> " status-msg)
        (send-gridfire-response! request config status status-msg))
      (log-str "-> Invalid JSON"))))

(defn stop-server! []
  (sockets/stop-server!))

(defn start-server! [{:keys [log-dir port] :as config}]
  (when log-dir (set-log-path! log-dir))
  (log-str "Running server on port " port)
  (active-fire-watcher/start! config stand-by-queue)
  (sockets/start-server! port (partial handler config))
  (process-requests! config))
