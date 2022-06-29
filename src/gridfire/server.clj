(ns gridfire.server
  (:require [clojure.core.async           :refer [<! >! >!! alts! chan go thread]]
            [clojure.data.json            :as json]
            [clojure.java.io              :as io]
            [clojure.java.shell           :as sh]
            [clojure.spec.alpha           :as spec]
            [clojure.string               :as str]
            [gridfire.active-fire-watcher :as active-fire-watcher]
            [gridfire.config              :as config]
            [gridfire.conversion          :refer [convert-date-string camel->kebab kebab->camel]]
            [gridfire.core                :as gridfire]
            [gridfire.simple-sockets      :as sockets]
            [gridfire.spec.server         :as server-spec]
            [gridfire.utils.server        :refer [nil-on-error throw-message]]
            [triangulum.logging           :refer [log log-str set-log-path!]]
            [triangulum.utils             :refer [parse-as-sh-cmd]]))

(set! *unchecked-math* :warn-on-boxed)

;;=============================================================================
;; Request/Response Functions
;;=============================================================================

(def date-from-format "yyyy-MM-dd HH:mm zzz")
(def date-to-format   "yyyyMMdd_HHmmss")

(defn- build-geosync-request [{:keys [fire-name ignition-time]} {:keys [host port]}]
  (let [timestamp (convert-date-string ignition-time date-from-format date-to-format)]
    (json/write-str
     {"action"             "add"
      "dataDir"            (format "/var/www/html/fire_spread_forecast/%s/%s" fire-name timestamp)
      "geoserverWorkspace" (format "fire-spread-forecast_%s_%s" fire-name timestamp)
      "responseHost"       host
      "responsePort"       port})))

;; FIXME: Pass the geosync-server-config values in through gridfire.cli rather than hardcoding them.
(defn- send-geosync-request! [request config]
  (let [geosync-server-config {:response-host "data.pyregence.org" :response-port 31337}]
    (when (spec/valid? ::server-spec/gridfire-server-response-minimal geosync-server-config)
      (sockets/send-to-server! (:response-host geosync-server-config)
                               (:response-port geosync-server-config)
                               (build-geosync-request request config)))))

(defn- build-gridfire-response [request {:keys [host port]} status status-msg]
  (json/write-str (merge request
                         {:status        status
                          :message       status-msg
                          :response-host host
                          :response-port port})
                  :key-fn (comp kebab->camel name)))

(defn- send-gridfire-response! [{:keys [response-host response-port] :as request} config status status-msg]
  (when (spec/valid? ::server-spec/gridfire-server-response-minimal request)
    (sockets/send-to-server! response-host
                             response-port
                             (build-gridfire-response request config status status-msg))))

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
;; Request Processing Functions
;;=============================================================================

(defn- run-post-process-scripts! [request config output-dir]
  (log-str "Running post-process scripts.")
  (let [commands [["./elmfire_post.sh" "Running elmfire_post."]
                  ["./make_tifs.sh" "Creating GeoTIFFs."]
                  ["./build_geoserver_directory.sh"]
                  ["./upload_tarball.sh"]
                  ["./cleanup.sh"]]]
    (doseq [[cmd response-msg] commands]
      (when response-msg
        (send-gridfire-response! request config 2 response-msg))
      (-> (sh-wrapper output-dir {} true cmd)
          (log :truncate? false :newline? false)))))

(defn- copy-post-process-scripts! [from-dir to-dir]
  (log-str "Copying post-process scripts into " to-dir)
  (sh-wrapper from-dir
              {}
              false
              (str "cp resources/elmfire_post.sh " to-dir)
              (str "cp resources/make_tifs.sh " to-dir)
              (str "cp resources/build_geoserver_directory.sh " to-dir)
              (str "cp resources/upload_tarball.sh " to-dir)
              (str "cp resources/cleanup.sh " to-dir)))

(defn- build-file-name [fire-name ignition-time]
  (str/join "_" [fire-name (convert-date-string ignition-time date-from-format date-to-format) "001"]))

(defn- unzip-tar!
  "Unzips a tar file and returns the file path to the extracted folder."
  [{:keys [fire-name ignition-time type]} {:keys [data-dir incoming-dir active-fire-dir]}]
  (let [file-name (build-file-name fire-name ignition-time)]
    (log-str "Unzipping input deck: " file-name)
    (sh-wrapper (if (= type :active-fire) active-fire-dir incoming-dir)
                {}
                false
                (format "tar -xf %s -C %s --one-top-level" (str file-name ".tar") data-dir))
    (.getPath (io/file data-dir file-name))))

;; TODO: Remove gridfire.config from the code base and use resources/elm_to_grid.clj instead.
;;       Try babashka's pod protocol to see if it's faster than shelling out.
(defn- process-request! [request {:keys [software-dir override-config] :as config}]
  (try
    (let [input-deck-path     (unzip-tar! request config)
          elmfire-data-file   (.getPath (io/file input-deck-path "elmfire.data"))
          gridfire-edn-file   (.getPath (io/file input-deck-path "gridfire.edn"))
          gridfire-output-dir (.getPath (io/file input-deck-path "outputs"))]
      (config/convert-config! elmfire-data-file override-config)
      (send-gridfire-response! request config 2 "Running simulation.")
      (if (gridfire/process-config-file! gridfire-edn-file) ; Returns true on success
        (do (copy-post-process-scripts! software-dir gridfire-output-dir)
            (run-post-process-scripts! request config gridfire-output-dir)
            [0 "Successful run! Results uploaded to GeoServer!"])
        (throw-message "Simulation failed. No results uploaded to GeoServer.")))
    (catch Exception e
      [1 (str "Processing error: " (ex-message e))])))

;;=============================================================================
;; Job Queue Management
;;=============================================================================

(defonce *job-queue-size      (atom 0))
(defonce *stand-by-queue-size (atom 0))

(defonce job-queue      (chan 10 (map (fn [x]
                                        (swap! *job-queue-size inc)
                                        (delay (swap! *job-queue-size dec) x)))))

(defonce stand-by-queue (chan 10 (map (fn [x]
                                        (swap! *stand-by-queue-size inc)
                                        (delay (swap! *stand-by-queue-size dec) x)))))

(defonce *server-running? (atom false))

(defn- process-requests! [config]
  (reset! *server-running? true)
  (go
    (loop [request @(first (alts! [job-queue stand-by-queue] :priority true))]
      (<!
        (thread
          (comment "we use" (thread ...) "here because some operations are blocking, as explained in" (doc go) ".")
          (log-str "Processing request: " request)
          (let [[status status-msg] (process-request! request config)]
            (log-str "-> " status-msg)
            (if (= (:type request) :active-fire)
              (send-geosync-request! request config)
              (send-gridfire-response! request config status status-msg)))))
      (when @*server-running?
        (recur @(first (alts! [job-queue stand-by-queue] :priority true)))))))

(defn- maybe-add-to-queue! [request]
  (try
    (if (spec/valid? ::server-spec/gridfire-server-request request)
      (do (>!! job-queue request)
          [2 (format "Added to job queue. You are number %d in line." @*job-queue-size)])
      [1 (str "Invalid request: " (spec/explain-str ::server-spec/gridfire-server-request request))])
    (catch AssertionError _
      [1 "Job queue limit exceeded! Dropping request!"])
    (catch Exception e
      [1 (str "Validation error: " (ex-message e))])))


(defn- parse-request-msg
  "Parses the given JSON-encoded request message, returning a request map (or nil in case of invalid JSON)."
  [^String request-msg]
  (comment "Returns either a" ::server-spec/gridfire-server-request "or" nil "in case of a deserialization error.")
  (-> request-msg
      (json/read-str :key-fn (comp keyword camel->kebab))
      (nil-on-error)))


(defn- schedule-handling! [config request-msg]
  (comment "Logically speaking, this function does" (process-request! (parse-request-msg request-msg) config) "."
           "However, in order to both limit the load and send progress-notification responses before completion,"
           "the handling goes through various queues and worker threads.")
  (thread
    (log-str "Request: " request-msg)
    (if-let [request (parse-request-msg request-msg)]
      (let [[status status-msg] (maybe-add-to-queue! request)]
        (log-str "-> " status-msg)
        (send-gridfire-response! request config status status-msg))
      (log-str "-> Invalid JSON"))))

;;=============================================================================
;; Start/Stop Servers
;;=============================================================================

(defn start-server! [{:keys [log-dir port] :as config}]
  (when log-dir (set-log-path! log-dir))
  (log-str "Running server on port " port)
  (active-fire-watcher/start! config stand-by-queue)
  (sockets/start-server! port (fn [request-msg] (schedule-handling! config request-msg)))
  (process-requests! config))

(defn stop-server! []
  (reset! *server-running? false)
  (sockets/stop-server!)
  (active-fire-watcher/stop!))
