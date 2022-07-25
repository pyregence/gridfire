(ns gridfire.server
  "For exposing GridFire through a socket server, making it act as a worker process behind a job queue,
  which sends notifications to the client as the handling progresses.

  This is an in-process (not distributed), singleton (its state is namespace-anchored, being held in Vars),
  single-threaded server, which implies some usage limitations (you can't have several servers in the same JVM,
  there's no resilience to JVM crashes, and you can't scale out to several worker processes behind the job queue.)"
  (:require [clojure.core.async           :refer [>!! alts!! chan thread]]
            [clojure.data.json            :as json]
            [clojure.edn                  :as edn]
            [clojure.java.io              :as io]
            [clojure.java.shell           :as sh]
            [clojure.pprint               :refer [pprint]]
            [clojure.spec.alpha           :as spec]
            [clojure.string               :as str]
            [gridfire.active-fire-watcher :as active-fire-watcher]
            [gridfire.conversion          :refer [convert-date-string camel->kebab kebab->camel]]
            [gridfire.core                :as gridfire]
            [gridfire.simple-sockets      :as sockets]
            [gridfire.spec.server         :as server-spec]
            [gridfire.utils.server        :refer [nil-on-error throw-message]]
            [triangulum.logging           :refer [log log-str set-log-path!]])
  (:import java.text.SimpleDateFormat
           java.util.Calendar
           java.util.TimeZone))

(set! *unchecked-math* :warn-on-boxed)

;;=============================================================================
;; Request/Response Functions
;;=============================================================================

(def date-from-format "yyyy-MM-dd HH:mm zzz")
(def date-to-format   "yyyyMMdd_HHmmss")

(defn- build-geosync-request [{:keys [fire-name ignition-time] :as _request}
                              {:keys [geosync-data-dir host] :as _config}]
  (let [timestamp (convert-date-string ignition-time date-from-format date-to-format)]
    (json/write-str
     {"action"             "add"
      "dataDir"            (format "%s/%s/%s" geosync-data-dir fire-name timestamp)
      "geoserverWorkspace" (format "fire-spread-forecast_%s_%s" fire-name timestamp)
      "responseHost"       host
      "responsePort"       5555})))

(defn- send-geosync-request! [request {:keys [geosync-host geosync-port] :as config}]
  (sockets/send-to-server! geosync-host
                           geosync-port
                           (build-geosync-request request config)))

(defn- build-gridfire-response [request {:keys [host port] :as _config} status status-msg]
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
;; Process override-config
;;=============================================================================

(defn- add-ignition-start-timestamp [config ignition-date-time]
  (assoc config :ignition-start-timestamp ignition-date-time))

(defn- calc-weather-start-timestamp [ignition-date-time]
  (doto (Calendar/getInstance (TimeZone/getTimeZone "UTC"))
    (.setTime ignition-date-time)
    (.set Calendar/MINUTE 0)))

(defn- add-weather-start-timestamp [config ignition-date-time]
  (assoc config :weather-start-timestamp (calc-weather-start-timestamp ignition-date-time)))

(defn- write-config! [output-file config]
  (log-str "Writing to config file: " output-file)
  (with-open [writer (io/writer output-file)]
    (pprint config writer)))

(defn- process-override-config! [{:keys [ignition-time] :as _request} file]
  (let [formatter          (SimpleDateFormat. "yyyy-MM-dd HH:mm zzz")
        ignition-date-time (.parse formatter ignition-time)
        config             (edn/read-string (slurp file))]
    (write-config! file
                   (-> config
                       (add-ignition-start-timestamp ignition-date-time)
                       (add-weather-start-timestamp ignition-date-time)))))

;;=============================================================================
;; Shell Commands
;;=============================================================================

(def path-env (System/getenv "PATH"))

;;=============================================================================
;; Request Processing Functions
;;=============================================================================

(defn- run-cmd-map!
  [request config output-dir cmd-map]
  (when-some [response-msg (:gf-send-notif-before cmd-map)]
    (send-gridfire-response! request config 2 response-msg))
  (-> (sh/with-sh-dir output-dir
        (sh/with-sh-env {:PATH path-env}
          (let [{:keys [out err]} (apply sh/sh (:shell-cmd-args cmd-map))]
            (str out err))))
      (log :truncate? false :newline? false)))

(defn run-cmd-maps!
  [request config output-dir cmd-maps]
  (->> cmd-maps
       (run!
         (fn [cmd-map]
           (run-cmd-map! request config output-dir cmd-map)))))

(defn- run-before-cmds!
  [request config output-dir]
  (run-cmd-maps! request config output-dir (:before-gridfire-run-cmds request)))

(defn- run-after-cmds!
  [request config output-dir]
  (run-cmd-maps! request config output-dir (:after-gridfire-run-cmds request)))

(defn- copy-post-process-scripts! [from-dir to-dir]
  (log-str "Copying post-process scripts into " to-dir)
  (sh/with-sh-dir from-dir
    (run!
      (fn [sh-args] (apply sh/sh sh-args))
      (->> ["resources/elmfire_post.sh"
            "resources/make_tifs.sh"
            "resources/build_geoserver_directory.sh"
            "resources/upload_tarball.sh"
            "resources/cleanup.sh"]
           (mapv
             (fn [script-rel-path]
               ["cp" script-rel-path to-dir]))))))

(defn- build-file-name [fire-name ignition-time]
  (str/join "_" [fire-name (convert-date-string ignition-time date-from-format date-to-format) "001"]))

(defn- unzip-tar!
  "Unzips a tar file and returns the file path to the extracted folder."
  [{:keys [fire-name ignition-time type] :as _request} {:keys [data-dir incoming-dir active-fire-dir] :as _config}]
  (let [file-name (build-file-name fire-name ignition-time)]
    (log-str "Unzipping input deck: " file-name)
    (sh/with-sh-env {:PATH path-env}
      (sh/with-sh-dir (if (= type :active-fire) active-fire-dir incoming-dir)
        (sh/sh
          "tar" "-x"
          "--file" (str file-name ".tar")
          "--directory" data-dir
          "--one-top-level")))
    (.getPath (io/file data-dir file-name))))

;;TODO Try babashka's pod protocol to see if it's faster than shelling out.
(defn- process-request!
  "Runs the requested simulation using the supplied config.

  WARNING: because each simulation requires exclusive access to various resources (e.g all the processors),
  do not make several parallel calls to this function."
  [request {:keys [software-dir override-config] :as config}]
  (try
    (let [input-deck-path     (unzip-tar! request config)
          gridfire-edn-file   (.getPath (io/file input-deck-path "gridfire.edn"))
          gridfire-output-dir (.getPath (io/file input-deck-path "outputs"))]
      (process-override-config! request override-config)
      (comment
        ;; FIXME REVIEW We've lost the following logic, hopefully that's OK? (Val, 28 Jul 2022)
        ;; It means that callers will need to know when to provide -o OVERRIDE-CONFIG
        ;; when submitting requests that invoke elm_to_grid.clj.
        (if override-config
          (sh/sh "resources/elm_to_grid.clj" "-e" elmfire-data-file "-o" override-config)
          (sh/sh "resources/elm_to_grid.clj" "-e" elmfire-data-file)))
      (run-before-cmds! request config gridfire-output-dir)
      (send-gridfire-response! request config 2 "Running simulation.")
      (if (gridfire/process-config-file! gridfire-edn-file) ; Returns true on success
        (do (copy-post-process-scripts!
              software-dir
              ;; FIXME REVIEW while we're at it, let's have a sub-directory for the scripts, it will be cleaner. Is that OK?
              (str gridfire-output-dir "/gridfire_helper_scripts"))
            (run-after-cmds! request config gridfire-output-dir)
            (sh/sh "rm" "-rf" (.getAbsolutePath (io/file input-deck-path)))
            [0 "Successful run! Results uploaded to GeoServer!"])
        (throw-message "Simulation failed. No results uploaded to GeoServer.")))
    (catch Exception e
      [1 (str "Processing error: " (ex-message e))])))

(comment
  ;; Example Gridfire request invoking pre/post-process scripts:
  ;; FIXME REVIEW: is this API too expressive? Does it create security exploits? (Val, 25 Jul 2022)
  ;; Is so, we might want to make the scripts-invoking API less open-ended.
  {;; ...
   :before-gridfire-run-cmds
   [{:shell-cmd-args     ["./gridfire_helper_scripts/elm_to_grid.clj" "-e" "./elmfire.data" "-o" "./override.edn"]}]
   :after-gridfire-run-cmds
   [{:shell-cmd-args       ["./gridfire_helper_scripts/elmfire_post.sh"]
     :gf-send-notif-before "Running elmfire_post."}
    {:shell-cmd-args       ["./gridfire_helper_scripts/make_tifs.sh"]
     :gf-send-notif-before "Creating GeoTIFFs."}
    {:shell-cmd-args ["./gridfire_helper_scripts/build_geoserver_directory.sh"]}
    {:shell-cmd-args ["./gridfire_helper_scripts/cleanup.sh"]}]})

;;=============================================================================
;; Job Queue Management
;;=============================================================================

(defonce *job-queue-size      (atom 0))
(defonce *stand-by-queue-size (atom 0))

(defonce =job-queue=
         (chan 10 (map (fn [x]
                         (swap! *job-queue-size inc)
                         (delay (swap! *job-queue-size dec) x)))))

(defonce =stand-by-queue=
         (chan 10 (map (fn [x]
                         (swap! *stand-by-queue-size inc)
                         (delay (swap! *stand-by-queue-size dec) x)))))

(defonce *server-running? (atom false))

(defn- process-requests-loop!
  "Starts a logical process which listens to queued requests and processes them.

  Requests are processed in order of priority, then FIFO.

  Returns a core.async channel which will close when the server is stopped."
  [config]
  (reset! *server-running? true)
  (thread
    (loop [request @(first (alts!! [=job-queue= =stand-by-queue=] :priority true))]
      (log-str "Processing request: " request)
      (let [[status status-msg] (process-request! request config)]
        (log-str "-> " status-msg)
        (if (= (:type request) :active-fire)
          (send-geosync-request! request config)
          (send-gridfire-response! request config status status-msg)))
      (when @*server-running?
        (recur @(first (alts!! [=job-queue= =stand-by-queue=] :priority true)))))))

(defn- maybe-add-to-queue! [request]
  (try
    (if (spec/valid? ::server-spec/gridfire-server-request request)
      (do (>!! =job-queue= request)
          [2 (format "Added to job queue. You are number %d in line." @*job-queue-size)])
      [1 (str "Invalid request: " (spec/explain-str ::server-spec/gridfire-server-request request))])
    (catch AssertionError _
      [1 "Job queue limit exceeded! Dropping request!"])
    (catch Exception e
      [1 (str "Validation error: " (ex-message e))])))

(defn- parse-request-msg
  "Parses the given JSON-encoded request message, returning a request map (or nil in case of invalid JSON)."
  [request-msg]
  (-> request-msg
      (json/read-str :key-fn (comp keyword camel->kebab))
      (nil-on-error)))

;; Logically speaking, this function does (process-request! (parse-request-msg request-msg) config).
;; However, in order to both limit the load and send progress-notification responses before completion,
;; the handling goes through various queues and worker threads.
(defn- schedule-handling! [config request-msg]
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
  (active-fire-watcher/start! config =stand-by-queue=)
  (sockets/start-server! port (fn [request-msg] (schedule-handling! config request-msg)))
  (process-requests-loop! config))

(defn stop-server! []
  (reset! *server-running? false)
  (sockets/stop-server!)
  (active-fire-watcher/stop!))
