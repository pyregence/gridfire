(ns gridfire.server
  (:require [clojure.core.async           :refer [>! chan go alts!]]
            [clojure.data.json            :as json]
            [clojure.edn                  :as edn]
            [clojure.java.shell           :as sh]
            [clojure.string               :as str]
            [clojure.tools.cli            :refer [parse-opts]]
            [gridfire.active-fire-watcher :as active-fire-watcher]
            [gridfire.simple-sockets      :as sockets]
            [gridfire.cli                 :as cli]
            [gridfire.config              :as config]
            [gridfire.utils.server        :refer [nil-on-error]]
            [gridfire.spec.server         :as spec-server]
            [triangulum.logging           :refer [log-str set-log-path! log]]
            [triangulum.utils             :refer [parse-as-sh-cmd]]
            [clojure.spec.alpha           :as spec])

  (:import java.util.TimeZone))

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

;; TODO remove when code is in triangulum
(defn camel->kebab
  "Converts camelString to kebab-string"
  [camel-string]
  (as-> camel-string s
    (str/split s #"(?<=[a-z])(?=[A-Z])")
    (map str/lower-case s)
    (str/join "-" s)))

;; TODO remove when code is in triangulum
(defn kebab->camel
  "Converts kebab-string to camelString."
  [kebab-string]
  (let [words (-> kebab-string
                  (str/lower-case)
                  (str/replace #"^[^a-z_$]|[^\w-]" "")
                  (str/split #"-"))]
    (->> (map str/capitalize (rest words))
         (cons (first words))
         (str/join ""))))

(defn convert-date-string [date-str]
  (let [in-format  (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm zzz")
        out-format (doto (java.text.SimpleDateFormat. "yyyyMMdd_HHmmss")
                     (.setTimeZone (TimeZone/getTimeZone "UTC")))]
    (->> date-str
         (.parse in-format)
         (.format out-format))))

(def ^:private path-env (System/getenv "PATH"))

(defn- sh-wrapper [dir env verbose & commands]
  (sh/with-sh-dir dir
    (sh/with-sh-env (merge {:PATH path-env} env)
      (reduce (fn [acc cmd]
                (let [{:keys [out err]} (apply sh/sh (parse-as-sh-cmd cmd))]
                  (str acc (when verbose out) err)))
              ""
              commands))))

;;-----------------------------------------------------------------------------
;; Server and Handler Functions
;;-----------------------------------------------------------------------------

(defonce job-queue-size (atom 0))
(defonce stand-by-queue-size (atom 0))
(defonce job-queue (chan 10 (map (fn [x]
                                   (swap! job-queue-size inc)
                                   (delay (swap! job-queue-size dec) x)))))
(defonce stand-by-queue (chan 10 (map (fn [x]
                                        (swap! stand-by-queue-size inc)
                                        (delay (swap! stand-by-queue-size dec) x)))))

(defn- build-file-name [fire-name ignition-time]
  (str/join "_" [fire-name (convert-date-string ignition-time) "001"]))

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

(defn- build-response [request {:keys [host port]} status status-msg]
  (json/write-str (merge request
                         {:status        status
                          :message       status-msg
                          :response-host host
                          :response-port port})
                  :key-fn (comp kebab->camel name)))

(defn- send-response!
  [{:keys [response-host response-port] :as request} options status status-msg]
  (when (and response-host response-port)
    (sockets/send-to-server! response-host
                             response-port
                             (build-response request options status status-msg))))

(defn- build-geosync-request [{:keys [fire-name ignition-time]}]
  (json/write-str
   {"action"             "add"
    "dataDir"            (format "/var/www/html/fire_spread_forecast/%s/%s"
                                 fire-name
                                 (convert-date-string ignition-time))
    "geoserverWorkspace" (format "fire-spread-forecast_%s_%s"
                                 fire-name
                                 (convert-date-string ignition-time))
    "responseHost"       "gridfire.pyregence.org"
    "responsePort"       31340}))

(defn- process-requests! [config options]
  (go (loop [[val _] (alts! [job-queue stand-by-queue]
                            :priority true)]
        (let [request             @val
              _                   (log-str "Processing Request: " request)
              respond-with        (partial send-response! request options)
              [status status-msg] (try
                                    (let [input-deck-path (unzip-tar config request)]
                                      (config/convert-config! "-c" (str input-deck-path "/elmfire.data"))
                                      (respond-with 2 "Running Simulation")
                                      (cli/process-config-file! (str input-deck-path "/gridfire.edn"))
                                      (copy-post-process-script (:software-dir config) input-deck-path)
                                      (post-process-script respond-with (str input-deck-path "/outputs"))
                                      [0 "Successful Run! Results uploaded to Geoserver!"])
                                    (catch Exception e
                                      [1 (str "Processing Error " (ex-message e))]))]
          (log-str "-> " status-msg)
          (if (= (:type request) :active-fire)
            (let [geosync-request (build-geosync-request request)]
              (log-str "Sending Geosync Request:" geosync-request)
              (sockets/send-to-server! "data.pyregence.org"
                                       31337
                                       geosync-request))
            (respond-with status status-msg)))
        (recur (alts! [job-queue stand-by-queue]
                      :priority true)))))

(defn- handler [host port request-msg]
  (go
    (log-str "Request: " request-msg)
    (if-let [request (nil-on-error (json/read-str request-msg :key-fn (comp keyword camel->kebab)))]
      (when-let [[status status-msg] (try
                                       (if (spec/valid? ::spec-server/gridfire-server-request request)
                                         (do (>! job-queue request)
                                             [2 (format "Added to Job Queue. You are number %d in line."
                                                        @job-queue-size)])
                                         [1 (str "Invalid Request: " (spec/explain-str ::spec-server/gridfire-server-request request))])
                                       (catch AssertionError _
                                         [1 "Job Queue Limit Exceeded! Dropping Request!"])
                                       (catch Exception e
                                         [1 (str "Validation Error: " (ex-message e))]))]
        (log-str "-> " status-msg)
        (when (spec/valid? ::spec-server/gridfire-server-response-minimal request)
          (sockets/send-to-server! (:response-host request)
                                   (:response-port request)
                                   (json/write-str (merge request
                                                          {:status        status
                                                           :message       status-msg
                                                           :response-host host
                                                           :response-port port})
                                                   :key-fn (comp kebab->camel name)))))
      (log-str "-> Invalid JSON"))))

(def cli-options
  [["-p" "--port PORT" "Port number"
    :default 31337
    :parse-fn #(if (int? %) % (Integer/parseInt %))
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]

   ["-h" "--host HOST" "Host domain name"
    :default "gridfire.pyregence.org"]

   ["-c" "--config CONFIG" "Server config file"
    :missing "You must provide a server config edn"]])

(defn stop-server! []
  (sockets/stop-server!))

(defn start-server! [& args]
  (let [{:keys [options summary errors]} (parse-opts args cli-options)]
    (if (or (seq errors) (empty? options))
      (do
        (when (seq errors)
          (run! println errors)
          (newline))
        (println (str "Usage:\n" summary))
        (shutdown-agents))
      (let [{:keys [host port]}          options
            {:keys [log-dir] :as config} (edn/read-string (slurp (:config options)))]
        (when log-dir (set-log-path! log-dir))
        (log-str (format "Running server on port %s" port))
        (active-fire-watcher/start! config stand-by-queue)
        (sockets/start-server! port (partial handler host port))
        (process-requests! config options)))))

(def -main start-server!)
