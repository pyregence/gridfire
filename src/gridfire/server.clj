(ns gridfire.server
  (:require [clojure.core.async      :refer [<! >! chan go]]
            [clojure.data.json       :as json]
            [clojure.edn             :as edn]
            [clojure.java.io         :as io]
            [clojure.java.shell      :as sh]
            [clojure.string          :as str]
            [clojure.tools.cli       :refer [parse-opts]]
            [gridfire.simple-sockets :as sockets]
            [gridfire.cli            :as cli]
            [gridfire.config         :as config]
            [gridfire.utils.server   :refer [nil-on-error]]
            [gridfire.spec.server    :as spec-server]
            [triangulum.logging      :refer [log-str set-log-path! log]]
            [triangulum.utils        :refer [parse-as-sh-cmd]]
            [clojure.spec.alpha      :as spec])
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

(defonce job-queue (chan 10))
(defonce job-queue-count 0)

(defn- build-file-name [fire-name ignition-time]
  (str/join "_" [fire-name (convert-date-string ignition-time) "001"]))

(defn- unzip-tar
  "Unzips tar file and returns file path to the extracted folder"
  [{:keys [data-dir incoming-dir]} {:keys [fire-name ignition-time]}]
  (let [file-name (build-file-name fire-name ignition-time)]
    (log-str "Unzipping input deck: " file-name)
    (sh-wrapper incoming-dir
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
  (let [commands [["./elmfire_post.sh" "GridFire: Running elmfire_post processing"]
                  ["./make_tifs.sh" "GridFire: Creating Geotiffs"]
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
  (sockets/send-to-server! response-host
                           response-port
                           (build-response request options status status-msg)))

(defn- process-requests! [config options]
  (go (loop [request (<! job-queue)]
        (swap! job-queue-count dec)
        (log-str "Processing Request: " request)
        (let [respond-with        (partial send-response! request options)
              [status status-msg] (try
                                    (let [input-deck-path (unzip-tar config request)]
                                      (config/convert-config! "-c" (str input-deck-path "/elmfire.data"))
                                      (respond-with 2 "GridFire: Running Simulation")
                                      (cli/-main (str input-deck-path "/gridfire.edn"))
                                      (copy-post-process-script (:software-dir config) input-deck-path)
                                      (post-process-script respond-with (str input-deck-path "/outputs"))
                                      [0 "GridFire: Successful Run! Results uploaded to Geoserver!"])
                                    (catch Exception e
                                      [1 (str "Processing Error " (ex-message e))]))]
          (log-str "-> " status-msg)
          (respond-with status status-msg))
        (recur (<! job-queue)))))

(defn- handler [host port request-msg]
  (go
    (log-str "Request: " request-msg)
    (if-let [request (nil-on-error (json/read-str request-msg :key-fn (comp keyword camel->kebab)))]
      (when-let [[status status-msg] (try
                                       (if (spec/valid? ::spec-server/gridfire-server-request request)
                                         (do (>! job-queue request)
                                             (swap! job-queue-count inc)
                                             [2 (format "GridFire: Added to Job Queue. You are %d in line."
                                                        @job-queue-count)])
                                         [1 (str "Invalid Request: " (spec/explain-str ::spec-server/gridfire-server-request request))])
                                       (catch AssertionError _
                                         [1 "GridFire: Job Queue Limit Exceeded! Dropping Request!"])
                                       (catch Exception e
                                         [1 (str "Validation Error: " (ex-message e))]))]
        (log-str "-> " status-msg)
        (when (spec/valid? ::spec-server/gridfire-server-request-minimal request)
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
        (println (str "Usage:\n" summary)))
      (let [{:keys [host port]}          options
            {:keys [log-dir] :as config} (edn/read-string (slurp (:config options)))]
        (when log-dir (set-log-path! log-dir))
        (log (format "Running server on port %s" port) :force-stdout? true)
        (sockets/start-server! port (partial handler host port))
        (process-requests! config options)))))

(def -main start-server!)
