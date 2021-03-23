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
            [triangulum.logging      :refer [log-str]]
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

;; TODO remove when code is in triangulum
(defn sh-wrapper [dir env & commands]
  (let [path-env (System/getenv "PATH")]
    (io/make-parents (str dir "/dummy"))
    (sh/with-sh-dir dir
      (sh/with-sh-env (merge {:PATH path-env} env)
        (doseq [cmd commands]
          (let [{:keys [exit err]} (apply sh/sh (parse-as-sh-cmd cmd))]
            (when-not (= 0 exit)
              (throw (ex-info err {})))))))))

;;-----------------------------------------------------------------------------
;; Server and Handler Functions
;;-----------------------------------------------------------------------------

(defonce job-queue (chan 10))

(defn- build-file-name [fire-name ignition-time]
  (str/join "_" [fire-name (convert-date-string ignition-time) "001"]))

(defn- unzip-tar
  "Unzips tar file and returns file path to the extracted folder"
  [{:keys [data-dir incoming-dir]} {:keys [fire-name ignition-time]}]
  (let [file-name (build-file-name fire-name ignition-time)]
    (sh-wrapper incoming-dir
                {}
                (format "tar -xvf %s -C %s" (str file-name ".tar") data-dir))
    (str/join "/" [data-dir file-name])))

(defn- copy-post-process-script [from-dir to-dir]
  (->> (sh-wrapper from-dir
                   {}
                   (format "cp resources/postprocess.sh %s"
                           (str to-dir "/outputs")))))

(defn- post-process-script [dir]
  (println "running post process script")
  (sh-wrapper dir {} "./postprocess.sh"))

(defn process-requests! [config {:keys [host port]}]
  (go (loop [{:keys [response-host response-port] :as request} (<! job-queue)]
        (log-str "Processing Request: " request)
        (let [[status status-msg] (try
                                    (let [input-deck-path (unzip-tar config request)]
                                      (config/convert-config! "-c" (str input-deck-path "/elmfire.data"))
                                      (cli/-main (str input-deck-path "/gridfire.edn"))
                                      (copy-post-process-script (:software-dir config) input-deck-path)
                                      (post-process-script (str input-deck-path "/outputs"))
                                      [0 "Successful Run. Results uploaded to Geoserver."])
                                    (catch Exception e
                                      [1 (str "Processing Error " (ex-message e))]))]
          (log-str "-> " status-msg)
          (sockets/send-to-server! response-host
                                   response-port
                                   (json/write-str (merge request
                                                          {:status        status
                                                           :message       status-msg
                                                           :response-host host
                                                           :response-port port})
                                                   :key-fn (comp kebab->camel name))))
        (recur (<! job-queue)))))

(defn handler [host port request-msg]
  (go
    (log-str "Request: " request-msg)
    (if-let [request (nil-on-error (json/read-str request-msg :key-fn (comp keyword camel->kebab)))]
      (when-let [[status status-msg] (try
                                       (if (spec/valid? ::spec-server/gridfire-server-request request)
                                         (do (>! job-queue request)
                                             [2 "Added to Job Queue"])
                                         [1 (str "Invalid Request: " (spec/explain-str ::spec-server/gridfire-server-request request))])
                                       (catch AssertionError _
                                         [1 "Job Queue Limit Exceeded! Dropping Request!"])
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
      (let [{:keys [host port]} options
            config              (edn/read-string (slurp (:config options)))]
        (println (format "Running server on port %s" port))
        (sockets/start-server! port (partial handler host port))
        (process-requests! config options)))))

(def -main start-server!)
