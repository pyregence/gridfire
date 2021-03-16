(ns gridfire.server
  (:require [clojure.core.async      :refer [<! >! chan go timeout]]
            [clojure.data.json       :as json]
            [clojure.edn             :as edn]
            [clojure.java.io         :as io]
            [clojure.java.shell      :as sh]
            [clojure.string          :as str]
            [clojure.tools.cli       :refer [parse-opts]]
            [gridfire.simple-sockets :as sockets]
            [gridfire.cli            :as cli]
            [gridfire.config         :as config]
            [triangulum.utils        :refer [parse-as-sh-cmd]])
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

;; TODO remove when code is in triangulum
(defn val->int
  ([val]
   (val->int val (int -1)))
  ([val default]
   (cond
     (instance? Integer val) val
     (number? val)           (int val)
     :else                   (try
                               (Integer/parseInt val)
                               (catch Exception _ (int default))))))

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
;; Main
;;-----------------------------------------------------------------------------

(defonce job-queue (chan 10))

(defn- build-file-name [fire-name ignition-time]
  (str/join "_" [fire-name (convert-date-string ignition-time) "001"]))

(defn- unzip-tar
  "Unzips tar file and returns file path to the extracted fodler"
  [{:keys [data-dir incoming-dir]} {:keys [fire-name ignition-time]}]
  (let [file-name     (build-file-name fire-name ignition-time)
        output-folder (str/join "/" [data-dir file-name])]
    (->> (sh-wrapper incoming-dir
                     {}
                     (format "mkdir -p %s" output-folder)
                     (format "tar -xvf %s -C %s" (str file-name ".tar") output-folder)))
    output-folder))

(defn process-requests! [config {:keys [host port]}]
  (go (loop [msg (<! job-queue)]
        (<! (timeout 500)) ;TODO remove
        (let [request   (json/read-str msg :key-fn (comp keyword camel->kebab))
              data-path (unzip-tar config request)]
          (println "Message:" request)
          (config/convert-config! "-c" (str data-path "/elmfire.data"))
          (cli/-main (str data-path "/gridfire.edn"))
          (sockets/send-to-server! (:response-host request)
                                   (val->int (:response-port request))
                                   (json/write-str {:fire-name     (:fire-name request)
                                                    :response-host host
                                                    :response-port port
                                                    :status        0}
                                                   :key-fn (comp kebab->camel name))))
        (recur (<! job-queue)))))

(defn handler [msg]
  (go (>! job-queue msg)))

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
      (let [port   (:port options)
            config (edn/read-string (slurp (:config options)))]
        (println (format "Running server on port %s" port))
        (sockets/start-server! port handler)
        (process-requests! config options)))))

(def -main start-server!)
