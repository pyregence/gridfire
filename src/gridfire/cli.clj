;; [[file:../../org/GridFire.org::command-line-interface][command-line-interface]]
(ns gridfire.cli
  (:gen-class)
  (:require [clojure.core.async    :refer [<!!]]
            [clojure.edn           :as edn]
            [clojure.java.io       :as io]
            [clojure.tools.cli     :refer [parse-opts]]
            [gridfire.config       :as config]
            [gridfire.core         :as gridfire]
            [gridfire.server       :as server]
            [gridfire.utils.server :refer [hostname? throw-message]]))

(set! *unchecked-math* :warn-on-boxed)

;;===========================================================
;; Argument Processing
;;===========================================================

(defn all-required-keys? [arguments options]
  (or (seq arguments)
      (every? options [:server-config :host :port])
      (:elmfire-data options)))

(defn process-options [arguments {:keys [server-config] :as options}]
  (cond (not (all-required-keys? arguments options))
        (throw-message (str "For gridfire cli mode, include "
                            "one or more gridfire.edn files.\n"
                            "For gridfire server mode, include these args: "
                            "--server-config --host --port\n"
                            "For converting elmfire.data to gridfire.edn, include this arg: "
                            "--elmfire-data"))

        server-config
        (let [config-file-params  (edn/read-string (slurp server-config))
              command-line-params (dissoc options :server-config)]
          (merge config-file-params command-line-params))

        :else
        options))

;;===========================================================
;; User Interface
;;===========================================================

(def cli-options
  [["-c" "--server-config CONFIG" "Server config file"
    :validate [#(.exists  (io/file %)) "The provided --server-config does not exist."
               #(.canRead (io/file %)) "The provided --server-config is not readable."]]

   ["-h" "--host HOST" "Host domain name"
    :validate [hostname? "The provided --host is invalid."]]

   ["-p" "--port PORT" "Port number"
    :parse-fn #(if (int? %) % (Integer/parseInt %))
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]

   ["-e" "--elmfire-data FILE" "Path to an elmfire.data file"
    :validate [#(.exists  (io/file %)) "The provided --elmfire-data does not exist."
               #(.canRead (io/file %)) "The provided --elmfire-data is not readable."]]

   ["-v" "--verbose" "Flag for controlling elmfire.data conversion output params"]

   ["-o" "--override-config OVERRIDE" "Path to override.edn file"
    :validate [#(.exists  (io/file %)) "The provided --override-config does not exist."
               #(.canRead (io/file %)) "The provided --override-config is not readable."]]])

(def program-banner
  (str "gridfire: Launch fire spread simulations via config files or in server mode.\n"
       "Copyright © 2014-2021 Spatial Informatics Group, LLC.\n"))

(defn -main
  [& args]
  (println program-banner)
  (let [{:keys [options arguments summary errors]} (parse-opts args cli-options)
        ;; {:options   The options map, keyed by :id, mapped to the parsed value
        ;;  :arguments A vector of unprocessed arguments
        ;;  :summary   A string containing a minimal options summary
        ;;  :errors    A vector of error message strings thrown during parsing; nil when no errors exist
        config-params (try
                        (process-options arguments options)
                        (catch Exception e
                          (ex-message e)))]
    (cond
      (seq errors)
      (do
        (run! println errors)
        (println (str "\nUsage:\n" summary)))

      (string? config-params)
      (do
        (println config-params)
        (println (str "\nUsage:\n" summary)))

      (:elmfire-data options)
      (config/convert-config! (:elmfire-data options) (:override-config options))

      (:server-config options)
      (<!! (server/start-server! config-params))

      :else
      (doseq [config-file arguments]
        (gridfire/process-config-file! config-file)))
    (System/exit 0)))
;; command-line-interface ends here
