;; [[file:../../org/GridFire.org::command-line-interface][command-line-interface]]
(ns gridfire.cli
  (:gen-class)
  (:require [clojure.core.async    :refer [<!!]]
            [clojure.edn           :as edn]
            [clojure.java.io       :as io]
            [clojure.tools.cli     :refer [parse-opts]]
            [gridfire.core         :as gridfire]
            [gridfire.server       :as server]
            [gridfire.utils.server :refer [hostname? nil-on-error]]))

(def cli-options
  [["-c" "--server-config CONFIG" "Server config file"
    :validate [#(.exists  (io/file %)) "The provided --server-config does not exist."
               #(.canRead (io/file %)) "The provided --server-config is not readable."]]

   ["-h" "--host HOST" "Host domain name"
    :validate [hostname? "The provided --host is invalid."]]

   ["-p" "--port PORT" "Port number"
    :parse-fn #(if (int? %) % (Integer/parseInt %))
    :validate [#(< 0 % 0x10000) "The provided --port is not a number between 0 and 65536."]]])

(def program-banner
  (str "gridfire: Launch fire spread simulations via config files or in server mode.\n"
       "Copyright © 2014-2022 Spatial Informatics Group, LLC.\n"))

(defn -main [& args]
  (println program-banner)
  (let [{:keys [options arguments summary errors]} (parse-opts args cli-options)]
    ;; {:options   The options map, keyed by :id, mapped to the parsed value
    ;;  :arguments A vector of unprocessed arguments
    ;;  :summary   A string containing a minimal options summary
    ;;  :errors    A vector of error message strings thrown during parsing; nil when no errors exist
    (cond
      ;; Errors encountered during input parsing
      (seq errors)
      (do
        (run! println errors)
        (println (str "\nUsage:\n" summary)))

      ;; Server mode invoked
      (every? options [:server-config :host :port])
      (if-let [config-file-params (nil-on-error (edn/read-string (slurp (:server-config options))))]
        (<!! (server/start-server! (merge config-file-params (dissoc options :server-config))))
        (do
          (println (:server-config options) "does not contain well-formed EDN.")
          (println (str "\nUsage:\n" summary))))

      ;; CLI mode invoked
      (seq arguments)
      (doseq [config-file arguments]
        (gridfire/process-config-file! config-file))

      ;; Incorrect CLI invocation
      :else
      (do
        (println "For gridfire cli mode, include one or more gridfire.edn files.")
        (println "For gridfire server mode, include these args: --server-config --host --port")
        (println (str "\nUsage:\n" summary))))

    ;; Exit cleanly
    (System/exit 0)))
;; command-line-interface ends here
