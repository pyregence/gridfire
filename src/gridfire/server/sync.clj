(ns gridfire.server.sync
  "A very basic, mono-threaded socket-based API into a GridFire server."
  (:require [clojure.core.async         :as async]
            [clojure.java.io            :as io]
            [clojure.string             :as str]
            [clojure.tools.cli          :as clj-cli]
            [gridfire.server.protocols  :as server-protocols]
            [gridfire.server.run-config :refer [start-run-config-handler!]]
            [triangulum.logging         :refer [log-str]])
  (:import (java.io BufferedReader PrintWriter)
           (java.net ServerSocket Socket)))

(def help-message-lines
  ["This socket server lets you call an already-running GridFire program, thus avoiding the slowness of cold starts."
   "Usage: send a line formatted as one of"
   "(1) run-config PATH_TO_MY_GRIDFIRE_CONFIG_FILE"
   "Runs simulations from a GridFire config file."
   "Upon success, you will see below a result like #:gridfire.run-config{:succeeded true}."
   "(2) help"
   "Shows this help message."
   ""
   "(Do not write the (1)/(2)/... at the beginning of the line.)"
   "The responses to your commands are EDN-encoded. This line starts with ;; because it is an EDN comment."
   "Calling programs can thus read the results as a stream of EDN data."])

;; NOTE we're using core.async as a wrapper for line-based IO.

(defn- =print-lines-channel=
  "Returns a core.async channel `=chan=` wrapping the given PrintWriter `dest`,
  so that the Strings put into `=chan=` will result in printing to `dest`."
  [^PrintWriter dest]
  (let [=ret= (async/chan)]
    (async/pipeline-blocking 1
                             (async/dropping-buffer 0)      ; this channel will receive nothing, it's only here to make async/pipeline-blocking work.
                             (comp (mapcat (fn [^String s]
                                             (str/split-lines s)))
                                   (mapcat (fn [^String l]
                                             (.println dest l)
                                             ;; HACK so that core.async does not complain of receiving nil.
                                             [])))
                             =ret=)
    =ret=))

(defn- print-help-message!
  [=notifications-channel=]
  (->> help-message-lines
       (str/join "\n")
       (async/>!! =notifications-channel=)))

(defn- run-config-file!
  [run-config-handler args =notifications-channel=]
  (let [[gridfire-config-path] args
        n-queued               (server-protocols/n-queued run-config-handler)
        _                      (async/>!! =notifications-channel= (format "Scheduled for processing (behind %s queued items): %s"
                                                                          (pr-str n-queued)
                                                                          (pr-str gridfire-config-path)))
        completion-dfr         (server-protocols/schedule-command run-config-handler gridfire-config-path =notifications-channel=)]
    (try
      (let [success-data @completion-dfr]
        (into {:gridfire.run-config/succeeded true} success-data))
      (catch Throwable err
        (let [err-map (or (try
                            (Throwable->map err)
                            (catch Throwable _parsing-err nil))
                          {:message (str "(FAILED Throwable->map) " (ex-message err))})]
          {:gridfire.run-config/succeeded  false
           :gridfire.run-config/error-data err-map})))))

(defn- handle-input-line!
  [run-config-handler ^String input-line =results-channel= =notifications-channel=]
  (let [[cmd & args] (str/split input-line #"\s+")]
    (case cmd
      "run-config" (async/>!! =results-channel=
                              (run-config-file! run-config-handler args =notifications-channel=))
      (print-help-message! =notifications-channel=))))

(def xform-results->edn
  "A transducer transforming values to EDN."
  (map (fn edn-encode-safe [res]
         (try
           (pr-str res)
           (catch Throwable err
             (pr-str (tagged-literal 'gridfire.server.sync/failed-printing-result
                                     {::ex-class-name (.getName (class err))
                                      ::ex-message    (ex-message err)})))))))

(def xform-notifs->edn-comments
  "A transducer for a sequence of notification Strings,
  which converts them to lines of EDN comments."
  (comp (mapcat str/split-lines)
        (map (fn to-edn-comment [^String notif-line]
               (str ";; " notif-line)))))

(defn- listen-to-client!
  [run-config-handler ^Socket client-socket]
  (with-open [client-socket client-socket
              out           (PrintWriter. (io/writer client-socket) true)
              in            (io/reader client-socket)]
    (let [=out=                   (=print-lines-channel= out)
          =results-channel=       (async/chan 8)
          =notifications-channel= (async/chan 8)]
      ;; Piping results and notifications to the text output:
      (async/pipeline 1
                      =out=
                      xform-results->edn
                      =results-channel=
                      false)
      (async/pipeline 1
                      =out=
                      xform-notifs->edn-comments
                      =notifications-channel=
                      false)
      (loop []
        (when-some [l (.readLine ^BufferedReader in)]
          (handle-input-line! run-config-handler l =results-channel= =notifications-channel=)
          (recur))))))

(defn- listen! [^long port]
  (let [run-config-handler (start-run-config-handler!)]
    (with-open [server-socket (ServerSocket. (int port))]
      (prn ::ready-to-accept-connections)                   ;; So that calling programs know when to connect.
      (log-str (str `gridfire.server.sync ": ready to accept connection on port " port "."))
      (loop []
        (let [client-socket (.accept server-socket)]
          (async/thread (listen-to-client! run-config-handler client-socket))
          (recur))))))

(comment

  (future (listen! 8085))

  *e)

(def cli-options
  [["-p" "--port PORT" "Port number"
    :parse-fn #(if (int? %) % (Long/parseLong % 10))
    :validate [#(< 0 % 0x10000) "The provided --port is not a number between 0 and 65536."]]])

(defn start-with-cli-args! [args]
  (let [parsed-opts (clj-cli/parse-opts args cli-options :strict true)
        errors      (concat (:errors parsed-opts)
                            (->> {:port "--port"}
                                 (keep (fn error-missing-required-option [[k opt-name]]
                                         (when-not (get (:options parsed-opts) k)
                                           (format "Missing required option: %s" opt-name))))))]
    (if (seq errors)
      (do
        (run! println errors)
        (println (str "\nUsage: " "start-sync-socket-server" "\n" (:summary parsed-opts)))
        (System/exit 1))
      (listen! (:port (:options parsed-opts))))))

(comment
  ;; Example client code in JVM Clojure:

  (require '[clojure.java.shell :as sh])
  (require '[clojure.edn])
  (sh/sh "clojure" "-M:run" "start-sync-socket-server" "--port" "8085")
  ;;:gridfire.server.sync/ready-to-accept-connections
  ;;01/10 16:47:06 gridfire.server.sync: ready to accept connection on port 8085.

  (def in
    (let [client-socket (java.net.Socket. "localhost" (int 8085))
          in            (io/reader (.getInputStream client-socket))
          out           (PrintWriter. (io/writer (.getOutputStream client-socket)) true)]
      (.println out "help")
      (.println out "run-config test/gridfire/lab/benchmarks/rhino-input-deck/gridfire.edn")
      in))

  (.readLine in)
  ;;=>
  ";; This socket server lets you call an already-running GridFire program, thus avoiding the slowness of cold starts."

  (clojure.edn/read (java.io.PushbackReader. in))
  ;;=>
  #:gridfire.run-config{:succeeded true}

  *e)
