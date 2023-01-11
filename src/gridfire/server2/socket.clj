(ns gridfire.server2.socket
  "A very basic, mono-threaded socket-based API into a GridFire server."
  (:require [clojure.core.async          :as async]
            [clojure.java.io             :as io]
            [clojure.string              :as str]
            [gridfire.server2.protocols  :as server2-protocols]
            [gridfire.server2.run-config :refer [start-run-config-handler!]]
            [manifold.deferred           :as mfd]
            [triangulum.logging          :refer [log-str]])
  (:import (java.io BufferedReader PrintWriter)
           (java.net ServerSocket Socket)))

(def help-message-lines
  ["This socket server lets you call an already-running GridFire program, thus avoiding the slowness of cold starts."
   "Usage: send a line formatted as one of"
   "(1) run-gridfire-config PATH_TO_MY_GRIDFIRE_CONFIG_FILE"
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

(defn- run-gridfire-config!
  [run-config-handler args =results-channel= =notifications-channel=]
  (let [[gridfire-config-path] args
        n-queued               (server2-protocols/n-queued run-config-handler)
        _                      (async/>!! =notifications-channel= (format "Scheduled for processing (behind %s queued items): %s"
                                                                          (pr-str n-queued)
                                                                          (pr-str gridfire-config-path)))
        completion-dfr         (server2-protocols/schedule-command run-config-handler gridfire-config-path =notifications-channel=)]
    (-> completion-dfr
        (mfd/chain (fn [success-data]
                     (async/put! =results-channel= (into {:gridfire.run-config/succeeded true} success-data))))
        (mfd/catch (fn [err]
                     (let [err-map (or (try
                                         (Throwable->map err)
                                         (catch Exception _parsing-err nil))
                                       {:message (str "(FAILED Throwable->map) " (ex-message err))})]
                       (async/put! =results-channel= {:gridfire.run-config/succeeded  false
                                                      :gridfire.run-config/error-data err-map})))))))

(defn- handle-input-line!
  [run-config-handler ^String input-line =results-channel= =notifications-channel=]
  (let [[cmd & args] (str/split input-line #"\s+")]
    (case cmd
      "run-gridfire-config" (run-gridfire-config! run-config-handler args =results-channel= =notifications-channel=)
      (print-help-message! =notifications-channel=))))

(def xform-results->edn
  "A transducer transforming values to EDN."
  (map (fn edn-encode-safe [res]
         (try
           (pr-str res)
           (catch Exception err
             (pr-str (tagged-literal 'gridfire.server2.socket/failed-printing-result
                                     {::ex-class-name (.getName (class err))
                                      ::ex-message    (ex-message err)})))))))

(def xform-notifs->edn-comments
  "A transducer for a sequence of notification Strings,
  which converts them to lines of EDN comments."
  (comp (mapcat str/split-lines)
        (map (fn to-edn-comment [^String notif-line]
               (str ";; " notif-line)))))

;; WARNING: the results of run-gridfire-config will produced sequentially and printed in the order of submission; however,
;; the client can still run other commands while a config is being processed,
;; which can result in notifications and results from different commands getting interleaved.
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
      (log-str (str `gridfire.server2.socket ": ready to accept connection on port " port "."))
      (loop []
        (let [client-socket (.accept server-socket)]
          (async/thread (listen-to-client! run-config-handler client-socket))
          (recur))))))

(comment

  (future (listen! 8085))

  *e)

(defn -main [port]
  (listen! (Long/parseLong port 10)))

(comment
  ;; Example client code in JVM Clojure:

  (require '[clojure.java.shell :as sh])
  (require '[clojure.edn])
  (sh/sh "clojure" "-J-XX:MaxRAMPercentage=90" "-M" "-m" "gridfire.server2.socket" "8085")
  ;;:gridfire.server2.socket/ready-to-accept-connections
  ;;01/10 16:47:06 gridfire.server2.socket: ready to accept connection on port 8085.

  (def in
    (let [client-socket (java.net.Socket. "localhost" (int 8085))
          in            (io/reader (.getInputStream client-socket))
          out           (PrintWriter. (io/writer (.getOutputStream client-socket)) true)]
      (.println out "help")
      (.println out "run-gridfire-config test/gridfire/lab/benchmarks/rhino-input-deck/gridfire.edn")
      in))

  (.readLine in)
  ;;=>
  ";; This socket server lets you call an already-running GridFire program, thus avoiding the slowness of cold starts."

  (clojure.edn/read (java.io.PushbackReader. in))
  ;;=>
  #:gridfire.run-config{:succeeded true}

  *e)