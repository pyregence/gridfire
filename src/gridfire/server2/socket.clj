(ns gridfire.server2.socket
  "A very basic, mono-threaded socket-based API into a GridFire server."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [gridfire.core :as gridfire]
            [triangulum.logging :refer [log-str]])
  (:import (java.io BufferedReader PrintWriter)
           (java.net ServerSocket)))

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

(defn print-edn-comment!
  [^PrintWriter out ^String comment-line]
  (.println out (str ";; " comment-line)))

(defn print-help-message!
  [^PrintWriter out]
  (->> help-message-lines
       (run! #(print-edn-comment! out %))))

(defn run-gridfire-config!
  [args ^PrintWriter out]
  (let [[gridfire-config-path] args]
    (try
      (let [config (gridfire/load-config! gridfire-config-path)
            _      (print-edn-comment! out "Parsed and validated the config. Loading inputs...")
            inputs (gridfire/load-inputs! config)
            _      (print-edn-comment! out "... done loading inputs. Running simulations...")
            outputs (gridfire/run-simulations! inputs)]
        (print-edn-comment! out "...done running simulations. Writing outputs...")
        (gridfire/write-outputs! outputs)
        (print-edn-comment! out "...done writing outputs.")
        (.println out (pr-str {:gridfire.run-config/succeeded true})))
      (catch Exception err
        (let [err-map (or (try
                            (Throwable->map err)
                            (catch Exception _parsing-err nil))
                          {:message (str "(FAILED Throwable->map) " (ex-message err))})
              err-str (pr-str {:gridfire.run-config/succeeded  false
                               :gridfire.run-config/error-data err-map})]
          (.println out err-str))))))

(defn handle-input-line!
  [^String input-line ^PrintWriter out]
  (let [[cmd & args] (str/split input-line #"\s+")]
    (case cmd
      "run-gridfire-config" (run-gridfire-config! args out)

      (print-help-message! out))))

(defn listen! [port]
  (with-open [server-socket (ServerSocket. (int port))]
    (prn ::ready-to-accept-connections)                     ;; So that calling programs know when to connect.
    (log-str (str `gridfire.server2.socket ": ready to accept connection on port " port "."))
    (loop []
      ;; LIMITATION: only 1 connected client at a time. (Val, 11 Jan 2023)
      (with-open [client-socket (.accept server-socket)
                  out           (PrintWriter. (io/writer client-socket) true)
                  in            (io/reader client-socket)]
        (loop []
          (when-some [l (.readLine ^BufferedReader in)]
            (handle-input-line! l out)
            (recur))))
      (recur))))

(defn -main [port]
  (listen! (Long/parseLong port 10)))

(comment
  ;; Example client code:

  (require '[clojure.java.shell :as sh])
  (sh/sh "clojure" "-M" "-m" "gridfire.server2.socket" "8085")
  ;; $ clojure -M -m gridfire.server2.socket 8085
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
