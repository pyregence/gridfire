;; FIXME LP coverage
#!/usr/bin/env bb
(ns gridfire.server.sync.call
  "A zero-deps script for invoking a running `gridfire.server.sync`.

  More precisely, for executing run-config commands

  Can be run via clojure -M / java -classpath / babashka / babashka --classpath / ...
  see the (comment ...) at the end of this file."
  (:require [clojure.edn     :as edn]
            [clojure.java.io :as io]
            [clojure.pprint  :refer [pprint]]
            [clojure.string  :as str])
  (:import (java.io PrintWriter PushbackReader)
           (java.net Socket))
  (:gen-class))

(defn call-sync-server-with-run-config!
  [^String host ^String port ^String config-file]
  (with-open [client-socket (Socket. host (int (Integer/parseInt port 10)))
              in            (PushbackReader. (io/reader (.getInputStream client-socket)))
              out           (PrintWriter. (io/writer (.getOutputStream client-socket)) true)]
    (.println out (str/join " " ["run-config" config-file]))
    (edn/read {:default tagged-literal} ;; This makes the EDN parsing tolerant of any tagged literal, which lets this program convey it without knowing what it means.
              in)))

;; NOTE why not make this a subpath of gridfire.cli? Because:
;; 1) that would add a big startup time (loading all of GridFire's codebase);
;; 2) that would make it impossible to run through Babashka.
(defn -main [host port config-file]
  (let [result (call-sync-server-with-run-config! host port config-file)]
    (if (:gridfire.run-config/succeeded result)
      (do
        (pprint result)
        (System/exit 0))
      (binding [*out* *err*]
        (pprint result)
        (System/exit 1)))))

;; Inspiration: https://book.babashka.org/#main_file
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

(comment
  ;; HOW TO RUN THIS SCRIPT (assuming a running gridfire.server.sync on localhost:8085):
  ;; clojure -M -m gridfire.server.sync.call                                 localhost 8085 path/to/my/gridfire.edn
  ;; bb src/gridfire/server/sync/call.clj                                    localhost 8085 path/to/my/gridfire.edn
  ;; java -classpath my-gridfire-uberjar.jar gridfire.server.sync.call       localhost 8085 path/to/my/gridfire.edn
  ;; bb --classpath my-gridfire-uberjar.jar --main gridfire.server.sync.call localhost 8085 path/to/my/gridfire.edn

  *e)
