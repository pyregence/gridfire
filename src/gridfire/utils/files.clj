(ns gridfire.utils.files
  "File-system utilities for GridFire."
  (:require [clojure.edn     :as edn]
            [clojure.java.io :as io]
            [clojure.walk    :as walk])
  (:import java.io.File))

(defn- build-absolute-path
  [^String current-dir-path, ^String rel-path]
  (let [^File file (apply io/file current-dir-path (if (string? rel-path)
                                                     [rel-path]
                                                     rel-path))]
    (.getCanonicalPath file)))

(defn location-aware-edn-readers
  "Returns a map of EDN :readers such that
  #gridfire.utils.files/from-this-file ... will work
  and be resolved as paths relative to the directory
  containing the same file."
  [current-file-path]
  (let [current-file     (io/file current-file-path)
        current-dir-path (.getParent current-file)]
    {'gridfire.utils.files/from-this-file                   ; INTRO
     (fn [rel-path]
       (build-absolute-path current-dir-path rel-path))}))

(comment

  (edn/read-string
   {:readers (location-aware-edn-readers (System/getProperty "java.home"))}
   (format " #gridfire.utils.files/from-this-file %s "
           (pr-str "bin/java")))
  ;;"/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home/bin/java"

  *e)

(defn- expand-abbreviations
  [[abbr->v data]]
  (walk/postwalk (fn [x]
                   (if-some [[_k v] (find abbr->v x)]
                     v
                     x))
                 data))

(def convenience-encoding-readers
  {'gridfire.config/abbreviating (fn [abbrv-tuple] (expand-abbreviations abbrv-tuple))})

(comment
  ;; EDN tag #gridfire.config/abbreviating is useful for concisely encoding redundant config: INTRO
  (edn/read-string {:readers convenience-encoding-readers}
                   "#gridfire.config/abbreviating [{m0 {:this :map :is :arguably :quite :verbose} m1 {:so :is :this :one :I :daresay}} {:my-maps [m0 m1 m1 m1 m0 m0 m1]}]")
  ;;=>
  {:my-maps [{:this :map :is :arguably :quite :verbose}
             {:so :is :this :one :I :daresay}
             {:so :is :this :one :I :daresay}
             {:so :is :this :one :I :daresay}
             {:this :map :is :arguably :quite :verbose}
             {:this :map :is :arguably :quite :verbose}
             {:so :is :this :one :I :daresay}]}

  *e)

(defn read-situated-edn-file
  "Slurps and parses an EDN file such that the EDN tag
  #gridfire.utils.files/from-this-file ... will work
  and be resolved as a path relative to the directory
  containing that same file."
  [edn-file]
  (edn/read-string
   {:readers (merge (location-aware-edn-readers edn-file)
                    convenience-encoding-readers)}
   (slurp edn-file)))
