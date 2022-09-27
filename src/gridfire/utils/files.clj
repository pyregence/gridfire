(ns gridfire.utils.files
  "File-system utilities for GridFire."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn- build-absolute-path
  [^String current-dir-path, ^String rel-path]
  (.getCanonicalPath
   (apply io/file current-dir-path (if (string? rel-path)
                                     [rel-path]
                                     rel-path))))

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

(defn read-situated-edn-file
  "Slurps and parses an EDN file such that the EDN tag
  #gridfire.utils.files/from-this-file ... will work
  and be resolved as a path relative to the directory
  containing that same file."
  [edn-file]
  (edn/read-string
   {:readers (location-aware-edn-readers edn-file)}
   (slurp edn-file)))
