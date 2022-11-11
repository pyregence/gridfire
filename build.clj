(ns build
  (:require [clojure.tools.build.api :as b])
  (:import java.util.Date))

(defn get-calendar-branch-version []
  (let [today  (Date.)
        commit (b/git-process {:git-args "rev-parse --short HEAD"})]
    (format "%d.%d.%d-%s"
            (+ 1900 (.getYear today))
            (+ 1 (.getMonth today))
            (.getDate today)
            commit)))

(def build-folder "target")
(def jar-content (str build-folder "/classes"))
(def basis (b/create-basis {:project "deps.edn"}))

(def app-name "gridfire")
(def version (get-calendar-branch-version))
(def uberjar-file-name (format "%s/%s-%s.jar" build-folder app-name version))

(defn clean [_]
  (b/delete {:path build-folder})
  (println (format "Build folder \"%s\" removed" build-folder)))

(defn uberjar [_]
  ;; clean up old files
  (clean nil)

  ;; copy resources to jar-content folder
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir jar-content})

  (b/compile-clj {:src-dirs ["src"]
                  :class-dir jar-content
                  :basis basis})

  ;; package jar-content into a jar
  (b/uber {:class-dir jar-content
           :uber-file uberjar-file-name
           :basis     basis
           :main      'gridfire.cli
           :manifest  {"Specification-Title"    "Java Advanced Imaging Image I/O Tools"
                       "Specification-Version"  "1.1"
                       "Specification-Vendor"   "Sun Microsystems, Inc."
                       "Implementation-Title"   "com.sun.media.imageio"
                       "Implementation-Version" "1.1"
                       "Implementation-Vendor"  "Sun Microsystems, Inc."}})

  (println (format "Uberjar file created: \"%s\"" uberjar-file-name)))
