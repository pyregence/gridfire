(ns build
  (:require [clojure.tools.build.api :as b]))

(def build-folder "target")
(def jar-content (str build-folder "/classes"))
(def basis (b/create-basis {:project "deps.edn"}))

(def app-name "gridfire")
(def version "2022.09.29")
(def uberjar-file-name (format "%s/%s-%s.jar" build-folder app-name version))

(defn clean [_]
  (b/delete {:path build-folder})
  (println (format "Build folder \"%s\" removed" build-folder)))

(defn uberjar [_]
  ;; clean up old files
  (clean nil)

  ;; copy resources to jar-content folder
  (b/copy-dir {:src-dirs ["resources"]
               :target-dir jar-content})

  (b/compile-clj {:src-dirs ["src"]
                  :class-dir jar-content
                  :basis basis})

  ;; package jar-content into a jar
  (b/uber {:class-dir jar-content
           :uber-file uberjar-file-name
           :basis     basis
           :main      'gridfire.cli})

  (println (format "Uberjar file created: \"%s\"" uberjar-file-name)))

;; FIXME: Include these attributes:
;;
;; :aot         true
;; :manifest
;; {:specification-title    "Java Advanced Imaging Image I/O Tools"
;;  :specification-version  "1.1"
;;  :specification-vendor   "Sun Microsystems, Inc."
;;  :implementation-title   "com.sun.media.imageio"
;;  :implementation-version "1.1"
;;  :implementation-vendor  "Sun Microsystems, Inc."}}}
