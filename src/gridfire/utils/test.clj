(ns gridfire.utils.test
  (:require [clojure.java.io          :as io]
            [clojure.string           :as str]
            [gridfire.core            :as core]
            [gridfire.magellan-bridge :refer [register-custom-projections!]]
            [gridfire.postgis-bridge  :refer [db-pool-cache close-db-pool]])
  (:import (java.io File)))

;;-----------------------------------------------------------------------------
;; Config
;;-----------------------------------------------------------------------------

(def input-dirname  "test/data/")
(def output-dirname "test/output/")

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

(defn in-file-path [dir filename]
  (str/join "/" [dir filename]))

(defn out-file-path [filename]
  (str output-dirname filename))

(defn make-directory [dirname]
  (.mkdirs (io/file dirname)))

(defn delete-directory [dirname]
  (doseq [^File file (reverse (file-seq (io/file dirname)))]
    (when (.exists file)
      (io/delete-file file))))

(defn run-gridfire! [config]
  (some-> config
          (core/load-inputs!)
          (core/run-simulations!)
          (core/write-outputs!)))

;;-----------------------------------------------------------------------------
;; Fixtures
;;-----------------------------------------------------------------------------

(defn with-temp-output-dir [test-fn]
  (make-directory output-dirname)
  (test-fn)
  (delete-directory output-dirname))

(defn with-temp-directories [directories]
  (fn [test-fn]
    (doseq [directory directories]
      (make-directory directory))
    (test-fn)
    (doseq [directory directories]
      (delete-directory directory))))

(defn with-reset-db-pool [test-fn]
  (reset! db-pool-cache nil)
  (test-fn)
  (when-let [db-pool @db-pool-cache]
    (close-db-pool)))

(defn with-register-custom-projections [test-fn]
  (register-custom-projections!)
  (test-fn))
