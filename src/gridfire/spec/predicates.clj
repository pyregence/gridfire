(ns gridfire.spec.predicates
  (:require [clojure.java.io :as io]))

(defn file-exits?
  [f]
  (.exists  (io/file f)))

(defn file-readable?
  [f]
 (.canRead (io/file f)))
