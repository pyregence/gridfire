;; FIXME LP coverage
(ns gridfire.utils.files.tar
  (:require [clojure.java.io :as io])
  (:import (org.apache.commons.compress.compressors.gzip GzipCompressorInputStream)
           (org.apache.commons.compress.archivers.tar TarArchiveInputStream TarArchiveEntry)
           (java.io ByteArrayOutputStream EOFException)))

(defn targz-input-stream
  "Convenience function for obtaining a TarArchiveInputStream from a tar.gz input."
  ^TarArchiveInputStream
  [archive-input]
  (-> archive-input
      (io/input-stream)
      (GzipCompressorInputStream.)
      (TarArchiveInputStream.)))

(defn tar-archive-entries
  "Returns a lazy sequence of [entry-name entry-bytes-array] pairs for each entry
  in the given TAR archive."
  [^TarArchiveInputStream tais]
  (lazy-seq
   (when-some [^TarArchiveEntry my-entry (.getNextTarEntry tais)]
     (try
       (let [entry-name  (.getName my-entry)
             entry-bytes (with-open [os (ByteArrayOutputStream.)]
                           (io/copy tais os)
                           (.toByteArray os))]
         (cons
          [entry-name entry-bytes]
          (tar-archive-entries tais)))
       ;; WARNING sometimes thrown for no apparent reason... (Val, 26 Sep 2022)
       (catch EOFException _eofex
         ())))))
