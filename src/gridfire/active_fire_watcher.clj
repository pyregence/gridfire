(ns gridfire.active-fire-watcher
  (:require [nextjournal.beholder :as beholder]
            [clojure.core.async   :refer [>!! go >!]])

  (:import java.util.TimeZone))

(def file-name-regex #"[^/]*(?=[.][a-zA-Z]+$)")

(def fire-name-regex #"[a-zA-Z]*[-[a-zA-Z2-9]]*")

(def ignition-time-regex #"\d{8}_\d{6}")

(defn- convert-date-string
  [date-str from-format to-format]
  (let [in-format  (doto (java.text.SimpleDateFormat. from-format)
                     (.setTimeZone (TimeZone/getTimeZone "UTC")))
        out-format (doto (java.text.SimpleDateFormat. to-format)
                     (.setTimeZone (TimeZone/getTimeZone "UTC")))]
    (->> date-str
         (.parse in-format)
         (.format out-format))))

(defn- parse-tar [path]
  (let [file-name     (re-find file-name-regex path)
        fire-name     (re-find fire-name-regex file-name)
        ignition-time (re-find ignition-time-regex file-name)]
    [fire-name (convert-date-string ignition-time "yyyyMMdd_HHmmss" "yyyy-MM-dd HH:mm zzz")]))

(defn- handler [job-queue]
  (fn [{:keys [type path]}]
    (go
     (when (= type :create)
       (let [[fire-name ignition-time] (parse-tar (.toString path))
             job                       {:fire-name     fire-name
                                        :ignition-time ignition-time
                                        :type          :active-fire}]
         (>! job-queue job))))))

(defn start! [{:keys [active-fire-dir]} job-queue]
  (when active-fire-dir
    (beholder/watch (handler job-queue) active-fire-dir)))
