(ns gridfire.active-fire-watcher
  (:require [clojure.core.async :refer [<! >!! go-loop timeout]]
            [nextjournal.beholder :as beholder]
            [triangulum.logging :refer [log-str]])
  (:import java.util.TimeZone))

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------
(defonce download-in-progress (atom {}))

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
    [fire-name (convert-date-string ignition-time
                                    "yyyyMMdd_HHmmss"
                                    "yyyy-MM-dd HH:mm zzz")]))

(defn build-job [path]
  (let [[fire-name ignition-time] (parse-tar path)]
    {:fire-name     fire-name
     :ignition-time ignition-time
     :type          :active-fire}))

(defn count-down [job-queue tar]
  (go-loop [seconds (get @download-in-progress tar)]
    (if (> @seconds 0)
      (do (<! (timeout 1000))
          (swap! seconds dec)
          (recur (get @download-in-progress tar)))
      (do (log-str "SCP Complete:" tar)
          (swap! download-in-progress dissoc tar)
          (>!! job-queue (build-job tar))))))

;;-----------------------------------------------------------------------------
;; Utils
;;-----------------------------------------------------------------------------

(defn- handler [job-queue]
  (fn [{:keys [type path]}]
    (let [path-str  (.toString path)]
      (case type
        :create (do (log-str "Active Fire input deck detected:" path-str)
                    (swap! download-in-progress assoc path-str (atom 5))
                    (count-down job-queue path-str))
        :modify (swap! download-in-progress assoc path-str (atom 5)) ;reset counter
        nil))))

(defn start! [{:keys [active-fire-dir]} job-queue]
  (when active-fire-dir
    (beholder/watch (handler job-queue) active-fire-dir)))
