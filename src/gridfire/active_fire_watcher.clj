(ns gridfire.active-fire-watcher
  (:require [clojure.core.async   :refer [<! >! go-loop timeout]]
            [clojure.edn          :as edn]
            [clojure.string       :as s]
            [gridfire.conversion  :refer [convert-date-string]]
            [nextjournal.beholder :as beholder]
            [triangulum.logging   :refer [log-str]])
  (:import java.util.Date))

(set! *unchecked-math* :warn-on-boxed)

;;=============================================================================
;; File Path -> Job
;;=============================================================================

(def file-name-regex #"[^/]*(?=[.][a-zA-Z]+$)")

(def fire-name-regex #"[a-zA-Z]*[-[a-zA-Z2-9]]*")

(def ignition-time-regex #"\d{8}_\d{6}")

(defn- build-job [path-str]
  (let [file-name     (re-find file-name-regex path-str)
        fire-name     (re-find fire-name-regex file-name)
        ignition-time (re-find ignition-time-regex file-name)]
    {:fire-name     fire-name
     :type          :active-fire
     :ignition-time (convert-date-string ignition-time
                                         "yyyyMMdd_HHmmss"
                                         "yyyy-MM-dd HH:mm zzz")}))

(defn- suppress? [{:keys [also-simulate-suppression? suppression-white-list]} file-path]
  (and also-simulate-suppression?
       (some #(s/includes? file-path %) (-> (slurp suppression-white-list)
                                            (edn/read-string)
                                            (:suppression-white-list)))))

;;=============================================================================
;; Server
;;=============================================================================

(defonce download-in-progress (atom {}))

(defn- now ^long []
  (inst-ms (Date.)))

(defn- still-waiting? [^long last-mod-time]
  (< (- (now) last-mod-time) 5000)) ; wait up to 5 seconds

(defn- count-down [config job-queue path-str]
  (go-loop [^long last-mod-time (get @download-in-progress path-str)]
    (if (still-waiting? last-mod-time)
      (do (<! (timeout 1000))
          (recur (get @download-in-progress path-str)))
      (do (log-str "SCP Complete: " path-str)
          (swap! download-in-progress dissoc path-str)
          (let [job (build-job path-str)]
            (>! job-queue job)
            (when (suppress? config path-str)
              (>! job-queue (assoc job :suppression {:suppression-dt                            60.0 ;TODO remove hard code
                                                     :suppression-curve-sharpness 2.0})))))))) ;TODO remove hard code

(defn- handler [config job-queue]
  (fn [{:keys [type path]}]
    (let [path-str (.toString path)]
      (case type
        :create (do (log-str "Active Fire input deck detected: " path-str)
                    (swap! download-in-progress assoc path-str (now))
                    (count-down config job-queue path-str))
        :modify (swap! download-in-progress assoc path-str (now)) ; reset counter
        nil))))

(defonce directory-watcher (atom nil))

(defn start! [{:keys [active-fire-dir] :as config} job-queue]
  (when (and active-fire-dir (nil? @directory-watcher))
    (reset! directory-watcher
            (beholder/watch (handler config job-queue) active-fire-dir))))

(defn stop! []
  (when @directory-watcher
    (beholder/stop @directory-watcher)
    (reset! directory-watcher nil)))
