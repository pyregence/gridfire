#!/usr/bin/env bb

(require '[clojure.tools.cli :refer [parse-opts]]
         '[clojure.data.csv :as csv]
         '[clojure.java.io :as io]
         '[clojure.string :as s])

;;-----------------------------------------------------------------------------
;; Parse elmfire.data
;;-----------------------------------------------------------------------------

(def regex-for-array-item #"^[A-Z0-9\_]+\(\d+\)")

(defn convert-val [s]
  (cond
    (re-matches #"^-?[0-9]\d*\.(\d+)?$" s) (Double/parseDouble s)
    (re-matches #"^-?\d+$" s)              (Integer/parseInt s)
    (re-matches #".TRUE." s)               true
    (re-matches #".FALSE." s)              false
    (re-matches #"'[0-9a-zA-Z_.//]*'" s)   (subs s 1 (dec (count s)))
    (s/includes? s "proj")                 s
    :else                                  nil))

(defn convert-key [s]
  (if (re-matches regex-for-array-item s)
    (s/join "-" (s/split s #"[\(\)]"))
    s))

(defn parse-elmfire [s]
  (->> (s/split s #"\n")
       (filter #(s/includes? % "="))
       (mapcat #(s/split % #" = "))
       (map s/trim)
       (apply hash-map)
       (reduce-kv (fn [m k v]
                    (assoc m (convert-key k) (convert-val v)))
                  {})))

;;-----------------------------------------------------------------------------
;; Parse fire_size_stats.csv
;;-----------------------------------------------------------------------------

(defn parse-fire-stats-line
  [[_ metband x y tstop & _] {:strs [COMPUTATIONAL_DOMAIN_CELLSIZE
                                     COMPUTATIONAL_DOMAIN_XLLCORNER
                                     COMPUTATIONAL_DOMAIN_YLLCORNER
                                     COMPUTATIONAL_DOMAIN_YDIM]}]
  [(-> y
       s/trim
       Float/parseFloat
       (- COMPUTATIONAL_DOMAIN_YLLCORNER)
       (- COMPUTATIONAL_DOMAIN_YDIM)
       Math/abs
       (/ COMPUTATIONAL_DOMAIN_CELLSIZE)
       int)
   (-> x
       s/trim
       Float/parseFloat
       (- COMPUTATIONAL_DOMAIN_XLLCORNER)
       (/ COMPUTATIONAL_DOMAIN_CELLSIZE)
       int)
   (* (Integer/parseInt (s/trim metband)) 60.0)
   (* (Float/parseFloat (s/trim tstop)) 60.0)]) ;TODO Check if maxruntime is added to inital start time.

;; NOTE GridFire's origin is upper left corner.


;;-----------------------------------------------------------------------------
;; main
;;-----------------------------------------------------------------------------

(def cli-options
  [["-c" "--fire-stats-csv FILE" "Elmfire's fire-stats.csv output file"
    :validate [#(.exists  (io/file %)) "The provided --fire-stats-csv does not exist."
               #(.canRead (io/file %)) "The provided --fire-stats-csv is not readable."]]
   ["-e" "--elmfire-data FILE" "Elmfire's elmfire.data input file"
    :validate [#(.exists  (io/file %)) "The provided --elmfire-data does not exist."
               #(.canRead (io/file %)) "The provided --elmfire-data is not readable."]]
   ["r" "--retention-policy-num NUM" "Keep every NUM-th row in fire-stats.csv"
    :default 1
    :validate [pos? int?]]])

(def program-banner "parse_fire_stats:")

(let [{:keys [options summary errors]} (parse-opts *command-line-args* cli-options)]
  (println program-banner)
  (if (or (seq errors) (not (and (:fire-stats-csv options) (:elmfire-data options))))
    (do (run! println errors)
        (println (str "\nUsage:\n" summary)))
    (let [elmfire-data (parse-elmfire (slurp (:elmfire-data options)))]
      (with-open [fire-stats-reader (io/reader (:fire-stats-csv options))
                  ignitions-writer (io/writer "ignitions.csv")]
        (->> (csv/read-csv fire-stats-reader)
             rest
             (take-nth (:retention-policy-num options))
             (map #(parse-fire-stats-line % elmfire-data))
             (cons ["ignition-row" "ignition-col" "ignition-start-time (min)" "max-runtime (min)"])
             (csv/write-csv ignitions-writer))))))

;; Usage
;; ./parse_fire_stats.clj -c outputs/fire_size_stats.csv -e elmfire.data

