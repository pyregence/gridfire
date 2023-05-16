;; [[file:../../../org/GridFire.org::gridfire.lab.benchmarks][gridfire.lab.benchmarks]]
(set! clojure.core/*assert* false)

(ns gridfire.lab.benchmarks
  (:require [clj-async-profiler.core      :as prof]
            [clojure.java.io              :as io]
            [clojure.pprint               :as pprint]
            [criterium.core               :as bench]
            [gridfire.core                :as gridfire]
            [gridfire.fire-spread         :refer [memoize-rfwo rothermel-fast-wrapper-optimal]]
            [gridfire.simulations         :as simulations]))

(defn simulation-result-summary
  [simulation-results]
  (select-keys simulation-results
               [:exit-condition
                :global-clock
                :surface-fire-count
                :crown-fire-count
                :spot-count]))

(defn- run-simulations!
  [{n-sims :simulations :as inputs}]
  (with-redefs [rothermel-fast-wrapper-optimal (memoize-rfwo rothermel-fast-wrapper-optimal)]
    (->> (range n-sims)
         (mapv (fn [sim-i]
                 (let [simulation-inputs  (simulations/prepare-simulation-inputs sim-i inputs)
                       simulation-results (gridfire.fire-spread/run-fire-spread simulation-inputs)]
                   (simulation-result-summary simulation-results)))))))

(defn- results-digest
  [sims-summaries]
  (->> sims-summaries
       (hash)))

(defn print-context!
  [inputs]
  ;; The digest helps make sure the compared versions have equivalent behavior.
  (println "Results digest:" (results-digest (run-simulations! inputs)))
  (pprint/pprint (bench/os-details))
  (pprint/pprint (into (sorted-map) (bench/runtime-details))))

(defn- benchmark-config!
  [config-file-path]
  (let [inputs (-> config-file-path
                   (gridfire/load-config!)
                   (gridfire/load-inputs!))]
    (print-context! inputs)
    (binding [bench/*report-progress* false
              bench/*report-warn* true
              bench/*report-debug* false]
      (println "Warming up...")
      (dotimes [_ 500] (run-simulations! inputs))
      (println "Measuring...")
      (bench/bench (run-simulations! inputs)
                   ;; With 25 samples,
                   ;; You can get an approximate 95% confidence interval centered around the empirical mean
                   ;; with radius computed by:
                   ;; 1. dividing the Criterium-reported standard-dev by 5 (5 = 25^0.5)
                   ;; 2. multiplying that by 2 - more precisely by 2.060, see:
                   ;; https://en.wikipedia.org/wiki/Student%27s_t-distribution#Confidence_intervals
                   :samples 25
                   :target-execution-time (* 10 bench/s-to-ns)))))

(defn- profile-config!
  [config-file-path]
  (let [inputs (-> config-file-path
                   (gridfire/load-config!)
                   (gridfire/load-inputs!))]
    (print-context! inputs)
    ;; warmup
    (dotimes [_ 30]
      (run-simulations! inputs))
    (let [flamegraph-file (prof/profile
                           {:return-file true}
                           (dotimes [_ 50]
                             (run-simulations! inputs)))]
      (println "flame graph:")
      (println (str "file://" (.getAbsolutePath (io/file flamegraph-file)))))))

(defn -main [command config-file-path]
  (case command
    "benchmark" (benchmark-config! config-file-path)
    "profile" (profile-config! config-file-path)))

;; Example:
;; $ clojure -J-Dclojure.compiler.direct-linking=true -M:perf-testing -m gridfire.lab.benchmarks benchmark test/gridfire/lab/benchmarks/rhino-input-deck/gridfire.edn
;; $ clojure -J-Dclojure.compiler.direct-linking=true -J-Djdk.attach.allowAttachSelf=true -M:perf-testing -m gridfire.lab.benchmarks profile test/gridfire/lab/benchmarks/rhino-input-deck/gridfire.edn
;; gridfire.lab.benchmarks ends here
