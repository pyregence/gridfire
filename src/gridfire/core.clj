;; [[file:../../org/GridFire.org::gridfire-core][gridfire-core]]
(ns gridfire.core
  (:require [clojure.spec.alpha           :as spec]
            [gridfire.fire-spread         :refer [memoize-rfwo rothermel-fast-wrapper-optimal]]
            [gridfire.inputs              :as inputs]
            [gridfire.outputs             :as outputs]
            [gridfire.simulations         :as simulations]
            [gridfire.spec.config         :as config-spec]
            [gridfire.utils.async         :as gf-async]
            [gridfire.utils.files         :as files]
            [manifold.deferred            :as mfd]
            [taoensso.tufte               :as tufte]
            [triangulum.logging           :refer [log log-str]]))

(set! *unchecked-math* :warn-on-boxed)

(defn write-outputs!
  [outputs]
  (->
   (mfd/zip
    (outputs/write-landfire-layers! outputs)
    (outputs/write-aggregate-layers! outputs)
    (outputs/write-csv-outputs! outputs))
   (deref))
  :success)

(defmacro with-multithread-profiling
  [& body]
  `(do (tufte/remove-handler! :accumulating)
       (let [stats-accumulator# (tufte/add-accumulating-handler! {:handler-id :accumulating})
             result#            (do ~@body)]
         (when simulations/*log-performance-metrics*
           (Thread/sleep 1000))
         (as-> {:format-pstats-opts {:columns [:n-calls :min :max :mean :mad :clock :total]}} $#
           (tufte/format-grouped-pstats @stats-accumulator# $#)
           (when simulations/*log-performance-metrics*
             (log $# :truncate? false)))
         result#)))

(defn run-simulations!
  [{:keys [^long simulations parallel-strategy] :as inputs}]
  (with-multithread-profiling                               ; TODO: Disable this to see how much performance is gained.
    (log-str "Running simulations")
    (let [sfmin-memoization (get-in inputs [:memoization :surface-fire-min] :across-sims)
          pmap-fn           (if (= parallel-strategy :between-fires)
                              (fn [f coll]
                                (->> coll
                                     (gf-async/pmap-in-n-threads (or (:max-parallel-simulations inputs) ; INTRO :max-parallel-simulations sets the maximum number of fires running simultaneously. Useful for limiting the memory usage of GridFire.
                                                                     (.availableProcessors (Runtime/getRuntime)))
                                                                 f)
                                     (deref)))
                              ;; NOTE the optimization behind {:parallel-strategy :within-fires} is no longer implemented, so we default to single-threaded sequential processing.
                              map)
          summary-stats     (with-redefs [rothermel-fast-wrapper-optimal (if (= sfmin-memoization :across-sims)
                                                                           (memoize-rfwo rothermel-fast-wrapper-optimal)
                                                                           rothermel-fast-wrapper-optimal)]
                              ;; NOTE :across-sims is useful to share the memo across simulations, with a risk of running out of memory. Pointless when there are perturbations.
                              ;; WARNING: omitting :memoization {} is not equivalent to :memoization {:surface-fire-min nil}, but to :memoization {:surface-fire-min :across-sims}, for backward compatibility. (Val, 09 Jan 2023)
                              (->> (range simulations)
                                   (pmap-fn (fn run-ith-simulation [i]
                                              (simulations/run-simulation! i inputs)))
                                   (remove nil?)
                                   (vec)))]
      (assoc inputs :summary-stats summary-stats))))

(defn load-inputs!
  [config]
  (-> config
      (inputs/add-input-layers)
      (inputs/add-misc-params)
      (inputs/add-ignition-csv)
      (inputs/add-sampled-params)
      (inputs/add-perturbation-params)
      (inputs/add-weather-params)
      (inputs/add-fuel-moisture-params)
      (inputs/add-random-ignition-sites)
      (inputs/add-aggregate-matrices)
      (inputs/add-ignition-start-times)
      (inputs/add-ignition-start-timestamps)
      (inputs/add-burn-period-samples)
      (inputs/add-suppression)
      (inputs/add-fuel-number->spread-rate-adjustment-array-lookup-samples)))

(defn load-config-or-throw!
  [config-file-path]
  (let [config (files/read-situated-edn-file config-file-path)]
    (if (spec/valid? ::config-spec/config config)
      (assoc config :config-file-path config-file-path)
      (throw (ex-info (format "Invalid config file [%s]:\n%s"
                              config-file-path
                              (spec/explain-str ::config-spec/config config))
                      {::config           config
                       ::spec-explanation (spec/explain-data ::config-spec/config config)})))))

(defn load-config!
  [config-file-path]
  (try
    (load-config-or-throw! config-file-path)
    (catch Exception err
      (log-str (ex-message err)))))

(defn process-config-file!
  [config-file-path]
  (try
    (some-> config-file-path
            (load-config!)
            (load-inputs!)
            (run-simulations!)
            (write-outputs!))
    (catch Exception e
      (log-str (ex-message e)))))
;; gridfire-core ends here
