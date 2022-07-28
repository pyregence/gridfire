;; [[file:../../org/GridFire.org::gridfire-core][gridfire-core]]
(ns gridfire.core
  (:require [clojure.core.reducers :as r]
            [clojure.edn           :as edn]
            [clojure.spec.alpha    :as spec]
            [gridfire.fire-spread  :refer [rothermel-fast-wrapper]]
            [gridfire.inputs       :as inputs]
            [gridfire.outputs      :as outputs]
            [gridfire.simulations  :as simulations]
            [gridfire.spec.config  :as config-spec]
            [manifold.deferred     :as mfd]
            [taoensso.tufte        :as tufte]
            [triangulum.logging    :refer [log log-str]]))

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

;; TODO: Disable this to see how much performance is gained.
(defmacro with-multithread-profiling
  [& body]
  `(do (tufte/remove-handler! :accumulating)
       (let [stats-accumulator# (tufte/add-accumulating-handler! {:handler-id :accumulating})
             result#            (do ~@body)]
         (Thread/sleep 1000)
         (as-> {:format-pstats-opts {:columns [:n-calls :min :max :mean :mad :clock :total]}} $#
           (tufte/format-grouped-pstats @stats-accumulator# $#)
           (log $# :truncate? false))
         result#)))

(defn run-simulations!
  [{:keys [^long simulations parallel-strategy] :as inputs}]
  (with-multithread-profiling
    (log-str "Running simulations")
    (let [parallel-bin-size (max 1 (quot simulations (.availableProcessors (Runtime/getRuntime))))
          reducer-fn        (if (= parallel-strategy :between-fires)
                              #(into [] (r/fold parallel-bin-size r/cat r/append! %))
                              #(into [] %))
          summary-stats     (with-redefs [rothermel-fast-wrapper (memoize rothermel-fast-wrapper)]
                              (->> (range simulations)
                                   (vec)
                                   (r/map #(simulations/run-simulation! % inputs))
                                   (r/remove nil?)
                                   (reducer-fn)))]
      (assoc inputs :summary-stats summary-stats))))

(defn ensure-ignitable-sites
  [inputs config-file-path]
  (if (seq (:ignitable-sites inputs))
    inputs
    (log-str (format "Invalid config file [%s]: No valid ignition sites." config-file-path))))

(defn load-inputs!
  [config]
  (-> config
      (inputs/add-input-layers)
      (inputs/add-misc-params)
      (inputs/add-ignition-csv)
      (inputs/add-sampled-params)
      (inputs/add-weather-params)
      (inputs/add-ignitable-sites)
      (inputs/add-aggregate-matrices)))

(defn load-config!
  [config-file-path]
  (let [config (edn/read-string (slurp config-file-path))]
    (if (spec/valid? ::config-spec/config config)
      config
      (log-str (format "Invalid config file [%s]:\n%s"
                       config-file-path
                       (spec/explain-str ::config-spec/config config))))))

(defn process-config-file!
  [config-file-path]
  (try
    (some-> config-file-path
            (load-config!)
            (load-inputs!)
            (ensure-ignitable-sites config-file-path)
            (run-simulations!)
            (write-outputs!))
    (catch Exception e
      (log-str (ex-message e)))))
;; gridfire-core ends here
