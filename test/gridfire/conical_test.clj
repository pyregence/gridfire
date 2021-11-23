(ns gridfire.conical-test
  (:require [clojure.edn          :as edn]
            [clojure.string       :as str]
            [clojure.java.io      :as io]
            [clojure.data.csv    :as csv]
            [clojure.spec.alpha   :as s]
            [clojure.test         :refer [deftest is run-tests]]
            [gridfire.core        :as gf]
            [gridfire.spec.config :as spec])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(defn- now []
  (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd_HH-mm-ss") (LocalDateTime/now)))

(defn- mkdirs [dir]
  (.mkdirs (io/file dir)))

(def ^:private conical-dir       "test/gridfire/resources/conical_test/")
(def ^:private base-config       (edn/read-string (slurp (str conical-dir "base-config.edn"))))
(def ^:private summary-stats-csv "summary_stats.csv")
(def ^:private scenarios         {:fuel-model         [:grass-fbfm40 :timber-litter-fbfm40]
                                  :canopy-cover       [:zero-raster :raster-100]
                                  :slope              [:zero-raster :slp-10 :slp-20 :slp-30]
                                  :foliar-moisture    [0 0.5 1.0]
                                  :wind-speed-20ft    [0 10 20 40]
                                  :canopy-base-height [:zero-raster :raster-2 :raster-10 :raster-20 :raster-40]
                                  :crown-bulk-density [:zero-raster :cbd-02 :cbd-035 :cbd-05]})

(defn- ->tif [filekey]
  {:source (str conical-dir (name filekey) ".tif")
   :type :geotiff})

(defn- ->dem-tif [slope]
  (case slope
    :zero-raster (assoc (->tif :zero-raster) :units :metric)
    :slp-10      (assoc (->tif :dem-10-slp)  :units :metric)
    :slp-20      (assoc (->tif :dem-20-slp)  :units :metric)
    :slp-30      (assoc (->tif :dem-30-slp)  :units :metric)))

(defn- ->ch [cbh]
  (case cbh
    :zero-raster (->tif :zero-raster)
    :raster-2    (->tif :raster-5)
    :raster-10   (->tif :raster-20)
    :raster-20   (->tif :raster-40)
    :raster-40   (->tif :raster-80)))

(defn- output-directory [{:keys [base-dir
                                 datetime
                                 fuel-model
                                 canopy-cover
                                 slope
                                 foliar-moisture
                                 wind-speed-20ft
                                 canopy-base-height
                                 crown-bulk-density]}]
  (str (when base-dir base-dir)
       "outputs"
       "/" datetime
       "/fuel-model_" (name fuel-model)
       "/canopy-cover_" (name canopy-cover)
       "/slope_" (name slope)
       "/moisture_" (int (* 100 foliar-moisture))
       "/wind-speed-20ft_" wind-speed-20ft
       "/canopy-base-height_" (name canopy-base-height)
       "/crown-bulk-density_" (name crown-bulk-density)
       "/"))

(defn- result->row [{:keys [params summary-stats]}]
  (let [stats (first summary-stats)]
    [(-> params :fuel-model name)
     (-> params :slope name)
     (:wind-speed-20ft params)
     (:foliar-moisture params)
     (-> params :canopy-cover name)
     (-> params :canopy-base-height name)
     (-> params :crown-bulk-density name)
     (:simulation stats)
     (:ignition-row stats)
     (:ignition-col stats)
     (:max-runtime stats)
     (:temperature stats)
     (:relative-humidity stats)
     (:wind-from-direction stats)
     (:ellipse-adjustment-factor stats)
     (:fire-size stats)
     (:flame-length-mean stats)
     (:flame-length-stddev stats)
     (:fire-line-intensity-mean stats)
     (:fire-line-intensity-stddev stats)
     (:crown-fire-size stats)
     (:spot-count stats)]))

(def results (atom []))

(defn- run-sim! [config]
  (mkdirs (:output-directory config))

  (if (s/valid? ::spec/config config)
    (->> config
         (gf/load-inputs!)
         (gf/run-simulations!)
         (result->row))
    (s/explain ::spec/config config)))

(def csv-header ["fuel-model" "slope" "wind-speed-20ft" "foliar-moisture" "canopy-cover" "canopy-base-height"
                 "canopy-bulk-density" "simulation" "ignition-row" "ignition-col" "max-runtime" "temperature"
                 "relative-humidity" "wind-from-direction"  "ellipse-adjustment-factor" "fire-size"
                 "flame-length-mean" "flame-length-stddev" "fire-line-intensity-mean"
                 "fire-line-intensity-stddev" "crown-fire-size" "spot-count"])

(defn- results->csv [filename results]
  (with-open [out (io/writer filename)]
    (->> results
         (cons csv-header)
         (csv/write-csv out))))

(defn- deep-flatten [x]
  (if (seq? x)
    (mapcat deep-flatten x)
    [x]))

(defn- deep-merge [a & maps]
  (if (map? a)
    (apply merge-with deep-merge a maps)
    (apply merge-with deep-merge maps)))

(defn- gen-scenario
  [{:keys [datetime
           fuel-model
           canopy-cover
           slope
           foliar-moisture
           wind-speed-20ft
           canopy-base-height
           crown-bulk-density] :as params}]
  (deep-merge base-config
              {:params          params
               :landfire-layers {:fuel-model   (->tif fuel-model)
                                 :canopy-cover (->tif canopy-cover)
                                 :slope        (->tif slope)
                                 :elevation    (->dem-tif slope)}
               :foliar-moisture foliar-moisture
               :wind-speed-20ft wind-speed-20ft
               :output-directory (output-directory (assoc params :datetime datetime))}
              (when-not (some #(= :zero-raster %) [canopy-cover canopy-base-height crown-bulk-density])
                {:landfire-layers {:canopy-base-height (->tif canopy-base-height)
                                   :canopy-height      (->ch canopy-base-height)
                                   :crown-bulk-density (->tif crown-bulk-density)}})))

(defn- gen-scenarios []
  (deep-flatten
    (let [datetime (now)]
      (for [fuel-model (:fuel-model scenarios)
            canopy-cover (:canopy-cover scenarios)
            slope (:slope scenarios)
            foliar-moisture (:foliar-moisture scenarios)
            wind-speed-20ft (:wind-speed-20ft scenarios)
            canopy-base-height (:canopy-base-height scenarios)
            crown-bulk-density (:crown-bulk-density scenarios)]
        (gen-scenario {:datetime           datetime
                       :fuel-model         fuel-model
                       :canopy-cover       canopy-cover
                       :slope              slope
                       :foliar-moisture    foliar-moisture
                       :crown-bulk-density crown-bulk-density
                       :canopy-base-height canopy-base-height
                       :wind-speed-20ft    wind-speed-20ft})))))

(defn- run-test-scenario! [{:keys [params] :as scenario}]
  (let [control-dir   (output-directory (assoc params :base-dir conical-dir))
        control-stats (str control-dir summary-stats-csv)
        new-stats     (str (:output-directory scenario) summary-stats-csv)]
    (run-sim! scenario)
    #_(is (= (slurp control-stats)
           (slurp new-stats)))))

(deftest all-scenarios
  (let [test-file (str "test-"(now)".csv")]
    (results->csv test-file (map run-sim! (gen-scenarios)))))

(comment

  (run-tests 'gridfire.conical-test)

  )
