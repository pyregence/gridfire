(ns gridfire.canonical-test
  (:require [clojure.edn          :as edn]
            [clojure.java.io      :as io]
            [clojure.data.csv     :as csv]
            [clojure.spec.alpha   :as s]
            [clojure.string       :as str]
            [clojure.test         :refer [deftest is run-tests]]
            [gridfire.core        :as gf]
            [gridfire.spec.config :as spec])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

;;; Constants

(def ^:private canonical-dir      "test/gridfire/resources/canonical_test/")
(def ^:private base-config        (edn/read-string (slurp (str canonical-dir "base-config.edn"))))
(def ^:private summary-stats-csv  "summary_stats.csv")
(def ^:private surface-scenarios  {:fuel-model         [:grass-fbfm40 :timber-litter-fbfm40 :grass-extreme-fbfm40 :shrub-fbfm40 :blowdown-fbfm40]
                                   :canopy-cover       [:zero-raster]
                                   :slope              [:zero-raster :slp-10 :slp-20 :slp-30]
                                   :wind-speed-20ft    [0.0 10.0 20.0 40.0]
                                   :fuel-moisture      (range 0.0 0.25 0.05)
                                   :foliar-moisture    [0.0 0.5 1.0]
                                   :canopy-base-height [:zero-raster]
                                   :crown-bulk-density [:zero-raster]})

(def ^:private crowning-scenarios {:fuel-model         [:grass-extreme-fbfm40 :shrub-fbfm40 :blowdown-fbfm40]
                                   :canopy-cover       [:raster-100]
                                   :slope              [:zero-raster]
                                   :wind-speed-20ft    [:ws_20ft-zero_24_zero]
                                   :fuel-moisture      [0.1]
                                   :foliar-moisture    [0.1]
                                   :canopy-base-height [:raster-2 :raster-10 :raster-20 :raster-40]
                                   :crown-bulk-density [:cbd-0005 :cbd-001 :cbd-005 :cbd-01 :cbd-02 :cbd-035 :cbd-05]})

(def ^:private surface-spotting-scenarios {:fuel-model         [:firebreak]
                                           :canopy-cover       [:zero-raster]
                                           :slope              [:zero-raster]
                                           :wind-speed-20ft    [5.0 10.0 15.0 20.0]
                                           :fuel-moisture      [0.0]
                                           :foliar-moisture    [0.0]
                                           :canopy-base-height [:zero-raster]
                                           :crown-bulk-density [:zero-raster]})

(def ^:private crown-spotting-scenarios {:fuel-model         [:firebreak]
                                         :canopy-cover       [:raster-100]
                                         :slope              [:zero-raster]
                                         :wind-speed-20ft    [5.0 10.0 15.0 20.0]
                                         :fuel-moisture      [0.0]
                                         :foliar-moisture    [0.0]
                                         :canopy-base-height [:raster-2]
                                         :crown-bulk-density [:raster-100]})

(def ^:private suppression-curve-scenarios {:fuel-model         [:blowdown-fbfm40]
                                            :canopy-cover       [:zero-raster]
                                            :slope              [:slp-10]
                                            :wind-speed-20ft    [0.0]
                                            :fuel-moisture      [0.0]
                                            :foliar-moisture    [0.0]
                                            :canopy-base-height [:zero-raster]
                                            :crown-bulk-density [:zero-raster]})

(def ^:private suppression-sdi-scenarios {:fuel-model                   [:blowdown-fbfm40]
                                          :canopy-cover                 [:zero-raster]
                                          :slope                        [:slp-10]
                                          :wind-speed-20ft              [0.0]
                                          :fuel-moisture                [0.0]
                                          :foliar-moisture              [0.0]
                                          :canopy-base-height           [:zero-raster]
                                          :crown-bulk-density           [:zero-raster]
                                          :suppression-difficulty-index [:ones-raster]})

;;; Helpers

(defn- now []
  (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd_HH-mm-ss") (LocalDateTime/now)))

(defn- mkdirs [dir]
  (.mkdirs (io/file dir)))

(defn- deep-flatten [x]
  (if (seq? x)
    (mapcat deep-flatten x)
    [x]))

(defn- deep-merge [a & maps]
  (if (map? a)
    (apply merge-with deep-merge a maps)
    (apply merge-with deep-merge maps)))

(defn- ->tif [filekey]
  {:source (str canonical-dir (name filekey) ".tif")
   :type   :geotiff})

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

(defn- ->fuel-moisture-map
  "Returns a fuel-moisture map given a constant fuel-moisture:
  {:dead {:1hr        (0-1)
          :10hr       (0-1)
          :100hr      (0-1)}
   :live {:herbaceous (0-1)
          :woody      (0-1)}}"
  [fuel-moisture]
  {:dead (zipmap [:1hr :10hr :100hr]  (repeat fuel-moisture))
   :live (zipmap [:woody :herbaceous] (repeat fuel-moisture))})

(defn- dir-name [in]
  (cond
    (float? in)   (-> in (str) (str/replace #"\." "_"))
    (keyword? in) (name in)
    :else         (str in)))

(defn- output-directory [{:keys [base-dir
                                 datetime
                                 scenario-type
                                 fuel-model
                                 canopy-cover
                                 slope
                                 fuel-moisture
                                 foliar-moisture
                                 wind-speed-20ft
                                 canopy-base-height
                                 crown-bulk-density]}]
  (str (when base-dir base-dir)
       "outputs"
       "/" datetime
       "/" (dir-name scenario-type)
       "/fuel-model_" (dir-name fuel-model)
       "/slope_" (dir-name slope)
       "/wind-speed-20ft_" (dir-name wind-speed-20ft)
       "/fuel-moisture_" (int (* 100 fuel-moisture))
       "/canopy-cover_" (name canopy-cover)
       "/foliar-moisture_" (int (* 100 foliar-moisture))
       "/canopy-base-height_" (dir-name canopy-base-height)
       "/crown-bulk-density_" (dir-name crown-bulk-density)
       "/"))

(defn- result->row [{:keys [params summary-stats]}]
  (let [stats (first summary-stats)]
    [(-> params :fuel-model dir-name)
     (-> params :slope dir-name)
     (-> params :wind-speed-20ft dir-name)
     (:fuel-moisture params)
     (-> params :canopy-cover dir-name)
     (:foliar-moisture params)
     (-> params :canopy-base-height dir-name)
     (-> params :crown-bulk-density dir-name)
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

(defn- run-sim! [config]
  (try
    (mkdirs (:output-directory config))

    (if (s/valid? ::spec/config config)
      (->> config
           (gf/load-inputs!)
           (gf/run-simulations!)
           (result->row))
      (s/explain ::spec/config config))
    (catch Throwable e
      (println (apply str (.getStackTrace e)))
      (println (.getMessage e)))))

(def csv-header ["fuel-model" "slope" "wind-speed-20ft" "fuel-moisture" "foliar-moisture" "canopy-cover"
                 "canopy-base-height" "canopy-bulk-density" "simulation" "ignition-row" "ignition-col"
                 "max-runtime" "temperature" "relative-humidity" "wind-from-direction"  "ellipse-adjustment-factor"
                 "fire-size" "flame-length-mean" "flame-length-stddev" "fire-line-intensity-mean"
                 "fire-line-intensity-stddev" "crown-fire-size" "spot-count"])

(defn- results->csv [filename results]
  (with-open [out (io/writer filename)]
    (->> results
         (cons csv-header)
         (csv/write-csv out))))

(defn- gen-scenario
  [{:keys
    [datetime
     fuel-model
     canopy-cover
     slope
     fuel-moisture
     foliar-moisture
     wind-speed-20ft
     canopy-base-height
     crown-bulk-density] :as params}]
  (deep-merge base-config
              {:params           params
               :landfire-layers  {:fuel-model   (->tif fuel-model)
                                  :canopy-cover (->tif canopy-cover)
                                  :slope        (->tif slope)
                                  :elevation    (->dem-tif slope)}
               :fuel-moisture    (->fuel-moisture-map fuel-moisture)
               :foliar-moisture  foliar-moisture
               :wind-speed-20ft  (if (keyword? wind-speed-20ft) (->tif wind-speed-20ft) wind-speed-20ft)
               :output-directory (output-directory (assoc params :datetime datetime))}
              (when-not (some #(= :zero-raster %) [canopy-cover canopy-base-height crown-bulk-density])
                {:landfire-layers {:canopy-base-height (->tif canopy-base-height)
                                   :canopy-height      (->ch canopy-base-height)
                                   :crown-bulk-density (->tif crown-bulk-density)}})))

(defn- gen-scenarios [scenario-type scenarios]
  (deep-flatten
   (let [datetime (now)]
     (for [fuel-model                   (:fuel-model scenarios)
           canopy-cover                 (:canopy-cover scenarios)
           slope                        (:slope scenarios)
           fuel-moisture                (:fuel-moisture scenarios)
           foliar-moisture              (:foliar-moisture scenarios)
           wind-speed-20ft              (:wind-speed-20ft scenarios)
           canopy-base-height           (:canopy-base-height scenarios)
           crown-bulk-density           (:crown-bulk-density scenarios)]
       (gen-scenario {:scenario-type                scenario-type
                      :fuel-model                   fuel-model
                      :canopy-base-height           canopy-base-height
                      :canopy-cover                 canopy-cover
                      :crown-bulk-density           crown-bulk-density
                      :datetime                     datetime
                      :fuel-moisture                fuel-moisture
                      :foliar-moisture              foliar-moisture
                      :slope                        slope
                      :wind-speed-20ft              wind-speed-20ft})))))

;;; Tests

(defn run-test-scenario! [{:keys [params] :as scenario}]
  (let [control-dir   (output-directory (assoc params :base-dir canonical-dir))
        control-stats (str control-dir summary-stats-csv)
        new-stats     (str (:output-directory scenario) summary-stats-csv)]
    (run-sim! scenario)
    (is (= (slurp control-stats)
           (slurp new-stats)))))

(deftest ^{:surface true :canonical true} test-surface-scenarios
  (let [test-file (str "test-surface-"(now)".csv")]
    (->> (gen-scenarios :surface surface-scenarios)
         ;(take 1) ; Uncomment me to run only 1 simulation.
         (pmap run-sim!)
         (results->csv test-file))))

(deftest ^{:crowning true :canonical true} test-crowning-scenarios
  (let [test-file (str "test-crowning-"(now)".csv")]
    (results->csv test-file (pmap run-sim! (gen-scenarios :crowning crowning-scenarios)))))

(deftest ^{:surface-spotting true :canonical true} test-surface-spotting-scenarios
  (let [test-file (str "test-surface-spotting-"(now)".csv")]
    (results->csv test-file (map run-sim! (gen-scenarios :surface-spotting surface-spotting-scenarios)))))

(deftest ^{:crown-spotting true :canonical true} test-crown-spotting-scenarios
  (let [test-file (str "test-crown-spotting-"(now)".csv")]
    (results->csv test-file (map run-sim! (gen-scenarios :crown-spotting crown-spotting-scenarios)))))

#_(deftest ^{:crown-spotting true :canonical true} test-crown-spotting-scenario
    (let [test-file (str "test-crown-spotting-"(now)".csv")]
      (run-sim! (first (gen-scenarios :spotting crown-spotting-scenarios)))))

(deftest ^{:suppression-curve true :simulation true :canonical true} test-suppression-scenario
  (run-sim! (-> (first (gen-scenarios :suppression-curve suppression-curve-scenarios))
                (assoc :output-layers {:directional-flame-length 72
                                       :flame-length             :final})
                (assoc :suppression {:suppression-dt          300.0
                                     :suppression-coefficient 2.0})
                (assoc :weather-start-timestamp #inst "1970-01-01T00-00:00"))))

(deftest ^{:suppression-sdi true :simulation true :canonical true} test-suppression-sdi-scenario
  (run-sim! (-> (first (gen-scenarios ))
                (assoc :output-layers {:directional-flame-length 72
                                       :flame-length             :final})
                (update-in [:suppression]
                           #(merge % {:suppression-dt                                300.0
                                      :sdi-layer                                     (->tif :zero-raster)
                                      :sdi-sensitivity-to-difficulty                 2.0
                                      :sdi-containment-overwhelming-area-growth-rate 50000
                                      :sdi-reference-suppression-speed               800}))
                (assoc :weather-start-timestamp #inst "1970-01-01T00-00:00"))))

(comment
  ;; TIP: this is how you run only a subset of test cases. (Val, 03 Nov 2022)
  (clojure.test/test-vars [#'test-surface-spotting-scenarios
                           #'test-crown-spotting-scenarios
                           #'test-suppression-scenario
                           #'test-suppression-sdi-scenario])

  (run-tests 'gridfire.canonical-test))
