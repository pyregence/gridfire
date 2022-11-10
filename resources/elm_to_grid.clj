#!/usr/bin/env bb

;; FIXME: document babashka (bb) and gdalsrsinfo as installation dependencies for running this script
;; FIXME: use babashka's pod protocol to integrate this script with gridfire.server

(require '[clojure.data.csv   :as csv]
         '[clojure.edn        :as edn]
         '[clojure.java.io    :as io]
         '[clojure.java.shell :refer [sh]]
         '[clojure.pprint     :refer [pprint]]
         '[clojure.string     :as str]
         '[clojure.test       :as test]
         '[clojure.tools.cli  :refer [parse-opts]])

;;=============================================================================
;; Units conversion functions
;;=============================================================================

(defn m->ft
  "Convert meters to feet."
  ^double
  [^double m]
  (* m 3.281))

(defn sec->min
  "Convert seconds to minutes."
  ^double
  [^double seconds]
  (* seconds 0.016666666666666666))

(defn kW-m->Btu-ft-s
  "Convert kilowatt per meter to BTU per feet per second."
  ^double
  [^double kW-m]
  (* kW-m 0.28887942532730604))


;;=============================================================================
;; Intrange Parsing
;;=============================================================================

(defn extract-fuel-range
  "Given elmfire key parse lower and upper bound of fuel-number
  values. If lower and/or upper bound is not specified use the given
  default values [L H]."
  [s L+H]
  (let [[L H]    L+H
        min-fuel (some-> (re-find #"\d+(?=:)" s) ; NOTE: ?=: is zero-width positive lookahead, matching digits before a ':'.
                         (Long/parseLong 10))
        max-fuel (some-> (re-find #"(?<=:)\d+" s) ; NOTE: ?<=: is negative lookbehind, matching digits after a ':'.
                         (Long/parseLong 10))]
    [(or min-fuel L) (or max-fuel H)]))

(test/deftest extract-fuel-range-test
  (test/are [min-val max-val s L H] (= [min-val max-val]
                                       (extract-fuel-range s [L H]))
    1   303 "CRITICAL_SPOTTING_FIRELINE_INTENSITY(:)"    1 303
    110 303 "CRITICAL_SPOTTING_FIRELINE_INTENSITY(110:)" 1 303
    1   110 "CRITICAL_SPOTTING_FIRELINE_INTENSITY(:110)" 1 303))

(defn- intrange?
  [v]
  (and (vector? v)
       (= 2 (count v))
       (let [[l h] v]
         (and (integer? l)
              (integer? h)
              (<= l h)))))

(defn- simplify-intranges
  "Simplifies a sequence of consecutive non-overlapping integer ranges,
  merging them where they touch."
  [irs]
  (when-not (empty? irs)
    (loop [ir0  (first irs)
           ir1+ (rest irs)]
      (if (empty? ir1+)
        [ir0]
        (let [[ir1 & ir2+] ir1+
              [l0 h0]      ir0
              [l1 h1]      ir1
              touch?       (= h0 (dec l1))]
          (assert (< h0 l1) "the ranges must be consecutive and non-overlapping.")
          (if touch?
            (recur [l0 h1]
                   ir2+)
            (lazy-cat [ir0]
                      (simplify-intranges ir1+))))))))

(defn- intrange-intersection
  [ir1 ir2]
  {:pre [(intrange? ir1)
         (intrange? ir2)]}
  (let [[l1 h1] ir1
        [l2 h2] ir2
        l3      (max (long l1) (long l2))
        h3      (min (long h1) (long h2))]
    (if (> l3 h3)
      nil
      [l3 h3])))

(defn- intranges-mapping?
  [m]
  (and (seqable? m)
       (->> m (every? (fn intranges-entry? [e]
                        (and (vector? e)
                             (= 2 (count e))
                             (let [[l+h _v] e]
                               (intrange? l+h))))))))

(defn- simplify-intranges-mapping
  [l+h->v]
  {:pre [(intranges-mapping? l+h->v)]}
  (->> l+h->v
       ;; Simplification:
       ;; 1) Grouping consecutive intranges which map to the same value:
       (sort-by (fn lower-bound [[[l _h] _v]] l))
       (partition-by (fn mapping-value [[_ir v]] v))
       ;; 2) Merging them where they touch:
       (mapcat (fn simplify-consecutive-intranges-with-same-value [ir+vs]
                 (let [[_ir0 the-value] (first ir+vs)
                       irs              (map first ir+vs)]
                   (->> irs
                        (simplify-intranges)
                        (mapv (fn to-mapping-entry [ir]
                                [ir the-value]))))))
       (into {})))

(defn- combine-intranges-mappings
  "Given intranges-mappings m1 and m2, and a 2-arg function f,
  creates an intranges-mapping over the integers supporting both m1 and m2:
  for each integer i, if m1 maps i to v1 and m2 maps i to v2,
  then the returned mapping maps i to (f v1 v2)."
  [m1 m2 f]
  {:pre [(intranges-mapping? m1)
         (intranges-mapping? m2)]}
  (->> (for [[k1 v1] m1
             [k2 v2] m2
             :let    [inter (intrange-intersection k1 k2)]
             :when   (some? inter)]
         [inter (f v1 v2)])
       (simplify-intranges-mapping)))

(test/deftest combine-intranges-mapping-test
  (test/is (= {[1 3] 9
               [4 5] 10
               [7 7] 20
               [8 9] 19}
              (combine-intranges-mappings {[1 5 ] 10
                                           [7 12] 20}
                                          {[1 3] 1
                                           [4 7] 0
                                           [8 9] 1}
                                          -))))

(defn- complete-intranges-mapping
  "Completes a mapping of integer ranges so that gaps are filled by mapping to default-value.
  Optionally, lower and upper bounds can be supplied to also complete left and right tails."
  ([l+h->v default-value]
   {:pre [(intranges-mapping? l+h->v)]}
   (if (= (count l+h->v) 1)
     l+h->v
     (->> l+h->v
          (sort-by (fn lower-bound [[[l _h] _v]] l))
          (partition 2 1)
          (mapcat (fn [[e1 e2]]
                    (let [[_l1 h1] (first e1)
                          [l2 _h2] (first e2)
                          touch?   (= h1 (dec l2))]
                      (if touch?
                        [e1
                         e2]
                        (let [gap [(inc h1) (dec l2)]]
                          [e1
                           [gap default-value]
                           e2])))))
          (dedupe)
          (into {}))))
  ([l+h->v l+h default-value]
   {:pre [(intranges-mapping? l+h->v)
          (intrange? l+h)]}
   (if (empty? l+h->v)
     {l+h default-value}
     (let [[l h] l+h
           lmin  (->> l+h->v
                      (map first)
                      (map (fn lower-bound [[l _h]] l))
                      (apply min))
           hmax  (->> l+h->v
                      (map first)
                      (map (fn upper-bound [[_l h]] h))
                      (apply max))]
       (-> l+h->v
           (cond-> (< l lmin) (conj [[l l] default-value])
                   (> h hmax) (conj [[h h] default-value]))
           (complete-intranges-mapping default-value)
           (simplify-intranges-mapping))))))

(test/deftest complete-intranges-mapping-test
  (test/testing "nominal case"
    (test/is (= {[0 1]  1.0
                 [2 4]  0.5
                 [5 5]  1.0
                 [6 8]  1.5
                 [9 10] 1.0}
                (complete-intranges-mapping {[2 4] 0.5
                                             [6 8] 1.5}
                                            [0 10]
                                            1.0))))

  (test/testing "edge cases"
    (test/testing "complete map with single entry"
      (test/is (= {[1 303] 1.0}
                  (complete-intranges-mapping {[1 303] 1.0} 1.0)))

      (test/is (= {[1 303] 1.0}
                  (complete-intranges-mapping {} [1 303] 1.0)))

      (test/is (= {}
                  (complete-intranges-mapping {} 1.0))))))

(defn- intranges-mapping-for-config-key
  [elmfire-config ef-key-prefix L+H default-val]
  (let [incomplete-intrange->v (some->> (filterv (fn by-key [[k _]] (str/starts-with? k ef-key-prefix)) elmfire-config)
                                        (seq)
                                        (mapv (fn parse-key [[k v]] [(extract-fuel-range k L+H) v]))
                                        (mapcat (fn [[k v]]
                                                  (let [[l h] k]
                                                    (if (vector? v)
                                                      (mapv (fn [l v] [[l l] v])
                                                            (range l (inc h))
                                                            v)
                                                      (mapv (fn [l] [[l l] v])
                                                            (range l (inc h))))))))]
    (if incomplete-intrange->v
      (complete-intranges-mapping incomplete-intrange->v L+H default-val)
      nil)))

(test/deftest intranges-mapping-for-config-key-test
  (test/testing "value is a scalar"
    (test/testing "multiple lines"
      (let [config {"SOME_PARAM(0:256)" 1200.0
                    "SOME_PARAM(257:)"  100.0}]
        (test/is (= {[0 256]   1200.0
                     [257 303] 100.0}
                    (intranges-mapping-for-config-key config "SOME_PARAM" [1 303] 1.0))))))

  (test/testing "value is a vector"
    (test/testing "single line"
      (let [config {"SOME_PARAM(2:8)" [5.0 5.0 6.0 6.0 7.0 7.0 8.0 8.0]}]
        (test/is (= {[1 1]  1.0
                     [2 3]  5.0
                     [4 5]  6.0
                     [6 7]  7.0
                     [8 8]  8.0
                     [9 10] 1.0}
                    (intranges-mapping-for-config-key config "SOME_PARAM" [1 10] 1.0)))))

    (test/testing "multiple lines"
      (let [config {"SOME_PARAM(2:4)" [2.0 3.0 4.0]
                    "SOME_PARAM(7:8)" [5.0 6.0]}]
        (test/is (= {[1 1]  1.0
                     [2 2]  2.0
                     [3 3]  3.0
                     [4 4]  4.0
                     [5 6]  1.0
                     [7 7]  5.0
                     [8 8]  6.0
                     [9 10] 1.0}
                    (intranges-mapping-for-config-key config "SOME_PARAM" [1 10] 1.0)))))

    (test/testing "unbounded max fuel number"
      (let [config {"SOME_PARAM(301:)" [4.0 5.0 6.0]}]
        (test/is (= {[1 300]   1.0
                     [301 301] 4.0
                     [302 302] 5.0
                     [303 303] 6.0}
                    (intranges-mapping-for-config-key config "SOME_PARAM" [1 303] 1.0)))
        "should create intrange entries from specified min up to max fuel number 303"))

    (test/testing "unbounded min fuel number"
      (let [config {"SOME_PARAM(:3)" [1.0 2.0 3.0]}]
        (test/is (= {[1 1]   1.0
                     [2 2]   2.0
                     [3 3]   3.0
                     [4 303] 1.0}
                    (intranges-mapping-for-config-key config "SOME_PARAM" [1 303] 1.0)))
        "should create fuel-number entries from minimal fuel number 1 up to end of specified max"))))

;;=============================================================================
;; File access functions
;;=============================================================================

(defn file->path
  ^String [f]
  (.getPath (io/file f)))

(defn relative-path?
  [path]
  (re-matches #"^((\.){1,2}\/)+.*" path))

(defn build-file-path
  [path]
  (if (relative-path? path)
    (tagged-literal 'gridfire.utils.files/from-this-file path)
    path))

;;=============================================================================
;; Write gridfire.edn
;;=============================================================================

(defn write-config [{:keys [output-dir output-edn] :as _options}]
  (let [output-file-path (.toString (io/file output-dir "gridfire.edn"))]
    (println "Creating config file:" output-file-path)
    (with-open [writer (io/writer output-file-path)]
      (pprint output-edn writer))))

;;=============================================================================
;; Merge Override Config
;;=============================================================================

(defn merge-override-config [override-config-file-path config-params]
  (if override-config-file-path
    (->> (slurp override-config-file-path)
         (edn/read-string)
         (reduce-kv (fn [acc k v]
                      (if (nil? v)
                        (dissoc! acc k)
                        (assoc! acc k v)))
                    (transient config-params))
         (persistent!))
    config-params))

;;=============================================================================
;; Resolve Layer spec Helper
;;=============================================================================

(def layer-key->unit
  {"CBH_FILENAME" :metric
   "CH_FILENAME"  :metric
   "CBD_FILENAME" :metric
   "DEM_FILENAME" :metric})

(def layer-key->multiplier
  {"CBH_FILENAME" 0.1
   "CH_FILENAME"  0.1
   "CBD_FILENAME" 0.01})

(defn- compute-grid2d
  [[i-length j-length] f]
  (->> (range i-length)
       (mapv (fn [grid-i]
               (->> (range j-length)
                    (mapv (fn [grid-j]
                            (f grid-i grid-j))))))))

(defn- build-grid-of-rasters
  [folder-name file-name]
  {:type         :grid-of-rasters
   :rasters-grid (compute-grid2d [3 3]
                                 (fn [^long grid-i ^long grid-j]
                                   (let [elm-x (inc grid-j)
                                         elm-y (inc (- 2 grid-i))]
                                     {:type   :gridfire-envi-bsq
                                      :source (build-file-path (file->path (io/file folder-name
                                                                                    (format "%s_%d_%d.bsq" file-name elm-x elm-y))))})))})

(defn resolve-layer-spec [{:strs [USE_TILED_IO] :as elmfire-config} folder-name layer-key]
  (let [file-name (get elmfire-config layer-key)]
    (cond-> (if (true? USE_TILED_IO)
              (build-grid-of-rasters folder-name file-name)
              {:type   :geotiff
               :source (build-file-path (file->path (io/file folder-name
                                                             (str file-name ".tif"))))})
      (contains? layer-key->unit layer-key)       (assoc :units (get layer-key->unit layer-key))
      (contains? layer-key->multiplier layer-key) (assoc :multiplier (get layer-key->multiplier layer-key)))))

(test/deftest resolve-layer-spec-test
  (test/testing "single geotiff"
    (test/is (= {:type :geotiff :source (tagged-literal 'gridfire.utils.files/from-this-file "./fuel_and_topography/asp.tif")}
                (resolve-layer-spec {"USE_TILED_IO" false "ASP_FILENAME" "asp"}
                                    "./fuel_and_topography"
                                    "ASP_FILENAME"))))
  (test/testing "grid of bsqs"
    (test/is (= {:type :grid-of-rasters,
                 :rasters-grid
                 [[{:type :gridfire-envi-bsq :source (tagged-literal 'gridfire.utils.files/from-this-file "./fuel_and_topography/asp_1_3.bsq")}
                   {:type :gridfire-envi-bsq :source (tagged-literal 'gridfire.utils.files/from-this-file "./fuel_and_topography/asp_2_3.bsq")}
                   {:type :gridfire-envi-bsq :source (tagged-literal 'gridfire.utils.files/from-this-file "./fuel_and_topography/asp_3_3.bsq")}]
                  [{:type :gridfire-envi-bsq :source (tagged-literal 'gridfire.utils.files/from-this-file "./fuel_and_topography/asp_1_2.bsq")}
                   {:type :gridfire-envi-bsq :source (tagged-literal 'gridfire.utils.files/from-this-file "./fuel_and_topography/asp_2_2.bsq")}
                   {:type :gridfire-envi-bsq :source (tagged-literal 'gridfire.utils.files/from-this-file "./fuel_and_topography/asp_3_2.bsq")}]
                  [{:type :gridfire-envi-bsq :source (tagged-literal 'gridfire.utils.files/from-this-file "./fuel_and_topography/asp_1_1.bsq")}
                   {:type :gridfire-envi-bsq :source (tagged-literal 'gridfire.utils.files/from-this-file "./fuel_and_topography/asp_2_1.bsq")}
                   {:type :gridfire-envi-bsq :source (tagged-literal 'gridfire.utils.files/from-this-file "./fuel_and_topography/asp_3_1.bsq")}]]}
                (resolve-layer-spec {"USE_TILED_IO" true "ASP_FILENAME" "asp"}
                                    "./fuel_and_topography"
                                    "ASP_FILENAME")))))

;;=============================================================================
;; LANDFIRE
;;=============================================================================

(defn process-landfire-layers
  [output-edn {:keys [ elmfire-config] :as _options}]
  (let [{:strs [FUELS_AND_TOPOGRAPHY_DIRECTORY]} elmfire-config]
    (assoc output-edn
           :landfire-layers
           {:aspect             (resolve-layer-spec elmfire-config FUELS_AND_TOPOGRAPHY_DIRECTORY "ASP_FILENAME")
            :canopy-base-height (resolve-layer-spec elmfire-config FUELS_AND_TOPOGRAPHY_DIRECTORY "CBH_FILENAME")
            :canopy-cover       (resolve-layer-spec elmfire-config FUELS_AND_TOPOGRAPHY_DIRECTORY "CC_FILENAME")
            :canopy-height      (resolve-layer-spec elmfire-config FUELS_AND_TOPOGRAPHY_DIRECTORY "CH_FILENAME")
            :crown-bulk-density (resolve-layer-spec elmfire-config FUELS_AND_TOPOGRAPHY_DIRECTORY "CBD_FILENAME")
            :elevation          (resolve-layer-spec elmfire-config FUELS_AND_TOPOGRAPHY_DIRECTORY "DEM_FILENAME")
            :fuel-model         (resolve-layer-spec elmfire-config FUELS_AND_TOPOGRAPHY_DIRECTORY "FBFM_FILENAME")
            :slope              (resolve-layer-spec elmfire-config FUELS_AND_TOPOGRAPHY_DIRECTORY "SLP_FILENAME")})))

;;=============================================================================
;; Ignition
;;=============================================================================
(defn setup-ignition-from-layers
  [output-edn {:keys [elmfire-config] :as _options}]
  (let [{:strs [FUELS_AND_TOPOGRAPHY_DIRECTORY RANDOM_IGNITIONS
                USE_IGNITION_MASK EDGEBUFFER]} elmfire-config]
    (if RANDOM_IGNITIONS
      (assoc output-edn
             :random-ignition
             (cond-> {}
               USE_IGNITION_MASK
               (assoc :ignition-mask (resolve-layer-spec elmfire-config FUELS_AND_TOPOGRAPHY_DIRECTORY "IGNITION_MASK_FILENAME"))

               EDGEBUFFER
               (assoc :edge-buffer (m->ft EDGEBUFFER))))
      (assoc output-edn
             :ignition-layer
             (-> (resolve-layer-spec elmfire-config FUELS_AND_TOPOGRAPHY_DIRECTORY "PHI_FILENAME")
                 (assoc :burn-values {:burned   -1.0
                                      :unburned 1.0}))))))

(defn process-ignition
  [output-edn options]
  (if (:elmfire-summary-maps options)
    output-edn
    (setup-ignition-from-layers output-edn options)))

;;=============================================================================
;; Weather
;;=============================================================================

;; FIXME: Since tmpf.tif and rh.tif aren't provided in elmfire.data, where are these files on disk?
(defn process-weather
  [output-edn {:keys [elmfire-config] :as _options}]
  (let [{:strs [WEATHER_DIRECTORY]} elmfire-config]
    (assoc output-edn
           :wind-speed-20ft     (resolve-layer-spec elmfire-config WEATHER_DIRECTORY "WS_FILENAME")
           :wind-from-direction (resolve-layer-spec elmfire-config WEATHER_DIRECTORY "WD_FILENAME"))))

;;=============================================================================
;; Output
;;=============================================================================

(defn process-output
  [output-edn {:keys [elmfire-config] :as _options}]
  (let [{:strs [OUTPUTS_DIRECTORY DUMP_BURN_PROBABILITY_AT_DTDUMP DTDUMP]} elmfire-config]
    (cond-> (assoc output-edn
                   :output-directory        (build-file-path OUTPUTS_DIRECTORY)
                   :outfile-suffix          ""
                   :output-landfire-inputs? false
                   :output-geotiffs?        false
                   :output-pngs?            false
                   :output-binary?          true
                   :output-csvs?            true)
      DUMP_BURN_PROBABILITY_AT_DTDUMP (assoc :burn-probability (sec->min DTDUMP)))))

;;=============================================================================
;; Perturbations
;;=============================================================================

(def unused-perturbations #{:crown-bulk-density :canopy-base-height})

(def layers-in-metric #{:crown-bulk-density :canopy-base-height :canopy-height})

(def layers-in-ratio #{:fuel-moisture-dead-1hr
                       :fuel-moisture-dead-10hr
                       :fuel-moisture-dead-100hr
                       :fuel-moisture-live-herbaceous
                       :fuel-moisture-live-woody})

(def elmfire->gridfire
  "A mapping of ELMFIRE string names to GridFire keywords"
  {"CBH"    :canopy-base-height
   "CC"     :canopy-cover
   "CH"     :canopy-height
   "CBD"    :crown-bulk-density
   "WS"     :wind-speed-20ft
   "WD"     :wind-from-direction
   "M1"     :fuel-moisture-dead-1hr
   "M10"    :fuel-moisture-dead-10hr
   "M100"   :fuel-moisture-dead-100hr
   "MLH"    :fuel-moisture-live-herbaceous
   "MLW"    :fuel-moisture-live-woody
   "GLOBAL" :global
   "PIXEL"  :pixel})

(defn perturbation-info
  [config index key]
  (cond-> {:spatial-type (->> (str "SPATIAL_PERTURBATION-" index)
                              (get config)
                              (get elmfire->gridfire))
           :range        [(get config (str "PDF_LOWER_LIMIT-" index))
                          (get config (str "PDF_UPPER_LIMIT-" index))]}
    (layers-in-metric key) (assoc :units :metric)
    (layers-in-ratio key)  (assoc :units :ratio)))

(defn perturbation-key
  [config index]
  (->> (str "RASTER_TO_PERTURB-" index)
       (get config)
       (get elmfire->gridfire)))

(defn extract-perturbations
  [{:strs [^long NUM_RASTERS_TO_PERTURB] :as config}]
  (when (and NUM_RASTERS_TO_PERTURB (pos? NUM_RASTERS_TO_PERTURB))
    (into {}
          (keep (fn [index]
                  (when-let [key (perturbation-key config index)]
                    (when-not (unused-perturbations key)
                      [key (perturbation-info config index key)]))))
          (range 1 (inc NUM_RASTERS_TO_PERTURB)))))

(defn process-perturbations
  [output-edn {:keys [elmfire-config] :as _options}]
  (if-let [perturbations (extract-perturbations elmfire-config)]
    (assoc output-edn :perturbations perturbations)
    output-edn))

;;=============================================================================
;; Spotting
;;=============================================================================

;; FIXME: Is this logic (and return format) right?
(defn extract-num-firebrands
  [{:strs [NEMBERS NEMBERS_MIN NEMBERS_MIN_LO NEMBERS_MIN_HI NEMBERS_MAX
           NEMBERS_MAX_LO NEMBERS_MAX_HI]}]
  (if (and (or NEMBERS_MIN NEMBERS_MIN_LO)
           (or NEMBERS_MAX NEMBERS_MAX_LO))
    {:lo (cond
           (and NEMBERS_MIN_LO (= NEMBERS_MIN_LO NEMBERS_MIN_HI)) NEMBERS_MIN_LO
           NEMBERS_MIN_LO                                         [NEMBERS_MIN_LO NEMBERS_MIN_HI]
           :else                                                  NEMBERS_MIN)
     :hi (cond (and NEMBERS_MAX_LO (= NEMBERS_MAX_LO NEMBERS_MAX_HI)) NEMBERS_MAX_LO
               NEMBERS_MAX_LO                                         [NEMBERS_MAX_LO NEMBERS_MAX_HI]
               :else                                                  NEMBERS_MAX)}
    NEMBERS))

;; FIXME: Is this logic (and return format) right?
(defn extract-crown-fire-spotting-percent
  [{:strs [^double CROWN_FIRE_SPOTTING_PERCENT_MIN
           ^double CROWN_FIRE_SPOTTING_PERCENT_MAX
           ^double CROWN_FIRE_SPOTTING_PERCENT]}]
  (if (and CROWN_FIRE_SPOTTING_PERCENT_MIN CROWN_FIRE_SPOTTING_PERCENT_MAX)
    [(* 0.01 CROWN_FIRE_SPOTTING_PERCENT_MIN) (* 0.01 CROWN_FIRE_SPOTTING_PERCENT_MAX)]
    (* 0.01 CROWN_FIRE_SPOTTING_PERCENT)))

(defn extract-normalized-distance-variance
  [{:strs [NORMALIZED_SPOTTING_DIST_VARIANCE_MIN
           NORMALIZED_SPOTTING_DIST_VARIANCE_MAX
           NORMALIZED_SPOTTING_DIST_VARIANCE]}]
  (or NORMALIZED_SPOTTING_DIST_VARIANCE
      {:lo NORMALIZED_SPOTTING_DIST_VARIANCE_MIN
       :hi NORMALIZED_SPOTTING_DIST_VARIANCE_MAX}))

(defn extract-flin-exp
  [{:strs [SPOT_FLIN_EXP_LO SPOT_FLIN_EXP_HI SPOT_FLIN_EXP]}]
  (or SPOT_FLIN_EXP
      {:lo SPOT_FLIN_EXP_LO
       :hi SPOT_FLIN_EXP_HI}))

(defn extract-ws-exp
  [{:strs [SPOT_WS_EXP_LO SPOT_WS_EXP_HI SPOT_WS_EXP]}]
  (or SPOT_WS_EXP
      {:lo SPOT_WS_EXP_LO
       :hi SPOT_WS_EXP_HI}))

(defn extract-mean-distance
  [{:strs [MEAN_SPOTTING_DIST_MIN MEAN_SPOTTING_DIST_MAX MEAN_SPOTTING_DIST]}]
  (or MEAN_SPOTTING_DIST
      {:lo MEAN_SPOTTING_DIST_MIN
       :hi MEAN_SPOTTING_DIST_MAX}))

(defn- multiply-spotting-pct
  [spotting-pct multiplier]
  (cond
    (number? spotting-pct) (* spotting-pct multiplier)
    (vector? spotting-pct) (let [[p0 p1] spotting-pct]
                             [(* p0 multiplier)
                              (* p1 multiplier)])))

(defn- apply-spotting-multipliers
  [output-edn l+h->multiplier]
  (update-in output-edn
             [:spotting :surface-fire-spotting :spotting-percent]
             (fn [l+h->spotting-pct]
               (-> (combine-intranges-mappings l+h->spotting-pct
                                               (-> l+h->multiplier
                                                   (complete-intranges-mapping [1 303] 1.0))
                                               multiply-spotting-pct)
                   (vec)))))

(defn extract-global-surface-spotting-percents
  [{:strs
    [^double GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN
     ^double GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX
     ^double GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT] :as elmfire-config}]
  (cond
    (and GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX)
    [[[1 303] [(* 0.01 GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MIN) (* 0.01 GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT_MAX)]]]

    GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT
    [[[1 303] (* 0.01 GLOBAL_SURFACE_FIRE_SPOTTING_PERCENT)]]

    :else (intranges-mapping-for-config-key elmfire-config "SURFACE_FIRE_SPOTTING_PERCENT(" [1 303] 0.0)))

(defn process-spotting
  [output-edn {:keys [elmfire-config] :as _options}]
  (let [{:strs [ENABLE_SPOTTING ENABLE_SURFACE_FIRE_SPOTTING CRITICAL_SPOTTING_FIRELINE_INTENSITY]} elmfire-config]
    (cond-> output-edn
      ENABLE_SPOTTING
      (assoc :spotting
             {:mean-distance                (extract-mean-distance elmfire-config)
              :ws-exp                       (extract-ws-exp elmfire-config)
              :flin-exp                     (extract-flin-exp elmfire-config)
              :normalized-distance-variance (extract-normalized-distance-variance elmfire-config)
              :crown-fire-spotting-percent  (extract-crown-fire-spotting-percent elmfire-config)
              :num-firebrands               (extract-num-firebrands elmfire-config)
              :decay-constant               0.005}
             ;; FIXME Elmfire does not use relative-humidity but GF needs it for Spotting.
             ;; Default to 20 or set in override-config
             :relative-humidity 20)

      (and ENABLE_SPOTTING ENABLE_SURFACE_FIRE_SPOTTING)
      (-> (assoc-in [:spotting :surface-fire-spotting]
                    {:spotting-percent             (extract-global-surface-spotting-percents elmfire-config)
                     :critical-fire-line-intensity (or (some-> CRITICAL_SPOTTING_FIRELINE_INTENSITY kW-m->Btu-ft-s)
                                                       (intranges-mapping-for-config-key elmfire-config "CRITICAL_SPOTTING_FIRELINE_INTENSITY(" [1 303] 0.0)
                                                       0.0)})
          (apply-spotting-multipliers (intranges-mapping-for-config-key elmfire-config "SURFACE_FIRE_SPOTTING_PERCENT_MULT" [1 303] 1.0))))))

;;=============================================================================
;; Fuel moisture layers
;;=============================================================================

;; FIXME: Since mlw.tif and mlh.tif aren't provided in elmfire.data, where are these files on disk?
(defn process-fuel-moisture
  [output-edn {:keys [elmfire-config] :as _options}]
  (let [{:strs [WEATHER_DIRECTORY USE_CONSTANT_LW USE_CONSTANT_LH LW_MOISTURE_CONTENT
                LH_MOISTURE_CONTENT]} elmfire-config]
    (assoc output-edn
           :fuel-moisture
           {:dead {:1hr   (resolve-layer-spec elmfire-config WEATHER_DIRECTORY "M1_FILENAME")
                   :10hr  (resolve-layer-spec elmfire-config WEATHER_DIRECTORY "M10_FILENAME")
                   :100hr (resolve-layer-spec elmfire-config WEATHER_DIRECTORY "M100_FILENAME")}
            :live {:woody      (if USE_CONSTANT_LW
                                 (* 0.01 ^double LW_MOISTURE_CONTENT)
                                 (resolve-layer-spec elmfire-config WEATHER_DIRECTORY "MLW_FILENAME"))
                   :herbaceous (if USE_CONSTANT_LH
                                 (* 0.01 ^double LH_MOISTURE_CONTENT)
                                 (resolve-layer-spec elmfire-config WEATHER_DIRECTORY "MLH_FILENAME"))}})))

;;=============================================================================
;; Suppression
;;=============================================================================

(defn- process-suppression
  [output-edn {:keys [elmfire-config]}]
  (let [{:strs
         [FUELS_AND_TOPOGRAPHY_DIRECTORY USE_SDI B_SDI
          AREA_NO_CONTAINMENT_CHANGE MAX_CONTAINMENT_PER_DAY]} elmfire-config]
    (if USE_SDI
      (assoc output-edn
             :suppression
             {:suppression-dt                                60.0
              :sdi-layer                                     (resolve-layer-spec elmfire-config FUELS_AND_TOPOGRAPHY_DIRECTORY "SDI_FILENAME")
              :sdi-sensitivity-to-difficulty                 B_SDI
              :sdi-containment-overwhelming-area-growth-rate AREA_NO_CONTAINMENT_CHANGE
              :sdi-reference-suppression-speed               MAX_CONTAINMENT_PER_DAY})
      output-edn)))

;;=============================================================================
;; Pyrome specific calibration
;;=============================================================================

(defn- pyrome-csv-rows->lookup-map
  ([csv-rows]
   (pyrome-csv-rows->lookup-map csv-rows nil))

  ([[header-row & data-rows :as _csv-rows] col-name-parse-fn]
   (let [[_pyrome-colname & rest-colname] header-row
         rest-colname-parsed              (if col-name-parse-fn
                                            (mapv col-name-parse-fn rest-colname)
                                            rest-colname)]
     (->> data-rows
          (map (fn to-map [[pyrome-id & double-params]]
                 [(Long/parseLong pyrome-id 10)
                  (zipmap rest-colname-parsed
                          (mapv (fn [s] (Double/parseDouble s)) double-params))]))
          (into {})))))

(defn- process-pyrome-calibration-csv
  [{:keys [pyrome-samples] :as output-edn } {:keys [pyrome-calibration-csv] :as _options}]
  (let [[header & _ :as csv-rows]     (with-open [reader (io/reader pyrome-calibration-csv)]
                                        (-> reader
                                            csv/read-csv
                                            doall))
        pyrome->calibration-constants (pyrome-csv-rows->lookup-map csv-rows keyword)
        header-set                    (set header)]
    (cond-> output-edn

      (contains? header-set "sdi-sensitivity-to-difficulty")
      (assoc :sdi-sensitivity-to-difficulty-samples
             (mapv (fn [pyrome-sample]
                     (get-in pyrome->calibration-constants
                             [pyrome-sample :sdi-sensitivity-to-difficulty]))
                   pyrome-samples))

      (contains? header-set "sdi-reference-suppression-speed")
      (assoc :sdi-reference-suppression-speed-samples
             (mapv (fn [pyrome-sample]
                     (get-in pyrome->calibration-constants
                             [pyrome-sample :sdi-reference-suppression-speed]))
                   pyrome-samples))

      (contains? header-set "sdi-containment-overwhelming-area-growth-rate")
      (assoc :sdi-containment-overwhelming-area-growth-rate-samples
             (mapv (fn [pyrome-sample]
                     (get-in pyrome->calibration-constants
                             [pyrome-sample :sdi-containment-overwhelming-area-growth-rate]))
                   pyrome-samples)))))

(defn- process-pyrome-spread-rate-adjustment-csv
  [{:keys [pyrome-samples] :as output-edn } {:keys [pyrome-spread-rate-adjustment-csv] :as _options}]
  (let [pyrome->spread-rate-adjustment (with-open [reader (io/reader pyrome-spread-rate-adjustment-csv)]
                                         (-> reader
                                             csv/read-csv
                                             doall
                                             (pyrome-csv-rows->lookup-map (fn [s] (Long/parseLong s 10)))))]
    (assoc output-edn
           :fuel-number->spread-rate-adjustment-samples
           (tagged-literal 'gridfire.config/abbreviating [pyrome->spread-rate-adjustment (vec pyrome-samples)]))))

(defn process-pyrome-specific-calibration
  [output-edn {:keys [pyrome-spread-rate-adjustment-csv pyrome-calibration-csv] :as options}]
  (cond-> output-edn
    pyrome-spread-rate-adjustment-csv (process-pyrome-spread-rate-adjustment-csv options)
    pyrome-calibration-csv            (process-pyrome-calibration-csv options)))

;;=============================================================================
;; elmfire-summary-file
;;=============================================================================

(defn process-elmfire-summary-maps
  [output-edn {:keys [elmfire-summary-maps]}]
  (if elmfire-summary-maps
    (-> output-edn
        (assoc :ignition-rows        (mapv :ignition-row elmfire-summary-maps))
        (assoc :ignition-cols        (mapv :ignition-col elmfire-summary-maps))
        (assoc :ignition-start-times (mapv :ignition-start-time elmfire-summary-maps))
        (assoc :max-runtime-samples  (mapv :max-runtime elmfire-summary-maps))
        (assoc :pyrome-samples       (mapv :pyrome elmfire-summary-maps))
        (assoc :simulations          (count elmfire-summary-maps)))
    output-edn))


;;=============================================================================
;; Build gridfire.edn
;;=============================================================================

(defn- remove-unecessary-keys
  [output-edn]
  (dissoc output-edn :pyrome-samples))

(defn add-output-edn
  [{:keys [elmfire-config] :as options}]
  (let [{:strs [COMPUTATIONAL_DOMAIN_CELLSIZE A_SRS SIMULATION_TSTOP SEED FOLIAR_MOISTURE_CONTENT
                NUM_ENSEMBLE_MEMBERS]} elmfire-config]
    (assoc options
           :output-edn
           (-> {:cell-size                       (m->ft COMPUTATIONAL_DOMAIN_CELLSIZE)
                :srid                            (or A_SRS "EPSG:32610")
                :max-runtime                     (sec->min SIMULATION_TSTOP)
                :simulations                     NUM_ENSEMBLE_MEMBERS
                ;; FIXME temperature elmfire does not use temperature, this is a required key in gridfire.
                ;; Default to 80 or set in override-config
                :temperature                     80
                :random-seed                     SEED
                :foliar-moisture                 FOLIAR_MOISTURE_CONTENT
                :ellipse-adjustment-factor       1.0
                :parallel-strategy               :between-fires
                :fractional-distance-combination :sum} ; FIXME: unused parameter
               (process-landfire-layers options)
               (process-ignition options)
               (process-weather options)
               (process-output options)
               (process-perturbations options)
               (process-spotting options)
               (process-fuel-moisture options)
               (process-suppression options)
               (process-elmfire-summary-maps options)
               (process-pyrome-specific-calibration options)
               (remove-unecessary-keys)))))


;;=============================================================================
;; Parse-elmfire-summary-csv
;;=============================================================================

(defn parse-fire-stats-line
  [{:strs [COMPUTATIONAL_DOMAIN_CELLSIZE
           COMPUTATIONAL_DOMAIN_XLLCORNER
           COMPUTATIONAL_DOMAIN_YLLCORNER
           COMPUTATIONAL_DOMAIN_YDIM]}
   [_ metband x y tstop & remaining-columns]]
  {:ignition-row        (-> y
                            str/trim
                            Float/parseFloat
                            (- COMPUTATIONAL_DOMAIN_YLLCORNER)
                            (- COMPUTATIONAL_DOMAIN_YDIM)
                            Math/abs
                            (/ COMPUTATIONAL_DOMAIN_CELLSIZE)
                            int)
   :ignition-col        (-> x
                            str/trim
                            Float/parseFloat
                            (- COMPUTATIONAL_DOMAIN_XLLCORNER)
                            (/ COMPUTATIONAL_DOMAIN_CELLSIZE)
                            int)
   :ignition-start-time (* (Long/parseLong (str/trim metband) 10) 60.0)
   :max-runtime         (* (Float/parseFloat (str/trim tstop)) 60.0)
   :pyrome              (* (Long/parseLong (str/trim (nth remaining-columns 8)) 10))})

(defn parse-elmfire-summary-csv [{:keys [elmfire-config elmfire-summary-csv] :as options}]
  (if elmfire-summary-csv
    (with-open [reader (io/reader elmfire-summary-csv)]
      (let [[_header-row & data-rows] (doall (csv/read-csv reader))]
        (->> data-rows
             (map #(parse-fire-stats-line elmfire-config %))
             (assoc options :elmfire-summary-maps))))
    options))

;;=============================================================================
;; Parse elmfire.data
;;=============================================================================

(defn get-srid
  [proj-string]
  (let [{:keys [err out]} (sh "gdalsrsinfo" "-e" proj-string "-o" "epsg" "--single-line")]
    (when (empty? err)
      (->> out
           (str/split-lines)
           (filter #(str/includes? % "EPSG"))
           (first)))))

(def regex-for-array-item #"^[A-Z0-9\_]+\(\d+\)")

(defn convert-key [s]
  (when (string? s)
    (let [s-trimmed (str/trim s)]
      (if (re-matches regex-for-array-item s-trimmed)
        (str/join "-" (str/split s-trimmed #"[\(\)]"))
        s-trimmed))))

(defn convert-val [s]
  (when (string? s)
    (let [s-trimmed  (str/trim s)
          char-count (count s-trimmed)]
      (cond
        (re-matches #"^-?[0-9]\d*\.(\d+)?$" s-trimmed) (Double/parseDouble s-trimmed)
        (re-matches #"^-?\d+$" s-trimmed)              (Long/parseLong s-trimmed 10)
        (re-matches #".TRUE." s-trimmed)               true
        (re-matches #".FALSE." s-trimmed)              false
        (re-matches #"'[0-9a-zA-Z_.//]*'" s-trimmed)   (subs s-trimmed 1 (dec char-count))
        (str/includes? s-trimmed "proj")               (get-srid (subs s-trimmed 1 (dec char-count)))
        (str/includes? s-trimmed ",")                  (mapv (fn [x] (Double/parseDouble x)) (str/split s-trimmed #","))
        :else                                          nil))))

(test/deftest convert-val-test
  (test/testing "value is comma seperated values"
    (let [to-convert "1., 1., 1.,"]
      (test/is (= [1.0 1.0 1.0] (convert-val to-convert) )))

    (let [to-convert "1.0, 1.0, 1.0,"]
      (test/is (= [1.0 1.0 1.0] (convert-val to-convert))))))

(defn parse-elmfire-config [{:keys [elmfire-config] :as options}]
  (let [content (slurp elmfire-config)]
    (assoc options
           :elmfire-config (transduce (comp (filter #(str/includes? % " = "))
                                            (map #(str/split % #" = ")))
                                      (completing (fn [acc [k v]]
                                                    (assoc acc
                                                           (convert-key k)
                                                           (convert-val v))))
                                      (sorted-map)
                                      (str/split content #"\n"))
           :output-dir (.getParent (io/file elmfire-config)))))

;;=============================================================================
;; Main
;;=============================================================================

(defn convert-config! [{:keys [override-config] :as options}]
  (println "Converting configuration file to one that GridFire accepts.")
  (->> options
       (parse-elmfire-config)
       (parse-elmfire-summary-csv)
       (add-output-edn)
       (merge-override-config override-config)
       (write-config)))

(def cli-options
  [[nil "--elmfire-config PATH" "PATH to an elmfire.data configuration file"
    :id :elmfire-config
    :validate [#(.exists  (io/file %)) "The provided --elmfire-config does not exist."
               #(.canRead (io/file %)) "The provided --elmfire-config--override-config is not readable."]]

   [nil "--elmfire-summary-csv PATH" "Optinal PATH to summary csv file for a completed elmfire run. Determines the ignition locations and pyrome-specific parameters."
    :id :elmfire-summary-csv
    :validate [#(.exists  (io/file %)) "The provided --elmfire-summary-csv does not exist."
               #(.canRead (io/file %)) "The provided --elmfire-summary-csv is not readable."]]

   [nil "--override-config PATH" "PATH to override.edn file that will append to and/or replace any computed keys and their value."
    :id :override-config
    :validate [#(.exists  (io/file %)) "The provided --override-config does not exist."
               #(.canRead (io/file %)) "The provided --override-config is not readable."]]

   [nil "--pyrome-spread-rate-adjustment-csv PATH" "Optional PATH to the pyrome specific fuel -> spread rate adjustments csv."
    :id :pyrome-spread-rate-adjustment-csv
    :validate [#(.exists  (io/file %)) "The provided --pyrome-spread-rate-adjustment-csv does not exist."
               #(.canRead (io/file %)) "The provided --pyrome-spread-rate-adjustment-csv is not readable."]]

   [nil "--pyrome-calibration-csv PATH" "Optinal PATH to pyrome specific calibration constants."
    :id :pyrome-calibration-csv
    :validate [#(.exists  (io/file %)) "The provided --pyrome-calibration-csv does not exist."
               #(.canRead (io/file %)) "The provided --pyrome-calibration-csv is not readable."]]])

(def program-banner
  (str "elm_to_grid.clj: Generate a gridfire.edn file from an elmfire.data file.\n"
       "Copyright © 2020-2022 Spatial Informatics Group, LLC.\n"))

(defn main [args]
  (println program-banner)
  (let [{:keys [options summary errors]} (parse-opts args cli-options)]
    ;; {:options   The options map, keyed by :id, mapped to the parsed value
    ;;  :summary   A string containing a minimal options summary
    ;;  :errors    A vector of error message strings thrown during parsing; nil when no errors exist
    (cond
      ;; Errors encountered during input parsing
      (seq errors)
      (do
        (run! println errors)
        (println (str "\nUsage:\n" summary)))

      ;; Valid --elmfire-config argument provided, so perform conversion
      (:elmfire-config options)
      (convert-config! options)

      ;; Incorrect CLI invocation
      :else
      (do
        (println "You must provide a valid --elmfire-config file path to initiate conversion.")
        (println (str "\nUsage:\n" summary))))

    ;; Exit cleanly
    (System/exit 0)))

;; (test/run-tests)

(main *command-line-args*)
