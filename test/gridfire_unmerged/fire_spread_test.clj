;; [[file:../../org/GridFire.org::gridfire-unmerged.fire-spread-test][gridfire-unmerged.fire-spread-test]]
;; (ns gridfire-unmerged.fire-spread-test
;;   (:require [clojure.java.io :as io]
;;             [clojure.core.matrix :as m]
;;             [clojure.core.matrix.operators :as mop]
;;             [clojure.core.reducers :as r]
;;             [clojure.java.jdbc :as jdbc]
;;             [clojure.data.csv :as csv]
;;             [clojure.set :as set]
;;             [matrix-viz.core :refer [save-matrix-as-png]]
;;             [gridfire.common :refer [burnable-fuel-model? burnable? burnable-neighbors?]]
;;             [gridfire.fuel-models :refer [fuel-models build-fuel-model moisturize]]
;;             [gridfire.surface-fire :refer [rothermel-surface-fire-spread-no-wind-no-slope
;;                                            rothermel-surface-fire-spread-max
;;                                            rothermel-surface-fire-spread-any
;;                                            anderson-flame-depth
;;                                            byram-fire-line-intensity
;;                                            byram-flame-length
;;                                            wind-adjustment-factor
;;                                            degrees-to-radians grass-fuel-model?]]
;;             [gridfire.fire-spread :refer [run-fire-spread rothermel-fast-wrapper]]
;;             [gridfire.monte-carlo :refer [cells-to-acres]]
;;             [gridfire.postgis-bridge :refer [postgis-raster-to-matrix]]
;;             [tech.v3.tensor         :as t]
;;             [tech.v3.datatype       :as d]
;;             [tech.v3.datatype.functional :as dfn])
;;   (:import org.postgresql.jdbc.PgArray))

;; (defn combine-into-map
;;   ([] {})
;;   ([r1] r1)
;;   ([r1 r2] (merge r1 r2)))

;; (def anderson-sample-values
;;   [[ 78.0  4.0]
;;    [ 35.0  6.0]
;;    [104.0 12.0]
;;    [ 75.0 19.0]
;;    [ 18.0  4.0]
;;    [ 32.0  6.0]
;;    [ 20.0  5.0]
;;    [  1.6  1.0]
;;    [  7.5  2.6]
;;    [  7.9  4.8]
;;    [  6.0  3.5]
;;    [ 13.0  8.0]
;;    [ 13.5 10.5]])

;; ;; Compare with tables in Anderson 1982
;; ;; Largest error is: +6.5 ft/min, -9.6 in
;; (for [fuel-model-number (range 1 14)]
;;   (let [[anderson-spread-rate anderson-flame-length] (anderson-sample-values (dec fuel-model-number))
;;         sample-fuel-moisture                         {:dead {:1hr 0.08 :10hr 0.08 :100hr 0.08} :live {:herbaceous 1.0 :woody 1.0}}
;;         spread-info-min                              (-> (build-fuel-model fuel-model-number)
;;                                                          (moisturize sample-fuel-moisture)
;;                                                          (rothermel-surface-fire-spread-no-wind-no-slope))
;;         spread-info-max                              (rothermel-surface-fire-spread-max spread-info-min 440.0 0.0 0.0 0.0 1.0)
;;         spread-rate                                  (:max-spread-rate spread-info-max)
;;         flame-depth                                  (anderson-flame-depth spread-rate (:residence-time spread-info-min))
;;         fire-line-intensity                          (byram-fire-line-intensity (:reaction-intensity spread-info-min)
;;                                                                                 flame-depth)
;;         flame-length                                 (byram-flame-length fire-line-intensity)]
;;     (format "%3d %5.1f %5.1f %4.1f(%4.1f) %4.1f %4.1f %4.1f(%4.1f)" fuel-model-number
;;             (/ spread-rate 1.1) anderson-spread-rate
;;             (- (/ spread-rate 1.1) anderson-spread-rate)
;;             (* 1.1 (- (/ spread-rate 1.1) anderson-spread-rate))
;;             flame-length anderson-flame-length
;;             (- flame-length anderson-flame-length)
;;             (* 12.0 (- flame-length anderson-flame-length)))))

;; (def scott-burgan-sample-values
;;   {101 [ :moderate       :low]
;;    102 [     :high  :moderate]
;;    103 [     :high  :moderate]
;;    104 [:very-high      :high]
;;    105 [:very-high      :high]
;;    106 [:very-high :very-high]
;;    107 [:very-high :very-high]
;;    108 [:very-high :very-high]
;;    109 [  :extreme   :extreme]
;;    121 [ :moderate       :low]
;;    122 [     :high  :moderate]
;;    123 [     :high  :moderate]
;;    124 [     :high :very-high]
;;    141 [ :very-low  :very-low]
;;    142 [      :low       :low]
;;    143 [      :low       :low]
;;    144 [     :high  :moderate]
;;    145 [:very-high :very-high]
;;    146 [     :high      :high]
;;    147 [     :high :very-high]
;;    148 [     :high      :high]
;;    149 [     :high :very-high]
;;    161 [      :low       :low]
;;    162 [ :moderate       :low]
;;    163 [     :high  :moderate]
;;    164 [ :moderate  :moderate]
;;    165 [ :moderate  :moderate]
;;    181 [ :very-low  :very-low]
;;    182 [ :very-low  :very-low]
;;    183 [ :very-low       :low]
;;    184 [      :low       :low]
;;    185 [      :low       :low]
;;    186 [ :moderate       :low]
;;    187 [      :low       :low]
;;    188 [ :moderate       :low]
;;    189 [ :moderate  :moderate]
;;    201 [ :moderate       :low]
;;    202 [ :moderate  :moderate]
;;    203 [     :high      :high]
;;    204 [:very-high :very-high]})

;; (defn qualify-spread-rate
;;   [spread-rate]
;;   (condp >= spread-rate
;;     2   :very-low
;;     5   :low
;;     20  :moderate
;;     50  :high
;;     150 :very-high
;;     300 :extreme))

;; (defn qualify-flame-length
;;   [flame-length]
;;   (condp >= flame-length
;;     1   :very-low
;;     4   :low
;;     8   :moderate
;;     12  :high
;;     25  :very-high
;;     100 :extreme))

;; ;; Compare with tables in Scott & Burgan 2005
;; (for [fuel-model-number (drop 18 (sort (keys fuel-models)))]
;;   (let [[sb-spread-rate sb-flame-length] (scott-burgan-sample-values fuel-model-number)
;;         sample-fuel-moisture             {:dead {:1hr 0.06 :10hr 0.07 :100hr 0.08} :live {:herbaceous 0.60 :woody 0.90}}
;;         spread-info-min                  (-> (build-fuel-model fuel-model-number)
;;                                              (moisturize sample-fuel-moisture)
;;                                              (rothermel-surface-fire-spread-no-wind-no-slope))
;;         spread-info-max                  (rothermel-surface-fire-spread-max spread-info-min 440.0 0.0 0.0 0.0 1.0)
;;         spread-rate                      (:max-spread-rate spread-info-max)
;;         flame-depth                      (anderson-flame-depth spread-rate (:residence-time spread-info-min))
;;         fire-line-intensity              (byram-fire-line-intensity (:reaction-intensity spread-info-min)
;;                                                                     flame-depth)
;;         flame-length                     (byram-flame-length fire-line-intensity)]
;;     (format "| %3d | %10s %2s %10s | %10s %2s %10s |" fuel-model-number
;;             (qualify-spread-rate (/ spread-rate 1.1))
;;             (if (= (qualify-spread-rate (/ spread-rate 1.1)) sb-spread-rate) "==" "!=")
;;             sb-spread-rate
;;             (qualify-flame-length flame-length)
;;             (if (= (qualify-flame-length flame-length) sb-flame-length) "==" "!=")
;;             sb-flame-length)))

;; (def low-fuel-models #{102.0 104.0 105.0 109.0 122.0 123.0 124.0 163.0})
;; (def error-fuel-models #{102.0 121.0 122.0 142.0 145.0 147.0 165.0 189.0})

;; ;; Compare with WAF paper Figure 4 (page 6)
;; (for [waf [0.2 0.4 0.7] wind-speed-20ft [4 8 12 16 20]]
;;   (let [midflame-wind-speed  (* waf 88 wind-speed-20ft)
;;         sample-fuel-moisture {:dead {:1hr 0.05 :10hr 0.05 :100hr 0.05} :live {:herbaceous 0.75 :woody 0.75}}
;;         spread-info-min      (-> (build-fuel-model 2)
;;                                  (moisturize sample-fuel-moisture)
;;                                  (rothermel-surface-fire-spread-no-wind-no-slope))
;;         spread-info-max      (rothermel-surface-fire-spread-max spread-info-min midflame-wind-speed 0.0 0.0 0.0 1.0)
;;         spread-rate          (:max-spread-rate spread-info-max)
;;         flame-depth          (anderson-flame-depth spread-rate (:residence-time spread-info-min))
;;         fire-line-intensity  (byram-fire-line-intensity (:reaction-intensity spread-info-min)
;;                                                         flame-depth)
;;         flame-length         (byram-flame-length fire-line-intensity)]
;;     (format "%2d %3.1f %4.1f %5.1f %4.1f"
;;             wind-speed-20ft waf (/ midflame-wind-speed 88) (/ spread-rate 1.1) flame-length)))

;; (def behaveplus5-surface-fire-values
;;   {1   [1 108.8 5.0]
;;    2   [2 50.6 7.8]
;;    3   [3 144.5 15.6]
;;    4   [4 109.2 24.5]
;;    5   [5 36.1 7.8]
;;    6   [6 41.7 7.1]
;;    7   [7 38.0 7.1]
;;    8   [8 2.5 1.3]
;;    9   [9 10.8 3.4]
;;    10  [10 12.0 6.4]
;;    11  [11 7.3 3.9]
;;    12  [12 16.4 9.4]
;;    13  [13 19.9 12.4]
;;    91  [91 0.0 0.0]
;;    92  [92 0.0 0.0]
;;    93  [93 0.0 0.0]
;;    98  [98 0.0 0.0]
;;    99  [99 0.0 0.0]
;;    101 [101 21.7 2.3]
;;    102 [102 54.8 5.8]
;;    103 [103 77.3 8.8]
;;    104 [104 110.3 10.8]
;;    105 [105 92.7 13.9]
;;    106 [106 121.1 17.4]
;;    107 [107 161.5 22.8]
;;    108 [108 169.6 29.9]
;;    109 [109 274.5 41.2]
;;    121 [121 23.0 4.2]
;;    122 [122 32.3 6.2]
;;    123 [123 50.1 10.1]
;;    124 [124 42.3 18.4]
;;    141 [141 5.4 1.5]
;;    142 [142 9.8 5.7]
;;    143 [143 5.4 2.9]
;;    144 [144 42.0 8.8]
;;    145 [145 77.3 16.4]
;;    146 [146 31.6 11.0]
;;    147 [147 51.0 15.6]
;;    148 [148 29.8 12.1]
;;    149 [149 55.2 20.0]
;;    161 [161 4.2 2.2]
;;    162 [162 15.5 4.2]
;;    163 [163 37.6 9.0]
;;    164 [164 16.0 6.7]
;;    165 [165 12.0 8.7]
;;    181 [181 1.1 0.7]
;;    182 [182 1.7 1.0]
;;    183 [183 2.1 1.2]
;;    184 [184 3.1 1.6]
;;    185 [185 5.6 2.5]
;;    186 [186 7.4 3.1]
;;    187 [187 3.6 2.3]
;;    188 [188 7.4 3.9]
;;    189 [189 11.1 5.6]
;;    201 [201 7.8 3.6]
;;    202 [202 18.9 7.0]
;;    203 [203 34.1 10.5]
;;    204 [204 65.6 14.7]})

;; (defn surface-fire-check [fuel-model-number fuel-moisture midflame-wind-speed slope]
;;   (let [spread-info-min     (second (rothermel-fast-wrapper fuel-model-number fuel-moisture))
;;         spread-info-max     (rothermel-surface-fire-spread-max
;;                              spread-info-min midflame-wind-speed 0.0 slope 0.0 1.0)
;;         flame-depth         (anderson-flame-depth (:max-spread-rate spread-info-max)
;;                                                   (:residence-time spread-info-min))
;;         fire-line-intensity (byram-fire-line-intensity (:reaction-intensity spread-info-min) flame-depth)
;;         flame-length        (byram-flame-length fire-line-intensity)
;;         trim-value          #(/ (Math/round (* % 10.0)) 10.0)]
;;     [(int fuel-model-number) (trim-value (/ (:max-spread-rate spread-info-max) 1.1)) (trim-value flame-length)]))

;; (for [fm-number (sort (keys fuel-models))]
;;   (let [gridfire-results   (surface-fire-check fm-number
;;                                                {:dead {:1hr 0.04 :10hr 0.04 :100hr 0.06}
;;                                                 :live {:herbaceous 0.40 :woody 0.70}}
;;                                                440.0
;;                                                0.1)
;;         behaveplus-results (behaveplus5-surface-fire-values (int fm-number))]
;;     (if (= gridfire-results behaveplus-results)
;;       [fm-number :matched]
;;       [fm-number :no-match gridfire-results behaveplus-results])))

;; (def test-duration 60.0) ; mins
;; (def test-cell-size 98.425) ; 30m
;; (def test-num-rows 200) ; 6km total
;; (def test-num-cols 200) ; 6km total
;; (def test-fuel-moisture {:dead {:1hr 0.04 :10hr 0.04 :100hr 0.06} :live {:herbaceous 0.40 :woody 0.70}})
;; (def test-wind-speed-20ft 25.0) ; mph
;; (def test-wind-direction 45.0) ; from NE
;; (def test-ignition-site [100 100])

;; (defn view-hfire-results
;;   [elevation-matrix slope-matrix fuel-model-matrix ignition-site ellipse-adjustment-factor outfile-base]
;;   (let [landfire-layers      {:elevation          elevation-matrix
;;                               :slope              slope-matrix
;;                               :aspect             (doto (t/new-tensor test-num-rows test-num-cols) (mop/+= 270.0)) ; downhill = west
;;                               :fuel-model         fuel-model-matrix
;;                               :canopy-height      (t/new-tensor test-num-rows test-num-cols)
;;                               :canopy-base-height (t/new-tensor test-num-rows test-num-cols)
;;                               :canopy-cover       (t/new-tensor test-num-rows test-num-cols)
;;                               :crown-bulk-density (t/new-tensor test-num-rows test-num-cols)}
;;         foliar-moisture      0.9
;;         hfire-results        (run-fire-spread test-duration test-cell-size landfire-layers
;;                                               test-wind-speed-20ft test-wind-direction test-fuel-moisture
;;                                               foliar-moisture ellipse-adjustment-factor ignition-site)
;;         fire-spread-outfile  (str "org/pics/" outfile-base "_hfire-fire-spread.png")
;;         flame-length-outfile (str "org/pics/" outfile-base "_hfire-flame-length.png")
;;         [i j]                ignition-site]
;;     (save-matrix-as-png :color 4 -1.0 (doto (:fire-spread-matrix  hfire-results) (t/mset! i j 2.0)) fire-spread-outfile)
;;     (save-matrix-as-png :color 4 -1.0 (doto (:flame-length-matrix hfire-results) (t/mset! i j 2.0)) flame-length-outfile)))

;; (defn elevation-west-aspect
;;   [elevation-change]
;;   (let [elev (t/new-tensor test-num-rows test-num-cols)]
;;     (doseq [i (range test-num-rows)
;;             j (range test-num-cols)]
;;       (t/mset! elev i j (* j elevation-change)))
;;     elev))

;; (defn make-slope-layer
;;   [elevation-change]
;;   (doto (t/new-tensor test-num-rows test-num-cols) (mop/+= (/ elevation-change test-cell-size))))

;; (defn make-fuel-model
;;   [fuel-model-number]
;;   (doto (t/new-tensor test-num-rows test-num-cols) (mop/+= fuel-model-number)))

;; (let [outfiles {[ 1   0] "grass_fire_slope_W000%"
;;                 [ 1  30] "grass_fire_slope_W030%"
;;                 [ 1  60] "grass_fire_slope_W060%"
;;                 [ 1 100] "grass_fire_slope_W100%"
;;                 [ 4   0] "chaparral_fire_slope_W000%"
;;                 [ 4  30] "chaparral_fire_slope_W030%"
;;                 [ 4  60] "chaparral_fire_slope_W060%"
;;                 [ 4 100] "chaparral_fire_slope_W100%"
;;                 [ 8   0] "timber_litter_fire_slope_W000%"
;;                 [ 8  30] "timber_litter_fire_slope_W030%"
;;                 [ 8  60] "timber_litter_fire_slope_W060%"
;;                 [ 8 100] "timber_litter_fire_slope_W100%"
;;                 [11   0] "logging_slash_fire_slope_W000%"
;;                 [11  30] "logging_slash_fire_slope_W030%"
;;                 [11  60] "logging_slash_fire_slope_W060%"
;;                 [11 100] "logging_slash_fire_slope_W100%"}]
;;   (doseq [fuel-model [1 4 8 11] elevation-gain [0 30 60 100]]
;;     (view-hfire-results (elevation-west-aspect elevation-gain)
;;                         (make-slope-layer elevation-gain)
;;                         (make-fuel-model fuel-model)
;;                         test-ignition-site
;;                         1.0
;;                         (str "fire_spread/" (outfiles [fuel-model elevation-gain])))))

;; (def db-spec {:classname   "org.postgresql.Driver"
;;               :subprotocol "postgresql"
;;               :subname     "//localhost:5432/calfire"
;;               :user        "gjohnson"})

;; ;; Create images of LANDFIRE inputs
;; (def validation-layers
;;   {:tile205 {:ignition             [46 154]
;;              :elevation            (postgis-raster-to-matrix db-spec "validation.elevation_tile205"     nil nil)
;;              :slope                (postgis-raster-to-matrix db-spec "validation.slope_tile205"         nil nil)
;;              :aspect               (postgis-raster-to-matrix db-spec "validation.aspect_tile205"        nil nil)
;;              :fuel-model           (postgis-raster-to-matrix db-spec "validation.fuel_model_tile205"    nil nil)
;;              :canopy-height        (postgis-raster-to-matrix db-spec "validation.canopy_height_tile205" nil nil)
;;              :canopy-cover         (postgis-raster-to-matrix db-spec "validation.canopy_cover_tile205"  nil nil)
;;              :flammap-fire-spread  (postgis-raster-to-matrix db-spec "validation.fire_presence_tile205" nil nil)
;;              :flammap-flame-length (postgis-raster-to-matrix db-spec "validation.flame_length_tile205"  nil nil)}
;;    :tile210 {:ignition             [111 81]
;;              :elevation            (postgis-raster-to-matrix db-spec "validation.elevation_tile210"     nil nil)
;;              :slope                (postgis-raster-to-matrix db-spec "validation.slope_tile210"         nil nil)
;;              :aspect               (postgis-raster-to-matrix db-spec "validation.aspect_tile210"        nil nil)
;;              :fuel-model           (postgis-raster-to-matrix db-spec "validation.fuel_model_tile210"    nil nil)
;;              :canopy-height        (postgis-raster-to-matrix db-spec "validation.canopy_height_tile210" nil nil)
;;              :canopy-cover         (postgis-raster-to-matrix db-spec "validation.canopy_cover_tile210"  nil nil)
;;              :flammap-fire-spread  (postgis-raster-to-matrix db-spec "validation.fire_presence_tile210" nil nil)
;;              :flammap-flame-length (postgis-raster-to-matrix db-spec "validation.flame_length_tile210"  nil nil)}
;;    :tile281 {:ignition             [79 124]
;;              :elevation            (postgis-raster-to-matrix db-spec "validation.elevation_tile281"     nil nil)
;;              :slope                (postgis-raster-to-matrix db-spec "validation.slope_tile281"         nil nil)
;;              :aspect               (postgis-raster-to-matrix db-spec "validation.aspect_tile281"        nil nil)
;;              :fuel-model           (postgis-raster-to-matrix db-spec "validation.fuel_model_tile281"    nil nil)
;;              :canopy-height        (postgis-raster-to-matrix db-spec "validation.canopy_height_tile281" nil nil)
;;              :canopy-cover         (postgis-raster-to-matrix db-spec "validation.canopy_cover_tile281"  nil nil)
;;              :flammap-fire-spread  (postgis-raster-to-matrix db-spec "validation.fire_presence_tile281" nil nil)
;;              :flammap-flame-length (postgis-raster-to-matrix db-spec "validation.flame_length_tile281"  nil nil)}
;;    :tile310 {:ignition             [86 113]
;;              :elevation            (postgis-raster-to-matrix db-spec "validation.elevation_tile310"     nil nil)
;;              :slope                (postgis-raster-to-matrix db-spec "validation.slope_tile310"         nil nil)
;;              :aspect               (postgis-raster-to-matrix db-spec "validation.aspect_tile310"        nil nil)
;;              :fuel-model           (postgis-raster-to-matrix db-spec "validation.fuel_model_tile310"    nil nil)
;;              :canopy-height        (postgis-raster-to-matrix db-spec "validation.canopy_height_tile310" nil nil)
;;              :canopy-cover         (postgis-raster-to-matrix db-spec "validation.canopy_cover_tile310"  nil nil)
;;              :flammap-fire-spread  (postgis-raster-to-matrix db-spec "validation.fire_presence_tile310" nil nil)
;;              :flammap-flame-length (postgis-raster-to-matrix db-spec "validation.flame_length_tile310"  nil nil)}
;;    :tile564 {:ignition             [109 92]
;;              :elevation            (postgis-raster-to-matrix db-spec "validation.elevation_tile564"     nil nil)
;;              :slope                (postgis-raster-to-matrix db-spec "validation.slope_tile564"         nil nil)
;;              :aspect               (postgis-raster-to-matrix db-spec "validation.aspect_tile564"        nil nil)
;;              :fuel-model           (postgis-raster-to-matrix db-spec "validation.fuel_model_tile564"    nil nil)
;;              :canopy-height        (postgis-raster-to-matrix db-spec "validation.canopy_height_tile564" nil nil)
;;              :canopy-cover         (postgis-raster-to-matrix db-spec "validation.canopy_cover_tile564"  nil nil)
;;              :flammap-fire-spread  (postgis-raster-to-matrix db-spec "validation.fire_presence_tile564" nil nil)
;;              :flammap-flame-length (postgis-raster-to-matrix db-spec "validation.flame_length_tile564"  nil nil)}
;;    :tile643 {:ignition             [86 116]
;;              :elevation            (postgis-raster-to-matrix db-spec "validation.elevation_tile643"     nil nil)
;;              :slope                (postgis-raster-to-matrix db-spec "validation.slope_tile643"         nil nil)
;;              :aspect               (postgis-raster-to-matrix db-spec "validation.aspect_tile643"        nil nil)
;;              :fuel-model           (postgis-raster-to-matrix db-spec "validation.fuel_model_tile643"    nil nil)
;;              :canopy-height        (postgis-raster-to-matrix db-spec "validation.canopy_height_tile643" nil nil)
;;              :canopy-cover         (postgis-raster-to-matrix db-spec "validation.canopy_cover_tile643"  nil nil)
;;              :flammap-fire-spread  (postgis-raster-to-matrix db-spec "validation.fire_presence_tile643" nil nil)
;;              :flammap-flame-length (postgis-raster-to-matrix db-spec "validation.flame_length_tile643"  nil nil)}})

;; (def validation-num-rows (-> (t/tensor->dimensions (-> validation-layers :tile205 :elevation)) :shape first))
;; (def validation-num-cols (-> (t/tensor->dimensions (-> validation-layers :tile205 :elevation)) :shape second))

;; (doseq [tile  [:tile205 :tile210 :tile281 :tile310 :tile564 :tile643]
;;         layer [:elevation :slope :aspect :fuel-model :canopy-height :canopy-cover]]
;;   (let [[i j]  (-> validation-layers tile :ignition)
;;         matrix (-> validation-layers tile layer)]
;;     (save-matrix-as-png :color 4 -1.0 (doto (d/clone matrix) (t/mset! i j -1.0))
;;                         (str "org/pics/validation/" (name tile) "_" (name layer) ".png"))))

;; (def validation-outputs
;;   (into {}
;;         (for [tile [:tile205 :tile210 :tile281 :tile310 :tile564 :tile643]]
;;           (let [[i j]                     (-> validation-layers tile :ignition)
;;                 elev                      (d/clone (d/emap #(* % 3.28) nil (-> validation-layers tile :elevation)))                    ; m -> ft
;;                 slp                       (d/clone (d/emap #(Math/tan (degrees-to-radians %)) nil (-> validation-layers tile :slope))) ; degrees -> %
;;                 asp                       (-> validation-layers tile :aspect)
;;                 fm                        (-> validation-layers tile :fuel-model)
;;                 ch                        (d/clone (d/emap #(* % 0.328) nil (-> validation-layers tile :canopy-height)))               ; 10*m -> ft
;;                 cc                        (-> validation-layers tile :canopy-cover)
;;                 ffs                       (-> validation-layers tile :flammap-fire-spread)
;;                 ffl                       (-> validation-layers tile :flammap-flame-length)
;;                 cbd                       (t/new-tensor test-num-rows test-num-cols)
;;                 cbh                       (t/new-tensor test-num-rows test-num-cols)
;;                 landfire-layers           {:elevation          elev
;;                                            :slope              slp
;;                                            :aspect             asp
;;                                            :fuel-model         fm
;;                                            :canopy-height      ch
;;                                            :canopy-base-height cbh
;;                                            :canopy-cover       cc
;;                                            :crown-bulk-density cbd}
;;                 foliar-moisture           0.9
;;                 ellipse-adjustment-factor 1.0
;;                 hfire-output              (run-fire-spread test-duration test-cell-size landfire-layers
;;                                                            test-wind-speed-20ft test-wind-direction test-fuel-moisture
;;                                                            foliar-moisture ellipse-adjustment-factor [i j])
;;                 hfs                       (-> hfire-output :fire-spread-matrix)
;;                 hfl                       (-> hfire-output :flame-length-matrix)
;;                 hfl-global                (d/clone
;;                                            (d/emap (fn [fm-number slope aspect canopy-height canopy-cover]
;;                                                      (if-not (burnable-fuel-model? fm-number)
;;                                                        0.0
;;                                                        (if (fuel-models (int fm-number))
;;                                                          (let [[fuel-model spread-info-min] (rothermel-fast-wrapper fm-number test-fuel-moisture)
;;                                                                waf                          (wind-adjustment-factor (:delta fuel-model) canopy-height canopy-cover)
;;                                                                midflame-wind-speed          (* test-wind-speed-20ft 88.0 waf)
;;                                                                spread-info-max              (rothermel-surface-fire-spread-max spread-info-min
;;                                                                                                                                midflame-wind-speed
;;                                                                                                                                test-wind-direction
;;                                                                                                                                slope
;;                                                                                                                                aspect
;;                                                                                                                                1.0)
;;                                                                flame-depth                  (anderson-flame-depth (:max-spread-rate spread-info-max)
;;                                                                                                                   (:residence-time spread-info-min))
;;                                                                fire-line-intensity          (byram-fire-line-intensity (:reaction-intensity spread-info-min) flame-depth)
;;                                                                flame-length                 (byram-flame-length fire-line-intensity)]
;;                                                            flame-length)
;;                                                          nil
;;                                                          -1.0)))
;;                                                    fm slp asp ch cc))
;;                 ffl-clipped               (d/clone (d/emap #(if (pos? %1) %2 0.0) nil hfs ffl))
;;                 fl-diff                   (d/clone (d/emap - nil ffl hfl-global))
;;                 mfl                       (max (dfn/reduce-max ffl) (dfn/reduce-max hfl) (dfn/reduce-max hfl-global))
;;                 low-fm                    (d/clone (d/emap #(cond (= -1.0 %)  -1.0 (low-fuel-models %) 1.0 :else 0.0) nil fm))
;;                 error-fm                  (d/clone (d/emap #(cond (= -1.0 %2) -1.0 (> (Math/abs ^double %1) 5.0) %2 :else 0.0) nil fl-diff fm))]
;;             (save-matrix-as-png :color 4 -1.0 (doto (d/clone hfs) (t/mset! i j -1.0))
;;                                 (str "org/pics/validation/" (name tile) "_hfire-fire-spread.png"))
;;             (save-matrix-as-png :color 4 -1.0 (doto (d/clone ffs) (t/mset! i j -1.0))
;;                                 (str "org/pics/validation/" (name tile) "_flammap-fire-spread.png"))
;;             (save-matrix-as-png :color 4 -1.0 (doto (d/clone hfl) (t/mset! i j mfl))
;;                                 (str "org/pics/validation/" (name tile) "_hfire-flame-length.png"))
;;             (save-matrix-as-png :color 4 -1.0 (doto (d/clone ffl) (t/mset! i j mfl))
;;                                 (str "org/pics/validation/" (name tile) "_flammap-flame-length.png"))
;;             (save-matrix-as-png :color 4 -1.0 (doto (d/clone hfl-global) (t/mset! i j mfl))
;;                                 (str "org/pics/validation/" (name tile) "_hfire-flame-length-global.png"))
;;             (save-matrix-as-png :color 4 -1.0 (doto (d/clone ffl-clipped) (t/mset! i j mfl))
;;                                 (str "org/pics/validation/" (name tile) "_flammap-flame-length-clipped.png"))
;;             (save-matrix-as-png :color 4 -1.0 fl-diff
;;                                 (str "org/pics/validation/" (name tile) "_flame-length-difference.png"))
;;             (save-matrix-as-png :color 4 -1.0 low-fm
;;                                 (str "org/pics/validation/" (name tile) "_low-fuel-models.png"))
;;             (save-matrix-as-png :color 4 -1.0 error-fm
;;                                 (str "org/pics/validation/" (name tile) "_error-fuel-models.png"))
;;             [tile {:hfire-fire-spread            hfs
;;                    :hfire-flame-length           hfl
;;                    :hfire-flame-length-global    hfl-global
;;                    :flammap-flame-length-clipped ffl-clipped
;;                    :flame-length-difference      fl-diff
;;                    :error-fuel-models            error-fm}]))))

;; (doseq [tile [:tile205 :tile210 :tile281 :tile310 :tile564 :tile643]]
;;   (let [fm            (-> validation-layers  tile :fuel-model)
;;         ffs           (-> validation-layers  tile :flammap-fire-spread)
;;         hfs           (-> validation-outputs tile :hfire-fire-spread)
;;         ffs-cells     (set (filter (fn [[i j]] (pos? (d/mget ffs i j))) (m/index-seq ffs)))
;;         hfs-cells     (set (filter (fn [[i j]] (pos? (d/mget hfs i j))) (m/index-seq hfs)))
;;         agreement     (count (set/intersection ffs-cells hfs-cells))
;;         overpred      (count (set/difference hfs-cells ffs-cells))
;;         underpred     (count (set/difference ffs-cells hfs-cells))
;;         target        (count ffs-cells)
;;         fl-diff       (-> validation-outputs tile :flame-length-difference)
;;         fl-diff-mean  (/ (dfn/sum fl-diff) (m/ecount fl-diff))
;;         fl-diff-var   (/ (dfn/sum (d/emap #(Math/pow (- fl-diff-mean %) 2.0) nil fl-diff)) (m/ecount fl-diff))
;;         fl-diff-stdev (Math/sqrt fl-diff-var)]
;;     (println (format "| %s | %3d%%(%3d) | %3d%%(%3d) | %3d%%(%3d) | %4.1f | %4.1f |"
;;                      (subs (name tile) 4)
;;                      (int (* 100.0 (/ agreement target)))
;;                      agreement
;;                      (int (* 100.0 (/ overpred target)))
;;                      overpred
;;                      (int (* 100.0 (/ underpred target)))
;;                      underpred
;;                      fl-diff-mean
;;                      fl-diff-stdev))))

;; (doseq [tile [:tile205 :tile210 :tile281 :tile310 :tile564 :tile643]]
;;   (let [hfire-fire-size   (->> (-> validation-outputs tile :hfire-fire-spread)
;;                                (t/tensor->buffer)
;;                                (filter #(= % 1.0))
;;                                (count)
;;                                (cells-to-acres test-cell-size))
;;         flammap-fire-size (->> (-> validation-layers tile :flammap-fire-spread)
;;                                (t/tensor->buffer)
;;                                (filter #(= % 1.0))
;;                                (count)
;;                                (cells-to-acres test-cell-size))
;;         grass-area        (->> (-> validation-layers tile :fuel-model)
;;                                (t/tensor->buffer)
;;                                (filter grass-fuel-model?)
;;                                (count)
;;                                (cells-to-acres test-cell-size))
;;         tile-number       (subs (name tile) 4)]
;;     (println (format "| %s | %s | %s | %s | %s |" tile-number flammap-fire-size hfire-fire-size (/ flammap-fire-size hfire-fire-size) grass-area))))

;; (for [tile [:tile205 :tile210 :tile281 :tile310 :tile564 :tile643]]
;;   (let [error-fm (-> validation-outputs tile :error-fuel-models)]
;;     [tile (sort (distinct (filter pos? (t/tensor->buffer error-fm))))]))

;; (sort (distinct (apply concat (vals (into {} (for [tile [:tile205 :tile210 :tile281 :tile310 :tile564 :tile643]]
;;                                                (let [fm (-> validation-layers tile :fuel-model)]
;;                                                  [tile (sort (distinct (filter pos? (t/tensor->buffer fm))))])))))))

;; (sort (distinct (apply concat (vals (into {} (for [tile [:tile205 :tile210 :tile281 :tile310 :tile564 :tile643]]
;;                                                (let [error-fm (-> validation-outputs tile :error-fuel-models)]
;;                                                  [tile (sort (distinct (filter pos? (t/tensor->buffer error-fm))))])))))))
;; gridfire-unmerged.fire-spread-test ends here
