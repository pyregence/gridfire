(ns gridfire.simulations
  (:require [clojure.core.matrix    :as m]
            [clojure.java.io        :as io]
            [gridfire.binary-output :as binary]
            [gridfire.common        :refer [get-neighbors in-bounds?]]
            [gridfire.conversion    :refer [min->hour kebab->snake snake->kebab]]
            [gridfire.fire-spread   :refer [run-fire-spread]]
            [gridfire.outputs       :as outputs]
            [taoensso.tufte         :as tufte])
  (:import java.util.Random))

(m/set-current-implementation :vectorz)

#_(set! *unchecked-math* :warn-on-boxed)

(defn layer-snapshot [burn-time-matrix layer-matrix t]
  (m/emap (fn [layer-value burn-time]
            (if (<= burn-time t)
              layer-value
              0))
          layer-matrix
          burn-time-matrix))

(defn previous-active-perimeter?
  [[i j :as here] matrix]
  (let [num-rows (m/row-count matrix)
        num-cols (m/column-count matrix)]
    (and
     (= (m/mget matrix i j) -1.0)
     (->> (get-neighbors here)
          (filter #(in-bounds? num-rows num-cols %))
          (map #(apply m/mget matrix %))
          (some pos?)))))

(defn to-color-map-values [burn-time-matrix current-clock]
  (m/emap-indexed (fn [here burn-time]
                    (let [delta-hours (->> (- current-clock burn-time)
                                           min->hour)]
                      (cond
                        (previous-active-perimeter? here burn-time-matrix) 201
                        (= burn-time -1.0)                                 200
                        (< 0 delta-hours 5)                                delta-hours
                        (>= delta-hours 5)                                 5
                        :else                                              0)))
                  burn-time-matrix))

(defn process-output-layers-timestepped
  [{:keys [simulation-id] :as config}
   {:keys [global-clock burn-time-matrix] :as fire-spread-results}
   name layer timestep envelope]
  (doseq [output-time (range 0 (inc global-clock) timestep)]
    (let [matrix          (if (= layer "burn_history")
                            (to-color-map-values layer output-time)
                            (fire-spread-results layer))
          filtered-matrix (layer-snapshot burn-time-matrix matrix output-time)]
      (outputs/output-geotiff config filtered-matrix name envelope simulation-id output-time)
      (outputs/output-png config filtered-matrix name envelope simulation-id output-time))))

(def layer-name->matrix
  [["fire_spread"         :fire-spread-matrix]
   ["flame_length"        :flame-length-matrix]
   ["fire_line_intensity" :fire-line-intensity-matrix]
   ["burn_history"        :burn-time-matrix]
   ["spread_rate"         :spread-rate-matrix]
   ["fire_type"           :fire-type-matrix]])

(defn filter-output-layers [output-layers]
  (let [layers-to-filter (set (map (comp kebab->snake name) (keys output-layers)))]
    (filter (fn [[name _]] (contains? layers-to-filter name)) layer-name->matrix)))

(defn process-output-layers!
  [{:keys [output-layers output-geotiffs? output-pngs?] :as config}
   {:keys [global-clock] :as fire-spread-results}
   envelope
   simulation-id]
  (let [layers (if output-layers
                 (filter-output-layers output-layers)
                 layer-name->matrix)]
    (doseq [[name layer] layers]
      (let [kw       (keyword (snake->kebab name))
            timestep (get output-layers kw)]
        (if (int? timestep)
          (process-output-layers-timestepped config
                                             fire-spread-results
                                             name
                                             layer
                                             timestep
                                             envelope)
          (let [matrix (if (= layer "burn_history")
                         (to-color-map-values layer global-clock)
                         (fire-spread-results layer))]
            (when output-geotiffs?
              (outputs/output-geotiff config matrix name envelope simulation-id))
            (when output-pngs?
              (outputs/output-png config matrix name envelope simulation-id))))))))

(defn process-burn-count!
  [{:keys [fire-spread-matrix burn-time-matrix global-clock]}
   burn-count-matrix
   timestep]
  (if (int? timestep)
    (doseq [clock (range 0 (inc global-clock) timestep)]
      (let [filtered-fire-spread (m/emap (fn [layer-value burn-time]
                                           (if (<= burn-time clock)
                                             layer-value
                                             0))
                                         fire-spread-matrix
                                         burn-time-matrix)
            band                 (int (quot clock timestep))]
        (m/add! (nth (seq burn-count-matrix) band) filtered-fire-spread)))
    (m/add! burn-count-matrix fire-spread-matrix)))

(defn process-aggregate-output-layers!
  [{:keys [burn-count-matrix flame-length-max-matrix flame-length-sum-matrix
           output-burn-probability spot-count-matrix]} fire-spread-results]
  (when-let [timestep output-burn-probability]
    (process-burn-count! fire-spread-results burn-count-matrix timestep))
  (when flame-length-sum-matrix
    (m/add! flame-length-sum-matrix (:flame-length-matrix fire-spread-results)))
  (when flame-length-max-matrix
    (m/emap! #(max %1 %2) flame-length-max-matrix (:flame-length-matrix fire-spread-results)))
  (when spot-count-matrix
    (m/add! spot-count-matrix (:spot-matrix fire-spread-results))))

(defn process-binary-output!
  [{:keys [output-binary? output-directory]}
   {:keys [burn-time-matrix flame-length-matrix spread-rate-matrix fire-type-matrix]}
   simulation]
  (when output-binary?
    (let [output-name (format "toa_0001_%05d.bin" (inc simulation))
          output-path (if output-directory
                        (.getPath (io/file output-directory output-name))
                        output-name)]
      (binary/write-matrices-as-binary output-path
                                       [:float :float :float :int]
                                       [(m/emap #(if (pos? %) (* 60 %) %) burn-time-matrix)
                                        flame-length-matrix
                                        spread-rate-matrix
                                        fire-type-matrix]))))

(defn cells-to-acres
  [cell-size num-cells]
  (let [acres-per-cell (/ (* cell-size cell-size) 43560.0)]
    (* acres-per-cell num-cells)))

(defn summarize-fire-spread-results
  [fire-spread-results cell-size]
  (let [flame-lengths              (filterv pos? (m/eseq (:flame-length-matrix fire-spread-results)))
        fire-line-intensities      (filterv pos? (m/eseq (:fire-line-intensity-matrix fire-spread-results)))
        burned-cells               (count flame-lengths)
        fire-size                  (cells-to-acres cell-size burned-cells)
        crown-fire-size            (cells-to-acres cell-size (:crown-fire-count fire-spread-results))
        flame-length-mean          (/ (m/esum flame-lengths) burned-cells)
        fire-line-intensity-mean   (/ (m/esum fire-line-intensities) burned-cells)
        flame-length-stddev        (->> flame-lengths
                                        (m/emap #(Math/pow (- flame-length-mean %) 2.0))
                                        (m/esum)
                                        (#(/ % burned-cells))
                                        (Math/sqrt))
        fire-line-intensity-stddev (->> fire-line-intensities
                                        (m/emap #(Math/pow (- fire-line-intensity-mean %) 2.0))
                                        (m/esum)
                                        (#(/ % burned-cells))
                                        (Math/sqrt))]
    {:fire-size                  fire-size
     :crown-fire-size            crown-fire-size
     :flame-length-mean          flame-length-mean
     :flame-length-stddev        flame-length-stddev
     :fire-line-intensity-mean   fire-line-intensity-mean
     :fire-line-intensity-stddev fire-line-intensity-stddev
     :spot-count                 (:spot-count fire-spread-results)}))

(defn matrix-or-i
  [inputs weather-type i]
  (or (get inputs (-> (name weather-type)
                      (str "-matrix")
                      keyword))
      (get-in inputs [(-> (name weather-type)
                          (str "-samples")
                          keyword)
                      i])))

;; FIXME: Replace input-variations expression with add-sampled-params
;;        and add-weather-params (and remove them from load-inputs).
;;        This will require making these function return single
;;        samples instead of sequences of samples. Also combine the
;;        initial-ignition-site calculation into input-variations or
;;        move it to run-fire-spread.
(defn run-simulation!
  [i
   {:keys
    [output-csvs? envelope ignition-matrix cell-size max-runtimes ignition-rows ignition-cols
     foliar-moistures ellipse-adjustment-factors perturbations random-seed ignition-start-times] :as inputs}]
  (tufte/profile
   {:id :run-simulation}
   (let [initial-ignition-site (or ignition-matrix
                                   (when (and (ignition-rows i) (ignition-cols i))
                                     [(ignition-rows i) (ignition-cols i)]))
         input-variations      {:rand-gen                      (if random-seed (Random. (+ random-seed i)) (Random.))
                                :max-runtime                   (max-runtimes i)
                                :foliar-moisture               (* 0.01 (foliar-moistures i))
                                :ellipse-adjustment-factor     (ellipse-adjustment-factors i)
                                :perturbations                 (when perturbations (perturbations i))
                                :temperature                   (matrix-or-i inputs :temperature i)
                                :relative-humidity             (matrix-or-i inputs :relative-humidity i)
                                :wind-speed-20ft               (matrix-or-i inputs :wind-speed-20ft i)
                                :wind-from-direction           (matrix-or-i inputs :wind-from-direction i)
                                :fuel-moisture-dead-1hr        (matrix-or-i inputs :fuel-moisture-dead-1hr i)
                                :fuel-moisture-dead-10hr       (matrix-or-i inputs :fuel-moisture-dead-10hr i)
                                :fuel-moisture-dead-100hr      (matrix-or-i inputs :fuel-moisture-dead-100hr i)
                                :fuel-moisture-live-herbaceous (matrix-or-i inputs :fuel-moisture-live-herbaceous i)
                                :fuel-moisture-live-woody      (matrix-or-i inputs :fuel-moisture-live-woody i)
                                :ignition-start-time           (get ignition-start-times i 0.0)}
         fire-spread-results   (tufte/p :run-fire-spread
                                        (run-fire-spread
                                         (merge inputs
                                                input-variations
                                                {:initial-ignition-site initial-ignition-site})))]
     (when fire-spread-results
       (process-output-layers! inputs fire-spread-results envelope i)
       (process-aggregate-output-layers! inputs fire-spread-results)
       (process-binary-output! inputs fire-spread-results i))
     (when output-csvs?
       (merge
        input-variations
        {:simulation       (inc i)
         :ignition-row     (ignition-rows i)
         :ignition-col     (ignition-cols i)
         :foliar-moisture  (foliar-moistures i)
         :global-clock     (:global-clock fire-spread-results)
         :exit-condition   (:exit-condition fire-spread-results :no-fire-spread)
         :crown-fire-count (:crown-fire-count fire-spread-results)}
        (if fire-spread-results
          (tufte/p
           :summarize-fire-spread-results
           (summarize-fire-spread-results fire-spread-results cell-size))
          {:fire-size                  0.0
           :flame-length-mean          0.0
           :flame-length-stddev        0.0
           :fire-line-intensity-mean   0.0
           :fire-line-intensity-stddev 0.0}))))))
