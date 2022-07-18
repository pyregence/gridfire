(ns gridfire.simulations
  (:require [clojure.java.io                              :as io]
            [gridfire.binary-output                       :as binary]
            [gridfire.common                              :refer [get-neighbors in-bounds?]]
            [gridfire.conversion                          :refer [min->hour kebab->snake snake->kebab]]
            [gridfire.fire-spread-optimal                 :refer [run-fire-spread]]
            [gridfire.outputs                             :as outputs]
            [gridfire.perturbations.pixel.hash-determined :as pixel-hdp]
            [gridfire.utils.gaussian-processes            :as gridfire-gp]
            [gridfire.utils.random                        :refer [my-rand-range]]
            [taoensso.tufte                               :as tufte]
            [tech.v3.datatype                             :as d]
            [tech.v3.datatype.functional                  :as dfn]
            [tech.v3.tensor                               :as t])
  (:import java.util.Random))

(set! *unchecked-math* :warn-on-boxed)

(defn layer-snapshot [burn-time-matrix layer-matrix ^double t]
  (d/clone
   (d/emap (fn [^double layer-value ^double burn-time]
             (if (and (not (neg? burn-time)) (<= burn-time t))
               layer-value
               0.0))
           :float64
           layer-matrix
           burn-time-matrix)))

(defn previous-active-perimeter?
  [[i j :as here] matrix]
  (let [[num-rows num-cols] (:shape (t/tensor->dimensions matrix))]
    (and
     (= (t/mget matrix i j) -1.0)
     (->> (get-neighbors here)
          (filter #(in-bounds? num-rows num-cols %))
          (map #(apply t/mget matrix %))
          (some pos?)))))

(defn to-color-map-values [burn-time-matrix ^double current-clock]
  (t/compute-tensor
   (:shape (t/tensor->dimensions burn-time-matrix))
   (fn [i j]
     (let [^double burn-time (t/mget burn-time-matrix i j)
           delta-hours       (->> (- current-clock burn-time)
                                  min->hour)]
       (cond
         (previous-active-perimeter? [i j] burn-time-matrix) 201
         (= burn-time -1.0)                                  200
         (< 0 delta-hours 5)                                 delta-hours
         (>= delta-hours 5)                                  5
         :else                                               0)))
   :int64))

(defn process-output-layers-timestepped
  [{:keys [simulation-id] :as config}
   {:keys [global-clock burn-time-matrix] :as fire-spread-results}
   name layer timestep envelope]
  (let [global-clock (double global-clock)]
   (doseq [output-time (range 0 (inc global-clock) timestep)]
     (let [matrix          (if (= layer "burn_history")
                             (to-color-map-values layer output-time)
                             (fire-spread-results layer))
           filtered-matrix (layer-snapshot burn-time-matrix matrix output-time)]
       (outputs/output-geotiff config filtered-matrix name envelope simulation-id output-time)
       (outputs/output-png config filtered-matrix name envelope simulation-id output-time)))))

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
    (let [global-clock (double global-clock)
          timestep     (long timestep)]
     (doseq [^double clock (range 0 (inc global-clock) timestep)]
       (let [filtered-fire-spread (d/clone
                                   (d/emap (fn [^double layer-value ^double burn-time]
                                             (if (<= burn-time clock)
                                               layer-value
                                               0.0))
                                           :float64
                                           fire-spread-matrix
                                           burn-time-matrix))
             band                 (int (quot clock timestep))
             burn-count-matrix-i  (nth burn-count-matrix band)]
         (d/copy! (dfn/+ burn-count-matrix-i filtered-fire-spread) burn-count-matrix-i))))
    (d/copy! (dfn/+ burn-count-matrix fire-spread-matrix) burn-count-matrix)))

(defn process-aggregate-output-layers!
  [{:keys [burn-count-matrix flame-length-max-matrix flame-length-sum-matrix
           output-burn-probability spot-count-matrix]} fire-spread-results]
  (when-let [timestep output-burn-probability]
    (process-burn-count! fire-spread-results burn-count-matrix timestep))
  (when flame-length-sum-matrix
    (d/copy! (dfn/+ flame-length-sum-matrix (:flame-length-matrix fire-spread-results))
             flame-length-sum-matrix))
  (when flame-length-max-matrix
    (d/copy! (dfn/max flame-length-max-matrix (:flame-length-matrix fire-spread-results))
             flame-length-max-matrix))
  (when spot-count-matrix
    (d/copy! (dfn/+ spot-count-matrix (:spot-matrix fire-spread-results))
             spot-count-matrix)))

(defn process-binary-output!
  [{:keys [output-binary? output-directory]}
   {:keys [burn-time-matrix flame-length-matrix spread-rate-matrix fire-type-matrix]}
   ^long simulation]
  (when output-binary?
    (let [output-name (format "toa_0001_%05d.bin" (inc simulation))
          output-path (if output-directory
                        (.getPath (io/file output-directory output-name))
                        output-name)]
      (binary/write-matrices-as-binary output-path
                                       [:float :float :float :int]
                                       [(->> burn-time-matrix
                                             (d/emap (fn ^double [^double x] (if (pos? x) (* 60.0 x) x)) :float64)
                                             (d/clone))
                                        flame-length-matrix
                                        spread-rate-matrix
                                        fire-type-matrix]))))

(defn cells-to-acres ^double
  [^double cell-size ^long num-cells]
  (let [acres-per-cell (/ (* cell-size cell-size) 43560.0)]
    (* acres-per-cell num-cells)))

(defn summarize-fire-spread-results
  [fire-spread-results cell-size]
  (let [flame-lengths              (filterv pos? (t/tensor->buffer (:flame-length-matrix fire-spread-results)))
        fire-line-intensities      (filterv pos? (t/tensor->buffer (:fire-line-intensity-matrix fire-spread-results)))
        burned-cells               (count flame-lengths)
        surface-fire-size          (cells-to-acres cell-size (:surface-fire-count fire-spread-results))
        crown-fire-size            (cells-to-acres cell-size (:crown-fire-count fire-spread-results))
        flame-length-mean          (/ (dfn/sum flame-lengths) burned-cells)
        fire-line-intensity-mean   (/ (dfn/sum fire-line-intensities) burned-cells)
        flame-length-stddev        (->> flame-lengths
                                        (d/emap #(Math/pow (- flame-length-mean ^double %) 2.0) nil)
                                        (dfn/sum)
                                        (#(/ ^double % burned-cells))
                                        (Math/sqrt))
        fire-line-intensity-stddev (->> fire-line-intensities
                                        (d/emap #(Math/pow (- fire-line-intensity-mean ^double %) 2.0) nil)
                                        (dfn/sum)
                                        (#(/ ^double % burned-cells))
                                        (Math/sqrt))]
    {:fire-size                  (+ surface-fire-size crown-fire-size)
     :surface-fire-size          surface-fire-size
     :crown-fire-size            crown-fire-size
     :flame-length-mean          flame-length-mean
     :flame-length-stddev        flame-length-stddev
     :fire-line-intensity-mean   fire-line-intensity-mean
     :fire-line-intensity-stddev fire-line-intensity-stddev
     :spot-count                 (:spot-count fire-spread-results)}))

(defn- matrix-or-i
  [inputs layer-name i]
  (or (get inputs (-> (name layer-name)
                      (str "-matrix")
                      keyword))
      (get-in inputs [(-> (name layer-name)
                          (str "-samples")
                          keyword)
                      i])))

(defn- get-index-multiplier
  [inputs layer-name]
  (get inputs (-> (name layer-name)
                  (str "-index-multiplier")
                  keyword)))

(defn- update-covariant-groups
  [inputs f]
  (update inputs :gridfire.perturbations/covariant-groups
    (fn [cgs]
      (reduce-kv
        (fn [cgs cg-name cg]
          (assoc cgs
            cg-name
            (f cg-name cg)))
        cgs
        cgs))))

(defn prepare-covariant-groups-samplers
  [inputs]
  (-> inputs
      (update-covariant-groups
        (fn [_cg-name cg]
          (let [phys-q->stddev   (->> (:perturbations inputs)
                                      (map
                                        (fn [[q pert]]
                                          [q (-> pert :gridfire.pertubation/std-dev (or 1.))]))
                                      (into {}))
                corr-m           (:gridfire.pertubations/correlation-matrix cg)
                phys-qi->qj->cov (->> (:gridfire.pertubations/covariant-quantities cg)
                                      (map-indexed
                                        (fn [^long i qi]
                                          [qi
                                           (->> (:gridfire.pertubations/covariant-quantities cg)
                                                (map-indexed
                                                  (fn [^long j qj]
                                                    [qj
                                                     (-> (or
                                                           (get-in corr-m [i j])
                                                           (get-in corr-m [j i])
                                                           (when (= i j) 1.))
                                                         (double)
                                                         (* (double (get phys-q->stddev qi)))
                                                         (* (double (get phys-q->stddev qj))))]))
                                                (into {}))]))
                                      (into {}))
                phys-cov-fn      (fn ^double [qi qj]
                                   (-> phys-qi->qj->cov (get qi) (get qj)))

                {:keys [num-rows num-cols max-runtime cell-size]} inputs
                max-b            (/ (double max-runtime) 60.)
                [sb si sj] (:gridfire.perturbation.smoothed-supergrid/supergrid-size cg)
                [ppsc-b ppsc-i ppsc-j] (mapv
                                         (fn pixels-per-supergrid-cell [s m] (/ (double m) (long s)))
                                         [sb si sj]
                                         [max-b num-rows num-cols])]
            (assoc cg
              :gridfire.perturbations/supergrid-sampler-fn
              (gridfire-gp/multiplicative-gaussian-process-sampler
                [[(:gridfire.pertubations/covariant-quantities cg) phys-cov-fn]
                 [(vec
                    (for [kb (range 0 (+ (long sb) 2))]
                      (* (long kb) (double ppsc-b) 60.)))
                  (gridfire-gp/kernel-from-correlation-config (:gridfire.pertubations/temporal-correlations cg))]
                 [(vec
                    (for [ki (range 0 (+ (long si) 2))
                          kj (range 0 (+ (long sj) 2))]
                      (gridfire-gp/->Point2D
                        (* (long ki) (double ppsc-i) (double cell-size))
                        (* (long kj) (double ppsc-j) (double cell-size)))))
                  (gridfire-gp/kernel-from-correlation-config (:gridfire.pertubations/spatial-correlations cg))]]
                cg)))))
      (update :perturbations
        (fn [q->pert]
          (reduce-kv
            (fn [q->p cg-name cg]
              (reduce-kv
                (fn [q->p q-pos q]
                  (update q->p q
                    (fn [pert]
                      (assoc pert
                        :gridfire.pertubation/covariant-group cg-name
                        :gridfire.perturbation/q-index q-pos))))
                q->p
                (:gridfire.pertubations/covariant-quantities cg)))
            q->pert
            (:gridfire.perturbations/covariant-groups inputs))))))

(defn add-sampled-covariant-supergrids
  [inputs rand-gen]
  (-> inputs
      (update-covariant-groups
        (fn [_cg-name cg]
          (let [sample-supergrid (:gridfire.perturbations/supergrid-sampler-fn cg)
                [_sb si sj]      (:gridfire.perturbation.smoothed-supergrid/supergrid-size cg)]
            (assoc cg
              :gridfire.perturbation.smoothed-supergrid/offsets (repeatedly 3 #(my-rand-range rand-gen 0. 1.))
              :gridfire.perturbations/sampled-supergrid
              (->
                (sample-supergrid rand-gen)
                (as-> sampled-t
                  (let [shp-flat-space (vec (d/shape sampled-t))
                        shp-2d-space   (into (pop shp-flat-space) [(+ (long si) 2) (+ (long sj) 2)])]
                    (t/reshape sampled-t shp-2d-space))))))))))

(def n-buckets 1024)

(defn- get-value-fn
  [{:keys [perturbations] :as inputs} rand-gen layer-name i]
  (when-let [matrix-or-num (matrix-or-i inputs layer-name i)]
    (let [index-multiplier             (get-index-multiplier inputs layer-name)
          pert                         (get perturbations layer-name)
          {:keys [spatial-type range]} pert
          [range-min range-max]        range
          gen-perturbation             (fn gen-in-range [_h]
                                         (my-rand-range rand-gen range-min range-max))]
      (cond
        (and (number? matrix-or-num) (nil? (get perturbations layer-name)))
        (fn
          (^double [_ _] matrix-or-num)
          (^double [_ _ _] matrix-or-num))

        (and (not (number? matrix-or-num)) (nil? (get perturbations layer-name)))
        (if index-multiplier
          (let [index-multiplier (double index-multiplier)]
            (fn
              (^double [^long i ^long j]
               (let [row (long (* i index-multiplier))
                     col (long (* j index-multiplier))]
                 (t/mget matrix-or-num row col)))
              (^double [^long b ^long i ^long j]
               (let [row (long (* i index-multiplier))
                     col (long (* j index-multiplier))]
                 (t/mget matrix-or-num b row col)))))
          (fn
            (^double [i j]
             (t/mget matrix-or-num i j))
            (^double [b i j]
             (t/mget matrix-or-num b i j))))

        (and (number? matrix-or-num) (= spatial-type :pixel))
        (let [matrix-or-num (double matrix-or-num)
              h->perturb    (pixel-hdp/gen-hash->perturbation n-buckets gen-perturbation)]
          (fn
            (^double [i j]
             (max 0.0 ;TODO document we are snapping negative values to 0
                  (+ (double matrix-or-num)
                     (pixel-hdp/resolve-perturbation-for-coords h->perturb i j))))
            (^double [^long b ^long i ^long j]
             (max 0.0 ;TODO document we are snapping negative values to 0
                  (+ (double matrix-or-num)
                     (pixel-hdp/resolve-perturbation-for-coords h->perturb b i j))))))

        (= spatial-type :smoothed-supergrid)
        (let [{:keys [num-rows num-cols max-runtime]} inputs
              max-b              (/ (double max-runtime) 60.)
              [sb si sj] (if-some [cg-name (:gridfire.pertubation/covariant-group pert)]
                           (get-in inputs [:gridfire.perturbations/covariant-groups cg-name :gridfire.perturbation.smoothed-supergrid/supergrid-size])
                           (:gridfire.perturbation.smoothed-supergrid/supergrid-size pert))
              [ppsc-b ppsc-i ppsc-j] (mapv
                                       (fn pixels-per-supergrid-cell [s m] (/ (double m) (long s)))
                                       [sb si sj]
                                       [max-b num-rows num-cols])
              ppsc-b             (double ppsc-b)
              ppsc-i             (double ppsc-i)
              ppsc-j             (double ppsc-j)
              sampled-grid       (if-some [cg-name (:gridfire.pertubation/covariant-group pert)]
                                   (->
                                     inputs
                                     (get-in [:gridfire.perturbations/covariant-groups cg-name
                                              :gridfire.perturbations/sampled-supergrid])
                                     (t/mget (:gridfire.perturbation/q-index pert)))
                                   (let [gen-data (reduce
                                                    (fn [g s]
                                                      (fn gen-tensor []
                                                        (vec (repeatedly s g))))
                                                    #(gen-perturbation nil)
                                                    (reverse
                                                      (map
                                                        (fn [s] (+ (long s) 2))
                                                        [sb si sj])))]
                                     (t/->tensor (gen-data))))
              ;; Why offset the supergrid? Pixels in the interior of supergrid cells
              ;; tend to have different distributions (less variance, smoother local variation)
              ;; than those near the boundaries.
              ;; Randomly offsetting ensures that all cells have an equal change
              ;; of being near the supergrid cell boundaries.
              offsets            (if-some [cg-name (:gridfire.pertubation/covariant-group pert)]
                                   (get-in inputs [:gridfire.perturbations/covariant-groups cg-name
                                                   :gridfire.perturbation.smoothed-supergrid/offsets])
                                   (repeatedly 3 #(my-rand-range (:rand-gen inputs) 0. 1.)))
              [o-b o-i o-j] offsets
              o-b (double o-b)
              o-i (double o-i)
              o-j (double o-j)
              lin-terpolate      (fn lin-terpolate ^double [^double frac ^double v0 ^double v1]
                                   (+
                                     (* (- 1. frac) v0)
                                     (* frac v1)))
              get-pert-at-coords (fn average-cube-corners ^double [^long b ^long i ^long j]
                                   ;; Implements a linear spline smoothing algorithm.
                                   (let [b-pos  (-> b (/ ppsc-b) (+ o-b))
                                         i-pos  (-> i (/ ppsc-i) (+ o-i))
                                         j-pos  (-> j (/ ppsc-j) (+ o-j))

                                         b-pos- (-> b-pos (Math/floor) (long))
                                         b-pos+ (inc b-pos-)
                                         b-posf (- b-pos b-pos-)
                                         i-pos- (-> i-pos (Math/floor) (long))
                                         i-pos+ (inc i-pos-)
                                         i-posf (- i-pos i-pos-)
                                         j-pos- (-> j-pos (Math/floor) (long))
                                         j-pos+ (inc j-pos-)
                                         j-posf (- j-pos j-pos-)

                                         p000   (t/mget sampled-grid b-pos- i-pos- j-pos-)
                                         p001   (t/mget sampled-grid b-pos- i-pos- j-pos+)
                                         p010   (t/mget sampled-grid b-pos- i-pos+ j-pos-)
                                         p011   (t/mget sampled-grid b-pos- i-pos+ j-pos+)
                                         p100   (t/mget sampled-grid b-pos+ i-pos- j-pos-)
                                         p101   (t/mget sampled-grid b-pos+ i-pos- j-pos+)
                                         p110   (t/mget sampled-grid b-pos+ i-pos+ j-pos-)
                                         p111   (t/mget sampled-grid b-pos+ i-pos+ j-pos+)]
                                     (lin-terpolate b-posf
                                       (lin-terpolate i-posf
                                         (lin-terpolate j-posf p000 p001)
                                         (lin-terpolate j-posf p010 p011))
                                       (lin-terpolate i-posf
                                         (lin-terpolate j-posf p100 p101)
                                         (lin-terpolate j-posf p110 p111)))))]
          (if (number? matrix-or-num)
            (let [matrix-or-num (double matrix-or-num)]
              (if index-multiplier
                (let [index-multiplier (double index-multiplier)]
                  (fn
                    (^double [^long i ^long j]
                     (let [row (long (* i index-multiplier))
                           col (long (* j index-multiplier))]
                       (max 0.0 ;TODO document we are snapping negative values to 0
                         (+ (double (t/mget matrix-or-num row col))
                            (double (get-pert-at-coords 0 i j))))))
                    (^double [^long b ^long i ^long j]
                     (let [row (long (* i index-multiplier))
                           col (long (* j index-multiplier))]
                       (max 0.0 ;TODO document we are snapping negative values to 0
                         (+ (double (t/mget matrix-or-num b row col))
                            (double (get-pert-at-coords b i j))))))))
                (fn
                  (^double [^long i ^long j]
                   (max 0.0                                   ;TODO document we are snapping negative values to 0
                     (+ (double matrix-or-num)
                        (double (get-pert-at-coords 0 i j)))))
                  (^double [^long b ^long i ^long j]
                   (max 0.0                                   ;TODO document we are snapping negative values to 0
                     (+ (double matrix-or-num)
                        (double (get-pert-at-coords b i j))))))))
            (fn
              (^double [^long i ^long j]
               (max 0.0 ;TODO document we are snapping negative values to 0
                 (+ (double (t/mget matrix-or-num i j))
                    (double (get-pert-at-coords 0 i j)))))
              (^double [^long b ^long i ^long j]
               (max 0.0 ;TODO document we are snapping negative values to 0
                 (+ (double (t/mget matrix-or-num b i j))
                    (double (get-pert-at-coords b i j))))))))

        (and (number? matrix-or-num) (= spatial-type :global))
        (let [perturbed-value-cache (atom nil)
              matrix-or-num         (double matrix-or-num)]
          (fn
            (^double [_ _]
             (or @perturbed-value-cache
                 (let [new-value (max 0.0 (+ matrix-or-num (my-rand-range rand-gen range-min range-max)))]
                   (reset! perturbed-value-cache new-value)
                   new-value)))
            (^double [b _ _]
             (or (get @perturbed-value-cache b)
                 (let [new-value (max 0.0 (+ matrix-or-num (my-rand-range rand-gen range-min range-max)))]
                   (reset! perturbed-value-cache {b new-value})
                   new-value)))))

        (and (not (number? matrix-or-num)) (= spatial-type :pixel))
        (let [h->perturb (pixel-hdp/gen-hash->perturbation n-buckets gen-perturbation)]
          (if index-multiplier
            (let [index-multiplier (double index-multiplier)]
              (fn
                (^double [^long i ^long j]
                 (let [row (long (* i index-multiplier))
                       col (long (* j index-multiplier))]
                   (max 0.0 ;TODO document we are snapping negative values to 0
                        (+ (double (t/mget matrix-or-num row col))
                           (pixel-hdp/resolve-perturbation-for-coords h->perturb i j)))))
                (^double [^long b ^long i ^long j]
                 (let [row (long (* i index-multiplier))
                       col (long (* j index-multiplier))]
                   (max 0.0 ;TODO document we are snapping negative values to 0
                        (+ (double (t/mget matrix-or-num b row col))
                           (pixel-hdp/resolve-perturbation-for-coords h->perturb b i j)))))))
            (fn
              (^double [^long i ^long j]
               (max 0.0 ;TODO document we are snapping negative values to 0
                    (+ (double (t/mget matrix-or-num i j))
                       (pixel-hdp/resolve-perturbation-for-coords h->perturb i j))))
              (^double [^long b ^long i ^long j]
               (max 0.0 ;TODO document we are snapping negative values to 0
                    (+ (double (t/mget matrix-or-num b i j))
                       (pixel-hdp/resolve-perturbation-for-coords h->perturb b i j)))))))

        (and (not (number? matrix-or-num)) (= spatial-type :global))
        (let [offset-cache (atom nil)]
          (if index-multiplier
            (let [index-multiplier (double index-multiplier)]
              (fn
                (^double [^long i ^long j]
                 (let [row            (long (* i index-multiplier))
                       col            (long (* j index-multiplier))
                       ^double offset (or @offset-cache
                                          (reset! offset-cache (my-rand-range rand-gen range-min range-max)))]
                   (max 0.0 (+ ^double (t/mget matrix-or-num row col) offset))))
                (^double [^long b ^long i ^long j]
                 (let [row            (long (* i index-multiplier))
                       col            (long (* j index-multiplier))
                       ^double offset (or (get @offset-cache b)
                                          (let [new-offset (my-rand-range rand-gen range-min range-max)]
                                            (reset! offset-cache {b new-offset})
                                            new-offset))]
                   (max 0.0 (+ ^double (t/mget matrix-or-num b row col) offset))))))
            (fn
              (^double [i j]
               (let [^double offset (or @offset-cache
                                        (reset! offset-cache (my-rand-range rand-gen range-min range-max)))]
                 (max 0.0 (+ ^double (t/mget matrix-or-num i j) offset))))
              (^double [b i j]
               (let [^double offset (or (get @offset-cache b)
                                        (let [new-offset (my-rand-range rand-gen range-min range-max)]
                                          (reset! offset-cache {b new-offset})
                                          new-offset))]
                 (max 0.0 (+ ^double (t/mget matrix-or-num b i j) offset)))))))))))

(defrecord SimulationInputs
    [^long num-rows
     ^long num-cols
     ^double cell-size
     ^double ignition-start-time
     weather-data-start-timestamp
     burn-period-start
     burn-period-end
     ^double max-runtime
     initial-ignition-site
     ^double ellipse-adjustment-factor
     ^boolean grass-suppression?
     ^Random rand-gen
     get-elevation
     get-slope
     get-aspect
     get-canopy-cover
     get-canopy-height
     get-canopy-base-height
     get-crown-bulk-density
     get-fuel-model
     get-temperature
     get-relative-humidity
     get-wind-speed-20ft
     get-wind-from-direction
     get-fuel-moisture-dead-1hr
     get-fuel-moisture-dead-10hr
     get-fuel-moisture-dead-100hr
     get-fuel-moisture-live-herbaceous
     get-fuel-moisture-live-woody
     get-foliar-moisture
     spotting])

(defn run-simulation!
  [^long i
   {:keys
    [num-rows num-cols grass-suppression? output-csvs? envelope ignition-matrix cell-size max-runtime-samples
     ignition-rows ignition-cols ellipse-adjustment-factor-samples random-seed ignition-start-times spotting
     burn-period-start burn-period-end weather-data-start-timestamp]
    :as inputs}]
  (tufte/profile
   {:id :run-simulation}
   (let [rand-gen            (if random-seed (Random. (+ ^long random-seed i)) (Random.))
         inputs              (add-sampled-covariant-supergrids inputs rand-gen)
         simulation-inputs   {:num-rows                          num-rows
                              :num-cols                          num-cols
                              :cell-size                         cell-size
                              :rand-gen                          rand-gen
                              :initial-ignition-site             (or ignition-matrix
                                                                     [(ignition-rows i) (ignition-cols i)])
                              :ignition-start-time               (get ignition-start-times i 0.0)
                              :weather-data-start-timestamp      weather-data-start-timestamp
                              :burn-period-start                 burn-period-start
                              :burn-period-end                   burn-period-end
                              :max-runtime                       (max-runtime-samples i)
                              :get-aspect                        (get-value-fn inputs rand-gen :aspect i)
                              :get-canopy-height                 (get-value-fn inputs rand-gen :canopy-height i)
                              :get-canopy-base-height            (get-value-fn inputs rand-gen :canopy-base-height i)
                              :get-canopy-cover                  (get-value-fn inputs rand-gen :canopy-cover i)
                              :get-crown-bulk-density            (get-value-fn inputs rand-gen :crown-bulk-density i)
                              :get-fuel-model                    (get-value-fn inputs rand-gen :fuel-model i)
                              :get-slope                         (get-value-fn inputs rand-gen :slope i)
                              :get-elevation                     (get-value-fn inputs rand-gen :elevation i)
                              :get-temperature                   (get-value-fn inputs rand-gen :temperature i)
                              :get-relative-humidity             (get-value-fn inputs rand-gen :relative-humidity i)
                              :get-wind-speed-20ft               (get-value-fn inputs rand-gen :wind-speed-20ft i)
                              :get-wind-from-direction           (get-value-fn inputs rand-gen :wind-from-direction i)
                              :get-fuel-moisture-dead-1hr        (get-value-fn inputs rand-gen :fuel-moisture-dead-1hr i)
                              :get-fuel-moisture-dead-10hr       (get-value-fn inputs rand-gen :fuel-moisture-dead-10hr i)
                              :get-fuel-moisture-dead-100hr      (get-value-fn inputs rand-gen :fuel-moisture-dead-100hr i)
                              :get-fuel-moisture-live-herbaceous (get-value-fn inputs rand-gen :fuel-moisture-live-herbaceous i)
                              :get-fuel-moisture-live-woody      (get-value-fn inputs rand-gen :fuel-moisture-live-woody i)
                              :get-foliar-moisture               (get-value-fn inputs rand-gen :foliar-moisture i)
                              :ellipse-adjustment-factor         (ellipse-adjustment-factor-samples i)
                              :grass-suppression?                (true? grass-suppression?)
                              :spotting                          spotting}
         simulation-results (tufte/p :run-fire-spread
                                     (run-fire-spread (map->SimulationInputs simulation-inputs)))]
     (when simulation-results
       (process-output-layers! inputs simulation-results envelope i)
       (process-aggregate-output-layers! inputs simulation-results)
       (process-binary-output! inputs simulation-results i))
     (when output-csvs?
       (-> simulation-inputs
           (dissoc :get-aspect
                   :get-canopy-height
                   :get-canopy-base-height
                   :get-canopy-cover
                   :get-crown-bulk-density
                   :get-fuel-model
                   :get-slope
                   :get-elevation
                   :get-temperature
                   :get-relative-humidity
                   :get-wind-speed-20ft
                   :get-wind-from-direction
                   :get-fuel-moisture-dead-1hr
                   :get-fuel-moisture-dead-10hr
                   :get-fuel-moisture-dead-100hr
                   :get-fuel-moisture-live-herbaceous
                   :get-fuel-moisture-live-woody
                   :get-foliar-moisture)
           (merge {:simulation         (inc i)
                   :ignition-row       (get ignition-rows i)
                   :ignition-col       (get ignition-cols i)
                   :global-clock       (:global-clock simulation-results)
                   :exit-condition     (:exit-condition simulation-results :no-fire-spread)
                   :surface-fire-count (:surface-fire-count simulation-results)
                   :crown-fire-count   (:crown-fire-count simulation-results)
                   :spot-count         (:spot-count simulation-results)})
           (merge
            (if simulation-results
              (tufte/p
               :summarize-fire-spread-results
               (summarize-fire-spread-results simulation-results cell-size))
              {:fire-size                  0.0
               :flame-length-mean          0.0
               :flame-length-stddev        0.0
               :fire-line-intensity-mean   0.0
               :fire-line-intensity-stddev 0.0})))))))
