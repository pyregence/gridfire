(ns gridfire.fire-spread-optimal
  (:require [gridfire.common             :refer [burnable-neighbors? non-zero-indices]]
            [taoensso.tufte              :as tufte]
            [tech.v3.datatype            :as d]
            [tech.v3.datatype.functional :as dfn]
            [tech.v3.tensor              :as t]))

(defrecord BurnVector
    [^long   i
     ^long   j
     ^byte   direction
     ^double spread-rate
     ^double terrain-distance
     ^double fractional-distance
     ^double burn-probability])

;; FIXME: stub
(defn- progress-spot-ignitions!
  [inputs matrices spot-ignitions ^double timestep]
  spot-ignitions)

;; FIXME: stub
(defn- progress-burn-vectors!
  [inputs matrices burn-vectors ^double timestep]
  burn-vectors)

(defn- find-max-spread-rate ^double
  [^double max-spread-rate burn-vector]
  (Math/max max-spread-rate ^double (:spread-rate burn-vector)))

(defn- compute-dt ^double
  [^double cell-size burn-vectors]
  (if (seq burn-vectors)
    (let [max-spread-rate (double (reduce find-max-spread-rate 0.0 burn-vectors))]
      (/ cell-size max-spread-rate))
    10.0)) ; Wait 10 minutes for spot ignitions to smolder and catch fire

;; FIXME: stub
;; FIXME: Store fire-line-intensity, fire-type, flame-length, and spread-rate on matrices for each ignited cell
;; FIXME: Return a vector of BurnTrajectory records
(defn- generate-burn-vectors!
  [inputs matrices ignited-cells]
  [])

(defn- run-loop
  [{:keys [cell-size ignition-start-time max-runtime] :as inputs} matrices ignited-cells]
  (let [cell-size           (double cell-size)
        max-runtime         (double max-runtime)
        ignition-start-time (double ignition-start-time)
        ignition-stop-time  (+ ignition-start-time max-runtime)]
    (loop [global-clock   ignition-start-time
           burn-vectors   (tufte/p
                           :generate-burn-vectors ;0%
                           (generate-burn-vectors! inputs matrices ignited-cells))
           spot-ignitions {}]
      (if (and (< global-clock ignition-stop-time)
               (or (seq burn-vectors) (seq spot-ignitions)))
        (let [timestep       (tufte/p
                              :calc-timestep ;4%
                              (Math/min (compute-dt cell-size burn-vectors)
                                        (- ignition-stop-time global-clock)))
              burn-vectors   (tufte/p
                              :progress-burn-vectors ;91%
                              (progress-burn-vectors! inputs matrices burn-vectors timestep))
              spot-ignitions (tufte/p
                              :progress-spot-ignitions ;0%
                              (progress-spot-ignitions! inputs matrices spot-ignitions timestep))]
          (recur (+ global-clock timestep)
                 burn-vectors
                 spot-ignitions))
        {:exit-condition             (if (>= global-clock ignition-stop-time) :max-runtime-reached :no-burnable-fuels)
         :global-clock               global-clock
         :burn-time-matrix           (matrices :burn-time-matrix)
         :fire-line-intensity-matrix (matrices :fire-line-intensity-matrix)
         :fire-spread-matrix         (matrices :fire-spread-matrix)
         :fire-type-matrix           (matrices :fire-type-matrix)
         :flame-length-matrix        (matrices :flame-length-matrix)
         :spot-matrix                (matrices :spot-matrix)
         :spread-rate-matrix         (matrices :spread-rate-matrix)
         ;; :crown-fire-count           crown-count ; FIXME: Calculate using tensor ops
         ;; :spot-count                 spot-count  ; FIXME: Calculate using tensor ops or spot-ignitions
         }))))

;;-----------------------------------------------------------------------------
;; Main Simulation Entry Point - Dispatches to Point/Perimeter Ignition
;;-----------------------------------------------------------------------------

;; FIXME: Move this multimethod check into run-simulations to avoid running it in every thread
(defmulti run-fire-spread
  "Runs the raster-based fire spread model with a map of these arguments:
  - max-runtime: double (minutes)
  - cell-size: double (feet)
  - elevation-matrix: core.matrix 2D double array (feet)
  - slope-matrix: core.matrix 2D double array (vertical feet/horizontal feet)
  - aspect-matrix: core.matrix 2D double array (degrees clockwise from north)
  - fuel-model-matrix: core.matrix 2D double array (fuel model numbers 1-256)
  - canopy-height-matrix: core.matrix 2D double array (feet)
  - canopy-base-height-matrix: core.matrix 2D double array (feet)
  - crown-bulk-density-matrix: core.matrix 2D double array (lb/ft^3)
  - canopy-cover-matrix: core.matrix 2D double array (0-100)
  - wind-speed-20ft: double (miles/hour)
  - wind-from-direction: double (degrees clockwise from north)
  - fuel-moisture: doubles (0-1) {:dead {:1hr :10hr :100hr} :live {:herbaceous :woody}}
  - foliar-moisture: double (0-1)
  - ellipse-adjustment-factor: (< 1.0 = more circular, > 1.0 = more elliptical)
  - initial-ignition-site: One of the following:
     - point represented as [row col]
     - a core.matrix 2D double array (0-2)
  - num-rows: integer
  - num-cols: integer"
  (fn [{:keys [initial-ignition-site]}]
    (if (vector? initial-ignition-site)
      :ignition-point
      :ignition-perimeter)))

;;-----------------------------------------------------------------------------
;; Point Ignition
;;-----------------------------------------------------------------------------

(defn- initialize-point-ignition-matrices
  [{:keys [num-rows num-cols initial-ignition-site ignition-start-time spotting]}]
  (let [shape [num-rows num-cols]
        [i j] initial-ignition-site]
    {:burn-time-matrix           (-> (t/new-tensor shape) (t/mset! i j ignition-start-time))
     :fire-line-intensity-matrix (t/new-tensor shape)
     :fire-spread-matrix         (-> (t/new-tensor shape) (t/mset! i j 1.0))
     :fire-type-matrix           (t/new-tensor shape)
     :firebrand-count-matrix     (when spotting (t/new-tensor shape))
     :flame-length-matrix        (t/new-tensor shape)
     :spot-matrix                (when spotting (t/new-tensor shape))
     :spread-rate-matrix         (t/new-tensor shape)
     :travel-lines-matrix        (-> (t/new-tensor shape :datatype :byte) (t/mset! i j 2r1111))}))

(defmethod run-fire-spread :ignition-point
  [{:keys [initial-ignition-site] :as inputs}]
  (run-loop inputs
            (initialize-point-ignition-matrices inputs)
            [initial-ignition-site]))

;;-----------------------------------------------------------------------------
;; Perimeter Ignition
;;-----------------------------------------------------------------------------

(defn- add-ignited-cells!
  [burn-time-matrix ignited-cells ignition-start-time]
  (doseq [[i j] ignited-cells]
    (t/mset! burn-time-matrix i j ignition-start-time))
  burn-time-matrix)

(defn- initialize-perimeter-ignition-matrices
  [{:keys [num-rows num-cols initial-ignition-site ignition-start-time spotting]}
   ignited-cells]
  (let [shape               [num-rows num-cols]
        positive-burn-scar  initial-ignition-site
        negative-burn-scar  (d/clone (dfn/* -1.0 positive-burn-scar))
        burn-time-matrix    (add-ignited-cells! (d/clone negative-burn-scar)
                                                ignited-cells
                                                ignition-start-time)
        travel-lines-matrix (d/clone (d/emap (fn [x] (if (pos? x) 2r1111 2r0000))
                                             :byte
                                             positive-burn-scar))]
    {:burn-time-matrix           burn-time-matrix
     :fire-line-intensity-matrix negative-burn-scar
     :fire-spread-matrix         (d/clone positive-burn-scar)
     :fire-type-matrix           (d/clone negative-burn-scar)
     :firebrand-count-matrix     (when spotting (t/new-tensor shape))
     :flame-length-matrix        (d/clone negative-burn-scar)
     :spot-matrix                (when spotting (t/new-tensor shape))
     :spread-rate-matrix         (d/clone negative-burn-scar)
     :travel-lines-matrix        travel-lines-matrix}))

;; FIXME Optimize the laziness out of this function
(defn- get-perimeter-cells
  [{:keys [num-rows num-cols initial-ignition-site fuel-model-matrix]}]
  (let [{:keys [row-idxs col-idxs]} (non-zero-indices initial-ignition-site)]
    (filterv #(burnable-neighbors? initial-ignition-site
                                   fuel-model-matrix
                                   num-rows
                                   num-cols
                                   %)
             (map vector row-idxs col-idxs))))

(defmethod run-fire-spread :ignition-perimeter
  [inputs]
  (let [ignited-cells (get-perimeter-cells inputs)]
    (when (seq ignited-cells)
      (run-loop inputs
                (initialize-perimeter-ignition-matrices inputs ignited-cells)
                ignited-cells))))
