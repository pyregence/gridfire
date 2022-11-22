(ns gridfire.common
  (:require [gridfire.fuel-models-optimal :as f-opt]
            [gridfire.grid-lookup         :as grid-lookup]
            [tech.v3.datatype             :as d]
            [tech.v3.datatype.argops      :as da]
            [tech.v3.datatype.functional  :as dfn]
            [tech.v3.tensor               :as t]))

(set! *unchecked-math* :warn-on-boxed)

(defn calc-emc
  "Computes the Equilibrium Moisture Content (EMC) from rh (relative
   humidity in %) and temp (temperature in F)."
  ^double
  [^double rh ^double temp]
  (/ (double
      (cond (< rh 10.0) (+ 0.03229 (* 0.281073 rh) (* -0.000578 rh temp))
            (< rh 50.0) (+ 2.22749 (* 0.160107 rh) (* -0.01478 temp))
            :else       (+ 21.0606 (* 0.005565 rh rh) (* -0.00035 rh temp) (* -0.483199 rh))))
     30.0))

(defn calc-fuel-moisture
  ^double
  [^double relative-humidity ^double temperature category size]
  (let [equilibrium-moisture (calc-emc relative-humidity temperature)]
    (case [category size]
      [:dead :1hr]        (+ equilibrium-moisture 0.002)
      [:dead :10hr]       (+ equilibrium-moisture 0.015)
      [:dead :100hr]      (+ equilibrium-moisture 0.025)
      [:live :herbaceous] (* equilibrium-moisture 2.0)
      [:live :woody]      (* equilibrium-moisture 0.5))))

(defn in-bounds?
  "Returns true if the point lies within the bounds [0,rows) by [0,cols)."
  [^long rows ^long cols [^long i ^long j]]
  (and (>= i 0)
       (>= j 0)
       (< i rows)
       (< j cols)))

(defn in-bounds-optimal?
  "Returns true if the point lies within the bounds [0,rows) by [0,cols)."
  [^long rows ^long cols ^long i ^long j]
  (and (>= i 0)
       (>= j 0)
       (< i rows)
       (< j cols)))

(defn burnable-fuel-model?
  ;; NOTE: the motivation for accepting a double-typed argument
  ;; is that (grid-lookup/double-at) return doubles.
  [^double fm-number]
  (f-opt/is-burnable-fuel-model-number? fm-number))

(defn overtakes-lower-probability-fire?
  "True when the cell [i j] has never burned at a :burn-probability level >= bv-burn-probability."
  [^double bv-burn-probability ^double fire-spread-matrix_ij]
  (> bv-burn-probability fire-spread-matrix_ij))

;; FIXME: This logic doesn't look right.
(defn burnable?
  "Returns true if cell [x y] has not yet been ignited (but could be)."
  [fire-spread-matrix fuel-model-matrix [i j] [x y]]
  (and (burnable-fuel-model? (t/mget fuel-model-matrix x y))
       (let [^double ignition-probability-here   (t/mget fire-spread-matrix x y)
             ^double source-ignition-probability (t/mget fire-spread-matrix i j)]
         (< ignition-probability-here (if (= source-ignition-probability 0.0)
                                        1.0
                                        source-ignition-probability)))))

(defn get-neighbors
  "Returns the eight points adjacent to the passed-in point."
  [[i j]]
  (let [i  (long i)
        j  (long j)
        i- (- i 1)
        i+ (+ i 1)
        j- (- j 1)
        j+ (+ j 1)]
    (list [i- j-] [i- j] [i- j+]
          [i  j-]        [i  j+]
          [i+ j-] [i+ j] [i+ j+])))

(defn burnable-neighbors?
  [fire-spread-matrix fuel-model-matrix num-rows num-cols cell]
  (some #(and (in-bounds? num-rows num-cols %)
              (burnable? fire-spread-matrix fuel-model-matrix cell %))
        (get-neighbors cell)))

(defn distance-3d
  "Returns the terrain distance between two points in feet."
  ^double
  [elevation-matrix ^double cell-size [i1 j1] [i2 j2]]
  (let [i1 (long i1)
        j1 (long j1)
        i2 (long i2)
        j2 (long j2)
        di (* cell-size (- i1 i2))
        dj (* cell-size (- j1 j2))
        dz (- ^double (t/mget elevation-matrix i1 j1)
              ^double (t/mget elevation-matrix i2 j2))]
    (Math/sqrt (+ (* di di) (* dj dj) (* dz dz)))))

(defn non-zero-indices [tensor]
  (let [[_ cols] (:shape (t/tensor->dimensions tensor))
        indices  (da/argfilter pos? tensor)
        row-idxs (dfn/quot indices cols)
        col-idxs (dfn/rem indices cols)]
    {:row-idxs (d/emap #(d/unchecked-cast % :int64) :int64 row-idxs)
     :col-idxs (d/emap #(d/unchecked-cast % :int64) :int64 col-idxs)}))

(defn non-zero-count [tensor]
  (-> tensor dfn/pos? dfn/sum (d/unchecked-cast :int64)))

(defn burnable-cell?
  [get-fuel-model fire-spread-matrix burn-probability num-rows num-cols i j]
  (and (in-bounds-optimal? num-rows num-cols i j)
       (burnable-fuel-model? (grid-lookup/double-at get-fuel-model i j))
       (overtakes-lower-probability-fire? (double burn-probability) (double (t/mget fire-spread-matrix i j)))))

(defn compute-terrain-distance
  [cell-size get-elevation num-rows num-cols i j new-i new-j]
  (let [cell-size (double cell-size)
        i         (long i)
        j         (long j)
        new-i     (long new-i)
        new-j     (long new-j)
        di        (* cell-size (- i new-i))
        dj        (* cell-size (- j new-j))]
    (if (in-bounds-optimal? num-rows num-cols new-i new-j)
      (let [dz (- (grid-lookup/double-at get-elevation i j)
                  (grid-lookup/double-at get-elevation new-i new-j))]
        (Math/sqrt (+ (* di di) (* dj dj) (* dz dz))))
      (Math/sqrt (+ (* di di) (* dj dj))))))
