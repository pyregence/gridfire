;; [[file:../../org/GridFire.org::gridfire.common][gridfire.common]]
;; FIXME LP coverage
(ns gridfire.common
  (:require [gridfire.fuel-models-optimal :as f-opt]
            [gridfire.grid-lookup         :as grid-lookup]
            [tech.v3.datatype             :as d]
            [tech.v3.datatype.argops      :as da]
            [tech.v3.datatype.functional  :as dfn]
            [tech.v3.tensor               :as t])
  (:import (clojure.lang IFn$LLLLD)))

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
  (and (burnable-fuel-model? (grid-lookup/mget-double-at fuel-model-matrix x y))
       (let [ignition-probability-here   (grid-lookup/mget-double-at fire-spread-matrix x y)
             source-ignition-probability (grid-lookup/mget-double-at fire-spread-matrix i j)]
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
        dz (- (grid-lookup/mget-double-at elevation-matrix i1 j1)
              (grid-lookup/mget-double-at elevation-matrix i2 j2))]
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

;; NOTE I don't see a workable way to primitive-partialize this function given how it's currently used. (Val, 24 Nov 2022)
(defn burnable-cell?
  [get-fuel-model fire-spread-matrix burn-probability num-rows num-cols i j]
  (let [i (long i)
        j (long j)]
    (and (in-bounds-optimal? num-rows num-cols i j)
         (burnable-fuel-model? (grid-lookup/double-at get-fuel-model i j))
         (overtakes-lower-probability-fire? (double burn-probability) (grid-lookup/mget-double-at fire-spread-matrix i j)))))

(defmacro dist-expr
  "A macro expanding to an expression for computing 3D terrain distance.

  i0, j0 must evaluate to positive integers < num-rows, num-cols.
  z0 and z1 must evaluate to elevations (in ft); they may remain unevaluated."
  [num-rows num-cols cell-size
   i0 j0 z0
   i1 j1 z1]
  `(let [i1#      ~i1
         j1#      ~j1
         cs#      ~cell-size
         di#      (* cs# (- ~i0 i1#))
         dj#      (* cs# (- ~j0 j1#))
         di2+dj2# (+ (* di# di#)
                     (* dj# dj#))]
     (Math/sqrt (if (in-bounds-optimal? ~num-rows ~num-cols i1# j1#)
                  (let [dz# (- ~z0 ~z1)]
                    (+ di2+dj2# (* dz# dz#)))
                  di2+dj2#))))

(defn terrain-distance-fn
  "Prepares a primitive-signature function for computing terrain distance,
  which can be invoked very efficiently using macro terrain-distance-invoke."
  [get-elevation ^long num-rows ^long num-cols ^double cell-size]
  (fn between-coords
    ^double [^long i0 ^long j0 ^long i1 ^long j1]
    (dist-expr num-rows num-cols cell-size
               i0 j0 (grid-lookup/double-at get-elevation i0 j0)
               i1 j1 (grid-lookup/double-at get-elevation i1 j1))))

(defmacro terrain-distance-invoke
  "Macro hiding the JVM interop for efficiently invoking the return value of `terrain-distance-fn`."
  [terrain-dist-fn i0 j0 i1 j1]
  (let [tdf-sym (vary-meta (gensym 'terrain-dist-fn) assoc :tag `IFn$LLLLD)]
    `(let [~tdf-sym ~terrain-dist-fn]
       (.invokePrim ~tdf-sym ~i0 ~j0 ~i1 ~j1))))

(defn terrain-distance-from-cell-getter
  "Prepares a function for computing the distance from an already-identified cell,
  to be invoked using `gridfire.grid-lookup/double-at`."
  [get-elevation num-rows num-cols cell-size i0 j0]
  (let [num-rows  (long num-rows)
        num-cols  (long num-cols)
        cell-size (double cell-size)
        i0        (long i0)
        j0        (long j0)
        z0        (grid-lookup/double-at get-elevation i0 j0)]
    (fn distance-from-cell
      ^double [^long i1 ^long j1]
      (dist-expr num-rows num-cols cell-size
                 i0 j0 z0
                 i1 j1 (grid-lookup/double-at get-elevation i1 j1)))))
;; gridfire.common ends here
