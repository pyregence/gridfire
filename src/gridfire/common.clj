(ns gridfire.common
  (:require [tech.v3.datatype            :as d]
            [tech.v3.datatype.argops     :as da]
            [tech.v3.datatype.functional :as dfn]
            [tech.v3.tensor              :as t]))

;; FIXME: unused
(defn calc-emc
  "Computes the Equilibrium Moisture Content (EMC) from rh (relative
   humidity in %) and temp (temperature in F)."
  ^double
  [^double rh ^double temp]
  (/ (double
      (cond (< rh 10) (+ 0.03229 (* 0.281073 rh) (* -0.000578 rh temp))
            (< rh 50) (+ 2.22749 (* 0.160107 rh) (* -0.01478 temp))
            :else     (+ 21.0606 (* 0.005565 rh rh) (* -0.00035 rh temp) (* -0.483199 rh))))
     30))

(def fuel-categories #{:dead :live})

(def fuel-sizes #{:1hr :10hr :100hr :herbaceous :woody})

(defn calc-fuel-moisture
  [relative-humidity temperature category size]
  (when (and (contains? fuel-categories category) (contains? fuel-sizes size))
    (let [equilibrium-moisture (calc-emc relative-humidity temperature)]
      (cond
        (and (= category :dead) (= size :1hr))
        (+ equilibrium-moisture 0.002)

        (and (= category :dead) (= size :10hr))
        (+ equilibrium-moisture 0.015)

        (and (= category :dead) (= size :100hr))
        (+ equilibrium-moisture 0.025)

        (and (= category :live) (= size :herbaceous))
        (+ equilibrium-moisture 2.0)


        (and (= category :live) (= size :woody))
        (+ equilibrium-moisture 0.5)

        :else
        nil))))

(defn in-bounds?
  "Returns true if the point lies within the bounds [0,rows) by [0,cols)."
  [^long rows ^long cols [^long i ^long j]]
  (and (>= i 0)
       (>= j 0)
       (< i rows)
       (< j cols)))

(defn burnable-fuel-model?
  [^double number]
  (and (pos? number)
       (or (< number 91.0)
           (> number 99.0))))

(defn burnable?
  "Returns true if cell [x y] has not yet been ignited (but could be)."
  [fire-spread-matrix fuel-model-matrix [i j :as source] [x y :as here]]
  (let [^double ignition-probability-here   (t/mget fire-spread-matrix x y)
        ^double source-ignition-probability (t/mget fire-spread-matrix i j)]
    (and (< ignition-probability-here ^double (if (= source-ignition-probability 0.0)
                                                1.0
                                                source-ignition-probability))
         (burnable-fuel-model? (t/mget fuel-model-matrix x y)))))

(defn get-neighbors
  "Returns the eight points adjacent to the passed-in point."
  [[i j]]
  (let [i  (long i)
        j  (long j)
        i- (- i 1)
        i+ (+ i 1)
        j- (- j 1)
        j+ (+ j 1)]
    (vector [i- j-] [i- j] [i- j+]
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
    {:row-idxs (d/unchecked-cast row-idxs :int64)
     :col-idxs (d/unchecked-cast col-idxs :int64)}))

(defn non-zero-count [tensor]
  (-> tensor dfn/pos? dfn/sum (d/unchecked-cast :int64)))
