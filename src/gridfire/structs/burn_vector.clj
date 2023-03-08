;; FIXME LP coverage
(ns gridfire.structs.burn-vector
  "Custom data structure for representing Burn Vectors, isolated in this namespace to wrap JVM interop.")

(defrecord BurnVector
  [^long   i
   ^long   j
   ;; The above i,j coordinates locate the origin of this burn vector.
   ;; The following direction (a multiple of 45°) locates its target cell:
   ^double direction
   ^double fractional-distance
   ^double spread-rate
   ^double terrain-distance
   ^double burn-probability])

(defmacro make-burn-vector
  "Fast constructor for BurnVector."
  [& args]
  ;; More efficient than ->BurnVector, in particular because it avoids boxing of arguments.
  `(BurnVector. ~@args))

(defn get-i
  ^long [^BurnVector bv]
  (.-i bv))

(defn get-j
  ^long [^BurnVector bv]
  (.-j bv))

(defn get-direction
  "The direction of the BurnVector, a double among #{0.0 45.0 90.0 ...}"
  ^double [^BurnVector bv]
  (.-direction bv))

(defn get-fractional-distance
  "Represents the progress of the Burn Vector along its travel line. In particular:
  - 0.5 is the starting point (center of the cell);
  - 1.0 is the crossing point to the target cell;
  - > 1.0 can only be an ephemeral state (calls for transition);
  - < 0.5 is in practice the 2nd half of the travel (after transitioning to target cell);
  - > 0.5 is in practice the 1st half of the travel (after igniting, before transitioning)."
  ^double [^BurnVector bv]
  (.-fractional-distance bv))

(defn get-spread-rate
  "The current speed of the Burn Vector."
  ^double [^BurnVector bv]
  (.-spread-rate bv))

(defn get-terrain-distance
  ^double [^BurnVector bv]
  (.-terrain-distance bv))

(defn get-burn-probability
  ^double [^BurnVector bv]
  (.-burn-probability bv))
