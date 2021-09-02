(ns gridfire.common)

(defn calc-emc
  "Computes the Equilibrium Moisture Content (EMC) from rh (relative
   humidity in %) and temp (temperature in F)."
  [rh temp]
  (/ (cond (< rh 10) (+ 0.03229 (* 0.281073 rh) (* -0.000578 rh temp))
           (< rh 50) (+ 2.22749 (* 0.160107 rh) (* -0.01478 temp))
           :else     (+ 21.0606 (* 0.005565 rh rh) (* -0.00035 rh temp) (* -0.483199 rh)))
     30))
