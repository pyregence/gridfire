;; FIXME LP coverage
(ns gridfire.utils.gradient)

(defn estimate-dF
  "Utility for gradient estimation: estimates the derivative dF := (∂F/∂u)*du ≈ (F(u + du) - F(u)) ≈ (F(u + du) - F(u - du))/2,
  based on (potentially missing) values of F(u - du), F(u) and F(u + du).
  Returns 0.0 when too few of the 3 values of F are available,
  (which is signaled by their being equal to NA-Fvalue, typically Double/NaN or -1.0)."
  ^double [^double NA-Fvalue ^double Fu-du ^double Fu ^double Fu+du]
  (if (= NA-Fvalue Fu+du)
    (if (= NA-Fvalue Fu-du)
      0.0                      ; "spear head"
      (if (= NA-Fvalue Fu)
        0.0
        (- Fu Fu-du)))
    (if (= NA-Fvalue Fu-du)
      (if (= NA-Fvalue Fu)
        0.0
        (- Fu+du Fu))
      ;; This is the favored way of estimating the gradient: https://numpy.org/doc/stable/reference/generated/numpy.gradient.html
      (* 0.5 (- Fu+du Fu-du)))))
