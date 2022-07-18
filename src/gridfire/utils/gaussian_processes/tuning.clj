(ns gridfire.utils.gaussian-processes.tuning
  (:require [clojure.core.matrix               :as mx]
            [clojure.pprint                    :as pp]
            [gridfire.utils.gaussian-processes :as gridfire-gp]))

;; ------------------------------------------------------------------------------
;; Choosing the lengthscale

(defn recommend-matern-lengthscale
  [{nu :covariance-kernel.matern/nu} ^double hill-diameter]
  ;; Based on formula (4.3) in Rasmussen & Williams GPML - https://gaussianprocess.org/gpml/chapters/RW4.pdf
  (let [two-pi        (* 2. Math/PI)
        -k20          (case nu
                        3/2 3.
                        5/2 (/ 5. 3.)
                        ##Inf 1.)
        n-0-upc-per-l (/ (Math/sqrt -k20) two-pi)]          ;; 0-upcrossings/lengthscale
    ;; [ft/lengthscale]
    (->
      hill-diameter                                         ;; ft/hill
      (/ 0.5)                                               ;; 0-upcrossings/hill
      (* n-0-upc-per-l))))

(comment
  recommend-matern-lengthscale "helps you choose a" :covariance-kernel.rbf/lengthscale ":"
  ;; When you sample a Gaussian Process, you generate an "undulating" landscape,
  ;; made of hills and depressions of about equal size.
  ;; This helps you choose a lengthscale based on the expected "diameter" of those hills:

  (recommend-matern-lengthscale {:covariance-kernel.matern/nu 3/2} 10e3)
  => 5513.288954217921
  (recommend-matern-lengthscale {:covariance-kernel.matern/nu 5/2} 10e3)
  => 4109.362960409999
  (recommend-matern-lengthscale {:covariance-kernel.matern/nu ##Inf} 10e3)
  => 3183.098861837907

  *e)

;; ------------------------------------------------------------------------------
;; Choosing the supergrid size (number of sampling points along each axis)

(defn unit-matern-kernel
  [{nu :covariance-kernel.matern/nu}]
  (gridfire-gp/kernel-from-correlation-config
    {:gridfire.correlation/kernel-type  :gridfire.correlation.kernel/matern
     :covariance-kernel.matern/nu       nu
     :covariance-kernel/std-dev         1.
     :covariance-kernel.rbf/lengthscale 1.
     :covariance-kernel.rbf/distance-fn :gridfire.distance/euclidean-1d}))

(defn- midpoint-interpolation-error-stddev
  [kernel-fn ^double grid-step]
  (Math/sqrt
    (+
      (* 1.5 (double (kernel-fn 0. 0.)))
      (* 0.5 (double (kernel-fn 0. grid-step)))
      (* -2. (double (kernel-fn 0. (* 0.5 grid-step)))))))

(defn- almost-equal?
  ([^double x0 ^double x1 ^double eps]
   (-> x0 (- x1) (/ x0) (Math/abs) (< eps))))

(defn- variance-of-linear-combination
  [cov-matrix coeffs-v]
  (->
    cov-matrix
    (mx/inner-product coeffs-v)
    (mx/dot coeffs-v)))

(assert
  (every? true?
    (let [interpolation-coeffs (mx/array [0.5 -1. 0.5])]
      (for [nu        [3/2 5/2]
            :let [kernel-fn (unit-matern-kernel {:covariance-kernel.matern/nu nu})]
            grid-step (repeatedly 10 #(Math/pow 10. (-> (rand) (double) (* -3.))))
            :let [grid-step (double grid-step)
                  index-points (mapv (fn index-point [^double u] (* u grid-step)) [0. 0.5 1.])
                  cov-matrix (->> (kernel-fn x0 x1)
                                  (for [x0 index-points]) (vec)
                                  (for [x1 index-points]) (vec)
                                  (mx/matrix))]]
        (almost-equal?
          (midpoint-interpolation-error-stddev kernel-fn grid-step)
          (Math/sqrt
            (variance-of-linear-combination cov-matrix interpolation-coeffs))
          1e-3))))
  (str
    (pr-str `midpoint-interpolation-error-stddev)
    "does computes the standard deviation or the linear interpolation error."))

(comment
  ;;;; Depending on grid step, how accurate is the linear interpolation?

  (require '[clojure.pprint :as pp])

  (defn print-MIErr-for!
    [kernel-fn]
    (pp/print-table
      (for [y (->> 1e-1 (iterate #(* % 0.1)) (take 2))
            x (reverse (range 2 11))]
        (let [grid-step (* x y)]
          {"grid-step / d" (format "%.2f" grid-step)
           "sigma_MIErr / sigma"
           (format "%.3e" (midpoint-interpolation-error-stddev kernel-fn grid-step))}))))

  (print-MIErr-for! (unit-matern-kernel {:covariance-kernel.matern/nu 3/2}))
  ;| grid-step / d | sigma_MIErr / sigma |
  ;|---------------+---------------------|
  ;|          1.00 |           4.146e-01 |
  ;|          0.90 |           3.699e-01 |
  ;|          0.80 |           3.241e-01 |
  ;|          0.70 |           2.774e-01 |
  ;|          0.60 |           2.304e-01 |
  ;|          0.50 |           1.835e-01 |
  ;|          0.40 |           1.376e-01 |
  ;|          0.30 |           9.364e-02 | <-- ≈10%
  ;|          0.20 |           5.345e-02 | <-- ≈5%
  ;|          0.10 |           1.982e-02 |
  ;|          0.09 |           1.701e-02 |
  ;|          0.08 |           1.432e-02 |
  ;|          0.07 |           1.178e-02 |
  ;|          0.06 |           9.393e-03 |
  ;|          0.05 |           7.180e-03 |
  ;|          0.04 |           5.163e-03 |
  ;|          0.03 |           3.370e-03 |
  ;|          0.02 |           1.843e-03 |
  ;; How to read this table:
  ;; Assume we sample a Matern-3/2 Gaussian Process of lengthscale d
  ;; and standard deviation sigma,
  ;; on a grid of step grid-step = 0.30 * d.
  ;; When using linear interpolation to estimate the value V0.5
  ;; at the middle point between 2 sampled grid points V0 and V1,
  ;; the interpolation error (0.5*(V0 + V1) - V0.5) is normally-distributed,
  ;; with mean 0. and standard deviation sigma_MIErr = 0.09.364e-02 * sigma.
  ;; For example, for a temperature-modeling GP with sigma=10°F and lengthscale d=100mi,
  ;; using a 30mi grid will yield a midpoint interpolation error
  ;; having 0.964 °F of standard deviation,
  ;; i.e with about 95% probability of staying within [-2*0.964°F, +2*0.964°F].

  (print-MIErr-for! (unit-matern-kernel {:covariance-kernel.matern/nu 5/2}))
  ;| grid-step / d | sigma_MIErr / sigma |
  ;|---------------+---------------------|
  ;|          1.00 |           3.236e-01 |
  ;|          0.90 |           2.790e-01 |
  ;|          0.80 |           2.348e-01 |
  ;|          0.70 |           1.917e-01 |
  ;|          0.60 |           1.502e-01 |
  ;|          0.50 |           1.114e-01 | <-- ≈10%
  ;|          0.40 |           7.616e-02 |
  ;|          0.30 |           4.581e-02 | <-- ≈5%
  ;|          0.20 |           2.179e-02 |
  ;|          0.10 |           5.832e-03 |
  ;|          0.09 |           4.757e-03 |
  ;|          0.08 |           3.784e-03 |
  ;|          0.07 |           2.917e-03 |
  ;|          0.06 |           2.158e-03 |
  ;|          0.05 |           1.509e-03 |
  ;|          0.04 |           9.726e-04 |
  ;|          0.03 |           5.509e-04 |
  ;|          0.02 |           2.465e-04 |

  (print-MIErr-for! (unit-matern-kernel {:covariance-kernel.matern/nu ##Inf}))
  ;| grid-step / d | sigma_MIErr / sigma |
  ;|---------------+---------------------|
  ;|          1.00 |           1.956e-01 |
  ;|          0.90 |           1.615e-01 |
  ;|          0.80 |           1.298e-01 |
  ;|          0.70 |           1.009e-01 | <-- ≈10%
  ;|          0.60 |           7.510e-02 |
  ;|          0.50 |           5.274e-02 | <-- ≈5%
  ;|          0.40 |           3.407e-02 |
  ;|          0.30 |           1.930e-02 |
  ;|          0.20 |           8.624e-03 |
  ;|          0.10 |           2.163e-03 |
  ;|          0.09 |           1.752e-03 |
  ;|          0.08 |           1.385e-03 |
  ;|          0.07 |           1.060e-03 |
  ;|          0.06 |           7.791e-04 |
  ;|          0.05 |           5.411e-04 |
  ;|          0.04 |           3.464e-04 |
  ;|          0.03 |           1.948e-04 |
  ;|          0.02 |           8.660e-05 |

  *e)

(defn- dichotomic-search
  ^double [f ^double yt ^double x-min ^double x-max]
  (letfn [(between? [^double y ^double y0 ^double y1]
            (or
              (<= y0 y y1)
              (<= y1 y y0)))]
    (loop [x0 (double x-min)
           y0 (f x-min)
           x1 (double x-max)
           y1 (f x-max)]
      (let [x (+ (* 0.5 x0) (* 0.5 x1))
            y (f x)]
        (cond
          (almost-equal? y yt 1e-4) x
          (between? yt y0 y) (recur x0 y0 x y)
          (between? yt y y1) (recur x y x1 y1))))))

(comment

  ;;;; How to tune grid-step for a target precision?

  (defn print-precision->grid-step! []
    (pp/print-table
      (for [mierr [0.2 0.1 0.05 0.025 0.01 0.005]]
        (into
          {"sigma_MIErr / sigma" (str (format "%.1f" (* 100. (double mierr))) "%")}
          (->> [3/2 5/2 ##Inf]
               (map-indexed
                 (fn [^long i nu]
                   (let [kernel-fn (unit-matern-kernel {:covariance-kernel.matern/nu nu})]
                     [(str
                        (when (zero? i) "grid-step / d : ")
                        (format "nu=%s" (pr-str nu)))
                      (str
                        (format "%.1f"
                          (* 100.
                            (dichotomic-search
                              #(midpoint-interpolation-error-stddev kernel-fn %)
                              mierr
                              0. 10.)))
                        "%")]))))))))

  (print-precision->grid-step!)
  ;| sigma_MIErr / sigma | grid-step / d : nu=3/2 | nu=5/2 | nu=##Inf |
  ;|---------------------+------------------------+--------+----------|
  ;|               20.0% |                  53.5% |  72.0% |   101.2% |
  ;|               10.0% |                  31.5% |  46.9% |    69.7% |
  ;|                5.0% |                  19.1% |  31.5% |    48.6% |
  ;|                2.5% |                  11.7% |  21.5% |    34.2% |
  ;|                1.0% |                   6.3% |  13.2% |    21.5% |
  ;|                0.5% |                   3.9% |   9.2% |    15.2% |
  ;; How to read: for nu=3/2, if you want sigma_MIErr to be 10% of sigma,
  ;; set grid-step to be about 31.5% of the lengthscale d.
  ;; Reminder: sigma_MIErr is the standard deviation of the linear-interpolation error at midpoint.

  *e)

(defn recommend-supergrid-size
  [{:keys [num-rows num-cols cell-size max-runtime] :as _inputs} covariant-group ^double rel-err]
  {:pre [(< 0. rel-err 1.)]}
  (letfn [(recommend-supergrid-side [^double map-length kernel-config kernel-1d-fn]
            (->
              (/
                map-length
                (dichotomic-search
                  #(midpoint-interpolation-error-stddev kernel-1d-fn %)
                  (* rel-err (double (or (:covariance-kernel/std-dev kernel-config) 1.)))
                  0.
                  (* 10. (double (:covariance-kernel.rbf/lengthscale kernel-config)))))
              (Math/ceil) (long)))]
    [(let [temporal-config (:gridfire.pertubations/temporal-correlations covariant-group)
           time-kernel-fn  (gridfire-gp/kernel-from-correlation-config temporal-config)]
       (recommend-supergrid-side max-runtime temporal-config time-kernel-fn))
     (let [spatial-config  (:gridfire.pertubations/spatial-correlations covariant-group)
           space-kernel-fn (gridfire-gp/kernel-from-correlation-config spatial-config)
           kernel-fn-1d    (fn [^double r0 ^double r1]
                             (space-kernel-fn
                               (gridfire-gp/->Point2D r0 0.)
                               (gridfire-gp/->Point2D r1 0.)))]
       (recommend-supergrid-side
         (* (double num-rows) (double cell-size))
         spatial-config
         kernel-fn-1d))
     (let [spatial-config  (:gridfire.pertubations/spatial-correlations covariant-group)
           space-kernel-fn (gridfire-gp/kernel-from-correlation-config spatial-config)
           kernel-fn-1d    (fn [^double r0 ^double r1]
                             (space-kernel-fn
                               (gridfire-gp/->Point2D 0. r0)
                               (gridfire-gp/->Point2D 0. r1)))]
       (recommend-supergrid-side
         (* (double num-cols) (double cell-size))
         spatial-config
         kernel-fn-1d))]))

(comment

  recommend-supergrid-size "helps with choosing a" :gridfire.perturbation.smoothed-supergrid/supergrid-size
  "to achieve a desired accuracy when using linear interpolation, for example:"
  (let [inputs {:num-rows    256
                :num-cols    256
                :cell-size   98.425
                ;; [...]
                :max-runtime 1440}
        covariant-group {
                         ;; [...]
                         :gridfire.pertubations/spatial-correlations
                         {:gridfire.correlation/kernel-type  :gridfire.correlation.kernel/matern
                          :covariance-kernel.rbf/lengthscale 4e3                   ;; same unit as :cell-size (feet)
                          :covariance-kernel.matern/nu       5/2                   ;; Gaussian Process shape parameter (higher is smoother.)
                          :covariance-kernel.rbf/distance-fn :gridfire.distance/euclidean-2d}
                         :gridfire.pertubations/temporal-correlations
                         {:gridfire.correlation/kernel-type  :gridfire.correlation.kernel/matern
                          :covariance-kernel.rbf/lengthscale 6e2                   ;; same unit as :max-runtime (minutes)
                          :covariance-kernel.matern/nu       5/2
                          :covariance-kernel.rbf/distance-fn :gridfire.distance/euclidean-1d}}
        precision 0.1]
    (recommend-supergrid-size inputs covariant-group precision))
  => [6 14 14]

  *e)

