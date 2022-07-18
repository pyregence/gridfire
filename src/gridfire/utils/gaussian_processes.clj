(ns gridfire.utils.gaussian-processes
  "Utilities for using Gaussian Processes in GridFire,
   which are well-established statistical models and sampling algorithms
   for modeling random functions with some spatial / temporal continuity."
  (:require [clojure.core.matrix :as mx]
            [clojure.core.matrix.linear :as mx-linalg]
            [clojure.core.matrix.random :as mx-random]
            [tech.v3.datatype :as d]
            [tech.v3.tensor :as t]))

(mx/set-current-implementation :vectorz)

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; ------------------------------------------------------------------------------
;; Covariance functions (kernels)

(defmulti distance-fn-from-name identity)

(defn grid-1d-distance
  ^double [^double i ^double j]
  (Math/abs (- i j)))

(defmethod distance-fn-from-name :gridfire.distance/euclidean-1d
  [_]
  grid-1d-distance)

(defrecord Point2D
  [^double point2d_i
   ^double point2d_j])

(defn grid-2d-indices
  [n]
  (vec
    (for [i (range n)
          j (range n)]
      (->Point2D (double i) (double j)))))

(defn grid-2d-euclidean-dist
  [^Point2D p0 ^Point2D p1]
  (let [id (- (.point2d_i p0) (.point2d_i p1))
        jd (- (.point2d_j p0) (.point2d_j p1))]
    (Math/sqrt
      (+
        (* id id)
        (* jd jd)))))

(defmethod distance-fn-from-name :gridfire.distance/euclidean-2d
  [_]
  grid-2d-euclidean-dist)

(defmulti kernel-from-correlation-config :gridfire.correlation/kernel-type)

(defn- constant-kernel
  [^double sigma]
  (let [sigma2 (Math/pow sigma 2.)]
    (fn [_x0 _x1]
      sigma2)))

(defmethod kernel-from-correlation-config :gridfire.correlation.kernel/constant ;; INTRO
  ;; Constant kernels constrain all sampled points to be the same value.
  ;; They are a limiting case of RBF kernels with very high lengthscales.
  [{sigma :covariance-kernel/std-dev
    :or   {sigma 1.}}]
  (constant-kernel sigma))

(defn- matern-3-2-kernel
  [^double sigma ^double d0 dist-fn]
  (let [sigma2 (Math/pow sigma 2.)
        sqrt3 (Math/sqrt 3.)]
    (fn matern-32-cov [x0 x1]
      (let [x (-> (dist-fn x0 x1) (double) (/ d0))]
        (-> sigma2
            (* (-> x (* sqrt3) (+ 1.)))
            (* (Math/exp (- (* x sqrt3)))))))))

(defn- matern-5-2-kernel
  [^double sigma ^double d0 dist-fn]
  (let [sigma2 (Math/pow sigma 2.)
        sqrt5 (Math/sqrt 5.)]
    (fn matern-52-cov [x0 x1]
      (let [s5x (-> (dist-fn x0 x1) (double) (/ d0) (* sqrt5))]
        (-> sigma2
            (*
              (+
                1.
                s5x
                (/ (* s5x s5x) 3.)))
            (* (Math/exp (- s5x))))))))

(defn- squared-exp-kernel
  [^double sigma ^double d0 dist-fn]
  (let [sigma2 (Math/pow sigma 2.)]
    (fn squared-exp-cov [x0 x1]
      (let [u (-> (dist-fn x0 x1) (double) (/ d0))]
        (* sigma2
          (Math/exp (- (* u u 0.5))))))))

(defmethod kernel-from-correlation-config :gridfire.correlation.kernel/matern
  [{d0         :covariance-kernel.rbf/lengthscale
    nu         :covariance-kernel.matern/nu
    sigma      :covariance-kernel/std-dev
    dist-fn-kw :covariance-kernel.rbf/distance-fn
    :or        {sigma 1.}}]
  (let [dist-fn (distance-fn-from-name dist-fn-kw)]
    (case nu
      3/2 (matern-3-2-kernel sigma d0 dist-fn)
      5/2 (matern-5-2-kernel sigma d0 dist-fn)
      ##Inf (squared-exp-kernel sigma d0 dist-fn)
      (throw
        (ex-info
          (format "Unsupported %s for Matern kernel, allowed values are: %s."
            (pr-str :covariance-kernel.matern/nu)
            (pr-str (into (sorted-set) #{3/2 5/2 ##Inf})))
          {:covariance-kernel.matern/nu nu})))))

;; ------------------------------------------------------------------------------
;; Sampling (drawing random functions from a given covariance function).

(comment ;;;; How long does it take to compute the Cholesky decomposition?

  (def cov
    (matern-3-2-kernel 4. 3. grid-1d-distance))

  (require '[criterium.core :as bench])

  (let [n (* 10 10)
        C (mx/matrix :vectorz
            (for [i (range n)]
              (vec
                (for [j (range n)]
                  (cov i j)))))]
    (bench/quick-bench
      (mx-linalg/cholesky C {:return [:L*]})))
  ;Evaluation count : 2424 in 6 samples of 404 calls.
  ;             Execution time mean : 175.993395 µs
  ;    Execution time std-deviation : 10.410309 µs
  ;   Execution time lower quantile : 165.975064 µs ( 2.5%)
  ;   Execution time upper quantile : 189.229979 µs (97.5%)
  ;                   Overhead used : 1.930390 ns

  (let [n (* 20 20)
        C (mx/matrix
            (for [i (range n)]
              (vec
                (for [j (range n)]
                  (cov i j)))))]
    (bench/quick-bench
      (mx-linalg/cholesky C {:return [:L*]})))
  ;Evaluation count : 84 in 6 samples of 14 calls.
  ;             Execution time mean : 7.802924 ms
  ;    Execution time std-deviation : 248.427012 µs
  ;   Execution time lower quantile : 7.644133 ms ( 2.5%)
  ;   Execution time upper quantile : 8.223330 ms (97.5%)
  ;                   Overhead used : 1.930390 ns
  ;
  ;Found 1 outliers in 6 samples (16.6667 %)
  ;	low-severe	 1 (16.6667 %)
  ; Variance from outliers : 13.8889 % Variance is moderately inflated by outliers

  *e)

(defn- vectorz-tensor->dtype-next
  [m]
  (-> m
      (mx/to-double-array)
      (t/->tensor)
      (t/reshape (mx/shape m))))

(defn- dtype-next->vectorz-tensor
  [t]
  (-> t
      (t/reshape [(d/ecount t)])
      (d/->double-array)
      (as-> arr1d (mx/array #_:vectorz arr1d))
      (mx/reshape (d/shape t))))

(defn- rotate-dims
  "Reshapes a tensor of shape [n0 n1 ... nd-1] to make it [n1 n2 ... nd-1 n0]."
  [ndarr]
  (comment "at the time of writing," mx/transpose "is heavily under-optimized, therefore we translate to" t/transpose ".")
  (let [n-dims (mx/dimensionality ndarr)]
    (-> ndarr
        (vectorz-tensor->dtype-next)
        (t/transpose
          (concat (range 1 n-dims) [0]))
        (dtype-next->vectorz-tensor))))

(defn- kronecker-inner-product
  "Computes the application (inner product) of a Kronecker-product of matrices to a vector.

  Given:
  - a non-empty sequence Ms of matrices, of shapes ([m0 n0] [m1 n1] ...),
  representing their Kronecker product;
  - an n-dimensional array v of shape [n0 n1 ...],
  representing a vector in tensor-product space
  of dimensions (* n0 n1 ...);

  returns a n-dimensional array of shape [m0 m1 ...],
  representing the inner product of K to v,
  where K is the Kronecker product of the matrices Ms,
  i.e K would be a matrix of shape [(* m0 m1 ...) (* n0 n1 ...)],
  mapping the indices pair [(i0, i1, ...) (j0, j1, ...)]
  to (* (M0 i0 j0) (M1 i1 j1) ...).
  Algebraically, the Kronecker product K represents a linear map
  from vector space (tensor-product-space (row-space M0) (row-space M1) ...)
  to vector space (tensor-product-space (col-space M0) (col-space M1) ...).

  The benefit of this function is to never fully realize the matrix K
  in memory, by leveraging the Kronecker-product algebraic properties."
  [Ms v]
  {:pre  [(seqable? Ms)
          (every? mx/matrix? Ms)
          (mx/array? v)
          (= (mx/shape v) (mapv mx/column-count Ms))]
   :post [mx/array?
          (fn returns-expected-shape? [y]
            (= (mx/shape y) (mapv mx/row-count Ms)))]}
  (->> Ms
       ;; It's not trivial that this works,
       ;; but the principle of this algorithm is beautifully simple:
       ;; Take the input vector (tensor-shaped), then throw each matrix
       ;; at it in turn, transforming the vector by applying the only possible
       ;; matrix-tensor inner product that makes sense.
       (reduce
         (fn apply-M-and-rotate-shape [v M]
           (-> (mx/inner-product M v)
               (rotate-dims)))
         v)))

(defn- covariance-matrix
  [index-points cov-fn]
  (mx/matrix #_:vectorz
    (->> index-points
         (mapv
           (fn [xi]
             (->> index-points
                  (mapv
                    (fn [xj]
                      (->
                        (cov-fn xi xj)
                        (double)
                        (cond-> (= xi xj)
                          (* (+
                               1.
                               ;; HACK this adds a very small i.i.d Gaussian noise, helping with
                               ;; the numerical stability of computing the Cholesky decomposition.
                               1e-10))))))))))))

(comment
  {:gridfire.gaussian/sample-with-low-entropy true} ;; INTRO enables an optimization to the GP-sampling algorithm.
  ;; The 'default' algorithm for sampling a GP on N points is to sample a standard i.i.d Gaussian vector u of size N,
  ;; and then compute Lu, where L is a 'square root' (Cholesky) of covariance matrix C = LL*.
  ;; Sampling those N random gaussians is costly, and is a waste of randomness: in typical settings,
  ;; the whole point of using a GP is that your process has much less randomness than N i.i.d Gaussians!
  ;; Due to correlations, most of the variance is concentrated on a few leading eigenvectors of C.
  ;; One solution is to still generate an i.i.d zero-mean unit-variance vector u,
  ;; but with a distribution which is cheaper to sample than a Gaussian, e.g a balanced binomial
  ;; or even a Bernouilli distribution. The end result will still be approximately a Gaussian process,
  ;; thanks to the 'mixing effect' of going through L (like in the Central Limit Theorem).

  ;; Another optimization approach could be 'Principal Components sampling':
  ;; instead of preparing a Cholesky decomposition, prepare a small selection of s leading eigenvectors of C,
  ;; capturing most of the variance, and sample using s i.i.d Gaussians.
  ;; This amounts to writing C≈SS*, where S is Nxs and S*S is diagonal, and computing Sv,
  ;; where v is generated as an s-size i.i.d standard Gaussian vector.
  ;; See also 'truncated Singular Value Decomposition' and 'Principal Components Analysis'
  ;; for related concepts.
  *e)

(defn- low-entropy-iid-sampler
  [shape]
  (comment ;; This other approach has been tried as well:
    (let [arr (object-array
                ;; Using an Object (Double) array rather than a primitive double array
                ;; is actually faster here, as it will later save Double wrapper allocation.
                (for [i (range 16)]
                  (->> (range 4)
                       (map
                         (fn [j]
                           (if (bit-test i j)
                             1.
                             -1.)))
                       (map #(/ (long %) 2.))
                       (apply + 0.))))
          two-d-1 (int (dec 16))]
      (fn sample-balanced-binomial [^java.util.Random rand-gen]
        (mx/compute-matrix shape
          (fn [& _idx]
            (aget arr (bit-and two-d-1 (.nextInt rand-gen))))))))
  (let [e-count (long (apply * 1 shape))]
    (fn sample-bernouilli [^java.util.Random rand-gen]
      (let [arr (double-array e-count 0.)]
        (loop [i (int 0)]
          (if (< i (alength arr))
            (do
              (aset arr i
                (let [drawn (.nextBoolean rand-gen)]
                  (double
                    (if drawn
                      1.
                      -1.))))
              (recur (unchecked-inc-int i)))
            (-> (mx/array arr)
                (mx/reshape shape))))))))

(defn multiplicative-gaussian-process-sampler
  "Creates a sampler function for a Gaussian Process with the provided multiplicative covariance structure.

  Given:
  - index-points+cov-fns: a sequence ([index-points-0 cov-fn-0] [index-points-1 cov-fn-1] ...),
  in which index-points-k is a sequence of points (p0 p1 ...),
  and cov-fn-k is a 2-args covariance function ('kernel'),
  with (cov-fn-k pi pj) returning the covariance between pi and pj (a double)

  returns a 1-arg non-deterministic function, accepting a source of randomness
  and returning each time a random tensor,
  sampled from the 0-mean Gaussian Process specified by the cov-fns.

  Each sampled tensor has shape [(count index-points-0) (count index-points-1) ...];
  you can view it as a random real-valued function over the 'grid'
  (cartesian-product index-points-0 index-points-1 ...). The indices of this tensor
  positionally match the index-points sequences, i.e the value at tensor index [i0 i1 ...]
  corresponds to grid coordinates [(nth index-points-0 i0) (nth index-points-1 i1) ...].
  Since this is a Gaussian Process, this random function follows a multivariate normal
  distribution (a.k.a 'jointly Gaussian'), with one real-valued random variable per grid point.

  The covariance of said GP has a multiplicative (Kronecker product) structure, i.e.
  the covariance between the 2 grid points of coordinates [p0 p1 ...] and [q0 q1 ...]
  is (* (cov-fn-0 p0 q0) (cov-fn-1 p1 q1) ...).

  This Kronecker-product structure enables major optimizations to the GP sampling algorithm:
  - we don't need to compute large Cholesky decomposition,
  - the entire grid-to-grid covariance matrix is never realized in memory,
  - and its square root is fast to compute
  ... and is straightforward to reason about: all other dimensions being equal,
  the correlation between grid points at pi and qi is given by (cov-fn-i pi qi).

  Typically, index-points+cov-fns could contain 3 tuples, one for space, one for time,
  and one for physical quantities e.g (:temperature :relative-humidity :wind-speed-20ft ...).
  You can independently choose a correlation structure for space, time and physical quantities,
  and they will be combined in a multiplicative way."
  [index-points+cov-fns
   {use-low-entropy-sampler? :gridfire.gaussian/sample-with-low-entropy
    :as _opts}]
  ;; Inspiration: https://docs.gpytorch.ai/en/stable/examples/02_Scalable_Exact_GPs/Scalable_Kernel_Interpolation_for_Products_CUDA.html
  (let [Ls         (->> index-points+cov-fns
                        (mapv (fn square-root-of-covariance [[xs cov-fn]]
                                (let [C (covariance-matrix xs cov-fn)]
                                  (->
                                    (mx-linalg/cholesky C {:return [:L]})
                                    :L)))))
        shape      (->> index-points+cov-fns
                        (mapv
                          (fn [[xs _]]
                            (count xs))))
        sample-iid (if use-low-entropy-sampler?
                     (low-entropy-iid-sampler shape)
                     #(mx-random/sample-normal shape %))]
    (fn sample-multiplicative-gp [rand-gen]
      (vectorz-tensor->dtype-next
        (let [u (sample-iid rand-gen)]
          (kronecker-inner-product Ls u))))))


(comment

  (def sample-me
    (multiplicative-gaussian-process-sampler
      [[(range 200) (squared-exp-kernel 4. 3e1 grid-1d-distance)]
       [(range 200.) (matern-5-2-kernel 1. 5e1 grid-1d-distance)]]
      {:gridfire.gaussian/sample-with-low-entropy false}))

  (require 'matrix-viz.core)
  (require 'matrix-viz.inspector.swing)

  (matrix-viz.inspector.swing/inspect-img
    (matrix-viz.core/matrix-to-buffered-image
      :color 3 nil
      (sample-me (java.util.Random.)))
    {})

  *e

  (require '[criterium.core :as bench])

  (defn my-gp-benchmark [opts]
    ((multiplicative-gaussian-process-sampler
       [[(range 10) (matern-3-2-kernel 4. 3. grid-1d-distance)]
        [(range (+ 20 2)) (matern-5-2-kernel 1. 5e1 grid-1d-distance)]
        [(grid-2d-indices (+ 16 2)) (matern-3-2-kernel 1. 3. grid-2d-euclidean-dist)]]
       opts)
     (java.util.Random.)))

  (defn my-gp-benchmark [opts]
    (let [sample-gp (multiplicative-gaussian-process-sampler
                      [[(range 10) (matern-3-2-kernel 4. 3. grid-1d-distance)]
                       [(range (+ 20 2)) (matern-5-2-kernel 1. 5e1 grid-1d-distance)]
                       [(grid-2d-indices (+ 16 2)) (matern-3-2-kernel 1. 3. grid-2d-euclidean-dist)]]
                      opts)
          rand-gen (java.util.Random.)]
      (dotimes [_ 5]
        (sample-gp rand-gen))))

  (bench/quick-bench (my-gp-benchmark {:gridfire.gaussian/sample-with-low-entropy false}))
  ;Evaluation count : 6 in 6 samples of 1 calls.
  ;             Execution time mean : 190.390873 ms
  ;    Execution time std-deviation : 1.442412 ms
  ;   Execution time lower quantile : 188.931012 ms ( 2.5%)
  ;   Execution time upper quantile : 192.675431 ms (97.5%)
  ;                   Overhead used : 1.889314 ns
  ;
  ;Found 1 outliers in 6 samples (16.6667 %)
  ;	low-severe	 1 (16.6667 %)
  ; Variance from outliers : 13.8889 % Variance is moderately inflated by outliers

  (bench/quick-bench (my-gp-benchmark {:gridfire.gaussian/sample-with-low-entropy true}))
  ;Evaluation count : 6 in 6 samples of 1 calls.
  ;             Execution time mean : 158.087689 ms
  ;    Execution time std-deviation : 1.384360 ms
  ;   Execution time lower quantile : 156.398787 ms ( 2.5%)
  ;   Execution time upper quantile : 159.758889 ms (97.5%)
  ;                   Overhead used : 1.889314 ns
  ;
  ;Found 2 outliers in 6 samples (33.3333 %)
  ;	low-severe	 1 (16.6667 %)
  ;	low-mild	 1 (16.6667 %)
  ; Variance from outliers : 13.8889 % Variance is moderately inflated by outliers

  (require '[clj-async-profiler.core :as prof])
  (prof/serve-files 8080)

  (prof/profile
    (dotimes [_ 20]
      (my-gp-benchmark {:gridfire.gaussian/sample-with-low-entropy true})))

  ;; NOTE: most of the time is taken by inner products and i.i.d sampling. (Val, 17 Jul 2022)
  ;; To my surprise, Cholesky decomp and data copying / transposing take little of the time.
  ;; We might want to switch to a faster ndarray implementation than Vectorz;
  ;; ND4J seems like a good candidate (so does Deep Diamond, but seems less mature).

  ;; https://deeplearning4j.konduit.ai/nd4j/tutorials/quickstart
  ;; https://javadoc.io/static/org.nd4j/nd4j-api/1.0.0-M2/org/nd4j/linalg/api/ndarray/INDArray.html#permute-int...-
  *e)
