;; FIXME LP coverage
(ns gridfire.utils.flow)

(defn- log2
  ^double [^double x]
  ;; TODO bump Clojure version then use clojure.math/log. (Val, 22 Nov 2022)
  (/ (Math/log x)
     (Math/log 2.0)))

(defn- h2
  "Binary Entropy function - the entropy of a Bernoulli random variable with parameter p, in bits."
  ^double [^double p]
  (cond
    (= 0.0 p) 0.0
    (= 1.0 p) 0.0
    :else     (let [q (- 1.0 p)]
                ;; NOTE we could instead use the natural logarithm,
                ;; which would return the entropy in nats instead of bits;
                ;; however, bits seem more natural in this case,
                ;; since bits can be viewed as an "expected number of binary tests required".
                (+ (* p (log2 (/ 1.0 p)))
                   (* q (log2 (/ 1.0 q)))))))

(defn- n*entropy
  "Computes the N * H[B], where N is (count ds),
  and B is the random variable (pred D), for D uniformly chosen among ds."
  ^double [ds pred]
  (let [n  (double (count ds))
        n+ (->> ds (filter pred) (count) (double))]
    (* n (h2 (/ n+ n)))))

(defn- total-information-gain
  "Computes N * I[X,Y], in which:
  - I[X,Y] is the Mutual Information: I[X,Y] = H[Y] - H[Y|X],
  - H[Z] is the Entropy of random variable Z,
  - N is the number of outcomes (count ds),
  - Y is the binary random variable (pred D),
  - X is the random variable (get d->v D),
  under the probability model that
  D is a random variable uniformly distributed among (keys d->v).

  I[X,Y] is a measure of how much uncertainty about X is eliminated by knowing Y (on average, and vice-versa)."
  ^double [pred d->v]
  ;; Background: https://vvvvalvalval.github.io/posts/2020-05-15_Using-Decision-Trees-for-charting-ill-behaved-datasets.html#Appendix:-deriving-the-Total-Information-Gain-formula
  (- (n*entropy (keys d->v) pred)
     (->> (keys d->v)
          (group-by d->v)
          (vals)
          (map (fn [ds] (n*entropy ds pred)))
          (reduce + 0.0)
          (double))))

(defn- double-bit-tester
 [^long bit-idx]
 (fn test-bit-of-double [^double v]
   (bit-test (Double/doubleToRawLongBits v) bit-idx)))

(defn- bit-information-gain
  ^double [d->ret ^long bit-idx]
  (/ (total-information-gain (double-bit-tester bit-idx) d->ret)
     (double (count d->ret))))

(defmacro case-double
  "Similar to (case ...) but optimized for a finite input set of doubles.
  match+rets must be a sequence of match clauses followed by return expressions,
  like in case, except that:
  1. a match clause can only be a number or list of numbers,
  2. there cannot be a fallback last expression,
  3. the caller MUST ensure that x will evaluate to be among the doubles enumerated in the match clauses;
  the behavior is unspecified if that assumption is not met."
  [x & match+rets]
  ;; Implementation: expands to a tree of bit tests on the raw bits of x;
  ;; the bits are chosen by eager entropy reduction (as in a Decision Tree),
  ;; assuming that all values in (keys double->ret) are equally probable.
  (let [double->ret  (->> match+rets
                          (partition 2)
                          (into {} (mapcat (fn match+ret->kvs [[match ret]]
                                             (cond
                                               (number? match) [[(double match)
                                                                 ret]]
                                               (list? match) (->> match
                                                                  (mapv (fn [d]
                                                                          [(double d)
                                                                           ret]))))))))
        raw-bits-sym (gensym 'raw-bits)]
    (letfn [(code-for [d->ret]
              (if (apply = (vals d->ret))
                ;; Leaf: all considered doubles have the same return expr, and so we emit that expr.
                (-> d->ret (first) (val))
                ;; We choose a most informative bit to test against, in the sense of Information Theory,
                ;; i.e. a bit that achieves the maximal entropy reduction.
                (let [most-informative-bit (->> (range 64)
                                                (apply max-key (fn [bit-idx] (bit-information-gain d->ret bit-idx))))]
                  `(if (bit-test ~raw-bits-sym ~most-informative-bit)
                     ~(code-for (select-keys d->ret (->> (keys d->ret)
                                                         (filter (double-bit-tester most-informative-bit)))))
                     ~(code-for (select-keys d->ret (->> (keys d->ret)
                                                         (remove (double-bit-tester most-informative-bit)))))))))]
      `(let [~raw-bits-sym (Double/doubleToRawLongBits ~x)]
         ~(code-for double->ret)))))

(comment

  (def x 45.0)
  (macroexpand '(case-double x
                  0.0   false
                  45.0  true
                  90.0  false
                  135.0 true
                  180.0 false
                  225.0 true
                  270.0 false
                  315.0 true))
  ;;=> expands to something like:
  (let [raw-bits15403 (Double/doubleToRawLongBits x)]
    (if (bit-test raw-bits15403 45)
      (if (bit-test raw-bits15403 52)
        (if (bit-test raw-bits15403 49)
          true
          false)
        true)
      (if (bit-test raw-bits15403 62)
        (if (bit-test raw-bits15403 53)
          false
          (if (bit-test raw-bits15403 52)
            false
            true))
        false)))
  ;; NOTE: it's not guaranteed that eager entropy reduction
  ;; achieves an optimal "depth" of decision tree;
  ;; even so, it's probably close to optimal efficiency,
  ;; as it will tend to be rewarded by CPU branch prediction.

  ;; Here's an example where the sign bit (64) is clearly the most informative,
  ;; and gets indeed tested:
  (macroexpand '(case-double x
                  (-1.0 -2.0 -3.0 -4.0) :negative
                  ( 1.0  2.0  3.0  4.0) :positive))
  ;;=> expands to something like:
  (let* [raw-bits2011 (Double/doubleToRawLongBits x)]
    (if (bit-test raw-bits2011 63)
      :negative
      :positive))

  ;; Here's a slightly more irregular variant of the above:
  (macroexpand '(case-double x
                  (0.0 -1.0 -2.0 -3.0 -4.0) :nonpositive
                  (     1.0  2.0  3.0  4.0) :positive))
  ;;=> expands to something like:
  (let* [raw-bits2016 (Double/doubleToRawLongBits x)]
    (if (bit-test raw-bits2016 63)
      :nonpositive
      (if (bit-test raw-bits2016 62)
        :positive
        (if (bit-test raw-bits2016 61)
          :positive
          :nonpositive))))

  (macroexpand '(case-double x
                  0.0 0
                  45.0 1
                  90.0 2
                  135.0 3
                  180.0 4
                  225.0 5
                  270.0 6
                  315.0 7))
  ;;=>
  (let [raw-bits15408 (Double/doubleToRawLongBits x)]
    (if (bit-test raw-bits15408 50)
      (if (bit-test raw-bits15408 53)
        (if (bit-test raw-bits15408 51)
          5
          4)
        (if (bit-test raw-bits15408 52)
          2
          1))
      (if (bit-test raw-bits15408 52)
        (if (bit-test raw-bits15408 49)
          7
          6)
        (if (bit-test raw-bits15408 62)
          3
          0))))

  *e)
