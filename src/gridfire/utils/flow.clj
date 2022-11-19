(ns gridfire.utils.flow)

(defn- h2
  "Binary Entropy function - the entropy of a Bernouilli random variable with parameter p."
  ^double [^double p]
  (cond
    (= 0.0 p) 0.0
    (= 1.0 p) 0.0
    :else     (+ (* -1.0 p (/ (Math/log p) (Math/log 2.0)))
                 (let [q (- 1.0 p)]
                   (* -1.0 q (/ (Math/log q) (Math/log 2.0)))))))

(defn- n*entropy
  ^double [ds pred]
  ;; Computes the N * H[B], where N is (count ds),
  ;; and B is the random variable (pred D), for D uniformly chosen among ds.
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
  D is a random variable uniformly distributed among (keys d->v)."
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

(defmacro case-double
  "Similar to (case ...) but optimized for a finite input set of doubles.
  double->ret must be a compile-time map from double value -> return expression.
  x must evaluate to a double among (keys double->ret)."
  [x double->ret]
  ;; Implementation: expands to a tree of bit tests on the raw bits of x;
  ;; the bits are chosen by eager entropy reduction (as in a Decision Tree),
  ;; assuming that all values in (keys double->ret) are equally probable.
  (let [raw-bits-sym (gensym 'raw-bits)]
    (letfn [(code-for [d->ret]
              (if (apply = (vals d->ret))
                ;; Leaf: all considered doubles have the same return expr, and so we emit that expr.
                (-> d->ret (first) (val))
                ;; We chose a most informative bit to test against, in the sense of Information Theory,
                ;; i.e. a bit that achieves the maximal entropy reduction.
                (let [most-informative-bit (->> (range 64)
                                                (apply max-key (fn [bit-idx]
                                                                 (total-information-gain (double-bit-tester bit-idx) d->ret))))]
                  `(if (bit-test ~raw-bits-sym ~most-informative-bit)
                     ~(code-for (select-keys d->ret (->> (keys d->ret)
                                                         (filter (double-bit-tester most-informative-bit)))))
                     ~(code-for (select-keys d->ret (->> (keys d->ret)
                                                         (remove (double-bit-tester most-informative-bit)))))))))]
      `(let [~raw-bits-sym (Double/doubleToRawLongBits ~x)]
         ~(code-for double->ret)))))

(comment

  (def double->v
    {0.0   false
     45.0  true
     90.0  false
     135.0 true
     180.0 false
     225.0 true
     270.0 false
     315.0 true})

  (macroexpand '(case-double x {0.0   false
                                45.0  true
                                90.0  false
                                135.0 true
                                180.0 false
                                225.0 true
                                270.0 false
                                315.0 true}))
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

  (macroexpand '(case-double x {0.0   0
                                45.0  1
                                90.0  2
                                135.0 3
                                180.0 4
                                225.0 5
                                270.0 6
                                315.0 7}))
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
