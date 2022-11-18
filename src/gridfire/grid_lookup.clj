(ns gridfire.grid-lookup
  "Utils for efficiently resolving values at [b i j] coordinates in a grid.

  Coordinates [b i j] correspond to:
  - b: 'hourly Band', a 1h-resolution temporal-grid coordinate;
  - i: matrix row, a spatial-grid coordinate, discretizing the y axis (yes, you read that right);
  - j: matrix column, a spatial-grid coordinate, discretizing the x axis;"
  (:require [tech.v3.datatype :as d])
  (:import (clojure.lang IFn$LD IFn$LLD IFn$LLLD IFn$OD IFn$LLD IFn$LLLD)))

(defn suitable-for-primitive-lookup?
  "Whether the given value can be used as the 1st argument to (double-at v & coords)"
  [v]
  ;; HACK relying on clojure.lang primitive-signature interfaces;
  ;; that's convenient, but strictly speaking these are implementation details
  ;; of Clojure execution.
  (or
   (instance? IFn$LD v)
   (instance? IFn$LLD v)
   (instance? IFn$LLLD v)))

(definline double-at-B*
  "See double-at-BIJ*."
  [f b]
  (let [fsym (-> (gensym 'f) (vary-meta assoc :tag `IFn$OD))]
    `(let [~fsym ~f]
       (.invokePrim ~fsym ~b))))

(definline double-at-b*
  "Like (double-at-b* ...), but with primitive-typed coordinates."
  [f b]
  (let [fsym (-> (gensym 'f) (vary-meta assoc :tag `IFn$LD))]
    `(let [~fsym ~f]
       (.invokePrim ~fsym ~b))))

(definline double-at-IJ*
  "See double-at-BIJ*."
  [f i j]
  (let [fsym (-> (gensym 'f) (vary-meta assoc :tag `IFn$LLD))]
    `(let [~fsym ~f]
       (.invokePrim ~fsym ~i ~j))))

(definline double-at-ij*
  "Like (double-at-IJ* ...), but with primitive-typed coordinates."
  [f i j]
  (let [fsym (-> (gensym 'f) (vary-meta assoc :tag `IFn$LLD))]
    `(let [~fsym ~f]
       (.invokePrim ~fsym ~i ~j))))

(definline double-at-BIJ*
  "Like (double-at ...), but will emit directly-inlined code, which can improve efficiency."
  [f b i j]
  (let [fsym (-> (gensym 'f) (vary-meta assoc :tag `IFn$LLLD))]
    `(let [~fsym ~f]
       (.invokePrim ~fsym ~b ~i ~j))))

(definline double-at-bij*
  "Like (double-at-BIJ* ...), but with primitive-typed coordinates."
  [f b i j]
  (let [fsym (-> (gensym 'f) (vary-meta assoc :tag `IFn$LLLD))]
    `(let [~fsym ~f]
       (.invokePrim ~fsym ~b ~i ~j))))

;; NOTE (definline ...) does not support multiple arities, hence the 3 definitions above.

(comment
 ;; (double-at-* ...) does allow lookup without primitive boxing:
 (require '[clj-java-decompiler.core])
 (clj-java-decompiler.core/decompile
  (let [f
        ;; Let's make sure the Clojure compiler won't guess the signature of f:
        (get {2
              (fn my-getter ^double [^long i ^long j]
                (* 2. (+ i j)))}
             (+ 1 1))]
    (fn my-computation ^double []
      (+ 3.
         (double-at-IJ* f 2 3)))))
 ;;public final class grid_lookup$fn__44795$my_computation__44798 extends AFunction implements D
 ;;{
 ;;    Object f;
 ;;
 ;;    public grid_lookup$fn__44795$my_computation__44798(final Object f) {
 ;;        this.f = f;
 ;;    }
 ;;
 ;;    @Override
 ;;    public final double invokePrim() {
 ;;        final double n = 3.0;
 ;;        final Object f44799 = this.f;
 ;;        return n + ((LLD)f44799).invokePrim(2L, 3L);
 ;;    }
 ;;
 ;;    @Override
 ;;    public Object invoke() {
 ;;        return ((IFn.D)this).invokePrim();
 ;;    }
 ;;}

 ;; In the above code, we see not only a primitive method invocation (no boxing),
 ;; but we also avoid the Var-lookup overhead, as our (double-at-ij* ...) call
 ;; is directly inlined.

 *e)

(defn double-at
  "Looks up a double-typed value at the given grid coordinates,
  avoiding boxing of the returned value."
  ;; You might wonder why we're not accepting ^long arguments;
  ;; that's because, in our typical usage, we'll have to pass boxed Long
  ;; coordinates to (t/mget ...) anyway. Un-boxing to primitive long,
  ;; then re-boxing to new Long objects, would actually create much more work
  ;; than passing the same Long objects all along.
  (^double [getter ^long b]
   (double-at-b* getter b))
  (^double [getter ^long i ^long j]
   (double-at-ij* getter i j))
  (^double [getter ^long b ^long i ^long j]
   (double-at-bij* getter b i j)))

(comment
 ;; (double-at ...) does allow lookup without primitive boxing:
 (clj-java-decompiler.core/decompile
  (let [f
        ;; Let's make sure the Clojure compiler won't guess the signature of f:
        (get {2
              (fn my-getter ^double [^long i ^long j]
                (* 2. (+ i j)))}
             (+ 1 1))]
    (fn my-computation ^double []
      (+ 3.
         (double-at f 2 3)))))
 ;;public final class grid_lookup$fn__44754$my_computation__44757 extends AFunction implements D
 ;;{
 ;;    // ...
 ;;
 ;;    @Override
 ;;    public final double invokePrim() {
 ;;        return 3.0 + ((OLLD)grid_lookup$fn__44754$my_computation__44757.const__2.getRawRoot()).invokePrim(this.f, 2L, 3L);
 ;;    }
 ;;
 ;;    // ...
 ;;}

 ;; In the above Java code, we do see that double-at is called without boxing.
 ;; We still see the overhead of Var-lookup (.getRawRoot()),
 ;; but that's not necessarily very significant (thanks to CPU caching),
 ;; and could eliminated by Clojure direct linking (https://clojure.org/reference/compilation#directlinking).

 *e)

(comment
 ;; NOTE Why not do this more cleanly with a Clojure protocol? As of Clojure 1.10,
 ;; (defprotocol ...) does not compile to a Java interface
 ;; with primitive signatures, and therefore cannot be used to avoid boxing:
 (in-ns 'user)
 (defprotocol IMyProtocol
   (get-me-a-double ^double [this ^long i ^long j]))
 (require 'clojure.reflect)
 (clojure.reflect/reflect user.IMyProtocol)
 ;=>
 {:bases   nil,
  :flags   #{:interface :public :abstract},
  :members #{#clojure.reflect.Method{:name            get_me_a_double,
                                     :return-type     java.lang.Object,
                                     :declaring-class user.IMyProtocol,
                                     :parameter-types [java.lang.Object java.lang.Object],
                                     :exception-types [],
                                     :flags           #{:public :abstract}}}}
 ;; Therefore, unfortunately, we have to use some other mechanism.
 *e)

(defn- unwrap-short-tensor
  ^shorts [m]
  ;; HACK relying on an implementation detail of d/. Because d/->short-array will tend to copy for some reason.
  ;; FIXME add the test case showing that this is correct.
  (.ary_data (d/as-array-buffer m)))

(comment

  *e)

(defn- tensor-wrapped-array
  [unwrap-fn m]
  (def unwrap-fn unwrap-short-tensor)
  (let [arr (unwrap-fn m)]
    (assert (identical? arr (unwrap-fn m)))
    arr))

(defn tensor-cell-getter
  "Returns a function roughly similar to (partial t/mget m),
  but is more tolerant of both m (may be a number)
  and the subsequently passed indices (the band index will be ignored
  if m is 2D.)

  The returned function can be called using grid-lookup/double-at."
  [m]
  {:post [(suitable-for-primitive-lookup? %)]}
  (if (number? m)
    (let [v (double m)]
      (fn get0d
        (^double [^long _b] v)
        (^double [^long _i ^long _j] v)
        (^double [^long _b ^long _i ^long _j] v)))
    (or (-> m (meta) ::double-getter)
        (let [shape                        (d/shape m)
              ;; FIXME other datatypes, like :float32 or :int32
              ^clojure.lang.IFn$LD aget-fn (case (d/elemwise-datatype m)
                                             (:double :float64)
                                             (let [arr (doubles (tensor-wrapped-array d/->double-array m))]
                                               (fn aget-double ^double [^long k]
                                                 (aget arr k)))
                                             :float32 (let [arr (floats (tensor-wrapped-array d/->float-array m))]
                                                        (fn aget-float ^double [^long k]
                                                          (double (aget arr k))))
                                             :int32 (let [arr (ints (tensor-wrapped-array d/->int-array m))]
                                                      (fn aget-int ^double [^long k]
                                                        (double (aget arr k))))
                                             :short (let [arr (shorts (tensor-wrapped-array unwrap-short-tensor #_d/->short-array m))]
                                                      (fn aget-short ^double [^long k]
                                                        (double (aget arr k)))))]
          (case (count shape)
            2 (let [[_nrows ncols] (d/shape m)
                    ncols (int ncols)]
                (fn get2d
                  (^double [^long i ^long j]
                   (.invokePrim aget-fn
                                (unchecked-add-int (unchecked-multiply-int ncols (int i))
                                                   (int j))))
                  ;; This case is important, because some input tensors (Val, 03 Nov 2022)
                  ;; like moisture will be provided sometimes in 2d, sometimes in 3d.
                  (^double [^long _b ^long i ^long j] (get2d i j))))
            3 (let [[_nbands nrows ncols] (d/shape m)
                    n-per-row  (int ncols)
                    n-per-band (int (* (long nrows) (long ncols)))]
                (fn get3d
                  (^double [^long i ^long j] (get3d 0 i j))
                  (^double [^long b ^long i ^long j]
                   (.invokePrim aget-fn
                                ;; FIXME compare with/wo long->int casting.
                                (-> (int j)
                                    (unchecked-add-int (unchecked-multiply-int n-per-row (int i)))
                                    (unchecked-add-int (unchecked-multiply-int n-per-band (int b)))))))))))))

(defn add-double-getter
  "Attaches to tensor m (in its metadata) a cached return value of (tensor-cell-getter),
  so that we don't need to recompute-it each time."
  [m]
  (vary-meta m assoc ::double-getter (tensor-cell-getter m)))
