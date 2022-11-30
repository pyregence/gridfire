(ns gridfire.grid-lookup
  "Utils for efficiently resolving values at [b i j] coordinates in a grid.

  Coordinates [b i j] correspond to:
  - b: 'hourly Band', a 1h-resolution temporal-grid coordinate;
  - i: matrix row, a spatial-grid coordinate, discretizing the y axis (yes, you read that right);
  - j: matrix column, a spatial-grid coordinate, discretizing the x axis."
  (:require [gridfire.structs.tensor-meta :as tensor-meta]
            [tech.v3.datatype             :as d]
            [tech.v3.tensor               :as t])
  (:import (clojure.lang IFn$LD IFn$LLD IFn$LLLD IFn$OD IFn$OOD IFn$OOOD)))
;; NOTE clj-kondo (2022.11.02) currently reports false-positive linting issues, not sure what to do about it: (Val, 28 Nov 2022)
;; $ clj-kondo --lint src/gridfire/grid_lookup.clj
;; src/gridfire/grid_lookup.clj:8:14: warning: namespace gridfire.structs.tensor-meta is required but never used
;; src/gridfire/grid_lookup.clj:9:14: warning: namespace tech.v3.datatype is required but never used
;; src/gridfire/grid_lookup.clj:10:14: warning: namespace tech.v3.tensor is required but never used
;; src/gridfire/grid_lookup.clj:175:12: warning: Unresolved namespace d. Are you missing a require?
;; src/gridfire/grid_lookup.clj:176:6: warning: Unresolved namespace t. Are you missing a require?
;; src/gridfire/grid_lookup.clj:202:12: error: Unresolved symbol: suitable-for-primitive-lookup?
;; src/gridfire/grid_lookup.clj:254:27: warning: Unresolved namespace tensor-meta. Are you missing a require?
;; src/gridfire/grid_lookup.clj:272:5: error: Unresolved symbol: double-at
;; linting took 51ms, errors: 2, warnings: 6

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
  (let [fsym (-> (gensym 'f) (vary-meta assoc :tag `IFn$OOD))]
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
  (let [fsym (-> (gensym 'f) (vary-meta assoc :tag `IFn$OOOD))]
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
  ;; NOTE turning this function into a macro only made simulations barely faster (455ms instead of 465ms) - not worth it IMHO. (Val, 25 Nov 2022)
  "Looks up a double-typed value at the given grid coordinates,
  avoiding boxing of the returned value."
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

(defn ensure-flat-jvm-tensor
  "Given a tensor, returns a tensor which is backed by a flat JVM array, copying if necessary."
  [m]
  (cond-> m
    (nil? (d/as-array-buffer-data m))
    (t/clone :container-type :jvm-heap)))

(defn- tensor-wrapped-array
  [unwrap-fn m]
  (let [arr (unwrap-fn m)]
    (assert (identical? arr (unwrap-fn m)))
    arr))

(defn indexes-out-of-bound-ex
  [max-b max-i max-j b i j]
  (ex-info (format "ND-array indexes [b i j] out of bounds (must be <= %s and > [0 0 0]): %s"
                   (pr-str [max-b max-i max-j])
                   (pr-str [b i j]))
           {::b+i+j [b i j]
            ::max-b+i+j [max-b max-i max-j]}))

(defmacro aget-3dim
  [arr n-per-row n-per-band default-v? default-v max-b max-i max-j b i j]
  ;; FIXME return default-v when b,i or j out of bounds.
  `(let [out-of-bounds?# (or (< ~b 0) (> ~b ~max-b)
                             (< ~i 0) (> ~i ~max-i)
                             (< ~j 0) (> ~j ~max-j))]
     (if out-of-bounds?#
       (if ~default-v?
         ~default-v
         (throw (indexes-out-of-bound-ex ~max-b ~max-i ~max-j ~b ~i ~j)))
       (aget ~arr (-> ~j
                      (unchecked-add (unchecked-multiply ~n-per-row ~i))
                      ;; NOTE simplifying when b is known to be zero at compile-time. (Val, 24 Nov 2022)
                      ~@(when-not (= 0 b) [`(unchecked-add (unchecked-multiply ~n-per-band ~b))])
                      (int))))))

(defn- unwrap-short-tensor
  ^shorts [m]
  ;; HACK relying on an implementation detail of d/. Because d/->short-array will tend to cause copying for some reason.
  (.ary_data (d/as-array-buffer m)))

(defn tensor-cell-getter
  "Given a tensor `m`, returns a value suitable to be called by `double-at`.

  `m` must have gone through `ensure-flat-jvm-tensor`.
  Optionally, a non-nil `default-value` number may be supplied,
  which will be returned when indexes fall out of bounds."
  [m default-value]
  {:post [(suitable-for-primitive-lookup? %)]}
  (if (number? m)
    (let [v (double m)]
      (fn get0d
        (^double [^long _b] v)
        (^double [^long _i ^long _j] v)
        (^double [^long _b ^long _i ^long _j] v)))
    (let [shape       (d/shape m)
          n-dims      (count shape)
          [n-per-band
           n-per-row
           ubounds]   (case n-dims
                         2 (let [[n-rows n-cols] shape]
                             ;; NOTE we are treating the 2-dims tensors as a special case of the 3-dims tensors.
                             ;; This is because we want to return always the same type (JVM class) of function,
                             ;; to limit the creation of polymorphic call sites.
                             [0
                              n-cols
                              [1 n-rows n-cols]])
                         3 (let [[n-bands n-rows n-cols] shape]
                             [(* (long n-rows) (long n-cols))
                              n-cols
                              [n-bands n-rows n-cols]]))
          n-per-band  (long n-per-band)
          n-per-row   (long n-per-row)
          default-v?  (some? default-value)
          default-v   (if default-v? (double default-value) Double/NaN)
          [mb mi mj]  ubounds
          max-b       (dec (long mb))
          max-i       (dec (long mi))
          max-j       (dec (long mj))]
      (case (d/elemwise-datatype m)
        (:float64 :double)
        (let [arr (doubles (tensor-wrapped-array d/->double-array m))]
          (fn mget-doubles
            (^double [^long i ^long j] (aget-3dim arr n-per-row n-per-band default-v? default-v max-b max-i max-j 0 i j))
            (^double [^long b ^long i ^long j] (aget-3dim arr n-per-row n-per-band default-v? default-v max-b max-i max-j b i j))))
        :float32
        (let [arr (floats (tensor-wrapped-array d/->float-array m))]
          (fn mget-floats
            (^double [^long i ^long j] (double (aget-3dim arr n-per-row n-per-band default-v? default-v max-b max-i max-j 0 i j)))
            (^double [^long b ^long i ^long j] (double (aget-3dim arr n-per-row n-per-band default-v? default-v max-b max-i max-j b i j)))))
        :int32
        (let [arr (ints (tensor-wrapped-array d/->int-array m))]
          (fn mget-ints
            (^double [^long i ^long j] (double (aget-3dim arr n-per-row n-per-band default-v? default-v max-b max-i max-j 0 i j)))
            (^double [^long b ^long i ^long j] (double (aget-3dim arr n-per-row n-per-band default-v? default-v max-b max-i max-j b i j)))))
        :int16
        (let [arr (shorts (tensor-wrapped-array d/->short-array m))]
          (fn mget-shorts-int16
            (^double [^long i ^long j] (double (aget-3dim arr n-per-row n-per-band default-v? default-v max-b max-i max-j 0 i j)))
            (^double [^long b ^long i ^long j] (double (aget-3dim arr n-per-row n-per-band default-v? default-v max-b max-i max-j b i j)))))

        :short
        (let [arr (shorts (tensor-wrapped-array unwrap-short-tensor m))]
          (fn mget-shorts-short
            (^double [^long i ^long j] (double (aget-3dim arr n-per-row n-per-band default-v? default-v max-b max-i max-j 0 i j)))
            (^double [^long b ^long i ^long j] (double (aget-3dim arr n-per-row n-per-band default-v? default-v max-b max-i max-j b i j)))))))))

(defn add-double-getter
  "Attaches to tensor m (in its metadata) a cached return value of (tensor-cell-getter),
  so that we don't need to recompute-it each time."
  ([m] (add-double-getter m nil))
  ([m default-value]
   (vary-meta m (fn [met] (tensor-meta/map->GettersMeta (assoc met :grid-lookup-double-getter (tensor-cell-getter m default-value)))))))

(defn mgetter-double
  "Given a tensor which has been accelerated through (add-double-getter),
  return a getter object g which can be looked up via (double-at g ...)."
  [m]
  (-> m
      (meta)
      (tensor-meta/double-getter)
      (or (throw (ex-info (format "This tensor has not been accelerated by going through %s." (pr-str `add-double-getter))
                          {::tensor m})))))

(defn mget-double-at
  "Like tech.v3.tensor/mget, but faster.
  The tensor m must have been accelerated by going through #'add-double-getter.
  (mget-double-at m i j) is equivalent to (let [my-getter (mgetter-double m)] (double-at my-getter i j));
  the latter form improve the performance of repeated invocations."
  (^double [m ^long b]
   (double-at (mgetter-double m) b))
  (^double [m ^long i ^long j]
   (double-at (mgetter-double m) i j))
  (^double [m ^long b ^long i ^long j]
   (double-at (mgetter-double m) b i j)))
