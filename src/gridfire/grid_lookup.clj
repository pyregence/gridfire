(ns gridfire.grid-lookup
  "Utils for efficiently resolving values at [b i j] coordinates in a grid.

  Coordinates [b i j] correspond to:
  - b: 'hourly Band', a 1h-resolution temporal-grid coordinate;
  - i: matrix row, a spatial-grid coordinate, discretizing the y axis (yes, you read that right);
  - j: matrix column, a spatial-grid coordinate, discretizing the x axis;"
  (:import (clojure.lang IFn$LD IFn$LLD IFn$LLLD)))

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

(definline double-at-b*
  "See double-at-bij*."
  [f b]
  (let [fsym (-> (gensym 'f) (vary-meta assoc :tag `IFn$LD))]
    `(let [~fsym ~f]
       (.invokePrim ~fsym ~b))))

(definline double-at-ij*
  "See double-at-bij*."
  [f i j]
  (let [fsym (-> (gensym 'f) (vary-meta assoc :tag `IFn$LLD))]
    `(let [~fsym ~f]
       (.invokePrim ~fsym ~i ~j))))

(definline double-at-bij*
  "Like (double-at ...), but will emit directly-inlined code, which can improve efficiency."
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
          (double-at-ij* f 2 3)))))
  ;public final class grid_lookup$fn__44795$my_computation__44798 extends AFunction implements D
  ;{
  ;    Object f;
  ;
  ;    public grid_lookup$fn__44795$my_computation__44798(final Object f) {
  ;        this.f = f;
  ;    }
  ;
  ;    @Override
  ;    public final double invokePrim() {
  ;        final double n = 3.0;
  ;        final Object f44799 = this.f;
  ;        return n + ((LLD)f44799).invokePrim(2L, 3L);
  ;    }
  ;
  ;    @Override
  ;    public Object invoke() {
  ;        return ((IFn.D)this).invokePrim();
  ;    }
  ;}

  ;; In the above code, we see not only a primitive method invocation (no boxing),
  ;; but we also avoid the Var-lookup overhead, as our (double-at-ij* ...) call
  ;; is directly inlined.

  *e)

(defn double-at
  "Looks up a double-typed value at the given grid coordinates, avoiding boxing of primitives."
  (^double [getter ^long b]
   (double-at-b* getter b))
  (^double [getter ^long i ^long j]
   (double-at-ij* getter i j))
  (^double [getter ^long b ^long i ^long j]
   (double-at-bij* getter b i j)))

(comment
  ;; (double-at ...) does allow lookup without primitive boxing:
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
          (double-at f 2 3)))))
  ;public final class grid_lookup$fn__44754$my_computation__44757 extends AFunction implements D
  ;{
  ;    // ...
  ;
  ;    @Override
  ;    public final double invokePrim() {
  ;        return 3.0 + ((OLLD)grid_lookup$fn__44754$my_computation__44757.const__2.getRawRoot()).invokePrim(this.f, 2L, 3L);
  ;    }
  ;
  ;    // ...
  ;}

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
