(ns gridfire.utils.vsampling
  "Values-Sampling: a utility similar to logging or tracing, useful for recording
  (subsets of) the values which occur in our formulas.")

;; You can activate this e.g. with clj -J-Dgridfire.utils.vsampling.enabled="true",
;; or by using the :vsampling CLI alias.
(defn is-enabled?
  []
  (= "true" (System/getProperty "gridfire.utils.vsampling.enabled")))

(def ^:private ^:dynamic *current-point-id* nil)

(def ^:dynamic db* (atom {}))

(defn recorded-values
  "Returns the sampled values as a map {point-id (m1 m2 ...)},
  in which each m is a map of the values recorded at one (record ...) call
  (the keys are Strings), most recent first.

  Each map m also has a :gridfire.utils.vsampling/source map
  in its metadata, providing a code location."
  []
  @db*)

(defn clear-recorded!
  []
  (reset! db* {}))

(defn record*
  [point-id recorded-map]
  (swap! db*
         ;; Those awkwardly nested (fn [...] ...) are for performance. (Val, 12 Oct 2022)
         (fn [db] (update db point-id (fn [rec-maps] (-> rec-maps (or ()) (conj recorded-map)))))))

(defmacro at-point
  "When point-id is not nil, values sampled during the execution of body
   will be recorded as belonging to point-id."
  [point-id & body]
  (if (is-enabled?)
    `(binding [*current-point-id* ~point-id]
       ~@body)
    `(do ~@body)))

(defn keep-hash-bucket
  "Convenience function for selecting m out of N point-ids based on its hash."
  [point-id ^long m ^long N]
  (when (-> (hash point-id) (mod N) (< m))
    point-id))

(defmacro record
  "Samples a consistent set of named values.
  vs must be either a vector of symbols,
  which will be evaluated as a map from symbol name to symbol value,
  or an expression which will evaluate to a map from string to recorded value.

  Does nothing unless *current-point-id* is bound to a non-nil value."
  [vs]
  (when (is-enabled?)
    `(when-some [point-id# *current-point-id*]
       (record* point-id#
                (-> ~(if (vector? vs)
                       (into {}
                             (map (fn [vsym]
                                    [(name vsym) vsym]))
                             vs)
                       vs)
                    (vary-meta merge {::source ~(let [expr-meta (meta &form)
                                                      file-path *file*]
                                                  (merge
                                                   (select-keys expr-meta [:line :column])
                                                   {:file file-path}))}))))))

(comment

  (def is-enabled? (constantly true))                       ; overriding

  (def rec-vals
    (binding [db* (atom {})]                                ; using a throwaway db
      (at-point "my-point-id"
        (let [a 1
              b 2
              c (+ a b)]
          (record [a b c])
          (record {"a - b" (- a b)})
          (recorded-values)))))

  rec-vals
  ;; =>
  {"my-point-id" ({"a - b" -1} {"a" 1, "b" 2, "c" 3})}

  (-> rec-vals
      (get "my-point-id")
      (->> (mapv meta)))
  [#:gridfire.utils.vsampling{:source {:line   8,
                                       :column 11,
                                       :file   "/Users/val/projects/SIG/gridfire/src/gridfire/utils/vsampling.clj"}}
   #:gridfire.utils.vsampling{:source {:line   7,
                                       :column 11,
                                       :file   "/Users/val/projects/SIG/gridfire/src/gridfire/utils/vsampling.clj"}}]

  *e)
