;; FIXME LP coverage
(ns gridfire.utils.async
  (:require [clojure.core.async :as async]
            [manifold.deferred  :as mfd]
            [manifold.executor  :as mexec])
  (:import java.util.concurrent.ExecutorService))

;; ------------------------------------------------------------------------------
;; Helper fns

(defn nil-when-completed
  "Returns a Manifold Deferred that will complete when d completes,
  but be resolved with nil instead of the result of d."
  [d]
  (mfd/chain d (constantly nil)))

(defn nil-when-all-completed
  "Waits for all the given Deferreds to complete, yielding nil.
  Returns immediately a Deferred, resolved with nil
  when all the Deferreds in ds have completed successfully.

  Useful for awaiting effects scheduled in parallel."
  [ds]
  (->> ds
       (apply mfd/zip)
       (nil-when-completed)))

;; ------------------------------------------------------------------------------
;; Executors

(defn executor-with-n-named-threads
  "Creates a fixed-sized thread pool for executing task.
  The Java threads will be named after the supplied prefix string."
  ^ExecutorService
  [thread-name-prefix n-threads exctr-options]
  (let [p-exctr (promise)
        exctr   (mexec/fixed-thread-executor
                  n-threads
                  (merge
                    {:thread-factory (mexec/thread-factory
                                       (let [*next-thread-id (atom -1)]
                                         (fn gen-thread-name []
                                           ;; Naming the threads makes them easy to observe in monitoring
                                           ;; tools such as VisualVM and jstat.
                                           (str thread-name-prefix "-" (swap! *next-thread-id inc))))
                                       p-exctr)}
                    exctr-options))]
    (deliver p-exctr exctr)
    exctr))

;; ------------------------------------------------------------------------------
;; Parallel execution

(defn pmap-in-n-threads
  "Parallel computations of `f` over a finite collection `coll`, with parallelism `n`.

  Returns a Manifold Deferred which will be realized with a value equal to (mapv f coll)."
  [n f coll]
  ;; NOTE it might seem excessive to use both core.async and Manifold here,
  ;; but Manifold has a massive advantage over core.async, which is
  ;; that it naturally lets the caller program in a functional style without side effects,
  ;; which is much more in harmony with the semantics of this function.
  ;; core.async really is nothing more than an implementation detail here,
  ;; and a more polished implementation would probably want to replace it
  ;; to have more control over the thread pool.
  (if (empty? coll)
    (mfd/success-deferred [])
    (let [=sink=  (async/chan (async/dropping-buffer 0))
          =tasks= (async/chan n)
          x+ds    (->> coll (mapv (fn [x] [x (mfd/deferred)])))]
      (async/pipeline-blocking (-> n
                                   (min (count x+ds)))
                               =sink=
                               (map (fn [[x d]]
                                      (try
                                        (mfd/success! d
                                                      (f x))
                                        (catch Exception err
                                          (mfd/error! d err)))
                                      d))
                               =tasks=)
      (async/onto-chan! =tasks= x+ds)
      (->> x+ds
           (map (fn [[_x d]] d))
           (apply mfd/zip)))))
