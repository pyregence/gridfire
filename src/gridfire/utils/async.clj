(ns gridfire.utils.async
  (:require [manifold.deferred :as mfd]
            [manifold.executor])
  (:import java.util.concurrent.ExecutorService))

;; ------------------------------------------------------------------------------
;; Helper fns

(defn nil-when-completed
  "Returns a Manifold Deferred that will complete when d completes,
  but be resolved with nil instead of the result of d."
  [d]
  (mfd/chain d (constantly nil)))

(defn nil-when-all-completed
  "Waits for all the given DeferredS to complete, yielding nil.
  Returns immediately a Deferred, resolved with nil
  when all the Deferred in ds have completed successfully.

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
        exctr   (manifold.executor/fixed-thread-executor
                  n-threads
                  (merge
                    {:thread-factory (manifold.executor/thread-factory
                                       (let [next-thread-id* (atom -1)]
                                         (fn gen-thread-name []
                                           ;; Naming the threads makes them easy to observe in monitoring
                                           ;; tools such as VisualVM and jstat.
                                           (str thread-name-prefix "-" (swap! next-thread-id* inc))))
                                       p-exctr)}
                    exctr-options))]
    (deliver p-exctr exctr)
    exctr))
