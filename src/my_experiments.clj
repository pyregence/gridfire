(ns my-experiments
  (:require [gridfire.core]
            [taoensso.tufte :as tufte]))


(require 'clj-async-profiler.core)
#_(tufte/add-basic-println-handler! {})
(comment
  (clj-async-profiler.core/serve-files 8080)

  (->> ...
       (tufte/p ::write-outputs!))


  (tufte/profile {:id ::outputs-benchmark}
    (dotimes [_ 10]
      (gridfire.core/process-config-file! "resources/my-gridfire-benchmark-FIXME.edn")))

  ;id: :my-experiments/outputs-benchmark
  ;pId                               nCalls        Min      50% ≤      90% ≤      95% ≤      99% ≤        Max       Mean   MAD      Clock  Total
  ;
  ;:gridfire.core/write-outputs!         10     2.52s      2.63s      2.83s      2.88s      2.88s      2.88s      2.64s    ±3%    26.39s     33%
  ;
  ;Accounted                                                                                                                      26.39s     33%
  ;Clock                                                                                                                           1.35m    100%

  ;id: :my-experiments/outputs-benchmark
  ;pId                               nCalls        Min      50% ≤      90% ≤      95% ≤      99% ≤        Max       Mean   MAD      Clock  Total
  ;
  ;:gridfire.core/write-outputs!         10     1.32s      1.37s      1.47s      1.68s      1.68s      1.68s      1.40s    ±5%    14.01s     20%
  ;
  ;Accounted                                                                                                                      14.01s     20%
  ;Clock                                                                                                                           1.16m    100%
  *e)
