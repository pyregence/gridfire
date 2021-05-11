(ns gridfire.utils.primitive)

(defn double-reduce ^double
  [f ^double init coll]
  (loop [result   init
         [x & xs] coll]
    (if x
      (recur (f result x) xs)
      result)))

(defn long-reduce ^long
  [f ^long init coll]
  (loop [result   init
         [x & xs] coll]
    (if x
      (recur (f result x) xs)
      result)))
