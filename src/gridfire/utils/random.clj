;; [[file:../../../org/GridFire.org::utils-random][utils-random]]
(ns gridfire.utils.random
  (:import (java.util ArrayList Collection Collections Random)))

(defn my-rand
  (^double [^Random rand-generator] (.nextDouble rand-generator))
  (^double [^Random rand-generator n] (* n (.nextDouble rand-generator))))

(defn my-rand-int
  ^long
  [rand-generator n]
  (long (my-rand rand-generator n)))

(defn my-rand-nth
  [rand-generator coll]
  (nth coll (my-rand-int rand-generator (count coll))))

(defn my-rand-range
  ^double
  [rand-generator min-val max-val]
  (let [range (- max-val min-val)]
    (+ min-val (my-rand rand-generator range))))

(defn- sample-from-list
  [rand-generator n xs]
  (repeatedly n #(my-rand-nth rand-generator xs)))

(defn- sample-from-range
  [rand-generator n min-val max-val]
  (repeatedly n #(my-rand-range rand-generator min-val max-val)))

(defn draw-samples
  [rand-generator n x]
  (into []
        (cond (list? x)   (sample-from-list rand-generator n x)
              (vector? x) (sample-from-range rand-generator n (x 0) (x 1))
              :else       (repeat n x))))

(defn my-shuffle
  [^Random rand-gen ^Collection coll]
  (if (< (count coll) 2)
    (if (vector? coll)
      coll
      (vec coll))
    (let [al (ArrayList. coll)]
      (Collections/shuffle al rand-gen)
      (vec (.toArray al)))))
;; utils-random ends here
