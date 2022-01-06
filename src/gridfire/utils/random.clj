;; [[file:../../../org/GridFire.org::utils-random][utils-random]]
(ns gridfire.utils.random
  (:import (java.util ArrayList Collection Collections Random)))

(defn my-rand
  ([^Random rand-generator] (.nextDouble rand-generator))
  (^double [^Random rand-generator n] (* n (my-rand rand-generator))))

(defn my-rand-int
  ^long
  [rand-generator n]
  (int (my-rand rand-generator n)))

(defn my-rand-nth
  [rand-generator coll]
  (nth coll (my-rand-int rand-generator (count coll))))

;; FIXME: This function is redundant with my-rand-range and can be removed.
(defn random-float
  [min-val max-val rand-generator]
  (let [range (- max-val min-val)]
    (+ min-val (my-rand rand-generator range))))

(defn my-rand-range
  [rand-generator [min-val max-val]]
  (let [range (- max-val min-val)]
    (+ min-val (if (int? range)
                 (my-rand-int rand-generator range)
                 (my-rand rand-generator range)))))

(defn sample-from-list
  [rand-generator n xs]
  (repeatedly n #(my-rand-nth rand-generator xs)))

(defn sample-from-range
  [rand-generator n [min-val max-val]]
  (repeatedly n #(my-rand-range rand-generator [min-val max-val])))

(defn draw-sample
  [rand-generator x]
  (cond (list? x)   (my-rand-nth rand-generator x)
        (vector? x) (my-rand-range rand-generator x)
        :else       x))

(defn draw-samples
  [rand-generator n x]
  (into []
        (cond (list? x)   (sample-from-list rand-generator n x)
              (vector? x) (sample-from-range rand-generator n x)
              :else       (repeat n x))))

(defn my-shuffle
  [^Random seed ^Collection coll]
  (if (< (count coll) 2)
    (if (vector? coll)
      coll
      (vec coll))
    (let [al (ArrayList. coll)]
      (Collections/shuffle al seed)
      (vec (.toArray al)))))
;; utils-random ends here
