;; [[file:../../../org/GridFire.org::utils-random][utils-random]]
(ns gridfire.utils.random
  (:import java.util.Random))

(defn my-rand
  ([^Random rand-generator] (.nextDouble rand-generator))
  ([^Random rand-generator n] (* n (my-rand rand-generator))))

(defn my-rand-int
  [rand-generator n]
  (int (my-rand rand-generator n)))

(defn my-rand-nth
  [rand-generator coll]
  (nth coll (my-rand-int rand-generator (count coll))))

(defn random-float
  [min-val max-val rand-generator]
  (let [range (- max-val min-val)]
    (+ min-val (my-rand rand-generator range))))

(defn my-rand-int-range
  [rand-generator [min-val max-val]]
  (let [range (- max-val min-val)]
    (+ min-val (int (my-rand rand-generator range)))))

(defn sample-from-list
  [rand-generator n xs]
  (repeatedly n #(my-rand-nth rand-generator xs)))

(defn sample-from-range
  [rand-generator n [min max]]
  (let [range (- max min)]
    (repeatedly n #(+ min (my-rand-int rand-generator range)))))

(defn draw-samples
  [rand-generator n x]
  (into []
        (cond (list? x)   (sample-from-list rand-generator n x)
              (vector? x) (sample-from-range rand-generator n x)
              :else       (repeat n x))))
;; utils-random ends here
