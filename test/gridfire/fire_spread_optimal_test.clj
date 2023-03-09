;; [[file:../../org/GridFire.org::gridfire.fire-spread-optimal-test][gridfire.fire-spread-optimal-test]]
;; FIXME LP coverage
(ns gridfire.fire-spread-optimal-test
  (:require [clojure.test                 :refer [deftest are is testing]]
            [gridfire.fire-spread-optimal :refer [create-new-burn-vectors!-pfn
                                                  create-new-burn-vectors!-invoke
                                                  diagonal?
                                                  direction-angle->bit
                                                  direction-angle->i-incr
                                                  direction-angle->j-incr]]
            [gridfire.grid-lookup         :as grid-lookup]
            [tech.v3.datatype             :as d]
            [tech.v3.tensor               :as t]))

(defn- is-this-fn-equivalent-to-that-double-map?
  [f d->v]
  (->> d->v
       (every? (fn [[d v]]
                 (is (= v (f d)))))))

(deftest ^:unit case-double-examples-test
  (testing (pr-str `diagonal?)
    (is-this-fn-equivalent-to-that-double-map? diagonal?
                                               {0.0   false
                                                90.0  false
                                                180.0 false
                                                270.0 false
                                                45.0  true
                                                135.0 true
                                                225.0 true
                                                315.0 true}))
  (testing (pr-str `direction-angle->bit)
    (is-this-fn-equivalent-to-that-double-map? direction-angle->bit
                                               {0.0   0
                                                45.0  1
                                                90.0  2
                                                135.0 3
                                                180.0 4
                                                225.0 5
                                                270.0 6
                                                315.0 7}))
  (testing (pr-str `direction-angle->i-incr)
    (is-this-fn-equivalent-to-that-double-map? direction-angle->i-incr
                                               {0.0   -1
                                                45.0  -1
                                                315.0 -1
                                                135.0 1
                                                180.0 1
                                                225.0 1
                                                90.0  0
                                                270.0 0}))
  (testing (pr-str `direction-angle->j-incr)
    (is-this-fn-equivalent-to-that-double-map? direction-angle->j-incr
                                               {45.0  1
                                                90.0  1
                                                135.0 1
                                                0.0   0
                                                180.0 0
                                                225.0 -1
                                                270.0 -1
                                                315.0 -1})))

(defn- new-travel-lines-matrix
  [shape]
  (-> (t/new-tensor shape :datatype :int16)
      (grid-lookup/add-double-getter)))

(deftest ^:unit create-new-burn-vectors_test
  (let [num-rows                    10
        num-cols                    10
        shape                       [num-rows num-cols]
        cell-size                   98.425
        get-elevation               (grid-lookup/tensor-cell-getter 1.0 nil)
        burn-probability            1.0
        zero-tensor                 (grid-lookup/add-double-getter (t/new-tensor shape))
        fire-spread-matrix          (d/clone zero-tensor)
        max-spread-rate-matrix      (d/clone zero-tensor)
        max-spread-direction-matrix (d/clone zero-tensor)
        eccentricity-matrix         (d/clone zero-tensor)
        i                           5
        j                           5]
    (are [result travel-lines-matrix] (let [create-new-burn-vectors! (create-new-burn-vectors!-pfn num-rows num-cols cell-size get-elevation
                                                                                                   travel-lines-matrix fire-spread-matrix
                                                                                                   max-spread-rate-matrix max-spread-direction-matrix eccentricity-matrix)
                                            burn-vectors             (persistent! (create-new-burn-vectors!-invoke create-new-burn-vectors! (transient []) i j burn-probability))]
                                        (= result (count burn-vectors)))
      8 (new-travel-lines-matrix shape)                          ; No burn vectors in cell
      7 (t/mset! (new-travel-lines-matrix shape) i j 2r00000001) ; N burn vector exists
      7 (t/mset! (new-travel-lines-matrix shape) i j 2r00010000) ; S burn vector exists
      6 (t/mset! (new-travel-lines-matrix shape) i j 2r00010001) ; N & S burn vector exists
      7 (t/mset! (new-travel-lines-matrix shape) i j 2r00000010) ; NE burn vector exists
      7 (t/mset! (new-travel-lines-matrix shape) i j 2r00100000) ; SW burn vector exists
      6 (t/mset! (new-travel-lines-matrix shape) i j 2r00100010) ; NE & SW burn vector exists
      7 (t/mset! (new-travel-lines-matrix shape) i j 2r00000100) ; E burn vector exists
      7 (t/mset! (new-travel-lines-matrix shape) i j 2r01000000) ; W burn vector exists
      6 (t/mset! (new-travel-lines-matrix shape) i j 2r01000100) ; E & W burn vector exists
      7 (t/mset! (new-travel-lines-matrix shape) i j 2r00001000) ; SE burn vector exists
      7 (t/mset! (new-travel-lines-matrix shape) i j 2r10000000) ; NW burn vector exists
      6 (t/mset! (new-travel-lines-matrix shape) i j 2r10001000) ; SE & NW burn vector exists
      6 (t/mset! (new-travel-lines-matrix shape) i j 2r01000001) ; N & W burn vectors exists
      5 (t/mset! (new-travel-lines-matrix shape) i j 2r11000001) ; N & W & NW burn vectors exists
      4 (t/mset! (new-travel-lines-matrix shape) i j 2r11000011)))) ; N & W & NW & NE burn vectors exists
;; gridfire.fire-spread-optimal-test ends here
