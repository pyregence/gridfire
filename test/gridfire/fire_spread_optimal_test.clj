(ns gridfire.fire-spread-optimal-test
  (:require [clojure.test                 :refer [deftest are]]
            [gridfire.fire-spread-optimal]
            [tech.v3.datatype             :as d]
            [tech.v3.tensor               :as t]))

(deftest ^:unit create-new-burn-vectors_test
  (let [num-rows                    10
        num-cols                    10
        shape                       [num-rows num-cols]
        cell-size                   98.425
        get-elevation               (fn [_ _] 1.0)
        get-fuel-model              (fn [_ _] 101)
        burn-probability            1.0
        zero-tensor                 (t/new-tensor shape)
        fire-spread-matrix          (d/clone zero-tensor)
        max-spread-rate-matrix      (d/clone zero-tensor)
        max-spread-direction-matrix (d/clone zero-tensor)
        eccentricity-matrix         (d/clone zero-tensor)
        i                           5
        j                           5]
    (are [result travel-lines-matrix] (let [burn-vectors (#'gridfire.fire-spread-optimal/create-new-burn-vectors!
                                                          (transient [])
                                                          num-rows num-cols cell-size get-elevation travel-lines-matrix
                                                          max-spread-rate-matrix max-spread-direction-matrix eccentricity-matrix
                                                          fire-spread-matrix i j burn-probability)]
                                        (= result (count burn-vectors)))
      8 (t/new-tensor shape :datatype :short)                          ; No burn vectors in cell
      7 (t/mset! (t/new-tensor shape :datatype :short) i j 2r00000001) ; N burn vector exists
      7 (t/mset! (t/new-tensor shape :datatype :short) i j 2r00010000) ; S burn vector exists
      6 (t/mset! (t/new-tensor shape :datatype :short) i j 2r00010001) ; N & S burn vector exists
      7 (t/mset! (t/new-tensor shape :datatype :short) i j 2r00000010) ; NE burn vector exists
      7 (t/mset! (t/new-tensor shape :datatype :short) i j 2r00100000) ; SW burn vector exists
      6 (t/mset! (t/new-tensor shape :datatype :short) i j 2r00100010) ; NE & SW burn vector exists
      7 (t/mset! (t/new-tensor shape :datatype :short) i j 2r00000100) ; E burn vector exists
      7 (t/mset! (t/new-tensor shape :datatype :short) i j 2r01000000) ; W burn vector exists
      6 (t/mset! (t/new-tensor shape :datatype :short) i j 2r01000100) ; E & W burn vector exists
      7 (t/mset! (t/new-tensor shape :datatype :short) i j 2r00001000) ; SE burn vector exists
      7 (t/mset! (t/new-tensor shape :datatype :short) i j 2r10000000) ; NW burn vector exists
      6 (t/mset! (t/new-tensor shape :datatype :short) i j 2r10001000) ; SE & NW burn vector exists
      6 (t/mset! (t/new-tensor shape :datatype :short) i j 2r01000001) ; N & W burn vectors exists
      5 (t/mset! (t/new-tensor shape :datatype :short) i j 2r11000001) ; N & W & NW burn vectors exists
      4 (t/mset! (t/new-tensor shape :datatype :short) i j 2r11000011)))) ; N & W & NW & NE burn vectors exists
