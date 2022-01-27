(ns gridfire.binary-output-test
  (:require [tech.v3.tensor :as t]
            [clojure.test :refer [deftest is use-fixtures]]
            [gridfire.utils.test :as utils]
            [gridfire.binary-output :as binary]))

;;-----------------------------------------------------------------------------
;; Fixtures
;;-----------------------------------------------------------------------------

(use-fixtures :once utils/with-temp-output-dir)

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(def binary-file (utils/out-file-path "toa_test.bin"))

(deftest ^:unit write-matrix-as-binary-test
  (let [matrix [[0.0 1.0 2.0] [3.0 0.0 4.0] [5.0 6.0 0.0]]]
    (binary/write-matrix-as-binary matrix binary-file)
    (is (= (t/->tensor matrix) (binary/read-matrix-as-binary binary-file)))))

(deftest ^:unit write-matrices-as-binary-test
  (let [matrices [[[0.0 1.0 2.0]
                   [3.0 0.0 4.0]
                   [5.0 6.0 0.0]]

                  [[0.0 1.0 1.0]
                   [1.0 0.0 1.0]
                   [1.0 1.0 0.0]]

                  [[0.0 2.0 2.0]
                   [2.0 0.0 2.0]
                   [2.0 2.0 0.0]]

                  [[0 3 3]
                   [3 0 3]
                   [3 3 0]]]
        _        (binary/write-matrices-as-binary binary-file
                                                  [:float :float :float :int]
                                                  matrices)
        result   (binary/read-matrices-as-binary binary-file [:float :float :float :int])]
    (is (= (t/->tensor (first matrices)) (first result)))

    (is (= (t/->tensor (second matrices)) (second result)))

    (is (= (t/->tensor (nth matrices 2)) (nth result 2)))

    (is (= (t/->tensor (nth matrices 3)) (nth result 3)))))
