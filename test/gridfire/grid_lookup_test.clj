(ns gridfire.grid-lookup-test
  (:require [gridfire.grid-lookup :as grid-lookup]
            [clojure.test         :refer [deftest is testing]])
  (:import (clojure.lang IFn$OLD IFn$OLLD IFn$OLLLD IFn$LD IFn$LLD IFn$LLLD)))

(deftest ^:unit double-at-primitiveness-test
  (testing "(double-at getter & coords)"
    (testing "accepts primitive long-typed coordinates, and returns a primitive double."
      (is (instance? IFn$OLD grid-lookup/double-at))
      (is (instance? IFn$OLLD grid-lookup/double-at))
      (is (instance? IFn$OLLLD grid-lookup/double-at)))))

(deftest ^:unit clojure-primitive-functions-test
  (testing "this example primitive-signature function"
    (let [my-getter (fn
                      (^double [^long b]
                       (* 2. b))
                      (^double [^long i ^long j]
                       (* 2. i j))
                      (^double [^long b ^long i ^long j]
                       (* 2. b i j)))]
      (testing "does indeed accept and return primitive values:"
        (let [implemented-types (-> my-getter (type) (ancestors))]
          (is (contains? implemented-types IFn$LD))
          (is (contains? implemented-types IFn$LLD))
          (is (contains? implemented-types IFn$LLLD))))
      (testing "may be called efficiently with (double-at ...)"
        (is (grid-lookup/suitable-for-primitive-lookup? my-getter))
        (testing "which returns correct results:"
          (is (=
               6.
               (grid-lookup/double-at my-getter 3)
               (grid-lookup/double-at-b* my-getter 3)))
          (is (=
               42.
               (grid-lookup/double-at my-getter 3 7)
               (grid-lookup/double-at-ij* my-getter 3 7)))
          (is (=
               420.
               (grid-lookup/double-at my-getter 10 3 7)
               (grid-lookup/double-at-bij* my-getter 10 3 7))))))))
