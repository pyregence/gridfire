(ns gridfire.grid-lookup-test
  (:require [gridfire.grid-lookup :as grid-lookup]
            [clojure.test :refer :all])
  (:import (clojure.lang IFn$OD IFn$OOD IFn$OOOD IFn$OOOOD)))

(deftest double-at-primitiveness-test
  (testing "(double-at getter & coords)"
    (testing "accepts primitive long-typed coordinates, and returns a primitive double."
      (is (instance? IFn$OOD grid-lookup/double-at))
      (is (instance? IFn$OOOD grid-lookup/double-at))
      (is (instance? IFn$OOOOD grid-lookup/double-at)))))

(deftest clojure-primitive-functions-test
  (testing "this example primitive-signature function"
    (let [my-getter (fn
                      (^double [b]
                       (* 2. (long b)))
                      (^double [i j]
                       (* 2. (long i) (long j)))
                      (^double [b i j]
                       (* 2. (long b) (long i) (long j))))]
      (testing "does indeed accept and return primitive values:"
        (let [implemented-types (-> my-getter (type) (ancestors))]
          (is (contains? implemented-types IFn$OD))
          (is (contains? implemented-types IFn$OOD))
          (is (contains? implemented-types IFn$OOOD))))
      (testing "may be called efficiently with (double-at ...)"
        (is (grid-lookup/suitable-for-primitive-lookup? my-getter))
        (testing "which returns correct results:"
          (is (=
               6.
               (grid-lookup/double-at my-getter 3)
               (grid-lookup/double-at-B* my-getter 3)))
          (is (=
               42.
               (grid-lookup/double-at my-getter 3 7)
               (grid-lookup/double-at-IJ* my-getter 3 7)))
          (is (=
               420.
               (grid-lookup/double-at my-getter 10 3 7)
               (grid-lookup/double-at-BIJ* my-getter 10 3 7))))))))

