(ns gridfire.spec.common-test
  (:require [clojure.spec.alpha   :as s]
            [clojure.test         :refer [deftest is testing]]
            [gridfire.spec.common :as common]))

(deftest number-or-range-map-test
  (testing "scalar"
    (is (s/valid? ::common/number-or-range-map 5.0)
        "scalar can be float")

    (is (s/valid? ::common/number-or-range-map 5)
        "scalar can be int"))

  (testing "range"
    (is (s/valid? ::common/number-or-range-map {:lo 5.0 :hi 15.0})
        "lo and hi can both be scalar floats")

    (is (s/valid? ::common/number-or-range-map {:lo 5 :hi 15})
        "lo and hi can both be scalar ints")

    (is (s/valid? ::common/number-or-range-map {:lo [1.0 5.0] :hi [15.0 16.0]})
        "lo and hi can both be float tuples")

    (is (s/valid? ::common/number-or-range-map {:lo [1 5] :hi [15 16]})
        "lo and hi can both be int tuples")

    (is (s/valid? ::common/number-or-range-map {:lo 5.0 :hi [15.0 16.0]})
        "lo and hi can either be scalar or range"))

  (testing "edge cases"
    (is (s/valid? ::common/number-or-range-map {:lo 5.0 :hi [5.0 6.0]})
        "edge of ranges can overlap")

    (is (s/valid? ::common/number-or-range-map {:lo [1.0 2.0] :hi [2.0 3.0]})
        "edge of ranges can overlap"))

  (testing "invalid range"
    (let [config {:lo 10.0
                  :hi 1.0 }]
      (is (not (s/valid? ::common/number-or-range-map config))
          "lo should not be higher than hi"))

    (let [config {:lo [1.0 3.0]
                  :hi [2.0 3.0]}]
      (is (not (s/valid? ::common/number-or-range-map config))
          "should not have overlapping ranges"))))
