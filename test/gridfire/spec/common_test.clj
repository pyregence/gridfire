;; [[file:../../../org/GridFire.org::gridfire.spec.common-test][gridfire.spec.common-test]]
(ns gridfire.spec.common-test
  (:require [clojure.spec.alpha   :as s]
            [clojure.test         :refer [deftest is testing]]
            [gridfire.spec.common :as common]))

(deftest ^:unit number-or-range-map-test
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

  (testing (pr-str :gridfire.input/add-correction-angle360)
    (is (s/valid? :gridfire.input/add-correction-angle360 1008.0)
        "may be > 360°")
    (is (s/valid? :gridfire.input/add-correction-angle360 -56.0)
        "may be negative"))

  (testing "invalid range"
    (let [config {:lo 10.0
                  :hi 1.0 }]
      (is (not (s/valid? ::common/number-or-range-map config))
          "lo should not be higher than hi"))

    (let [config {:lo [1.0 3.0]
                  :hi [2.0 3.0]}]
      (is (not (s/valid? ::common/number-or-range-map config))
          "should not have overlapping ranges"))))

(deftest ^:unit file-path-test
  (is (s/valid? ::common/file-path "/absolute/file/path"))
  (is (s/valid? ::common/file-path "./relative/file/path"))
  (is (s/valid? ::common/file-path "../relative/file/path"))
  (is (s/valid? ::common/file-path "../../relative/file/path"))
  (is (not (s/valid? ::common/file-path ".../relative/file/path")))
  (is (not (s/valid? ::common/file-path "./relative/file/path/"))
      "should not end in /"))

(deftest ^:unit directory-path-test
  (is (s/valid? ::common/directory-path "/absolute/directory/path")
      "may end without /")
  (is (s/valid? ::common/directory-path "/absolute/directory/path/")
      "may end with /")
  (is (s/valid? ::common/directory-path "../relative/directory/path/"))
  (is (s/valid? ::common/directory-path "../../relative/directory/path/")))
;; gridfire.spec.common-test ends here
