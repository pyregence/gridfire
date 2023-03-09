;; [[file:../../../org/GridFire.org::gridfire.utils.geo-test][gridfire.utils.geo-test]]
;; FIXME LP coverage
(ns gridfire.utils.geo-test
  (:require [clojure.test        :refer [deftest is testing use-fixtures]]
            [gridfire.core       :refer [load-config! load-inputs!]]
            [gridfire.utils.geo  :refer [resolve-simulation-lat+lon]]
            [gridfire.utils.test :refer [with-temp-directories]]))

(use-fixtures :once (with-temp-directories ["outputs/simple/"]))

(deftest ^:unit resolve-simulation-lat+lon-example-test
  (testing `resolve-simulation-lat+lon
    (testing "on our Canonical config"
      (let [config (load-config! "test/gridfire/resources/canonical_test/base-config.edn")
            inputs (load-inputs! config)]
        (is (= [36.144687673692644 -123.00000000000001]
               (resolve-simulation-lat+lon inputs))
            "returns an USA-typical [lat lon] pair.")))))
;; gridfire.utils.geo-test ends here
