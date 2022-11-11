(ns gridfire.spec.burn-period-test
  (:require [clojure.spec.alpha        :as s]
            [clojure.test              :refer [deftest is testing]]
            [gridfire.spec.burn-period :as burn-period]))

(deftest ^:unit burn-period-test
  (testing "valid"
    (is (s/valid? ::burn-period/burn-period {:start "08:00"
                                             :end   "20:00"}))))
