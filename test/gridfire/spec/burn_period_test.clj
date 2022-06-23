(ns gridfire.spec.burn-period-test
  (:require [clojure.spec.alpha   :as s]
            [clojure.test         :refer [deftest is testing]]
            [gridfire.spec.burn-period :as burn-period]))

(deftest burn-period-test
  (testing "valid"
   (is (s/valid? ::burn-period/burn-period {:start                        "08:00"
                                            :end                          "20:00"
                                            :weather-data-start-timestamp #inst "1970-01-01T00:00:00"})))
  (testing "bad start and end range"
    (is (not
         (s/valid? ::burn-period/burn-period {:start                        "21:00"
                                              :end                          "20:00"
                                              :weather-data-start-timestamp #inst "1970-01-01T00:00:00"}))
        "start should not be after end")

    (is (not
         (s/valid? ::burn-period/burn-period {:start                        "20:00"
                                              :end                          "20:00"
                                              :weather-data-start-timestamp #inst "1970-01-01T00:00:00"}))
        "start should not be equal to end")))
