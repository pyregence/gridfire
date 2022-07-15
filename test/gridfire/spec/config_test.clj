(ns gridfire.spec.config-test
  (:require [clojure.spec.alpha   :as s]
            [clojure.test         :refer [deftest is testing]]
            [gridfire.spec.config :as config]))

(deftest ^:unit valid-timestamps-test
  (testing "valid"
   (let [config {:weather-start-timestamp  #inst "1970-01-01T00:00:00.000-00:00"
                 :ignition-start-timestamp #inst "1970-01-01T08:00:00.000-00:00"}]
     (is (s/valid? ::config/valid-timestamps config)
         "weather-start-timestamp should be before ignition-start-timestamp"))

   (let [config {:weather-start-timestamp  #inst "1970-01-01T00:00:00.000-00:00"
                 :ignition-start-timestamp #inst "1970-01-01T00:00:00.000-00:00"}]
     (is (s/valid? ::config/valid-timestamps config)
         "weather-start-timestamp can be equal to ignition-start-timestamp")))

  (testing "invalid"
    (let [config {:weather-start-timestamp  #inst "1970-01-01T08:00:00.000-00:00"
                  :ignition-start-timestamp #inst "1970-01-01T00:00:00.000-00:00"}]
      (is (not (s/valid? ::config/valid-timestamps config))
          "weather-start-timestamp should be not be before ignition-start-timestamp"))))
