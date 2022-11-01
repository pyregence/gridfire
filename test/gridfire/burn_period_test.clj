(ns gridfire.burn-period-test
  (:require [clojure.test  :refer [deftest is testing]]
            [gridfire.core :refer [load-config! load-inputs!]]))

(deftest ^:unit burn-period-examples-test
  (testing (str "The Burn period gets resolved into " :burn-period-samples)
    (testing (str "from sunrise/sunset when " :burn-period-frac " is configured:")
      (let [base-config  (merge (load-config! "test/gridfire/resources/canonical_test/base-config.edn")
                                {:burn-period-frac        0.6
                                 :weather-start-timestamp #inst "2022-01-01"
                                 :ignition-start-times    [(* 60 24 365 0.0)
                                                           (* 60 24 365 0.15)
                                                           (* 60 24 365 0.25)
                                                           (* 60 24 365 0.5)
                                                           (* 60 24 365 0.75)
                                                           (* 60 24 365 0.99)]})
            ign-start-ts [#inst "2022-01-01T00:00:00.000-00:00"
                          #inst "2022-02-24T18:00:00.000-00:00"
                          #inst "2022-04-02T06:00:00.000-00:00"
                          #inst "2022-07-02T12:00:00.000-00:00"
                          #inst "2022-10-01T18:00:00.000-00:00"
                          #inst "2022-12-28T08:24:00.000-00:00"]]
        (testing (str "using the sunrise-to-sunset duration when " :burn-period-length " is not configured")
          (let [config base-config
                inputs (load-inputs! config)]
            (is (= {:ignition-start-timestamps ign-start-ts
                    ;; Those weird >24 hours arise from the offset to UTC. (Val, 01 Nov 2022)
                    ;; They make for straightforward arithmetics, but are not proper times-of-the-day.
                    ;; Shall we fix them using #(-> % (mod 24))? FIXME REVIEW
                    :burn-period-samples       [{:burn-period-start "15:23" :burn-period-end "25:07"}
                                                {:burn-period-start "14:49" :burn-period-end "26:02"}
                                                {:burn-period-start "13:58" :burn-period-end "26:34"}
                                                {:burn-period-start "12:58" :burn-period-end "27:33"}
                                                {:burn-period-start "14:06" :burn-period-end "25:56"}
                                                {:burn-period-start "15:22" :burn-period-end "25:05"}]}
                   (-> inputs
                       (select-keys [:ignition-start-timestamps
                                     :burn-period-samples]))))))
        (testing (str "using " :burn-period-length " when configured")
          (let [config (merge base-config
                              {:burn-period-length 10.0})
                inputs (load-inputs! config)]
            (is (= {:ignition-start-timestamps ign-start-ts
                    :burn-period-samples       [{:burn-period-start "16:13" :burn-period-end "26:13"}
                                                {:burn-period-start "16:33" :burn-period-end "26:33"}
                                                {:burn-period-start "16:32" :burn-period-end "26:32"}
                                                {:burn-period-start "16:43" :burn-period-end "26:43"}
                                                {:burn-period-start "16:12" :burn-period-end "26:12"}
                                                {:burn-period-start "16:12" :burn-period-end "26:12"}]}
                   (-> inputs
                       (select-keys [:ignition-start-timestamps
                                     :burn-period-samples]))))))))))
