;; [[file:../../../org/GridFire.org::gridfire.spec.output-test][gridfire.spec.output-test]]
;; FIXME LP coverage
(ns gridfire.spec.output-test
  (:require [clojure.spec.alpha   :as s]
            [clojure.test         :refer [deftest is]]
            [gridfire.spec.config :as config]))

(deftest ^:unit scalar-test
  (let [config {:fire-spread 10}]
    (is (s/valid? ::config/output-layers config))))

(deftest ^:unit final-keyword-test
  (let [config {:fire-spread :final}]
    (is (s/valid? ::config/output-layers config))))

(deftest ^:unit flame-length-test
  (let [config {:flame-length 10}]
    (is (s/valid? ::config/output-layers config))))

(deftest ^:unit fire-line-inentisyt-test
  (let [config {:fire-line-intensity 10}]
    (is (s/valid? ::config/output-layers config))))

(deftest ^:unit burn-history-test
  (let [config {:burn-history 10}]
    (is (s/valid? ::config/output-layers config))))

(deftest ^:unit multiple-output-layer-test
  (let [config {:fire-spread         10
                :flame-lenth         10
                :fire-line-intensity 10
                :burn-history        10}]
    (is (s/valid? ::config/output-layers config))))
;; gridfire.spec.output-test ends here
