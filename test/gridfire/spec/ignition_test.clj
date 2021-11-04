(ns gridfire.spec.ignition-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is]]
            [gridfire.spec.config :as config]))

(deftest path-test
  (is (s/valid? ::config/ignition-layer {:type   :geotiff
                                         :source "test/gridfire/resources/asp.tif"})))

(deftest sql-test
  (is (s/valid? ::config/ignition-layer {:type   :postgis
                                         :source "landfire.ign WHERE rid=1"})))

(deftest burn-value-test
  (is (s/valid? ::config/ignition-layer {:type        :geotiff
                                         :source      "test/gridfire/resources/asp.tif"
                                         :burn-values {:burned   1.0
                                                       :unburned -1.0}})))
