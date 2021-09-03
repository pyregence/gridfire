(ns gridfire.spec.ignition-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is]]
            [gridfire.spec.ignition :as ignition]))

(deftest path-test
  (is (s/valid? ::ignition/ignition-layer {:type   :postgis
                                           :source "/some/path/to.tif"})))

(deftest sql-test
  (is (s/valid? ::ignition/ignition-layer {:type   :postgis
                                           :source "landfire.ign WHERE rid=1"})))

(deftest burn-value-test
  (is (s/valid? ::ignition/ignition-layer {:type        :geotiff
                                           :source      "/some/path/to.tif"
                                           :burn-values {:burned   1.0
                                                         :unburned -1.0}})))
