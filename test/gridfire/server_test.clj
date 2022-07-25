(ns gridfire.server-test
  (:require [gridfire.utils.test :refer [with-temp-directories]]
            [clojure.java.shell  :refer [sh]]
            [clojure.java.io     :as io]
            [clojure.test        :refer [deftest is use-fixtures testing]]))


;;-----------------------------------------------------------------------------
;; paths
;;-----------------------------------------------------------------------------

(def data-dir        "test/gridfire/temp/data/")
(def incoming-dir    "test/gridfire/temp/incoming/")
(def active-fire-dir "test/gridfire/temp/active_fires/")

;;-----------------------------------------------------------------------------
;; Fixtures
;;-----------------------------------------------------------------------------

(use-fixtures :once (with-temp-directories [data-dir incoming-dir active-fire-dir]))

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(deftest ^:unit unzip-tar-test
  (sh "mv" "test/unzip-fire-test_19700101_000000_001.tar" "test/gridfire/server_test/active-fires")
  (let [request-base {:fire-name     "unzip-fire-test"
                      :ignition-time "1970-01-01 00:00 UTC"
                      :type          :active-fire}
        config       {:data-dir        data-dir
                      :incoming-dir    incoming-dir
                      :active-fire-dir active-fire-dir}]
    (testing "unsuppressed"
      (#'gridfire.server/unzip-tar! request-base config)
      (is (.exists (io/file data-dir "unzip-fire-test_19700101_000000_001"))))

    (testing "suppressed"
      (#'gridfire.server/unzip-tar! (assoc request-base :suppression  {:suppression-dt         1440
                                                                       :suppression-coefficent 2.0})
                                    config)
      (is (.exists (io/file data-dir "unzip-fire-test-suppressed_19700101_000000_001"))))))
