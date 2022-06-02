(ns gridfire.elm-to-grid-test
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell :as sh]
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is use-fixtures]]
   [gridfire.spec.config :as spec]
   [clojure.edn :as edn]))

;;-----------------------------------------------------------------------------
;; Fixtures
;;-----------------------------------------------------------------------------

(defn clean-gridfire-edn [test-fn]
  (.delete (io/file "test/gridfire/resources/config_test/gridfire.edn"))
  (sh/sh "./resources/elm_to_grid.clj" "-e" "test/gridfire/resources/config_test/sample-elmfire.data")
  (test-fn)
  (.delete (io/file "test/gridfire/resources/config_test/gridfire.edn")))

(use-fixtures :once clean-gridfire-edn)

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(deftest ^:unit elm-to-grid-test
  (sh/sh "./resources/elm_to_grid.clj" "-e" "test/gridfire/resources/config_test/sample-elmfire.data")
  (let [config (-> "test/gridfire/resources/config_test/gridfire.edn"
                   slurp
                   edn/read-string)]
    (is (s/valid? ::spec/config config))))
