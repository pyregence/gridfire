(ns gridfire.elm-to-grid-test
  (:require
            [clojure.java.io      :as io]
            [clojure.java.shell   :as sh]
            [clojure.spec.alpha   :as s]
            [clojure.test         :refer [deftest is use-fixtures]]
            [gridfire.spec.config :as spec]
            [gridfire.utils.test  :refer [with-temp-directories]]
            [clojure.edn          :as edn]))

(def gridfire-edn-path "test/gridfire/resources/config_test/gridfire.edn")
(def elmfire-data-path "test/gridfire/resources/config_test/sample-elmfire.data")
(def elm-to-grid-path "./resources/elm_to_grid.clj")

;;-----------------------------------------------------------------------------
;; Fixtures
;;-----------------------------------------------------------------------------

(defn delete-gridfire-edn []
  (.delete (io/file gridfire-edn-path)))

(defn with-clean-gridfire-edn [test-fn]
  (delete-gridfire-edn)
  (test-fn)
  (delete-gridfire-edn))

(use-fixtures :once
  with-clean-gridfire-edn
  (with-temp-directories ["test/gridfire/resources/config_test/outputs"]))

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(deftest ^:unit elm-to-grid-test
  (sh/sh elm-to-grid-path "-e" elmfire-data-path)
  (let [config (-> gridfire-edn-path
                   slurp
                   edn/read-string)]
    (is (s/valid? ::spec/config config))))
