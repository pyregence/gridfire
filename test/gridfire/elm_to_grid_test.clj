;; [[file:../../org/GridFire.org::gridfire.elm-to-grid-test][gridfire.elm-to-grid-test]]
;; FIXME LP coverage
(ns gridfire.elm-to-grid-test
  (:require [clojure.java.io      :as io]
            [clojure.java.shell   :as sh]
            [clojure.spec.alpha   :as s]
            [clojure.test         :refer [deftest is testing use-fixtures]]
            [gridfire.spec.common :refer [*check-files-exist?*]]
            [gridfire.spec.config :as spec]
            [gridfire.utils.files :as files]
            [gridfire.utils.test  :refer [with-temp-directories]]))

(def elm-to-grid-path "./resources/elm_to_grid.clj")

;;-----------------------------------------------------------------------------
;; Fixtures
;;-----------------------------------------------------------------------------

(defn- delete-file-if-exists
  [file]
  (let [f (io/file file)]
    (when (.exists f)
      (.delete f))))

(use-fixtures :once
  (with-temp-directories ["test/gridfire/resources/config_test/outputs"]))

;;-----------------------------------------------------------------------------
;; Tests
;;-----------------------------------------------------------------------------

(defn- is-emitted-gridfire-end-as-expected
  [elm-to-grid-sh-args emitted-gfr-conf-path expected-gfr-conf-path]
  (try
    (let [ret (apply sh/sh elm-to-grid-path elm-to-grid-sh-args)]
      (or (is (= 0 (:exit ret)) "The script completed successfully.")
          ;; Throwing to avoid continuing execution of the test.
          (throw (ex-info (format "elm_to_grid.clj returned exited with error status %d: %s"
                                  (:exit ret)
                                  (:err ret))
                          ret))))
    (comment
      ;; TIP: Run this when you want to update the expected config after changing elm_to_grid.
      (sh/sh "cp" emitted-gfr-conf-path expected-gfr-conf-path))
    (testing "The emitted GridFire config"
      (let [config (files/read-situated-edn-file emitted-gfr-conf-path)]
        (if-not (is (s/valid? ::spec/config config) "is valid.")
          (s/explain ::spec/config config)
          (is (= config
                 (files/read-situated-edn-file expected-gfr-conf-path))
              ;; This testing technique is sometimes known as 'Snapshot Testing'.
              "has not changed."))))
    (finally
      (delete-file-if-exists emitted-gfr-conf-path))))

(deftest ^:unit elm-to-grid-examples-test
  (testing "Basic example."
    (let [elm-data-path "test/gridfire/resources/config_test/sample-elmfire.data"
          gfr-conf-path "test/gridfire/resources/config_test/gridfire.edn"]
      (is-emitted-gridfire-end-as-expected ["--elmfire-config" elm-data-path]
                                           gfr-conf-path
                                           "test/gridfire/resources/config_test/expected-gridfire.edn")))
  (testing "FF AWS example."
    (binding [*check-files-exist?* false]
      (let [dir-path      "test/gridfire/resources/elmfire-examples/ff-aws-2022-10/"
            elm-data-path (str dir-path "elmfire.data")
            gfr-conf-path (str dir-path "gridfire.edn")]
        (testing "With minimal options."
          (is-emitted-gridfire-end-as-expected ["--elmfire-config" elm-data-path]
                                               gfr-conf-path
                                               (str dir-path "expected-0-gridfire.edn")))
        (testing "With maximal options."
          (is-emitted-gridfire-end-as-expected ["--elmfire-config"                    elm-data-path
                                                "--override-config"                   (str dir-path "gridfire_base.edn")
                                                ;; NOTE created manually by editing another file. (Val, 10 Nov 2022)
                                                "--elmfire-summary-csv"               (str dir-path "synthetic-elmfire-summary.csv")
                                                "--pyrome-spread-rate-adjustment-csv" (str dir-path "pyrome_adjustment_factors.csv")
                                                "--pyrome-calibration-csv"            (str dir-path "pyrome_calibration_constants.csv")]
                                               gfr-conf-path
                                               (str dir-path "expected-1-gridfire.edn")))))))
;; gridfire.elm-to-grid-test ends here
