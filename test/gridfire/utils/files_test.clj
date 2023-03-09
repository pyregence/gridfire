;; [[file:../../../org/GridFire.org::gridfire.utils.files-test][gridfire.utils.files-test]]
(ns gridfire.utils.files-test
  (:require [clojure.edn          :as edn]
            [clojure.test         :refer [deftest is are testing]]
            [gridfire.utils.files :refer [convenience-encoding-readers]]))

(deftest ^:unit abbreviating-examples-test
  (testing "#gridfire.config/abbreviating eliminates redundancy in EDN-encoded text"
    (testing "transparently to the reading program, for example:"
      (is (= {:my-maps [{:this :map :is :arguably :quite :verbose}
                        {:so :is :this :one :I :daresay}
                        {:so :is :this :one :I :daresay}
                        {:so :is :this :one :I :daresay}
                        {:this :map :is :arguably :quite :verbose}
                        {:this :map :is :arguably :quite :verbose}
                        {:so :is :this :one :I :daresay}]}
             (->> (tagged-literal 'gridfire.config/abbreviating
                                  '[{m0 {:this :map :is :arguably :quite :verbose}
                                     m1 {:so :is :this :one :I :daresay}}
                                    {:my-maps [m0 m1 m1 m1 m0 m0 m1]}])
                  (pr-str)
                  (edn/read-string {:readers convenience-encoding-readers})))))
    (testing "by using 'abbreviations' as map keys, which can be values of any type, although clojure Symbols are arguably a natural choice:"
      (let [repeated-value {:this :map :is :arguably :quite :verbose}]
        (are [abbr]
          (= {:my-maps [repeated-value
                        repeated-value
                        repeated-value]}
             (->> (tagged-literal 'gridfire.config/abbreviating
                                  [{abbr repeated-value}
                                   {:my-maps [abbr
                                              abbr
                                              abbr]}])
                  (pr-str)
                  (edn/read-string {:readers convenience-encoding-readers})))
          'my-abbr
          "my-abbr"
          :my-abbr
          42
          nil
          ["my" "abbr"]
          {:my 'abbr})))
    (testing "by reading a repeated abbreviation as identical values:"
      (let [[v0 v1] (->> (tagged-literal 'gridfire.config/abbreviating
                                         '[{m0 {:this :map :is :arguably :quite :verbose}}
                                           [m0 m0]])
                         (pr-str)
                         (edn/read-string {:readers convenience-encoding-readers}))]
        (is (identical? v0 v1))))))
;; gridfire.utils.files-test ends here
