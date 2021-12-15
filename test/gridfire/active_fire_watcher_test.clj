(ns gridfire.active-fire-watcher-test
  (:require [gridfire.active-fire-watcher :refer [file-name-regex
                                                  fire-name-regex
                                                  ignition-time-regex]]
            [clojure.test :refer [deftest is]]))

(deftest ^:unit file-name-regex-test

  (is (= "nm-johnson_20210617_203600_001"
         (->>  "nm-johnson_20210617_203600_001.tar"
               (re-find file-name-regex))))

  (is (= "nm-johnson_20210617_203600_001"
         (->>  "/home/nm-johnson_20210617_203600_001.tar"
               (re-find file-name-regex))))

  (is (= "nm-johnson_20210617_203600_001"
         (->>  "/gridfire/data/nm-johnson_20210617_203600_001.tar"
               (re-find file-name-regex)))))

(deftest ^:unit fire-name-regex-test

  (is (= "nm-johnson"
         (->>  "nm-johnson_20210617_203600_001"
               (re-find fire-name-regex))))

  (is (= "ut-bear-bennion-creek"
         (->>  "ut-bear-bennion-creek_20210610_184300_001"
               (re-find fire-name-regex))))
  (is (= "nm"
         (->>  "nm_20210617_203600_001"
               (re-find fire-name-regex))))

  (is (= "ca-success-2"
         (->>  "ca-success-2_20210618_000000_001.tar"
               (re-find fire-name-regex)))))

(deftest ^:unit ignition-time-regex-test

  (is (= "20210617_203600"
         (->>  "nm-johnson_20210617_203600_001"
               (re-find ignition-time-regex)))))
