(ns gridfire.server-test
  (:require [clojure.spec.alpha   :as spec]
            [clojure.test         :refer [deftest testing is]]
            [gridfire.spec.server]))

(def minimal-request-example
  {:fire-name     "some-fire-name"
   :ignition-time "2021-03-15 05:00 PST"})

(deftest server-request-examples
  (testing "Valid requests"
    (testing "must provide at least:"
      (is
        (spec/valid? :gridfire.spec.server/gridfire-server-request
          minimal-request-example)))
    (testing "may request that pre/post-processing scripts be run:"
      (is
        (spec/valid? :gridfire.spec.server/gridfire-server-request
          (merge
            minimal-request-example
            ;; Example Gridfire request invoking pre/post-process scripts:
            ;; FIXME REVIEW: is this API too expressive? Does it create security exploits? (Val, 25 Jul 2022)
            ;; Is so, we might want to make the scripts-invoking API less open-ended.
            {;; ...
             :before-gridfire-run-cmds
             [{:shell-cmd-args ["./gridfire_helper_scripts/elm_to_grid.clj" "-e" "./elmfire.data" "-o" "./override.edn"]}]
             :after-gridfire-run-cmds
             [{:shell-cmd-args       ["./gridfire_helper_scripts/elmfire_post.sh"]
               :gf-send-notif-before "Running elmfire_post."}
              {:shell-cmd-args       ["./gridfire_helper_scripts/make_tifs.sh"]
               :gf-send-notif-before "Creating GeoTIFFs."}
              {:shell-cmd-args ["./gridfire_helper_scripts/build_geoserver_directory.sh"]}
              {:shell-cmd-args ["./gridfire_helper_scripts/cleanup.sh"]}]}))))
    (testing "may request :active-fire special treatment:"
      (is
        (spec/valid? :gridfire.spec.server/gridfire-server-request
          (merge
            minimal-request-example
            {:type :active-fire}))))
    (testing "may supply the network location to which this server should send responses:"
      (is
        (spec/valid? :gridfire.spec.server/gridfire-server-request
          (merge
            minimal-request-example
            {:response-host "localhost"
             :response-port 80})))
      (is
        (spec/valid? :gridfire.spec.server/gridfire-server-request
          (merge
            minimal-request-example
            {:response-host "10.45.78.12"
             :response-port 443}))))))
