(task-options!
 pom  {:project     'sig-gis/gridfire
       :version     "1.2.0"
       :description "SIG's Raster-based Fire Spread and Severity Model"}
 repl {:eval        '(set! *warn-on-reflection* true)
       :init-ns     'gridfire.cli}
 aot  {:namespace   '#{gridfire.cli}}
 jar  {:main        'gridfire.cli})

(set-env!
 :source-paths   #{"src" "test"}
 :resource-paths #{"resources"}
 :dependencies   '[[org.clojure/clojure                 "1.7.0"]
                   [org.clojure/data.csv                "0.1.3"]
                   [org.clojure/java.jdbc               "0.4.2"]
                   [postgresql/postgresql               "9.3-1102.jdbc41"]
                   [net.mikera/core.matrix              "0.42.0"]
                   [net.mikera/vectorz-clj              "0.36.0"]
                   [sig-gis/magellan                    "0.1.0"]
                   [org.clojars.lambdatronic/matrix-viz "0.1.7"]
                   [adzerk/boot-test                    "1.1.0" :scope "test"]]
 :repositories   #(conj %
                        ["java.net"  "http://download.java.net/maven/2"]
                        ["osgeo.org" "http://download.osgeo.org/webdav/geotools/"]))

(require '[adzerk.boot-test :refer :all])

(deftask build
  "Build my project."
  []
  (comp (aot) (pom) (uber) (jar)))

(deftask testing
  "Automatically run tests after each file save."
  []
  (comp (watch) (speak) (test)))
