(task-options!
 pom    {:project     'sig-gis/gridfire
         :version     "1.5.0"
         :description "SIG's Raster-based Fire Behavior Model"}
 repl   {:eval        '(set! *warn-on-reflection* true)
         :init-ns     'gridfire.cli}
 aot    {:namespace   '#{gridfire.cli}}
 jar    {:main        'gridfire.cli
         :file        "gridfire-1.5.0.jar"
         :manifest    {"Specification-Title" "Java Advanced Imaging Image I/O Tools"
                       "Specification-Version" "1.1"
                       "Specification-Vendor" "Sun Microsystems, Inc."
                       "Implementation-Title" "com.sun.media.imageio"
                       "Implementation-Version" "1.1"
                       "Implementation-Vendor" "Sun Microsystems, Inc."}}
 target {:dir         #{"target"}})

(set-env!
 :source-paths   #{"src" "test"}
 :resource-paths #{"resources"}
 :dependencies   '[[org.clojure/clojure                 "1.9.0-alpha12"]
                   [org.clojure/data.csv                "0.1.4"]
                   [org.clojure/java.jdbc               "0.7.6"]
                   [org.postgresql/postgresql           "42.2.2.jre7"]
                   [net.mikera/core.matrix              "0.62.0"]
                   [net.mikera/vectorz-clj              "0.47.0"]
                   [sig-gis/magellan                    "0.1.0"]
                   [org.clojars.lambdatronic/matrix-viz "0.1.7"]
                   [adzerk/boot-test                    "1.2.0" :scope "test"]]
 :repositories   #(conj %
                        ["java.net"  "http://download.java.net/maven/2"]
                        ["osgeo.org" "http://download.osgeo.org/webdav/geotools/"]))

(require '[adzerk.boot-test :refer :all])

(deftask build
  "Build my project."
  []
  (comp (aot) (pom) (uber) (jar) (target)))

(deftask testing
  "Automatically run tests after each file save."
  []
  (comp (watch) (speak) (test)))
