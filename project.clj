(defproject sig-gis/gridfire "1.3.0"
  :description      "SIG's Raster-based Fire Behavior Model"
  :dependencies     [[org.clojure/clojure                 "1.7.0"]
                     [org.clojure/data.csv                "0.1.3"]
                     [org.clojure/java.jdbc               "0.4.2"]
                     [postgresql/postgresql               "9.3-1102.jdbc41"]
                     [net.mikera/core.matrix              "0.42.0"]
                     [net.mikera/vectorz-clj              "0.36.0"]
                     [sig-gis/magellan                    "0.1.0"]
                     [org.clojars.lambdatronic/matrix-viz "0.1.7"]]
  :repositories     [["java.net"  "http://download.java.net/maven/2"]
                     ["osgeo.org" "http://download.osgeo.org/webdav/geotools/"]]
  :manifest         {"Specification-Title" "Java Advanced Imaging Image I/O Tools"
                     "Specification-Version" "1.1"
                     "Specification-Vendor" "Sun Microsystems, Inc."
                     "Implementation-Title" "com.sun.media.imageio"
                     "Implementation-Version" "1.1"
                     "Implementation-Vendor" "Sun Microsystems, Inc."}
  :min-lein-version "2.5.2"
  :aot              [gridfire.cli]
  :main             gridfire.cli
  :repl-options     {:init-ns gridfire.cli}
  :global-vars      {*warn-on-reflection* true})
