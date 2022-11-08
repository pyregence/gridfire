(ns gridfire.fetch-test
  (:require [clojure.test                :refer [deftest is use-fixtures]]))


(comment

  ;; FIXME an illustrative example of :grid_of_rasters
  {:type         :grid_of_rasters
   :rasters_grid [[{:type :geotiff :source ...} {:type :postgis :source ...}]
                  [{:type :postgis :source ...} {:type :geotiff :source ...}]
                  [{:type :geotiff :source ...} {:type :postgis :source ...}]]}
  *e)
