;; [[file:../../org/GridFire.org::random_ignition.clj][random_ignition.clj]]
(ns gridfire.random-ignition
  (:require [clojure.core.matrix :as m]
            [gridfire.common     :refer [burnable-fuel-model?]]))

(m/set-current-implementation :vectorz)

(defn- in-edge-buffer?
  "Returns true if give [row col] is within the buffer region defined
  by buffer-size. The buffer size is the thickness (pixel) of the edge
  buffer region."
  [num-rows num-cols buffer-size row col]
  (or  (<= row buffer-size)
       (> row (- num-rows buffer-size))
       (<= col buffer-size)
       (> col (- num-cols buffer-size))))

(defn valid-ignition-site? [{:keys [num-rows num-cols random-ignition cell-size landfire-rasters]} row col]
  (let [{:keys [edge-buffer]} random-ignition
        {:keys [fuel-model]}  landfire-rasters]
    (and (if edge-buffer
           (let [buffer-size (int (Math/ceil (/ edge-buffer cell-size)))]
             (not (in-edge-buffer? num-rows num-cols buffer-size row col)))
           true)
         (burnable-fuel-model? (m/mget fuel-model row col)))))
;; random_ignition.clj ends here
