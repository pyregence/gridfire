(ns gridfire.fetch.base
  (:require [gridfire.grid-lookup :as grid-lookup]
            [tech.v3.datatype     :as d]))

(defmulti get-wrapped-tensor-multi (fn [_context-map layer-spec _convert-fn _target-dtype] (:type layer-spec)))

(defn get-wrapped-tensor
  "Fetches a layer into a tensor wrapped in a map with geospatial metadata."
  [context-map layer-spec convert-fn target-dtype]
  {:pre [(map? context-map)
         (:type layer-spec)
         (or (nil? target-dtype)
             (keyword? target-dtype))]}
  (-> (get-wrapped-tensor-multi context-map layer-spec convert-fn target-dtype)
      ;; NOTE at the time of writing, the tensors returned by Magellan are not backed by a flat JVM array,
      ;; being assembled via d/concat:
      ;; https://github.com/sig-gis/magellan/blob/8d72d0484b15c0d26a073684d07d405dae5c715d/src/magellan/raster/inspect.clj#L145
      (update :matrix grid-lookup/ensure-flat-jvm-tensor)))

(defn convert-tensor-as-requested
  "General utility function for applying conversions and type coercions to tensors,
  useful when the data provider cannot do it directly."
  [layer-map convert-fn target-dtype]
  (let [old-tensor        (:matrix layer-map)
        old-dtype         (d/elemwise-datatype old-tensor)
        needs-converting? (or (some? convert-fn)
                              (and (some? target-dtype)
                                   (not= target-dtype old-dtype)))]
    (if needs-converting?
      (let [converted  (d/emap (or convert-fn identity)
                               target-dtype
                               old-tensor)
            new-tensor (if (= old-dtype target-dtype)
                         (do
                           ;; converts in-place when possible, as it's more efficient.
                           (d/copy! converted old-tensor)
                           ;; Redundant since the above (d/copy! ...) already returns old-tensor,
                           ;; but I'd rather be very explicit about mutation. (Val, 04 Nov 2022)
                           old-tensor)
                         (d/clone converted))]
        (assoc layer-map :matrix new-tensor))
      layer-map)))
