;; [[file:../../org/GridFire.org::perturbation][perturbation]]
(ns gridfire.perturbation
  (:require [gridfire.utils.random :refer [my-rand]]
            [gridfire.conversion :refer [conversion-table]]))

(defn add-rand-generator
  [rand-generator config]
  (into config
        (map (fn [[layer spec]] [layer (assoc spec :rand-generator rand-generator)]))
        config))

(defn add-simulation-id
  [id config]
  (into config
        (map (fn [[layer spec]] [layer (assoc spec :simulation-id id)]))
        config))

(defn convert-ranges
  [config]
  (into config
        (map (fn [[layer {:keys [units range] :as spec}]]
               (if (= units :metric)
                 (let [convert-fn (conversion-table layer)]
                   [layer (assoc spec :range (map convert-fn range))])
                 [layer spec])))
        config))

(defn add-global-values
  [config]
  (into config
        (map (fn [[layer {:keys [spatial-type rand-generator range] :as spec}]]
               (if (= spatial-type :global)
                 [layer (assoc spec :global-value (my-rand rand-generator (apply - range)))]
                 [layer spec])))
        config))

(defn- enrich-info
  [perturbations rand-generator id]
  (->> perturbations
      (add-rand-generator rand-generator)
      (add-simulation-id id)
      (convert-ranges)
      (add-global-values)))

(defn draw-samples
  [rand-generator n perturbations]
  (when perturbations
    (mapv #(enrich-info perturbations rand-generator %) (range n))))

(defn value-at
  ([perturb-info raster here]
   (value-at perturb-info raster here nil))

  ([{:keys [range spatial-type global-value rand-generator]} raster here frequency-band]
   (if (= spatial-type :global)
     global-value
     (my-rand rand-generator (apply - range)))))

(def value-at
  (memoize value-at))

(defn- update?
  [global-clock next-clock frequency]
  (< (quot global-clock frequency)
     (quot next-clock frequency)))

(defn- global-temporal-perturbations
  [perturbations]
  (->> perturbations
       (filter (fn [[k v]] (and (:frequency v) (= (:spatial-type v) :global))))
       keys))

(defn update-global-vals
  [{:keys [perturbations] :as constants} current-clock next-clock]
  (let [layers-to-update (global-temporal-perturbations perturbations)]
    (reduce
     (fn [acc layer-name]
       (let [{:keys [frequency
                     range
                     rand-generator]} (get-in acc [:perturbations layer-name])
             new-global               (my-rand rand-generator (apply - range))]
         (if (update? current-clock next-clock frequency)
           (assoc-in acc [:perturbations layer-name :global-value] new-global)
           acc)))
     constants
     layers-to-update)))
;; perturbation ends here
