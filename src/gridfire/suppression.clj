(ns gridfire.suppression
  "An algorithm emulating human interventions reacting to fire spread
  by suppressing ('putting out') chosen contiguous segments of the
  fire front, typically backing and flanking fires."
  (:require [gridfire.conversion :refer [rad->deg]]))

(set! *unchecked-math* :warn-on-boxed)

(defn- comangular-slicee-average
  [^double avg-old ^long count-old ^double avg-new ^long count-new]
  (if (zero? (+ count-old count-new))
    0.0
    (/ (+ (* avg-old count-old)
          (* avg-new count-new))
       (+ count-old count-new))))

(defn- remove-average
  [^double avg-old ^long count-old ^double avg-to-remove ^long count-of-avg-to-remove]
  (if (zero? (- count-old count-of-avg-to-remove))
    0.0
    (/ (- (* avg-old count-old) (* avg-to-remove count-of-avg-to-remove))
       (- count-old count-of-avg-to-remove))))

;; NOTE In the, unlikely, case that multiple contiguous angular-slice-degree segments that
;; satisfy our num-cellst-to-suppress that have the same average
;; spread rate value, The entrie that is kept in the map will the the
;; segment that was proccessed the latest.
(defn- compute-contiguous-angular-slices
  "Given number of cells to suppress and a map of average directional spread
  rate data with the form: angular-slices -> [average-dsr cell-count] return a
  sorted map where each map entry:

  [`avg-dsr` [`list-of-angular-slices` `cell-count`]]

  represents a contiguous segment of the fire front, which we locate
  by `list-of-angular-slices`, the list of successive degree angular-slices covering it; cell-count
  represents the number of active perimiter cells in that segment, with
  `cell-count` no smaller than num-cells-to-suppress, but possibly bigger. Note
  that the returned segments will tend to overlap - think of a sliding window of
  (up to num-cells-to-suppress) contiguous active cells, rotating around the
  centroid: the segments returned by this function are regular snapshots of this
  window."
  [^long num-cells-to-suppress angular-slice->avg-dsr+num-cells]
  (loop [sorted-contiguous-angular-slices (sorted-map)
         angular-slice-data               (into [] (seq angular-slice->avg-dsr+num-cells))
         cur-contiguous-angular-slices    '()
         cur-dsr                          0
         cur-count                        0
         left-idx                         -1
         right-idx                        0]
    (cond

      (= left-idx 0)
      sorted-contiguous-angular-slices

      ;; Do not include already suppressed regions in the longest
      ;; contiguous angular-slice calculation.
      (let [[_ [_ cell-count]] (nth angular-slice-data right-idx)
            cell-count         (double cell-count)]
        (and (< cur-count num-cells-to-suppress) (zero? cell-count)))
      (let [next-right-idx (if (= right-idx (dec (count angular-slice-data)))
                             0
                             (+ right-idx 1))]
        (recur (if (seq cur-contiguous-angular-slices)
                 (assoc sorted-contiguous-angular-slices cur-dsr [cur-contiguous-angular-slices cur-count])
                 sorted-contiguous-angular-slices)
               angular-slice-data
               '()
               0
               0
               (if (< right-idx left-idx) 0 next-right-idx)
               next-right-idx))

      (< cur-count num-cells-to-suppress)
      ;; expand right
      (let [[angular-slice [avg-dsr cell-count]] (nth angular-slice-data right-idx)
            cell-count                           (long cell-count)]
        (recur sorted-contiguous-angular-slices
               angular-slice-data
               (conj cur-contiguous-angular-slices angular-slice)
               (comangular-slicee-average cur-dsr cur-count avg-dsr cell-count)
               (+ cur-count cell-count)
               left-idx
               (if (= right-idx (dec (count angular-slice-data)))
                 0
                 (+ right-idx 1))))

      :else
      ;; shrink left
      (let [[_ [avg-dsr cell-count]] (nth angular-slice-data (if (= -1 left-idx) 0 left-idx))
            cell-count               (long cell-count)]
        (recur (assoc sorted-contiguous-angular-slices cur-dsr [cur-contiguous-angular-slices cur-count])
               angular-slice-data
               (drop-last cur-contiguous-angular-slices)
               (remove-average cur-dsr cur-count avg-dsr cell-count)
               (- cur-count cell-count)
               (long
                (cond
                  (= left-idx (dec (count angular-slice-data))) 0
                  (= -1 left-idx)                               1
                  :else                                         (+ left-idx 1)))
               right-idx)))))

(defn- compute-sub-segment
  [angular-slice->BurnVectors angular-slices cells-needed]
  (let [angular-slices                   (set angular-slices)
        angular-slice->avg-dsr+num-cells (reduce (fn [acc [angular-slice angular-slice->avg-dsr+num-cells]]
                                                   (if (contains? angular-slices angular-slice)
                                                     (assoc acc angular-slice angular-slice->avg-dsr+num-cells)
                                                     (assoc acc angular-slice [0.0 0.0]))) ;; Needed because the segment should not be treated as circular list
                                                 (sorted-map)
                                                 angular-slice->BurnVectors)
        contiguous-angular-slices        (compute-contiguous-angular-slices cells-needed angular-slice->avg-dsr+num-cells)
        [_ [angular-slices cell-count]]  (first contiguous-angular-slices)]
    [angular-slices cell-count]))

(defn- compute-angular-slices-to-suppress
  "Returns a tuple `angular-slices-to-suppress` and `suppressed-count`.
  This alogrithm will convert `angular-slice->avg-dsr+num-cells` to a sorted map of
  `avg-dsr->angular-slices+num-cells`. Using this map the algorithm will collect
  the sequence of angular-slices until we have a cell-count of at least
  `num-cells-to-suppress`."
  [^long num-cells-to-suppress angular-slice->avg-dsr+num-cells]
  (let [avg-dsr->angular-slices+num-cells (compute-contiguous-angular-slices num-cells-to-suppress angular-slice->avg-dsr+num-cells)
        [_ [angular-slices cell-count]]   (first avg-dsr->angular-slices+num-cells)
        cell-count                        (long cell-count)]
    (if (>= cell-count num-cells-to-suppress)
      [angular-slices cell-count 0]
      ;; The optimal segment does not contain enough cells, so we stich more:
      (loop [[segment & rest-to-process] (rest avg-dsr->angular-slices+num-cells)
             cells-needed                (- num-cells-to-suppress cell-count)
             angular-slices-to-suppress  angular-slices]
        (if (and segment (pos? cells-needed))
          (let [[_ [angular-slices cell-count]] segment
                cell-count                      (long cell-count)]
            (if (<= cell-count cells-needed)
              (recur rest-to-process (- cells-needed cell-count) (into angular-slices-to-suppress angular-slices))
              (let [[sub-segment-angular-slices sub-segment-count] (compute-sub-segment angular-slice->avg-dsr+num-cells angular-slices cells-needed)
                    sub-segment-count                              (long sub-segment-count)]
                (recur rest-to-process (- cells-needed sub-segment-count) (into angular-slices-to-suppress sub-segment-angular-slices)))))
          [angular-slices-to-suppress (- num-cells-to-suppress cells-needed)])))))

(defn- average
  [coll]
  (/ (double (reduce + coll)) (long (count coll))))

(defn- compute-avg-dsr
  [burn-vectors]
  (-> (reduce (fn ^double [^double acc burn-vector]
                (+ acc (double (:spread-rate burn-vector))))
              0.0
              burn-vectors)
      double
      (/ (count burn-vectors))))

(defn- compute-cell-count
  [burn-vectors]
  (count
   (into #{}
         (map (juxt :i :j))
         burn-vectors)))

(defn- compute-avg-dsr-data
  "Returns a sorted map where each map entry:

  [`angular-slice` [`directional-flame-length` `cell-count`]]

  represents a collection of stats computed for a `angular-slice`. The
  `directional-flame-length`is the average value among the active
  perimeter cells that fall within that angular-slice. The `cell-count` is the
  count of those perimeter cells."
  [^double angular-slice-size angular-slice->BurnVectors]
  (reduce (fn [acc angular-slice]
            (let [burn-vectors (get angular-slice->BurnVectors angular-slice)]
              (if (seq burn-vectors)
                (assoc acc angular-slice [(compute-avg-dsr burn-vectors) (compute-cell-count burn-vectors)])
                (assoc acc angular-slice [0.0 0.0]))))
          (sorted-map)
          (range 0.0 (inc (/ 360 angular-slice-size)))))

(defn- angle-cw-from-east ^double
  [^long i1 ^long j1 ^long i0 ^long j0]
  (let [di    (- i1 i0)
        dj    (- j1 j0)
        theta (rad->deg (Math/atan2 di dj))]
    (if (neg? di)
      (+ theta 360.0)
      theta)))

(defn- nearest-angular-slice ^double
  [^double theta ^double angular-slice-size]
  (Math/floor (/ theta angular-slice-size)))

(defn- group-burn-vectors
  "Returns a map where each entry:

  [`angular-slice` [BurnVector BurnVector ...]]

  represents a collection of BurnVector that that fall within a `angular-slice`.
  The `angular-slice` is defined as the degree clockwise from EAST of the
  `centroid`cell. angular-slice 0 = East = 0.0 degrees."
  [centroid angular-slice-size burn-vectors]
  (let [[i0 j0] centroid]
    (group-by (fn [burn-vector] (-> (angle-cw-from-east (:i burn-vector) (:j burn-vector) i0 j0)
                                    (nearest-angular-slice angular-slice-size)))
              burn-vectors)))

(defn- compute-centroid-cell
  "Returns [i j] that is the centroid of a given list of [i j] cells"
  [cells]
  (let [row (average (mapv #(nth % 0) cells))
        col (average (mapv #(nth % 1) cells))]
    [(long row) (long col)]))

(defn- compute-fraction-contained ^double
  [^double max-runtime-fraction ^double suppression-coefficient]
  (Math/pow (/ (* 2.0 max-runtime-fraction)
               (+ 1.0 (Math/pow max-runtime-fraction 2.0)))
            suppression-coefficient))

(defn suppress-burn-vectors
  [max-runtime-fraction suppression-coefficient previous-num-perimeter-cells previous-suppressed-count burn-vectors]
  (let [max-runtime-fraction         (double max-runtime-fraction)
        suppression-coefficient      (double suppression-coefficient)
        previous-num-perimeter-cells (long previous-num-perimeter-cells)
        previous-suppressed-count    (long previous-suppressed-count)
        active-perimeter-cells       (into #{}
                                           (map (juxt :i :j))
                                           burn-vectors)
        fraction-contained           (compute-fraction-contained max-runtime-fraction suppression-coefficient)
        num-tracked-perimeter-cells  (+ (long (count active-perimeter-cells)) previous-suppressed-count)
        num-fizzled-perimeter-cells  (max 0 (- previous-num-perimeter-cells num-tracked-perimeter-cells))
        num-perimeter-cells          (max previous-num-perimeter-cells num-tracked-perimeter-cells)
        current-suppressed-count     (+ previous-suppressed-count num-fizzled-perimeter-cells)
        next-suppressed-count        (long (* fraction-contained num-perimeter-cells))
        num-cells-to-suppress        (- next-suppressed-count current-suppressed-count)]
    (if (> num-cells-to-suppress 0)
      (let [centroid-cell                  (compute-centroid-cell active-perimeter-cells)
            angular-slice-size             5.0
            angular-slice->BurnVectors     (group-burn-vectors centroid-cell angular-slice-size burn-vectors)
            [angular-slices-to-suppress
             suppressed-count]             (->> (compute-avg-dsr-data angular-slice-size angular-slice->BurnVectors)
                                                (compute-angular-slices-to-suppress num-cells-to-suppress))
            angular-slices-to-suppress-set (set angular-slices-to-suppress)
            angular-slices-to-keep         (remove #(contains? angular-slices-to-suppress-set %) (keys angular-slice->BurnVectors))
            burn-vectors-to-keep           (into []
                                                 (mapcat #(get angular-slice->BurnVectors %))
                                                 angular-slices-to-keep)]
        [burn-vectors-to-keep (+ current-suppressed-count ^long suppressed-count) num-perimeter-cells])
      [burn-vectors current-suppressed-count num-perimeter-cells])))
