;; [[file:../../org/GridFire.org::suppression][suppression]]
(ns gridfire.suppression
  "An algorithm emulating human interventions reacting to fire spread
  by suppressing ('putting out') chosen contiguous segments of the
  fire front, typically backing and flanking fires."
  (:require [gridfire.conversion :refer [cells->acres
                                         min->day
                                         percent->dec
                                         rad->deg]]))

(set! *unchecked-math* :warn-on-boxed)

(defn- combine-average ^double
  [^double avg-old ^long count-old ^double avg-new ^long count-new]
  (if (zero? (+ count-old count-new))
    0.0
    (/ (+ (* avg-old count-old)
          (* avg-new count-new))
       (+ count-old count-new))))

(defn- remove-average ^double
  [^double avg-old ^long count-old ^double avg-to-remove ^long count-of-avg-to-remove]
  (if (zero? (- count-old count-of-avg-to-remove))
    0.0
    (/ (- (* avg-old count-old) (* avg-to-remove count-of-avg-to-remove))
       (- count-old count-of-avg-to-remove))))

(defn- compute-contiguous-slices
  "Given number of cells to suppress and a map of average directional spread
  rate data with the form: angular-slice -> [average-dsr cell-count] return a
  sorted map where each map entry:

  [[`list-of-slices` `avg-dsr`] `cell-count`]

  represents a contiguous segment of the fire front, which we locate
  by `list-of-slices`, the list of successive degree slices covering
  it; `cell-count` represents the number of active perimeter cells in
  that segment, with `cell-count` no smaller than
  `num-cells-to-suppress`, but possibly bigger.

  NOTE: This constraint may be violated if a segment is adjacent to an
  already suppressed slice, in which case the segment will be included
  in the returned map even if its `cell-count` is smaller than
  `num-cells-to-suppress`.

  Note that the returned segments will tend to overlap - think of a
  sliding window of (up to `num-cells-to-suppress`) contiguous active
  cells, rotating around the centroid: the segments returned by this
  function are regular snapshots of this window."
  [^long num-cells-to-suppress angular-slice->avg-dsr+num-cells]
  (loop [sorted-contiguous-slices (sorted-map-by (fn [[_ x1] [_ x2]]
                                                   (compare x1 x2)))
         slice-data               (into [] (seq angular-slice->avg-dsr+num-cells))
         cur-contiguous-slices    []
         cur-dsr                  0.0
         cur-count                0
         left-idx                 -1
         right-idx                0]
    (cond

      (= left-idx 0)
      sorted-contiguous-slices

      ;; Do not include already suppressed regions in the longest
      ;; contiguous slice calculation.
      (let [[_ [_ cell-count]] (nth slice-data right-idx)
            cell-count         (long cell-count)]
        (and (< cur-count num-cells-to-suppress) (zero? cell-count)))
      (let [next-right-idx (if (= right-idx (dec (count slice-data)))
                             0
                             (+ right-idx 1))]
        (recur (if (seq cur-contiguous-slices)
                 (assoc sorted-contiguous-slices [cur-contiguous-slices cur-dsr] cur-count)
                 sorted-contiguous-slices)
               slice-data
               []
               0.0
               0
               (if (< right-idx left-idx) 0 next-right-idx)
               next-right-idx))

      (< cur-count num-cells-to-suppress)
      ;; expand right
      (let [[slice [avg-dsr cell-count]] (nth slice-data right-idx)
            cell-count                   (long cell-count)]
        (recur sorted-contiguous-slices
               slice-data
               (conj cur-contiguous-slices slice)
               (combine-average cur-dsr cur-count avg-dsr cell-count)
               (+ cur-count cell-count)
               left-idx
               (if (= right-idx (dec (count slice-data)))
                 0
                 (+ right-idx 1))))

      :else
      ;; shrink left
      (let [[_ [avg-dsr cell-count]] (nth slice-data (if (= -1 left-idx) 0 left-idx))
            cell-count               (long cell-count)]
        (recur (assoc sorted-contiguous-slices [cur-contiguous-slices cur-dsr] cur-count)
               slice-data
               (subvec cur-contiguous-slices 1)
               (remove-average cur-dsr cur-count avg-dsr cell-count)
               (- cur-count cell-count)
               (long
                (cond
                  (= left-idx (dec (count slice-data))) 0
                  (= -1 left-idx)                       1
                  :else                                 (+ left-idx 1)))
               right-idx)))))

(defn- compute-sub-segment
  [angular-slice->avg-dsr+num-cells slices cells-needed]
  (let [slices                           (set slices)
        angular-slice->avg-dsr+num-cells (reduce (fn [acc [slice avg-dsr+num-cells]]
                                                   (if (contains? slices slice)
                                                     (assoc acc slice avg-dsr+num-cells)
                                                     (assoc acc slice [0.0 0.0]))) ;; Needed because the segment should not be treated as circular list
                                                 (sorted-map)
                                                 angular-slice->avg-dsr+num-cells) ;; FIXME: This seems inefficient
        contiguous-slices                (compute-contiguous-slices cells-needed angular-slice->avg-dsr+num-cells)
        [[slices _] cell-count]          (first contiguous-slices)]
    [slices cell-count]))

(defn- compute-slices-to-suppress
  "Chooses slices to be suppressed, and reports the number of suppressed cells.

  Given:
  - num-cells-to-suppress: a number of cells, the suppression objective,
  - angular-slice->avg-dsr+num-cells: a map of statistics over all angular slices,

  returns a tuple [slices-to-suppress suppressed-count], in which:
  - slices-to-suppress is the set of angular slices to suppress, chosen as a tradeoff
  between contiguity, closeness to the num-cells-to-suppress objective,
  and low average spread rate.
  - suppressed-count is the number of cells in slices-to-suppress,
  which is returned to save callers the work of re-computing it.
  Warning: it may well be that suppressed-count > num-cells-to-suppress.

  This algorithm will convert `angular-slice->avg-dsr+num-cells` to a sorted map of
  `angular-slices+avg-dsr->num-cells`, representing candidate segments for suppression.
  Using this map the algorithm will collect the sequence of angular-slices
  until we have a cell-count of at least `num-cells-to-suppress`, if possible."
  [^long num-cells-to-suppress angular-slice->avg-dsr+num-cells]
  ;; NOTE this algorithm is most likely under-optimized;
  ;; having said that, it's probably not a performance bottleneck
  ;; (suppression events are typically few and far between)
  ;; and experience has shown that we'd better make this right
  ;; before making it fast.
  ;; TODO enhance performance or rethink the overall suppression algorithm.
  (letfn [(n-cells-in-slices ^long [slices]
            (transduce (map (fn n-cells-in-slice [slice]
                              (let [[_ num-cells] (get angular-slice->avg-dsr+num-cells slice)]
                                num-cells)))
                       (completing +)
                       0
                       slices))]
    (let [angular-slices+avg-dsr->num-cells (compute-contiguous-slices num-cells-to-suppress angular-slice->avg-dsr+num-cells)]
      (loop [remaining-segments angular-slices+avg-dsr->num-cells
             n-cells-needed     num-cells-to-suppress
             slices-to-suppress #{}]
        (if-some [segment (when (pos? n-cells-needed)
                            (first remaining-segments))]
          (let [[[slices _]]            segment
                yet-unsuppressed-slices (remove slices-to-suppress slices)
                n-would-be-suppressed   (long (n-cells-in-slices yet-unsuppressed-slices))]
            (if (<= n-would-be-suppressed n-cells-needed)
              (let [new-n-cells-needed     (- n-cells-needed n-would-be-suppressed)
                    new-slices-to-suppress (into slices-to-suppress yet-unsuppressed-slices)]
                (recur (rest remaining-segments)
                       new-n-cells-needed
                       new-slices-to-suppress))
              ;; this segment has more than we need, compute subsegment:
              (let [[sub-segment-slices _] (compute-sub-segment angular-slice->avg-dsr+num-cells slices n-cells-needed)
                    n-more-suppressed      (long (n-cells-in-slices (remove slices-to-suppress sub-segment-slices)))
                    new-n-cells-needed     (- n-cells-needed n-more-suppressed)
                    new-slices-to-suppress (into slices-to-suppress sub-segment-slices)]
                (recur (rest remaining-segments)
                       new-n-cells-needed
                       new-slices-to-suppress))))
          ;; no more segments needed or available, so we return:
          (let [n-suppressed (- num-cells-to-suppress n-cells-needed)]
            [slices-to-suppress n-suppressed]))))))

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

  [`angular-slice` [`directional-spread-rate` `cell-count`]]

  represents a collection of stats computed for an `angular-slice`. The
  `directional-spread-rate` is the average value among the active
  perimeter cells that fall within that slice. The `cell-count` is the
  count of those perimeter cells."
  [^double angular-slice-size slice->BurnVectors]
  (reduce (fn [acc slice]
            (let [burn-vectors (get slice->BurnVectors slice)]
              (if (seq burn-vectors)
                (assoc acc slice [(compute-avg-dsr burn-vectors) (compute-cell-count burn-vectors)])
                (assoc acc slice [0.0 0.0]))))
          (sorted-map)
          (range 0.0 (/ 360.0 angular-slice-size))))

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

  represents a collection of BurnVectors that fall within an `angular-slice`.
  The `angular-slice` is defined as the degree clockwise from EAST of the
  `centroid` cell. angular-slice 0 = East = 0.0 degrees."
  [centroid ^double angular-slice-size burn-vectors]
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

(defn- compute-suppression-difficulty-factor ^double
  [^double sdi-sensitivity-to-difficulty ^double change-in-fraction-contained-sign-multiplier ^double mean-sdi]
  (double
   (if (>= change-in-fraction-contained-sign-multiplier 0.0)
     (Math/exp (* -1.0 sdi-sensitivity-to-difficulty mean-sdi))
     (Math/exp (* sdi-sensitivity-to-difficulty mean-sdi)))))

(defn- compute-mean-sdi ^double
  [get-suppression-difficulty-index ignited-cells]
  (/ (double
      (reduce (fn ^double [^double acc [i j]]
                (+ acc (double (get-suppression-difficulty-index i j))))
              0.0
              ignited-cells))
     (count ignited-cells)))

(defn- compute-area-growth-rate ^double
  [^double cell-size ^double suppression-dt ignited-cells-since-last-suppression]
  (/ (cells->acres cell-size (count ignited-cells-since-last-suppression))
     (min->day suppression-dt)))

(def ^:const sdi-reference-areal-growth-rate
  "[ac/day] a shape parameter for the suppression curve, the area-growth-rate A_d at which ΔC/Δt=χ in 0-SDI settings,
  in which χ is the :sdi-reference-suppression-speed parameter."
  1.0)

(defn- compute-change-in-fraction-contained-sign-multiplier ^double
  [^double sdi-containment-overwhelming-area-growth-rate ^double area-growth-rate]
  (- 1.0
     ;; Note that the following ratio is insensitive to the choice of logarithm base.
     (/ (Math/log (/ area-growth-rate
                     sdi-reference-areal-growth-rate))
        (Math/log (/ sdi-containment-overwhelming-area-growth-rate
                     sdi-reference-areal-growth-rate)))))

(defn- compute-fraction-contained-sdi
  "Compute the updated fraction contained using suppression difficulty index algorithm"
  [inputs ignited-cells-since-last-suppression ^double previous-fraction-contained]
  (let [cell-size                                     (double (:cell-size inputs))
        suppression                                   (:suppression inputs)
        get-suppression-difficulty-index              (:get-suppression-difficulty-index inputs)
        suppression-dt                                (double (:suppression-dt suppression))
        sdi-containment-overwhelming-area-growth-rate (double (:sdi-containment-overwhelming-area-growth-rate suppression))
        sdi-sensitivity-to-difficulty                 (double (:sdi-sensitivity-to-difficulty suppression))
        sdi-reference-suppression-speed               (double (:sdi-reference-suppression-speed suppression))
        area-growth-rate                              (compute-area-growth-rate cell-size suppression-dt ignited-cells-since-last-suppression)
        change-in-fraction-contained-sign-multiplier  (compute-change-in-fraction-contained-sign-multiplier sdi-containment-overwhelming-area-growth-rate
                                                                                                            area-growth-rate)
        mean-sdi                                      (compute-mean-sdi get-suppression-difficulty-index ignited-cells-since-last-suppression)
        suppression-difficulty-factor                 (compute-suppression-difficulty-factor sdi-sensitivity-to-difficulty
                                                                                             change-in-fraction-contained-sign-multiplier
                                                                                             mean-sdi)
        change-in-fraction-contained                  (-> (* sdi-reference-suppression-speed
                                                             change-in-fraction-contained-sign-multiplier
                                                             suppression-difficulty-factor)
                                                          (percent->dec)
                                                          (* (min->day suppression-dt)))]
    (max 0.0 (+ previous-fraction-contained change-in-fraction-contained))))

(defn- compute-fraction-contained-sc
  "Compute fraction contained using suppression curve algorithm"
  ^double
  [^double max-runtime-fraction ^double suppression-coefficient]
  (Math/pow (/ (* 2.0 max-runtime-fraction)
               (+ 1.0 (Math/pow max-runtime-fraction 2.0)))
            suppression-coefficient))

(defn suppress-burn-vectors
  [inputs
   max-runtime-fraction
   previous-num-perimeter-cells
   previous-suppressed-count
   burn-vectors
   ignited-cells-since-last-suppression
   previous-fraction-contained]
  (let [max-runtime-fraction         (double max-runtime-fraction)
        suppression-coefficient      (get-in inputs [:suppression
                                                     :suppression-coefficient])
        previous-num-perimeter-cells (long previous-num-perimeter-cells)
        previous-suppressed-count    (long previous-suppressed-count)
        active-perimeter-cells       (into #{}
                                           (map (juxt :i :j))
                                           burn-vectors)
        fraction-contained           (if suppression-coefficient
                                       (compute-fraction-contained-sc max-runtime-fraction
                                                                      (double suppression-coefficient))
                                       (compute-fraction-contained-sdi inputs
                                                                       ignited-cells-since-last-suppression
                                                                       previous-fraction-contained))
        num-tracked-perimeter-cells  (+ (long (count active-perimeter-cells)) previous-suppressed-count)
        num-fizzled-perimeter-cells  (max 0 (- previous-num-perimeter-cells num-tracked-perimeter-cells))
        num-perimeter-cells          (max previous-num-perimeter-cells num-tracked-perimeter-cells)
        current-suppressed-count     (+ previous-suppressed-count num-fizzled-perimeter-cells)
        next-suppressed-count        (long (* ^double fraction-contained num-perimeter-cells))
        num-cells-to-suppress        (- next-suppressed-count current-suppressed-count)]
    (if (> num-cells-to-suppress 0)
      (let [centroid-cell          (compute-centroid-cell active-perimeter-cells)
            angular-slice-size     5.0
            slice->BurnVectors     (group-burn-vectors centroid-cell angular-slice-size burn-vectors)
            [slices-to-suppress
             suppressed-count]     (->> (compute-avg-dsr-data angular-slice-size slice->BurnVectors)
                                        (compute-slices-to-suppress num-cells-to-suppress))
            slices-to-suppress-set (set slices-to-suppress)
            slices-to-keep         (remove #(contains? slices-to-suppress-set %) (keys slice->BurnVectors))
            burn-vectors-to-keep   (into []
                                         (mapcat #(get slice->BurnVectors %))
                                         slices-to-keep)]
        [burn-vectors-to-keep (+ current-suppressed-count ^long suppressed-count) num-perimeter-cells fraction-contained])
      [burn-vectors current-suppressed-count num-perimeter-cells fraction-contained])))
;; suppression ends here
