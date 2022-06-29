(ns gridfire.suppression
  (:require [gridfire.conversion :refer [rad->deg]]))

(set! *unchecked-math* :warn-on-boxed)

(defn- combine-average
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

(defn- compute-contiguous-bins
  "Return a sorted map of avg-dsr -> [list-of-bins cell-count]
  given number of cells to suppress and a map of average directional
  spread rate data with the form: degree-bins -> [average-dsr cell-count]"
  [^long num-cells-to-suppress avg-dsr-data]
  (loop [sorted-contiguous-bins (sorted-map)
         bin-data               (into [] (seq avg-dsr-data))
         cur-contiguous-bins    '()
         cur-dsr                0
         cur-count              0
         i                      -1
         j                      0]
    (cond

      (= i 0)
      sorted-contiguous-bins

      ;; Do not include already suppressed regions in the longest
      ;; contiguous bin calculation.
      (let [[_ [_ cell-count]] (nth bin-data j)
            cell-count         (double cell-count)]
        (and (< cur-count num-cells-to-suppress) (zero? cell-count)))
      (let [next-j (if (= j (dec (count bin-data)))
                     0
                     (+ j 1))]
        (recur (if (seq cur-contiguous-bins)
                 (assoc sorted-contiguous-bins cur-dsr [cur-contiguous-bins cur-count])
                 sorted-contiguous-bins)
               bin-data
               '()
               0
               0
               (if (< j i) 0 next-j)
               next-j))

      (< cur-count num-cells-to-suppress)
      ;; expand right
      (let [[bin [avg-dsr cell-count]] (nth bin-data j)
            cell-count                 (long cell-count)]
        (recur sorted-contiguous-bins
               bin-data
               (conj cur-contiguous-bins bin)
               (combine-average cur-dsr cur-count avg-dsr cell-count)
               (+ cur-count cell-count)
               i
               (if (= j (dec (count bin-data)))
                 0
                 (+ j 1))))

      :else
      ;; shrink left
      (let [[_ [avg-dsr cell-count]] (nth bin-data (if (= -1 i) 0 i))
            cell-count               (long cell-count)]
        (recur (assoc sorted-contiguous-bins cur-dsr [cur-contiguous-bins cur-count])
               bin-data
               (drop-last cur-contiguous-bins)
               (remove-average cur-dsr cur-count avg-dsr cell-count)
               (- cur-count cell-count)
               (long
                (cond
                  (= i (dec (count bin-data))) 0
                  (= -1 i)                     1
                  :else                        (+ i 1)))
               j)))))

(defn- compute-sub-segment
  [grouped-burn-vectors bins cells-needed]
  (let [bins                  (set bins)
        avg-dsr-data          (reduce (fn [acc [bin avg-dsr-data]]
                                        (if (contains? bins bin)
                                          (assoc acc bin avg-dsr-data)
                                          (assoc acc bin [0.0 0.0]))) ;; Needed because the segment should not be treated as circular list
                                      (sorted-map)
                                      grouped-burn-vectors)
        contiguous-bins       (compute-contiguous-bins cells-needed avg-dsr-data)
        [_ [bins cell-count]] (first contiguous-bins)]
    [bins cell-count]))

(defn- compute-bins-to-suppress
  "avg-dsr-data is a map of degree-bins -> [directional-flame-length cell-count]"
  [^long num-cells-to-suppress avg-dsr-data]
  (let [contiguous-bins       (compute-contiguous-bins num-cells-to-suppress avg-dsr-data)
        [_ [bins cell-count]] (first contiguous-bins)
        cell-count            (long cell-count)]
    (if (>= cell-count num-cells-to-suppress)
      [bins cell-count 0]
      (loop [[segment & rest-to-process] (rest contiguous-bins)
             cells-needed                (- num-cells-to-suppress cell-count)
             bins-to-suppress            bins]
        (if (and segment (pos? cells-needed))
          (let [[_ [bins cell-count]] segment
                cell-count            (long cell-count)]
            (if (<= cell-count cells-needed)
              (recur rest-to-process (- cells-needed cell-count) (into bins-to-suppress bins))
              (let [[sub-segment-bins sub-segment-count] (compute-sub-segment avg-dsr-data bins cells-needed)
                    sub-segment-count                    (long sub-segment-count)]
                (recur rest-to-process (- cells-needed sub-segment-count) (into bins-to-suppress sub-segment-bins)))))
          [bins-to-suppress (- num-cells-to-suppress cells-needed)])))))

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
  [^double bin-size grouped-burn-vectors]
  (reduce (fn [acc bin]
            (let [burn-vectors (get grouped-burn-vectors bin)]
              (if (seq burn-vectors)
                (assoc acc bin [(compute-avg-dsr burn-vectors) (compute-cell-count burn-vectors)])
                (assoc acc bin [0.0 0.0]))))
          (sorted-map)
          (range 0.0 (inc (/ 360 bin-size)))))

(defn- angle-cw-from-east ^double
  [^long i1 ^long j1 ^long i0 ^long j0]
  (let [di    (- i1 i0)
        dj    (- j1 j0)
        theta (rad->deg (Math/atan2 di dj))]
    (if (neg? di)
      (+ theta 360.0)
      theta)))

(defn- nearest-degree-bin ^double
  [^double theta ^double bin-size]
  (Math/floor (/ theta bin-size)))

(defn- group-burn-vectors
  "Returns a map of bin->vector of BurnVector grouped by bin-size
  clockwise from EAST of centroid cell. Bin 0 = East = 90
  degrees"
  [centroid bin-size burn-vectors]
  (let [[i0 j0] centroid]
    (group-by (fn [burn-vector] (-> (angle-cw-from-east (:i burn-vector) (:j burn-vector) i0 j0)
                                    (nearest-degree-bin bin-size)))
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
  [^double max-runtime-fraction ^double suppression-coefficient ^long previous-suppressed-count burn-vectors]
  (let [active-perimeter-cells (into #{}
                                     (map (juxt :i :j))
                                     burn-vectors)
        fraction-contained     (compute-fraction-contained max-runtime-fraction suppression-coefficient)
        num-cells-to-suppress  (max 0 (- (long (* fraction-contained (+ (long (count active-perimeter-cells)) previous-suppressed-count)))
                                         previous-suppressed-count))]
    (if (> num-cells-to-suppress 0)
      (let [centroid-cell        (compute-centroid-cell active-perimeter-cells)
            bin-size             5.0
            grouped-burn-vectors (group-burn-vectors centroid-cell bin-size burn-vectors)
            [bins-to-suppress
             suppressed-count]   (->> (compute-avg-dsr-data bin-size grouped-burn-vectors)
                                      (compute-bins-to-suppress num-cells-to-suppress))
            bins-to-keep         (remove #(contains? (set bins-to-suppress) %) (keys grouped-burn-vectors))
            burn-vectors-to-keep (into []
                                       (mapcat #(get grouped-burn-vectors %))
                                       bins-to-keep)]
        [burn-vectors-to-keep (+ previous-suppressed-count ^long suppressed-count)])
      [burn-vectors previous-suppressed-count])))
