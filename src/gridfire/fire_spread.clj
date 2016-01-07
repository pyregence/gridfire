(ns gridfire.fire-spread
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.operators :as mop]
            [gridfire.fuel-models :refer [build-fuel-model moisturize]]
            [gridfire.surface-fire :refer [rothermel-surface-fire-spread-no-wind-no-slope
                                           rothermel-surface-fire-spread-max
                                           rothermel-surface-fire-spread-any
                                           anderson-flame-depth byram-fire-line-intensity
                                           byram-flame-length wind-adjustment-factor]]
            [gridfire.crown-fire :refer [van-wagner-crown-fire-initiation?
                                         cruz-crown-fire-spread
                                         crown-fire-line-intensity]]))

(m/set-current-implementation :vectorz)

;; for surface fire, tau = 10 mins, t0 = 0, and t = global-clock
;; for crown fire, tau = 20 mins, t0 = time of first torch, t = global-clock
;; (defn lautenberger-spread-acceleration
;;   [equilibrium-spread-rate t0 t tau]
;;   (* equilibrium-spread-rate (- 1.0 (Math/exp (/ (- t0 t 0.2) tau)))))
;;
;; Note: Because of our use of adaptive timesteps, if the spread rate on
;;       the first timestep is not at least 83 ft/min, then the timestep will
;;       be calculated as greater than 60 minutes, which will terminate the
;;       one hour fire simulation instantly.
(defn lautenberger-spread-acceleration
  [equilibrium-spread-rate t0 t tau]
  equilibrium-spread-rate)

(defn random-cell
  "Returns a random [i j] pair with i < num-rows and j < num-cols."
  [num-rows num-cols]
  [(rand-int num-rows)
   (rand-int num-cols)])

(defn get-neighbors
  "Returns the eight points adjacent to the passed-in point."
  [[i j]]
  (let [i- (- i 1)
        i+ (+ i 1)
        j- (- j 1)
        j+ (+ j 1)]
    (vector [i- j-] [i- j] [i- j+]
            [i  j-]        [i  j+]
            [i+ j-] [i+ j] [i+ j+])))

(defn in-bounds?
  "Returns true if the point lies within the bounds [0,rows) by [0,cols)."
  [rows cols [i j]]
  (and (>= i 0)
       (>= j 0)
       (< i rows)
       (< j cols)))

(defn burnable-fuel-model?
  [^double number]
  (and (pos? number)
       (or (< number 91.0)
           (> number 99.0))))

(defn burnable?
  "Returns true if cell [i j] has not yet been ignited (but could be)."
  [ignition-matrix fuel-model-matrix [i j]]
  (and (pos? (m/mget ignition-matrix i j))
       (burnable-fuel-model? (m/mget fuel-model-matrix i j))))

(defn distance-3d
  "Returns the terrain distance between two points in feet."
  [elevation-matrix cell-size [i1 j1] [i2 j2]]
  (let [di (* cell-size (- i1 i2))
        dj (* cell-size (- j1 j2))
        dz (- (m/mget elevation-matrix i1 j1)
              (m/mget elevation-matrix i2 j2))]
    (Math/sqrt (+ (* di di) (* dj dj) (* dz dz)))))

(def offset-to-degrees
  "Returns clockwise degrees from north."
  {[-1  0]   0.0   ; N
   [-1  1]  45.0   ; NE
   [ 0  1]  90.0   ; E
   [ 1  1] 135.0   ; SE
   [ 1  0] 180.0   ; S
   [ 1 -1] 225.0   ; SW
   [ 0 -1] 270.0   ; W
   [-1 -1] 315.0}) ; NW

(defn rothermel-fast-wrapper [fuel-model-number fuel-moisture]
  (let [fuel-model      (-> (build-fuel-model (int fuel-model-number))
                            (moisturize fuel-moisture))
        spread-info-min (rothermel-surface-fire-spread-no-wind-no-slope fuel-model)]
    [fuel-model spread-info-min]))
(def rothermel-fast-wrapper (memoize rothermel-fast-wrapper))

(defn compute-neighborhood-fire-spread-rates!
  "Returns a vector of {:cell [i j], :trajectory [di dj], :terrain-distance ft,
  :spread-rate ft/min} for each cell adjacent to here."
  [ignition-matrix flame-length-matrix fire-line-intensity-matrix landfire-layers
   wind-speed-20ft wind-from-direction fuel-moisture foliar-moisture
   ellipse-adjustment-factor cell-size num-rows num-cols global-clock
   [i j :as here]]
  (let [fuel-model-number   (m/mget (:fuel-model         landfire-layers) i j)
        slope               (m/mget (:slope              landfire-layers) i j)
        aspect              (m/mget (:aspect             landfire-layers) i j)
        canopy-height       (m/mget (:canopy-height      landfire-layers) i j)
        canopy-base-height  (m/mget (:canopy-base-height landfire-layers) i j)
        crown-bulk-density  (m/mget (:crown-bulk-density landfire-layers) i j)
        canopy-cover        (m/mget (:canopy-cover       landfire-layers) i j)
        [fuel-model spread-info-min] (rothermel-fast-wrapper fuel-model-number fuel-moisture)
        midflame-wind-speed (* wind-speed-20ft 88.0
                               (wind-adjustment-factor (:delta fuel-model)
                                                       canopy-height
                                                       canopy-cover)) ; mi/hr -> ft/min
        spread-info-max     (rothermel-surface-fire-spread-max
                             spread-info-min midflame-wind-speed wind-from-direction
                             slope aspect ellipse-adjustment-factor)
        fire-line-intensity (byram-fire-line-intensity
                             (:reaction-intensity spread-info-min)
                             (anderson-flame-depth (:max-spread-rate spread-info-max)
                                                   (:residence-time spread-info-min)))
        crown-fire?         (van-wagner-crown-fire-initiation? canopy-cover canopy-base-height
                                                               foliar-moisture fire-line-intensity)
        crown-spread-rate   (if crown-fire?
                              (cruz-crown-fire-spread wind-speed-20ft crown-bulk-density
                                                      (-> fuel-moisture :dead :1hr))
                              0.0)
        spread-info-max     (update spread-info-max :max-spread-rate max crown-spread-rate)
        fire-line-intensity (if crown-fire?
                              (+ fire-line-intensity
                                 (crown-fire-line-intensity
                                  crown-spread-rate
                                  crown-bulk-density
                                  canopy-height
                                  canopy-base-height
                                  (-> fuel-model :h :dead :1hr)))
                              fire-line-intensity)
        flame-length        (byram-flame-length fire-line-intensity)]
    (m/mset! flame-length-matrix        i j flame-length)
    (m/mset! fire-line-intensity-matrix i j fire-line-intensity)
    (into []
          (comp
           (filter #(and (in-bounds? num-rows num-cols %)
                         (burnable? ignition-matrix (:fuel-model landfire-layers) %)))
           (map (fn [neighbor]
                  (let [trajectory  (mop/- neighbor here)
                        spread-rate (rothermel-surface-fire-spread-any
                                     spread-info-max (offset-to-degrees trajectory))]
                    {:cell             neighbor
                     :trajectory       trajectory
                     :terrain-distance (distance-3d (:elevation landfire-layers)
                                                    cell-size here neighbor)
                     :spread-rate      spread-rate}))))
          (get-neighbors here))))

(defn update-ignition-matrix!
  [ignition-matrix fuel-model-matrix burn-front timestep num-rows num-cols]
  (let [heating-values
        (reduce (fn [heating-values {:keys [cell trajectory terrain-distance spread-rate]}]
                  (let [[i j]            cell
                        percent-heated   (/ (* spread-rate timestep) terrain-distance)
                        percent-unheated (- (m/mget ignition-matrix i j) percent-heated)
                        heating-values   (if (> percent-heated (get heating-values cell 0.0))
                                           (assoc! heating-values cell percent-heated)
                                           heating-values)]
                    (if (neg? percent-unheated)
                      (let [overflow-cell (mop/+ cell trajectory)
                            overflow-heat (* -1.0 percent-unheated)]
                        (if (and (in-bounds? num-rows num-cols overflow-cell)
                                 (burnable? ignition-matrix fuel-model-matrix overflow-cell))
                          (if (> overflow-heat (get heating-values overflow-cell 0.0))
                            (assoc! heating-values overflow-cell overflow-heat)
                            heating-values)
                          heating-values))
                      heating-values)))
                (transient {})
                burn-front)]
    (doseq [[[i j] percent-heated] (persistent! heating-values)]
      (m/mset! ignition-matrix i j
               (max 0.0 (- (m/mget ignition-matrix i j) percent-heated))))))

(defn burnable-neighbors?
  [ignition-matrix fuel-model-matrix num-rows num-cols cell]
  (some #(and (in-bounds? num-rows num-cols %)
              (burnable? ignition-matrix fuel-model-matrix %))
        (get-neighbors cell)))

(defn update-ignited-cells
  [ignition-matrix fuel-model-matrix burn-front ignited-cells num-rows num-cols]
  (let [all-ignited-cells (into ignited-cells
                                (comp
                                 (map :cell)
                                 (filter (fn [[i j]] (zero? (m/mget ignition-matrix i j)))))
                                burn-front)]
    (into #{}
          (filter #(burnable-neighbors? ignition-matrix fuel-model-matrix
                                        num-rows num-cols %))
          all-ignited-cells)))

(defn select-random-ignition-site
  [fuel-model-matrix]
  (let [num-rows        (m/row-count    fuel-model-matrix)
        num-cols        (m/column-count fuel-model-matrix)
        ignition-matrix (doto (m/zero-matrix num-rows num-cols) (mop/+= 1.0))]
    (loop [[i j :as ignition-site] (random-cell num-rows num-cols)]
      (if (and (burnable-fuel-model? (m/mget fuel-model-matrix i j))
               (burnable-neighbors? ignition-matrix fuel-model-matrix
                                    num-rows num-cols ignition-site))
        ignition-site
        (recur (random-cell num-rows num-cols))))))

(defn run-fire-spread
  "Runs the raster-based fire spread model with these arguments:
  - max-runtime: double (minutes)
  - cell-size: double (feet)
  - elevation-matrix: core.matrix 2D double array (feet)
  - slope-matrix: core.matrix 2D double array (vertical feet/horizontal feet)
  - aspect-matrix: core.matrix 2D double array (degrees clockwise from north)
  - fuel-model-matrix: core.matrix 2D double array (fuel model numbers 1-256)
  - canopy-height-matrix: core.matrix 2D double array (feet)
  - canopy-base-height-matrix: core.matrix 2D double array (feet)
  - crown-bulk-density-matrix: core.matrix 2D double array (lb/ft^3)
  - canopy-cover-matrix: core.matrix 2D double array (0-100)
  - wind-speed-20ft: double (miles/hour)
  - wind-from-direction: double (degrees clockwise from north)
  - fuel-moisture: doubles (%){:dead {:1hr :10hr :100hr} :live {:herbaceous :woody}}
  - foliar-moisture: double (%)
  - ellipse-adjustment-factor: (< 1.0 = more circular, > 1.0 = more elliptical)
  - initial-ignition-site: point represented as [row col] (randomly chosen if omitted)"
  ([max-runtime cell-size landfire-layers wind-speed-20ft wind-from-direction
    fuel-moisture foliar-moisture ellipse-adjustment-factor]
   (let [ignition-site (select-random-ignition-site (:fuel-model landfire-layers))]
     (run-fire-spread max-runtime cell-size landfire-layers wind-speed-20ft
                      wind-from-direction fuel-moisture foliar-moisture
                      ellipse-adjustment-factor ignition-site)))
  ([max-runtime cell-size landfire-layers wind-speed-20ft wind-from-direction
    fuel-moisture foliar-moisture ellipse-adjustment-factor
    [i j :as initial-ignition-site]]
   ;;(println "Fire ignited at" initial-ignition-site)
   (let [fuel-model-matrix (:fuel-model landfire-layers)
         num-rows          (m/row-count fuel-model-matrix)
         num-cols          (m/column-count fuel-model-matrix)
         ignition-matrix   (doto (m/zero-matrix num-rows num-cols) (mop/+= 1.0))]
     (when (and (in-bounds? num-rows num-cols initial-ignition-site)
                (burnable? ignition-matrix fuel-model-matrix initial-ignition-site)
                (burnable-neighbors? ignition-matrix fuel-model-matrix
                                     num-rows num-cols initial-ignition-site))
       (loop [global-clock               0.0
              ignited-cells              #{initial-ignition-site}
              ignition-matrix            (doto ignition-matrix (m/mset! i j 0.0))
              flame-length-matrix        (m/zero-matrix num-rows num-cols)
              fire-line-intensity-matrix (m/zero-matrix num-rows num-cols)]
         (if (and (< global-clock max-runtime)
                  (seq ignited-cells))
           (let [burn-front (into []
                                  (mapcat #(compute-neighborhood-fire-spread-rates!
                                            ignition-matrix
                                            flame-length-matrix
                                            fire-line-intensity-matrix
                                            landfire-layers
                                            wind-speed-20ft
                                            wind-from-direction
                                            fuel-moisture
                                            foliar-moisture
                                            ellipse-adjustment-factor
                                            cell-size
                                            num-rows
                                            num-cols
                                            global-clock
                                            %))
                                  ignited-cells)
                 timestep   (let [dt (/ cell-size (transduce (map :spread-rate) max 0.0 burn-front))]
                              (if (> (+ global-clock dt) max-runtime)
                                (- max-runtime global-clock)
                                dt))]
             ;; (println "Clock:"         global-clock
             ;;          "Timestep:"      timestep
             ;;          "Ignited Cells:" (count ignited-cells)
             ;;          "Burn Front:"    (count burn-front))
             (update-ignition-matrix! ignition-matrix fuel-model-matrix burn-front
                                      timestep num-rows num-cols)
             (recur (+ global-clock timestep)
                    (update-ignited-cells ignition-matrix fuel-model-matrix burn-front
                                          ignited-cells num-rows num-cols)
                    ignition-matrix
                    flame-length-matrix
                    fire-line-intensity-matrix))
           (do
             (doseq [cell ignited-cells]
               (compute-neighborhood-fire-spread-rates!
                ignition-matrix
                flame-length-matrix
                fire-line-intensity-matrix
                landfire-layers
                wind-speed-20ft
                wind-from-direction
                fuel-moisture
                foliar-moisture
                ellipse-adjustment-factor
                cell-size
                num-rows
                num-cols
                global-clock
                cell))
             {:global-clock               global-clock
              :initial-ignition-site      initial-ignition-site
              :ignited-cells              ignited-cells
              :fire-spread-matrix         (m/emap #(- 1.0 %) ignition-matrix)
              :flame-length-matrix        flame-length-matrix
              :fire-line-intensity-matrix fire-line-intensity-matrix})))))))
