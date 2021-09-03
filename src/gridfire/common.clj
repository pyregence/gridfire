(ns gridfire.common
  (:require
   [clojure.core.matrix :as m]
   [gridfire.perturbation :as perturbation]))

(defn matrix-value-at ^double
  [[i j] ^double global-clock matrix]
  (if (> (m/dimensionality matrix) 2)
    (let [band (int (quot global-clock 60.0))] ;Assuming each band is 1 hour
      (m/mget matrix band i j))
    (m/mget matrix i j)))

(defn sample-at
  ([here global-clock matrix multiplier]
   (sample-at here global-clock matrix multiplier nil))

  ([here global-clock matrix multiplier perturb-info]
   (let [cell       (if multiplier
                      (mapv #(quot ^long % ^long multiplier) here)
                      here)
         value-here (matrix-value-at cell global-clock matrix)]
     (if perturb-info
       (if-let [^long freq (:frequency perturb-info)]
         (+ value-here (perturbation/value-at perturb-info matrix cell (quot ^double global-clock freq)))
         (+ value-here (perturbation/value-at perturb-info matrix cell)))
       value-here))))

(defn get-value-at
  [here global-clock matrix-or-val multiplier perturb-info]
  (if (> (m/dimensionality matrix-or-val) 1)
    (sample-at here global-clock matrix-or-val multiplier perturb-info)
    matrix-or-val))

(defn calc-emc
  "Computes the Equilibrium Moisture Content (EMC) from rh (relative
   humidity in %) and temp (temperature in F)."
  ^double
  [^double rh ^double temp]
  (/ (double
      (cond (< rh 10)  (+ 0.03229 (* 0.281073 rh) (* -0.000578 rh temp))
            (< rh 50)  (+ 2.22749 (* 0.160107 rh) (* -0.01478 temp))
            :else (+ 21.0606 (* 0.005565 rh rh) (* -0.00035 rh temp) (* -0.483199 rh))))
     30))

(defn get-fuel-moisture [relative-humidity temperature]
  (let [equilibrium-moisture (calc-emc relative-humidity temperature)]
    {:dead {:1hr   (+ equilibrium-moisture 0.002)
            :10hr  (+ equilibrium-moisture 0.015)
            :100hr (+ equilibrium-moisture 0.025)}
     :live {:herbaceous (* equilibrium-moisture 2.0)
            :woody      (* equilibrium-moisture 0.5)}}))

(defn extract-fuel-moisture
  [fuel-moisture-layers multiplier-lookup here global-clock]
  (let [f (fn [path]
            (fn [raster] (sample-at here
                                    global-clock
                                    (:matrix raster)
                                    (get-in multiplier-lookup path))))]
    (-> fuel-moisture-layers
        (update-in [:dead :1hr] (f [:dead :1hr]))
        (update-in [:dead :10hr] (f [:dead :10hr]))
        (update-in [:dead :100hr] (f [:dead :100hr]))
        (update-in [:live :herbaceous] (f [:live :herbaceous]))
        (update-in [:live :woody] (f [:live :woody])))))

(defn fuel-moisture-from-raster
  "Returns a map of moisture
  {:dead {:1hr          (percent)
            :10hr       (percent)
            :100hr      (percent)}
     :live {:herbaceous (percent)
            :woody      (percent)}}"
  ([constants here]
   (fuel-moisture-from-raster constants here 0))

  ([{:keys [fuel-moisture-layers multiplier-lookup]} here global-clock]
   (when fuel-moisture-layers
     (extract-fuel-moisture fuel-moisture-layers multiplier-lookup here global-clock))))

(defn in-bounds?
  "Returns true if the point lies within the bounds [0,rows) by [0,cols)."
  [^long rows ^long cols [^long i ^long j]]
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
  "Returns true if cell [x y] has not yet been ignited (but could be)."
  [fire-spread-matrix fuel-model-matrix [i j :as source] [x y :as here]]
  (let [^double ignition-probability-here   (m/mget fire-spread-matrix x y)
        ^double source-ignition-probability (m/mget fire-spread-matrix i j)]
    (and (< ignition-probability-here ^double (if (= source-ignition-probability 0.0)
                                                1.0
                                                source-ignition-probability))
         (burnable-fuel-model? (m/mget fuel-model-matrix x y)))))

(defn get-neighbors
  "Returns the eight points adjacent to the passed-in point."
  [[i j]]
  (let [i (long i)
        j (long j)
        i- (- i 1)
        i+ (+ i 1)
        j- (- j 1)
        j+ (+ j 1)]
    (vector [i- j-] [i- j] [i- j+]
            [i  j-]        [i  j+]
            [i+ j-] [i+ j] [i+ j+])))

(defn burnable-neighbors?
  [fire-spread-matrix fuel-model-matrix num-rows num-cols cell]
  (some #(and (in-bounds? num-rows num-cols %)
              (burnable? fire-spread-matrix fuel-model-matrix cell %))
        (get-neighbors cell)))

(defn distance-3d
  "Returns the terrain distance between two points in feet."
  ^double
  [elevation-matrix ^double cell-size [i1 j1] [i2 j2]]
  (let [i1 (long i1)
        j1 (long j1)
        i2 (long i2)
        j2 (long j2)
        di (* cell-size (- i1 i2))
        dj (* cell-size (- j1 j2))
        dz (- ^double (m/mget elevation-matrix i1 j1)
              ^double (m/mget elevation-matrix i2 j2))]
    (Math/sqrt (+ (* di di) (* dj dj) (* dz dz)))))
