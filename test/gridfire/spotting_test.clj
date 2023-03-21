;; [[file:../../org/GridFire.org::gridfire.spotting-test][gridfire.spotting-test]]
(ns gridfire.spotting-test
  (:require [clojure.pprint              :as pp]
            [clojure.test                :refer [are deftest is testing use-fixtures run-tests]]
            [gridfire.conversion         :as c]
            [gridfire.grid-lookup        :as grid-lookup]
            [gridfire.spotting           :as spotting]
            [gridfire.spotting.sardoy    :as spotting-sardoy]
            [tech.v3.datatype            :as d]
            [tech.v3.datatype.functional :as dfn]
            [tech.v3.tensor              :as t])
  (:import java.util.Random))

(def ^:private seed 123456789)
(def ^{:dynamic true :private true} *rand-gen* nil)

(defn- create-random-generator [f]
  (binding [*rand-gen* (Random. seed)]
    (f)))

(use-fixtures :each create-random-generator)

(defn- within? [^double a ^double b ^double epsilon]
  (> epsilon (Math/abs (- a b))))

(defn deltas-wind->coord
  "Converts deltas from the torched tree in the wind direction to deltas
  in the coordinate plane"
  [deltas ^double wind-towards-direction]
  (let [w-rad (c/deg->rad wind-towards-direction)
        cos-w (Math/cos w-rad)
        sin-w (Math/sin w-rad)]
    (mapv (fn [[delta-x delta-y]]
            (let [delta-x (double delta-x)
                  delta-y (double delta-y)
                  grid-dx (spotting/delta->grid-dx cos-w sin-w delta-x delta-y)
                  grid-dy (spotting/delta->grid-dy cos-w sin-w delta-x delta-y)]
              [grid-dx grid-dy]))
          deltas)))

(deftest ^:unit deltas-axis-test
  (testing "Simple 1-unit parallel to wind."
    (are [result args] (let [[dx-res dy-res] result
                             [dx dy]         (first (apply deltas-wind->coord args))]
                         (and (within? dx-res dx 0.01) (within? dy-res dy 0.01)))
         [0.0     -1.0]   [[[1 0]] 0]     ;; North
         [0.707   -0.707] [[[1 0]] 45]    ;; NE
         [1.0     0.0]    [[[1 0]] 90]    ;; East
         [0.707   0.707]  [[[1 0]] 135]   ;; SE
         [0.0     1.0]    [[[1 0]] 180]   ;; South
         [-0.707  0.707]  [[[1 0]] 225]   ;; SW
         [-1.0    0.0]    [[[1 0]] 270]   ;; West
         [-0.707  -0.707] [[[1 0]] 315]   ;; NW
         [0.0     -1.0]   [[[1 0]] 360])) ;; North

  (testing "1 unit parallel, 1 unit perpindicular to wind direction."
    (are [result args] (let [[dx-res dy-res] result
                             [dx dy]         (first (apply deltas-wind->coord args))]
                         (and (within? dx-res dx 0.01) (within? dy-res dy 0.01)))
         [1.0    -1.0]   [[[1 1]] 0]      ;; North
         [1.414  0.0]    [[[1 1]] 45]     ;; NE
         [1.0    1.0]    [[[1 1]] 90]     ;; East
         [0.0    1.414]  [[[1 1]] 135]    ;; SE
         [-1.0   1.0]    [[[1 1]] 180]    ;; South
         [-1.414 0.0]    [[[1 1]] 225]    ;; SW
         [-1.0   -1.0]   [[[1 1]] 270]    ;; West
         [0.0    -1.414] [[[1 1]] 315]    ;; NW
         [1.0    -1.0]   [[[1 1]] 360]))) ;; North

(defn hypotenuse
  ^double [^double x ^double y]
  (Math/sqrt (+ (* x x) (* y y))))

(defn firebrands
  "Returns a sequence of cells [i,j] that firebrands land in.
   Note: matrix index [i,j] refers to [row, column]. Therefore, we need to flip
   [row,column] to get to [x,y] coordinates."
  [deltas wind-towards-direction cell ^double cell-size]
  (let [[i0 j0]      cell
        coord-deltas (deltas-wind->coord deltas wind-towards-direction)]
    (mapv (fn [[grid-dx grid-dy]]
            (let [grid-dx (double grid-dx)
                  grid-dy (double grid-dy)
                  i1      (+ i0 (spotting/distance->n-cells cell-size grid-dy))
                  j1      (+ j0 (spotting/distance->n-cells cell-size grid-dx))
                  H       (hypotenuse grid-dx grid-dy)]
              [i1 j1 H]))
          coord-deltas)))

(deftest ^:unit firebrand-test
  (testing "Convert deltas to (i,j) cell in matrix."
    ; NOTE: matrix index [i,j] refers to [row, column]. Therefore, we need to flip
    ; [row,column] to get to [x,y] coordinates."
    (let [cell-size 10.0
          cell      [8 12]]
      (are [result args] (let [[i-res j-res] result
                               [delta wd]    args
                               [i j]         (first (firebrands [delta] wd cell cell-size))]
                           (and (= i-res i) (= j-res j)))
           [8   12]  [[1.0  0.0]  0]    ;; Same origin
           [7   12]  [[10.0 0.0]  0]    ;; North
           [8   13]  [[10.0 0.0]  90]   ;; East
           [9   12]  [[10.0 0.0]  180]  ;; South
           [8   11]  [[10.0 0.0]  270]  ;; West
           [7   13]  [[10.0 10.0] 0]    ;; North w/ perpindicular component
           [9   13]  [[10.0 10.0] 90]   ;; East w/ perpindicular component
           [9   11]  [[10.0 10.0] 180]  ;; South w/ perpindicular component
           [7   11]  [[10.0 10.0] 270]  ;; West w/ perpindicular component
           [-12 12]  [[200  0]    0]    ;; North, out of bounds
           [8  -8]  [[200  0]    270]  ;; West, out of bounds
           ))))

(comment                                                    ;; FIXME restore or delete
  (deftest ^:unit surface-spot-percents
    (testing "Fuel model is within expected ranges."
      (let [spot-percents [[[1 140] 1.0]
                           [[141 149] 2.0]
                           [[150 256] 3.0]]]

        (is (= 1.0 (spotting/surface-spot-percent spot-percents 5 *rand-gen*)))

        (is (= 2.0 (spotting/surface-spot-percent spot-percents 143 *rand-gen*)))

        (is (= 3.0 (spotting/surface-spot-percent spot-percents 155 *rand-gen*)))))

    (testing "Overlapping ranges"
      (let [spot-percents [[[1 140] 1.0]
                           [[130 149] 2.0]]]

        (is (= 2.0 (spotting/surface-spot-percent spot-percents 133 *rand-gen*))
            "should overwite percents of previous range in the sequence")))

    (testing "TODO: Given a pair of percentages, random percent is generated")))

(deftest ^:unit test-heat-of-preignition
  (testing "Heat of Preignition using eq. 9 of Schroeder (1969)."
    (are [result args] (within? result (apply spotting/heat-of-preignition args) 0.001)
         ; Q_ig (cal/g) [temperature (C) fuel-moisture (%)]
         0.0            [320.0          0.0]
         144.512        [0.0            0.0]

         ; Schroeder Fig. 2
         153.816        [(c/F->C 30.0)  0.01]
         193.996        [(c/F->C 160.0) 0.1]
         248.086        [(c/F->C 70.0)  0.15]
         292.814        [(c/F->C 130.0) 0.25])))

(deftest ^:unit test-schroeder-ign-prob
  (testing "Using Schroeder (1969)."
    (are [result args] (within? result (apply spotting/schroeder-ign-prob args) 0.01)
         ; Probability (0-1) [temperature (C) fuel-moisture (%)]
         0.0                 [0.0   0.3703]
         1.0                 [150.0 0.01]

         ; Schroeder Table 1
         0.87                [(c/F->C 35.0) 0.015]
         0.97                [(c/F->C 75.0) 0.015]
         0.74                [(c/F->C 35.0) 0.025]
         0.84                [(c/F->C 75.0) 0.025]
         0.51                [(c/F->C 35.0) 0.05]
         0.59                [(c/F->C 75.0) 0.05])))

(deftest ^:unit test-spot-ignition-probability
  (testing "Using Perryman (2012)."
    (let [lambda (/ 1.0 200.0)]
      (->> [;; Increasing Schroeder Ignition Probability
            [0.121 [0.2 lambda 100]]
            [0.242 [0.4 lambda 100]]
            [0.363 [0.6 lambda 100]]
            [0.485 [0.8 lambda 100]]
            [0.606 [1.0 lambda 100]]
            ;; ; Increasing Distance
            [0.303 [0.5 lambda 100]]
            [0.183 [0.5 lambda 200]]
            [0.0   [0.5 lambda 2000]]]
           (every? (fn [[expected-prob [schr-ign-prob decay-constant spotting-distance]]]
                     (let [ps              (spotting/spot-ignition-probability* (spotting/yield-lazy-sequence)
                                                                                (double decay-constant)
                                                                                (double spotting-distance)
                                                                                (double schr-ign-prob))
                           spot-ignition-p (double (last ps))
                           upper-bounds    (butlast ps)]
                       (and (is (within? expected-prob spot-ignition-p 0.001)
                                "Ultimately, the correct probability of spot ignition is computed.")
                            (is (= spot-ignition-p (spotting/spot-ignition-probability schr-ign-prob decay-constant spotting-distance))
                                (str `spotting/spot-ignition-probability " computes the same end result directly, without upper bounds."))
                            (->> upper-bounds (every? (fn [^double upper-bound]
                                                        (is (>= upper-bound spot-ignition-p)
                                                            "Each upper bound is indeed no smaller than the end result."))))))))))))

(deftest ^:unit test-spot-ignition-time
  (testing "Calculating t-max from Albini (1976)."
    (are [result args] (within? result (apply spotting/albini-t-max args) 0.1)
         ; Time to max height (min) [Flame length (m)]
         183.3                      [1.0]
         19.9                       [5.0]
         8.5                        [10.0]
         2.7                        [30.0]
         1.28                       [84.0]))

  (testing "Calculating time to spot ignition using Perryman."
    (are [result args] (within? result (apply spotting/spot-ignition-time args) 0.1)
         ; Ignition time (m) [Global clock time (m) Flame length (m)]
         386.6               [0 1.0]
         59.8                [0 5.0]
         37.0                [0 10.0]
         25.5                [0 30.0]
         22.56               [0 84.0])))

(deftest ^:unit test-spot-ignition?
  (testing "Spot ignition probability exceeds a random number generator."
    (are [result args] (= result (apply spotting/spot-ignition? args))
         ; Result [RNG spot-probability (0-1)]
         true     [*rand-gen* 1.0]
         false    [*rand-gen* 0.0]
         true     [*rand-gen* 0.5])))

(deftest ^:unit test-crown-spot-fire?
  (testing "Whether spotting from crown fire occurs based on a single or range of probabilities."
    (let [->argmap (fn [spotting] {:rand-gen *rand-gen*
                                   :spotting {:crown-fire-spotting-percent spotting}})]
      (are [result args] (= result (apply spotting/crown-spot-fire? args))
           ; Result [crown fire spotting probability (0-1)]
           true     [(->argmap 1.0)]
           false    [(->argmap 0.0)]
           true     [(->argmap 0.5)]))))

(deftest ^:unit test-surface-fire-spot-fire?
  (testing "Whether spotting from surface fire occurs based on a single or range of probabilities."
    (let [fuel-model (d/clone (dfn/+ (t/new-tensor [10 10]) 10))
          ->argmap   (fn [crit-fire-line-intensity spotting]
                       {:rand-gen       *rand-gen*
                        :get-fuel-model (-> fuel-model
                                            (grid-lookup/add-double-getter)
                                            (grid-lookup/mgetter-double))
                        :spotting       {:surface-fire-spotting
                                         {:critical-fire-line-intensity crit-fire-line-intensity
                                          :spotting-percent             spotting}}})]
      (are [result args] (= result (apply spotting/surface-fire-spot-fire? args))
           ; Result [crit. fire line intensity (kW/m) surface fire spotting probability (0-1) here (coords) fire line intensity (kW/m)]
           nil      [(->argmap 1500 nil)                [0 0] 1000]
           true     [(->argmap 500  [[[1 20] 1.0]])     [0 0] 1000]
           false    [(->argmap 500  [[[1 20] 0.001]])   [0 0] 1000]
           true     [(->argmap 500  [[[1 20] 0.999]])   [0 0] 1000]))))

(comment
  (run-tests 'gridfire.spotting-test)
  )
;; gridfire.spotting-test ends here
;; [[file:../../org/GridFire.org::himoto-typical-ranges][himoto-typical-ranges]]
(defn himoto-eq-28-values
  [B*]
  (let [D                  0.08
        ;; NOTE the above value for D seems absurdly low, and so do the predicted E[ΔX].
        ;; I suspect a typo in the Himoto2005 paper.
        std-delta-x-over-D (* 0.88 (Math/pow B* (/ 1.0 3)))
        exp-delta-x-over-D (* 0.47 (Math/pow B* (/ 2.0 3)))

        cv-delta-x         (/ std-delta-x-over-D exp-delta-x-over-D)
        sigma-x            (Math/sqrt (Math/log (+ 1.0 cv-delta-x)))
        exp-delta-x        (* exp-delta-x-over-D D)
        mu-x               (- (Math/log exp-delta-x)
                              (/ (Math/pow sigma-x 2)
                                 2))
        sigma-y            (* D 0.92)]
    {"$B^*$"                              B*
     ;; NOTE the following ended up being nonsensical:
     ;"E[ΔX] (m)" exp-delta-x
     "$\\text{CV}[\\Delta_X]$"            (str (format "%.0f" (* 100 cv-delta-x)) "%")
     ;"μ_X"       mu-x
     "$\\sigma_X$"                        (format "%.2f" sigma-x)
     "$\\sigma_Y/\\mathbb{E}[\\Delta_X]$" (format "%.2f" (/ sigma-y exp-delta-x))}))

(comment
  (pp/print-table
   (->> [20 50 100 150 200]
        (mapv himoto-eq-28-values)))
  ;| $B^*$ | $\text{CV}[\Delta_X]$ | $\sigma_X$ | $\sigma_Y/\mathbb{E}[\Delta_X]$ |
  ;|-------+-----------------------+------------+---------------------------------|
  ;|    20 |                   69% |       0.72 |                            0.27 |
  ;|    50 |                   51% |       0.64 |                            0.14 |
  ;|   100 |                   40% |       0.58 |                            0.09 |
  ;|   150 |                   35% |       0.55 |                            0.07 |
  ;|   200 |                   32% |       0.53 |                            0.06 |

  *e)
;; himoto-typical-ranges ends here
;; [[file:../../org/GridFire.org::spotting-params-examples][spotting-params-examples]]
(def sardoy-wind-driven-lognormal-params
  {:delta-x-ln-mu-a    1.32
   :delta-x-ln-mu-p    0.26
   :delta-x-ln-mu-q    0.11
   :delta-x-ln-mu-b    -0.02
   :delta-x-ln-sigma-a 4.95
   :delta-x-ln-sigma-p -0.01
   :delta-x-ln-sigma-q -0.02
   :delta-x-ln-sigma-b -3.48})

(def sardoy-buoyancy-driven-lognormal-params
  {:delta-x-ln-mu-a    1.47
   :delta-x-ln-mu-p    0.54
   :delta-x-ln-mu-q    -0.55
   :delta-x-ln-mu-b    1.14
   :delta-x-ln-sigma-a 0.86
   :delta-x-ln-sigma-p -0.21
   :delta-x-ln-sigma-q 0.44
   :delta-x-ln-sigma-b 0.19})

(defn sardoy-dist-values
  [spotting-config mu-iu sigma-iu]
  (let [mu-x        (+ (* (double (:delta-x-ln-mu-a spotting-config))
                          mu-iu)
                       (double (:delta-x-ln-mu-b spotting-config)))
        sigma-x     (+ (* (double (:delta-x-ln-sigma-a spotting-config))
                          sigma-iu)
                       (double (:delta-x-ln-sigma-b spotting-config)))
        exp-delta-x (spotting/deltax-expected-value mu-x sigma-x)]
    {"$I^{p_\\mu}U^{q_\\mu}$"               (format "%.2f" mu-iu)
     "$\\mu_X$"                             (format "%.2f" mu-x)
     "$I^{p_\\sigma}U^{q_\\sigma}$"         (format "%.2f" sigma-iu)
     "$\\sigma_X$"                          (format "%.3f" sigma-x)
     "$\\mathbb{E}[\\Delta_X]\\text{ (m)}$" (format "%.2f" exp-delta-x)
     "$\\text{CV}[\\Delta_X]$"              (str (format "%.0f" (* 100 (spotting/deltax-coefficient-of-variation sigma-x)))
                                                 "%")}))

(defn sardoy-typical-ranges-row
  [model-name spotting-config [mu-iu-min mu-iu-max] [sigma-iu-min sigma-iu-max]]
  (into {"Model" model-name}
        (merge-with (fn [v1 v2] (str v1 " - " v2))
                    (sardoy-dist-values spotting-config mu-iu-min sigma-iu-min)
                    (sardoy-dist-values spotting-config mu-iu-max sigma-iu-max))))

(comment

  (pp/print-table
   (->> [["Wind-driven" sardoy-wind-driven-lognormal-params
          [2.25 3.8]
          ;; NOTE it appears that the 1st plot of Fig 10 has incorrect x labels, (Val, 16 Mar 2023)
          ;; so I worked out the above bounds by inverting the y = ax + b relationship.
          #_[1.5 2.0]
          [0.88 0.93]]
         ["Buoyancy-driven" sardoy-buoyancy-driven-lognormal-params [1.8 2.6] [1.05 1.3]]]
        (mapv #(apply sardoy-typical-ranges-row %))))
  ;|           Model | $I^{p_\mu}U^{q_\mu}$ |     $\mu_X$ | $I^{p_\sigma}U^{q_\sigma}$ |    $\sigma_X$ | $\mathbb{E}[\Delta_X]\text{ (m)}$ | $\text{CV}[\Delta_X]$ |
  ;|-----------------+----------------------+-------------+----------------------------+---------------+-----------------------------------+-----------------------|
  ;|     Wind-driven |          2.25 - 3.80 | 2.95 - 5.00 |                0.88 - 0.93 | 0.876 - 1.124 |                    92.00 - 911.67 |           107% - 159% |
  ;| Buoyancy-driven |          1.80 - 2.60 | 3.79 - 4.96 |                1.05 - 1.30 | 1.093 - 1.308 |                  262.82 - 1102.77 |           152% - 213% |

  *e)

(deftest lognormal-params-sanity-checks-test
  (let [fireline-intensity 30e3
        wind-speed         11.17]
    (testing "When using the Sardoy2008 wind-driven parameters"
      (let [spotting-params sardoy-wind-driven-lognormal-params
            expected-mux    4.147842572687786
            expected-sigmax 1.0790349643407962
            expected-sigmay 457.4026146906983
            avg-deltax      (spotting/deltax-expected-value expected-mux expected-sigmax)]
        (testing "the value of mu_X"
          (is (within? (spotting-sardoy/sardoy-resolve-mu-x spotting-params fireline-intensity wind-speed)
                       expected-mux
                       1e-6))
          (testing "is close to the value in Fig9 of Sardoy2008"
            (is (within? expected-mux 4.20 0.1))))
        (testing "the value of sigma_X"
          (is (within? (spotting-sardoy/sardoy-resolve-sigma-x spotting-params fireline-intensity wind-speed)
                       expected-sigmax
                       1e-6))
          (testing "is close to the value in Fig9 of Sardoy2008"
            (is (within? expected-sigmax 0.96 0.2))))
        (testing "E[ΔX] (the average ΔX)"
          (is (within? avg-deltax 371.72303737837905
                       1e-3)
              (format "is about %.0f ft." avg-deltax)))
        (testing "the default value of sigma_Y"
          (is (within? (spotting/himoto-resolve-default-sigma-y-from-lognormal-params expected-mux expected-sigmax)
                       expected-sigmay
                       1e-3)
              (format "is about %.0f ft." expected-sigmay))
          (is (> expected-sigmay avg-deltax)
              "is larger than E[ΔX], which means it is probably a bad idea to use it."))))
    (testing "When using the Sardoy2008 buoyancy-driven parameters"
      (let [spotting-params sardoy-buoyancy-driven-lognormal-params
            expected-mux    3.586434356261935
            expected-sigmax 1.407436684216556
            expected-sigmay 1112.8976983507514
            avg-deltax      (spotting/deltax-expected-value expected-mux expected-sigmax)]
        (is (within? (spotting-sardoy/sardoy-resolve-mu-x spotting-params fireline-intensity wind-speed)
                     expected-mux
                     1e-6)
            (format "mu_X is about %.2f" expected-mux))
        (is (within? (spotting-sardoy/sardoy-resolve-sigma-x spotting-params fireline-intensity wind-speed)
                     expected-sigmax
                     1e-6)
            (format "sigma_X is about %.2f" expected-sigmax))
        (testing "E[ΔX] (the average ΔX)"
          (is (within? avg-deltax 318.94593966227154
                       1e-3)
              (format "is about %.0f ft." avg-deltax)))
        (testing "the default value of sigma_Y"
          (is (within? (spotting/himoto-resolve-default-sigma-y-from-lognormal-params expected-mux expected-sigmax)
                       expected-sigmay
                       1e-3)
              (format "is about %.0f ft" expected-sigmay))
          (is (> expected-sigmay avg-deltax)
              "is larger than E[ΔX], which means it is probably a bad idea to use it."))))))
;; spotting-params-examples ends here