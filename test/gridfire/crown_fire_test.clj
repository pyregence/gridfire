(ns gridfire.crown-fire-test
  (:require [clojure.test        :refer [deftest testing are run-tests]]
            [gridfire.conversion :as c]
            [gridfire.crown-fire :refer [crown-fire-line-intensity
                                         cruz-crown-fire-spread
                                         cruz-crown-fire-spread-metric
                                         cruz-active-crown-fire-spread
                                         cruz-passive-crown-fire-spread
                                         van-wagner-crown-fire-line-intensity
                                         van-wagner-crown-fire-initiation-metric?
                                         van-wagner-crown-fire-initiation?]]))

(defn- within-%? [^double a ^double b ^double percent]
  (<= (Math/abs ^double (- a b)) (Math/abs (* percent a))))

(defn- within-5%? [^double a ^double b]
  (within-%? a b 0.05))

(defn- crown-fire-within-5%? [[fire-type-a fire-rate-a] [fire-type-b fire-rate-b]]
  (and (= fire-type-a fire-type-b)
       (within-5%? fire-rate-a fire-rate-b)))

(deftest ^:unit test-van-wagner-crown-fire-line-intensity
  (testing "Fire intensity using Van Wagner (1976), equation 4."
    (are [expected args] (within-5%? expected (apply van-wagner-crown-fire-line-intensity args))
         ; Crit. surf. intensity (kW/m) [canopy-base-height (m) foliar-moisture epsilon (%)]
         0.0                            [0 0]      ; No Canopy Base Height/Intensity
         2890.0                         [7 95]     ; Red pine (C6)
         4560.0                         [7 135]    ; Red pine (C4)
         210.0                          [1 120]))) ; Balsam fir under pine (F3)

(deftest ^:unit test-van-wagner-crown-fire-initation-metric?
  (testing "Fire intensity reaches Van Wagner crown fire threshold using SI units."
    (are [expected args] (= expected (apply van-wagner-crown-fire-initiation-metric? args))
         ; true/false [canopy-cover (%) canopy-base-height (m) foliar-moisture (%) final-intensity (kW/m)]
         false        [0   0.0  0.0  0]     ; No canopy cover
         false        [50  0.0  0.0  0]     ; No Canopy Base Height/No final-intensity
         false        [50  0.0  0.0  10]    ; No Canopy Base Height
         true         [50  1.0  0.0  10]    ; CC, CBH, No Moisture
         true         [50  7.0  95   10500] ; Red pine (C6)
         true         [50  7.0  135  9500]  ; Red pine (C4)
         false        [50  1.0  120  85]))) ; Balsam fir under pine (F3)

(deftest ^:unit test-van-wagner-crown-fire-initation?
  (testing "Fire intensity reaches Van Wagner crown fire threshold using imperial units."
    (are [expected args] (= expected (apply van-wagner-crown-fire-initiation? args))
         ; true/false [canopy-cover (%) canopy-base-height (m) foliar-moisture (%) final-intensity (kW/m)]
         false        [0   0.0           0.0  0]     ; No canopy cover
         false        [50  0.0           0.0  0]     ; No Canopy Base Height/No final-intensity
         false        [50  0.0           0.0  (c/kW-m->Btu-ft-s 10.0)]    ; No Canopy Base Height
         true         [50  (c/m->ft 1.0) 0.0  (c/kW-m->Btu-ft-s 10.0)]    ; CC, CBH, No Moisture
         true         [50  (c/m->ft 7.0) 0.95 (c/kW-m->Btu-ft-s 10500.0)] ; Red pine (C6)
         true         [50  (c/m->ft 7.0) 1.35 (c/kW-m->Btu-ft-s 9500.0)]  ; Red pine (C4)
         false        [50  (c/m->ft 1.0) 1.20 (c/kW-m->Btu-ft-s 85.0)]))) ; Balsam fir under pine (F3)

(deftest ^:unit test-cruz-active-fire-spread
  (testing "Crown Fire spread rate (Cruz 2005) using SI units."
    (are [expected args] (within-5%? expected (apply cruz-active-crown-fire-spread args))
         ; m/min [wind-speed-10m (km/hr) canopy-bulk-density (kg/m^3) est. fine fuel moisture (%)]
         22.6    [15.8 0.27 8.8]    ; Mean values
         137     [50   0.2  4.0]    ; 1983 Mount Muirhead Fire
         72.2    [74   0.1  9.0]))) ; 1973 Burnt Fire

(deftest ^:unit test-cruz-passive-fire-spread
  (testing "Passive fire spread rate (Cruz 2005) using SI units."
    (are [expected args] (within-5%? expected (apply cruz-passive-crown-fire-spread args))
         ; m/min [wind-speed-10m (km/hr) canopy-bulk-density (kg/m^3) est. fine fuel moisture (%)]
         7.1     [16.3 0.16 8.6]    ; Mean values
         7.6     [7    0.08 8.0]))) ; Bor Island Fire Experiment

(deftest ^:unit test-cruz-fire-spread
  (testing "Active crown fires using SI units."
    (are [expected args] (crown-fire-within-5%? [:active-crown expected] (apply cruz-crown-fire-spread-metric args))
         ; m/min [wind-speed-10m (km/hr) canopy-bulk-density (kg/m^3) est. fine fuel moisture (%)]
         22.6    [15.8 0.27 8.8] ; Mean values
         32.0    [35 0.1 10.0]))

  (testing "Passive crown fires using SI units."
    (are [expected args] (crown-fire-within-5%? [:passive-crown expected] (apply cruz-crown-fire-spread-metric args))
         ; m/min [wind-speed-10m (km/hr) canopy-bulk-density (kg/m^3) est. fine fuel moisture (%)]
         7.6     [7  0.08 8.0] ; Bor Island Fire Experiment
         11.0    [30 0.1 10.0])))

(deftest ^:unit test-cruz-fire-spread-imperial
  (testing "Testing using imperial units fires."
    (are [expected args] (crown-fire-within-5%? [:active-crown expected] (apply cruz-crown-fire-spread args))
         ; ft/min [wind-speed-20ft (mph) canopy-bulk-density (lb/ft^3) est. fine fuel moisture (0-1)]
         (c/m->ft 22.6) [(c/U-10m->U-20ft 15.8) (c/kg-m3->lb-ft3 0.27) 0.088]
         (c/m->ft 32.0) [(c/U-10m->U-20ft 35.0) (c/kg-m3->lb-ft3 0.1)  0.1]))

  (testing "Passive crown fires"
    (are [expected args] (crown-fire-within-5%? [:passive-crown expected] (apply cruz-crown-fire-spread args))
         ; ft/min     [wind-speed-20ft (mph) canopy-bulk-density (lb/ft^3) est. fine fuel moisture (0-1)]
         (c/m->ft 7.6)  [(c/U-10m->U-20ft 7.0)  (c/kg-m3->lb-ft3 0.08) 0.08]
         (c/m->ft 11.0) [(c/U-10m->U-20ft 30.0) (c/kg-m3->lb-ft3 0.1)  0.1])))

(deftest ^:unit test-crown-fire-line-intensity
  (testing "Crown Fire line intensity using SI units."
    (are [expected args] (within-5%? expected (apply crown-fire-line-intensity args))
         ; kW/m  [crown-spread-rate (m/min) crown-bulk-density (kg/m^3) canopy-height (m) canopy-base-height (m) heat-of-combustion (kJ/kg)]
         78 [1.0 0.25 2.0 1.0 (c/Btu-lb->kJ-kg 8000.0)])) ; Metric

  (testing "Crown Fire line intensity using imperial units."
    (are [expected args] (within-5%? expected (apply crown-fire-line-intensity args))
         ; Btu/ft*s  [crown-spread-rate (f/min) crown-bulk-density (lb/ft^3) canopy-height (ft) canopy-base-height (ft) heat-of-combustion (Btu/lb)]
         22 [(c/m->ft 1.0) (c/kg-m3->lb-ft3 0.25) (c/m->ft 2.0) (c/m->ft 1.0) 8000.0])))

(comment
  (run-tests 'gridfire.crown-fire-test)
  )
