(ns gridfire.behaveplus-results
  (:require [gridfire.fuel-models :refer [fuel-models]]))

(defn within [a b epsilon]
  (<= (Math/abs ^double (- a b)) epsilon))

(def sb40-fuel-models (filterv #(> % 100) (sort (keys fuel-models))))

(def test-fuel-moisture
  {:dry {:dead {:1hr 0.04 :10hr 0.04 :100hr 0.06}
         :live {:herbaceous 0.30 :woody 0.70}}
   :mid {:dead {:1hr 0.04 :10hr 0.04 :100hr 0.06}
         :live {:herbaceous 0.75 :woody 0.70}}
   :wet {:dead {:1hr 0.04 :10hr 0.04 :100hr 0.06}
         :live {:herbaceous 1.20 :woody 0.70}}})

;; INPUTS TO BEHAVEPLUS5:
;; - fuel-moisture = (test-fuel-moisture :dry)
;; - midflame wind speed (mph) = 0
;; - slope steepness (%) = 0
;;
;; OUTPUTS LISTED IN TABLE BELOW:
;; - ROS_max (ch/h)
;; - Fireline Intensity (Btu/ft/s)
;; - Flame Length (ft)
;; - Reaction Intensity (Btu/ft2/min)
;; - Live Fuel Moisture of Extinction (%)
;; - Residence Time (min)
;; - Fuel Load Transferred (%)
(def behaveplus5-surface-fire-values-dry-no-wind-no-slope
  {:GR1 [1.1  2   0.6  430   15   0.19 100]
   :GR2 [2.3  10  1.3  1135  15   0.21 100]
   :GR3 [2.8  22  1.9  1487  30   0.30 100]
   :GR4 [4.7  40  2.4  2209  15   0.21 100]
   :GR5 [4.8  88  3.5  4260  40   0.24 100]
   :GR6 [7.2  164 4.7  6520  40   0.19 100]
   :GR7 [8.7  258 5.8  7770  15   0.21 100]
   :GR8 [8.6  449 7.5  9700  30   0.29 100]
   :GR9 [14.9 977 10.7 14978 40   0.24 100]
   :GS1 [1.1  6   1.0  1512  258  0.21 100]
   :GS2 [1.6  15  1.6  2467  304  0.21 100]
   :GS3 [2.4  44  2.6  4110  456  0.24 100]
   :GS4 [3.2  245 5.7  17939 227  0.24 100]
   :SH1 [0.4  2   0.6  966   76   0.23 100]
   :SH2 [0.8  20  1.8  6083  124  0.23 0]
   :SH3 [0.3  3   0.8  1981  53   0.28 0]
   :SH4 [1.8  28  2.1  3610  128  0.23 0]
   :SH5 [2.9  93  3.6  5720  337  0.31 0]
   :SH6 [1.6  51  2.8  5375  670  0.34 0]
   :SH7 [2.4  104 3.8  7668  356  0.31 0]
   :SH8 [1.7  73  3.2  8462  197  0.28 0]
   :SH9 [3.1  215 5.3  13425 283  0.28 100]
   :TU1 [0.3  2   0.7  1821  202  0.24 100]
   :TU2 [1.0  8   1.2  2059  2392 0.22 0]
   :TU3 [2.0  36  2.3  4106  519  0.24 100]
   :TU4 [1.4  31  2.2  6882  503  0.17 0]
   :TU5 [1.2  62  3.0  9075  737  0.31 0]
   :TL1 [0.1  0   0.3  527   30   0.22 0]
   :TL2 [0.2  1   0.4  769   25   0.21 0]
   :TL3 [0.2  1   0.4  881   20   0.25 0]
   :TL4 [0.3  2   0.5  1101  25   0.24 0]
   :TL5 [0.5  4   0.8  1751  25   0.22 0]
   :TL6 [0.7  7   1.1  2500  25   0.20 0]
   :TL7 [0.4  4   0.9  1779  25   0.31 0]
   :TL8 [0.9  13  1.5  3617  35   0.22 0]
   :TL9 [1.3  27  2.1  5243  35   0.22 0]
   :SB1 [0.8  9   1.3  2753  25   0.23 0]
   :SB2 [1.7  36  2.3  5547  25   0.20 0]
   :SB3 [2.8  79  3.4  7633  25   0.20 0]
   :SB4 [4.5  134 4.3  8049  25   0.20 0]})

;; INPUTS TO BEHAVEPLUS5:
;; - fuel-moisture = (test-fuel-moisture :mid)
;; - midflame wind speed (mph) = 0
;; - slope steepness (%) = 0
;;
;; OUTPUTS LISTED IN TABLE BELOW:
;; - ROS_max (ch/h)
;; - Fireline Intensity (Btu/ft/s)
;; - Flame Length (ft)
;; - Reaction Intensity (Btu/ft2/min)
;; - Live Fuel Moisture of Extinction (%)
;; - Residence Time (min)
;; - Fuel Load Transferred (%)
(def behaveplus5-surface-fire-values-mid-no-wind-no-slope
  {:GR1 [0.6 1   0.4 448   404  0.19 50]
   :GR2 [1.1 5   0.9 1140  290  0.21 50]
   :GR3 [1.2 9   1.3 1435  411  0.30 50]
   :GR4 [2.2 19  1.7 2233  307  0.21 50]
   :GR5 [2.1 36  2.4 3998  411  0.24 50]
   :GR6 [2.8 57  2.9 5852  309  0.19 50]
   :GR7 [4.2 128 4.2 7937  335  0.21 50]
   :GR8 [3.6 183 4.9 9254  385  0.29 50]
   :GR9 [6.4 389 7.0 13981 401  0.24 50]
   :GS1 [0.8 4   0.9 1345  108  0.21 50]
   :GS2 [1.2 11  1.3 2291  169  0.21 50]
   :GS3 [1.5 23  1.9 3429  161  0.24 50]
   :GS4 [2.5 169 4.8 16001 116  0.24 50]
   :SH1 [0.1 0   0.2 369   56   0.23 50]
   :SH2 [0.8 20  1.8 6083  124  0.23 0 ]
   :SH3 [0.3 3   0.8 1981  53   0.28 0 ]
   :SH4 [1.8 28  2.1 3610  128  0.23 0 ]
   :SH5 [2.9 93  3.6 5720  337  0.31 0 ]
   :SH6 [1.6 51  2.8 5375  670  0.34 0 ]
   :SH7 [2.4 104 3.8 7668  356  0.31 0 ]
   :SH8 [1.7 73  3.2 8462  197  0.28 0 ]
   :SH9 [3.0 217 5.3 14085 217  0.28 50]
   :TU1 [0.3 2   0.6 1816  150  0.24 50]
   :TU2 [1.0 8   1.2 2059  2392 0.22 0 ]
   :TU3 [1.6 27  2.0 3828  317  0.24 50]
   :TU4 [1.4 31  2.2 6882  503  0.17 0 ]
   :TU5 [1.2 62  3.0 9075  737  0.31 0 ]
   :TL1 [0.1 0   0.3 527   30   0.22 0 ]
   :TL2 [0.2 1   0.4 769   25   0.21 0 ]
   :TL3 [0.2 1   0.4 881   20   0.25 0 ]
   :TL4 [0.3 2   0.5 1101  25   0.24 0 ]
   :TL5 [0.5 4   0.8 1751  25   0.22 0 ]
   :TL6 [0.7 7   1.1 2500  25   0.20 0 ]
   :TL7 [0.4 4   0.9 1779  25   0.31 0 ]
   :TL8 [0.9 13  1.5 3617  35   0.22 0 ]
   :TL9 [1.3 27  2.1 5243  35   0.22 0 ]
   :SB1 [0.8 9   1.3 2753  25   0.23 0 ]
   :SB2 [1.7 36  2.3 5547  25   0.20 0 ]
   :SB3 [2.8 79  3.4 7633  25   0.20 0 ]
   :SB4 [4.5 134 4.3 8049  25   0.20 0 ]})

;; INPUTS TO BEHAVEPLUS5:
;; - fuel-moisture = (test-fuel-moisture :mid)
;; - midflame wind speed (mph) = 10
;; - slope steepness (%) = 20
;;
;; OUTPUTS LISTED IN TABLE BELOW:
;; - ROS_max (ch/h)
;; - Fireline Intensity (Btu/ft/s)
;; - Flame Length (ft)
;; - Reaction Intensity (Btu/ft2/min)
;; - Live Fuel Moisture of Extinction (%)
;; - Residence Time (min)
;; - Fuel Load Transferred (%)
;; - Wind Factor
;; - Slope Factor
(def behaveplus5-surface-fire-values-mid-with-wind-and-slope
  {:GR1 [11.9  18     1.7  448  404  0.19 50 68.8 1.5]
   :GR2 [72.0  318    6.4  1140 290  0.21 50 65.4 1.5]
   :GR3 [84.1  659    8.9  1435 411  0.30 50 68.0 1.5]
   :GR4 [148.3 1277  12.1  2233 307  0.21 50 66.0 1.5]
   :GR5 [113.8 1965  14.7  3998 411  0.24 50 51.8 1.2]
   :GR6 [148.8 3055  18.0  5852 309  0.19 50 51.0 1.2]
   :GR7 [223.7 6814  26.1  7937 335  0.21 50 51.2 1.2]
   :GR8 [182.0 9107  29.8  9254 385  0.29 50 47.7 1.2]
   :GR9 [326.3 19924 42.7 13981 401  0.24 50 49.1 1.2]
   :GS1 [47.1  243    5.6  1345 108  0.21 50 58.3 1.3]
   :GS2 [70.8  625    8.7  2291 169  0.21 50 55.3 1.3]
   :GS3 [85.2  1275  12.1  3429 161  0.24 50 53.1 1.3]
   :GS4 [85.1  5874  24.4 16001 116  0.24 50 32.8 0.9]
   :SH1 [2.1   3      0.8  369   56  0.23 50 51.9 1.2]
   :SH2 [24.6  629    8.7  6083 124  0.23  0 29.3 0.8]
   :SH3 [12.6  128    4.2  1981  53  0.28  0 36.8 1.0]
   :SH4 [108.3 1636  13.5  3610 128  0.23  0 56.3 1.3]
   :SH5 [174.2 5603  23.9  5720 337  0.31  0 57.9 1.4]
   :SH6 [68.2  2254  15.7  5375 670  0.34  0 41.8 1.1]
   :SH7 [113.6 4972  22.6  7668 356  0.31  0 45.7 1.2]
   :SH8 [69.6  2991  17.9  8462 197  0.28  0 39.0 1.0]
   :SH9 [123.9 8914  29.5 14085 217  0.28 50 39.0 1.0]
   :TU1 [9.6   76     3.3  1816 150  0.24 50 32.4 0.9]
   :TU2 [40.5  333    6.5  2059 2392 0.22  0 39.2 1.0]
   :TU3 [77.5  1297  12.2  3828 317  0.24 50 46.6 1.1]
   :TU4 [46.4  1013  10.9  6882 503  0.17  0 30.5 0.7]
   :TU5 [26.0  1360  12.4  9075 737  0.31  0 20.3 0.7]
   :TL1 [1.2   3      0.7  527   30  0.22  0 17.3 0.5]
   :TL2 [3.1   9      1.2  769   25  0.21  0 19.1 0.5]
   :TL3 [4.3   18     1.7  881   20  0.25  0 20.3 0.6]
   :TL4 [7.4   37     2.4  1101  25  0.24  0 22.0 0.7]
   :TL5 [14.0  101    3.8  1751  25  0.22  0 24.7 0.7]
   :TL6 [19.7  179    4.9  2500  25  0.20  0 25.4 0.7]
   :TL7 [7.6   77     3.3  1779  25  0.31  0 15.7 0.6]
   :TL8 [18.7  270    5.9  3617  35  0.22  0 19.2 0.6]
   :TL9 [27.8  591    8.5  5243  35  0.22  0 20.1 0.6]
   :SB1 [19.3  226    5.5  2753  25  0.23  0 22.8 0.7]
   :SB2 [50.1  1038  11.0  5547  25  0.20  0 26.9 0.7]
   :SB3 [92.1  2558  16.6  7633  25  0.20  0 30.7 0.8]
   :SB4 [177.7 5281  23.2  8049  25  0.20  0 37.6 0.9]})

   ;;  Fuel  ROS   Fireline  Flame Direction Effective
   ;; Model (max) Intensity Length  Max ROS     Wind
   ;;        ch/h  Btu/ft/s   ft      deg       mi/h
(def behaveplus5-surface-fire-values-mid-with-cross-wind-and-slope-90-180
  {:GR1 [11.9    18      1.7     271       4.6]
   :GR2 [70.5    311     6.3     271       10.0]
   :GR3 [82.4    645     8.8     271       10.0]
   :GR4 [145.2   1250    12.0    271       10.0]
   :GR5 [111.3   1921    14.6    271       10.0]
   :GR6 [145.5   2989    17.9    271       10.0]
   :GR7 [218.7   6663    25.8    271       10.0]
   :GR8 [177.7   8893    29.5    271       10.0]
   :GR9 [318.9   19468   42.3    271       10.0]
   :GS1 [46.1    238     5.6     271       10.0]
   :GS2 [69.3    611     8.6     271       10.0]
   :GS3 [83.3    1246    11.9    271       10.0]
   :GS4 [82.9    5728    24.1    272       10.0]
   :SH1 [2.1     3       0.8     271       3.8]
   :SH2 [23.9    613     8.6     272       10.0]
   :SH3 [12.3    125     4.2     272       10.0]
   :SH4 [105.9   1600    13.4    271       10.0]
   :SH5 [170.3   5479    23.6    271       10.0]
   :SH6 [66.5    2199    15.5    272       10.0]
   :SH7 [110.9   4853    22.3    271       10.0]
   :SH8 [67.9    2917    17.7    272       10.0]
   :SH9 [120.8   8694    29.2    272       10.0]
   :TU1 [9.3     74      3.3     272       10.0]
   :TU2 [39.6    325     6.4     271       10.0]
   :TU3 [75.7    1267    12.0    271       10.0]
   :TU4 [45.4    992     10.8    271       10.0]
   :TU5 [25.2    1318    12.3    272       10.0]
   :TL1 [1.2     3       0.7     272       5.4]
   :TL2 [3.1     9       1.2     272       7.9]
   :TL3 [4.3     18      1.7     272       9.0]
   :TL4 [7.2     36      2.3     272       10.0]
   :TL5 [13.7    98      3.7     272       10.0]
   :TL6 [19.2    175     4.8     271       10.0]
   :TL7 [7.4     75      3.3     272       10.0]
   :TL8 [18.3    263     5.8     272       10.0]
   :TL9 [27.0    576     8.4     272       10.0]
   :SB1 [18.8    220     5.4     272       10.0]
   :SB2 [48.9    1013    10.9    271       10.0]
   :SB3 [89.9    2498    16.4    271       10.0]
   :SB4 [173.6   5160    23.0    271       10.0]})

   ;;  Fuel  ROS   Fireline  Flame Direction Effective
   ;; Model (max) Intensity Length  Max ROS     Wind
   ;;        ch/h  Btu/ft/s   ft      deg       mi/h
(def behaveplus5-surface-fire-values-mid-with-cross-wind-and-slope-135-180
  {:GR1 [11.9    18      1.7     316       4.6]
   :GR2 [71.6    316     6.4     316       10.1]
   :GR3 [83.6    655     8.9     316       10.1]
   :GR4 [147.4   1269    12.0    316       10.1]
   :GR5 [113.1   1952    14.7    316       10.1]
   :GR6 [147.8   3036    18.0    316       10.1]
   :GR7 [222.2   6771    26.0    316       10.1]
   :GR8 [180.8   9045    29.7    316       10.1]
   :GR9 [324.2   19791   42.6    316       10.1]
   :GS1 [46.8    242     5.6     316       10.1]
   :GS2 [70.4    621     8.7     316       10.1]
   :GS3 [84.7    1267    12.0    316       10.1]
   :GS4 [84.4    5831    24.3    316       10.1]
   :SH1 [2.1     3       0.8     316       3.8]
   :SH2 [24.4    624     8.7     316       10.1]
   :SH3 [12.5    128     4.2     316       10.2]
   :SH4 [107.6   1625    13.5    316       10.1]
   :SH5 [173.1   5567    23.8    316       10.1]
   :SH6 [67.7    2238    15.6    316       10.2]
   :SH7 [112.8   4938    22.5    316       10.2]
   :SH8 [69.1    2970    17.8    316       10.1]
   :SH9 [123.0   8850    29.4    316       10.2]
   :TU1 [9.5     76      3.3     316       10.1]
   :TU2 [40.3    331     6.5     316       10.1]
   :TU3 [77.0    1288    12.1    316       10.1]
   :TU4 [46.1    1007    10.8    316       10.1]
   :TU5 [25.8    1348    12.4    316       10.2]
   :TL1 [1.2     3       0.7     316       5.4]
   :TL2 [3.1     9       1.2     316       7.9]
   :TL3 [4.3     18      1.7     316       9.0]
   :TL4 [7.3     36      2.3     316       10.2]
   :TL5 [13.9    100     3.7     316       10.1]
   :TL6 [19.6    178     4.9     316       10.1]
   :TL7 [7.5     77      3.3     316       10.2]
   :TL8 [18.6    268     5.9     316       10.1]
   :TL9 [27.6    587     8.4     316       10.1]
   :SB1 [19.2    225     5.4     316       10.1]
   :SB2 [49.7    1031    10.9    316       10.1]
   :SB3 [91.5    2541    16.6    316       10.1]
   :SB4 [176.5   5246    23.1    316       10.1]})
