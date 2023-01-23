(ns gridfire.lab.replay.compare-85f1467-9d09fa9
  "Records a comparison of gridfire.lab.replay results for 2 version of GridFire,
  before and after directional crowning initiation."
  (:require [clojure.string :as str]))

;; HACK for historical reasons, it was much more expedient to plot the comparison through string manipulation.
;; Admittedly, that's an ugly thing to include in a codebase, but losing these results would be much worse,
;; and spending time refactoring one-off code seems like a waste of time.

(comment

  ;; Old text-encoded table of results (85f1467)
  (def s0 (slurp "src/gridfire/lab/replay/results-stats-table-85f1467.txt"))
  ;; New table: (9d09fa9)
  (def s1 (slurp "src/gridfire/lab/replay/results-stats-table-9d09fa9.txt"))

  ;; Let us print the old and new tables interleaved:

  (defn project-line
    [l]
    (let [cpnts
                  (-> l
                      (str/replace #"\s+" "")
                      (str/split #"\|"))
          [_ fire-name variation t0 t1] cpnts
          version (last cpnts)]
      [fire-name t0 t1 version variation]))

  (->> (for [[s suffix] [[s0 "OLD"]
                         [s1 "NEW"]]
             l (->> s
                    (str/split-lines)
                    (drop 3))]
         (str l suffix))
       (sort-by project-line)
       (partition-by (fn replay-id [l] (->> l (project-line) (take 3))))
       (filter #(-> % (count) (= 4)))
       (map (fn uniformize-1st-column-width [lines]
              (letfn [(line-width [l] (count l))]
                (let [max-lw (->> lines (map line-width) (apply max))]
                  (->> lines
                       (mapv (fn adjust-1st-column-width [l]
                               (let [lw (line-width l)]
                                 (cond-> l
                                         (< lw max-lw)
                                         (str/replace ";;|"
                                                      (str ";;|" (->> " "
                                                                      (repeat (- max-lw lw))
                                                                      (str/join)))))))))))))
       (concat [(->> s1 (str/split-lines) (drop 1) (take 2))]) ; header
       (interpose ["\n"])
       (sequence cat)
       (str/join "\n")
       (println))

  ;;|               Fire name |             Variation |                           t0 |                                   t1 | n cells really burned |    sim ∩ real | ToA-ratio: mean |  p50 |  p75 |  p90 |  p95 |      sim - real |    real - sim | s∩r crowning |  s-r crowning |
  ;;|-------------------------+-----------------------+------------------------------+--------------------------------------+-----------------------+---------------+-----------------+------+------+------+------+-----------------+---------------+--------------+---------------|

  ;;|               ca-barnes |          00) original | Tue Sep 13 13:18:00 UTC 2022 | Tue Sep 13 19:12:00 UTC 2022 (+ 6hr) |                 888.0 |   358 (40.3%) |            0.25 | 0.05 | 0.48 | 0.81 | 0.94 |       69 (7.8%) |   530 (59.7%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|               ca-barnes | 01) crowning disabled | Tue Sep 13 13:18:00 UTC 2022 | Tue Sep 13 19:12:00 UTC 2022 (+ 6hr) |                 888.0 |   358 (40.3%) |            0.25 | 0.05 | 0.48 | 0.81 | 0.94 |       69 (7.8%) |   530 (59.7%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|               ca-barnes |          00) original | Tue Sep 13 13:18:00 UTC 2022 | Tue Sep 13 19:12:00 UTC 2022 (+ 6hr) |                 888.0 |   327 (36.8%) |            0.24 | 0.05 | 0.05 | 0.94 | 0.95 |        2 (0.2%) |   561 (63.2%) |     0 (0.0%) |      0 (0.0%) |OLD
  ;;|               ca-barnes | 01) crowning disabled | Tue Sep 13 13:18:00 UTC 2022 | Tue Sep 13 19:12:00 UTC 2022 (+ 6hr) |                 888.0 |   327 (36.8%) |            0.24 | 0.05 | 0.05 | 0.94 | 0.95 |        2 (0.2%) |   561 (63.2%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|             ca-mosquito |          00) original | Wed Sep 14 22:01:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+12hr) |              108947.0 | 75383 (69.2%) |            0.27 | 0.18 | 0.44 | 0.75 | 0.87 |   30809 (28.3%) | 33564 (30.8%) |  3045 (4.0%) | 10479 (34.0%) |NEW
  ;;|             ca-mosquito | 01) crowning disabled | Wed Sep 14 22:01:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+12hr) |              108947.0 | 73465 (67.4%) |            0.28 | 0.19 | 0.46 | 0.76 | 0.87 |   18154 (16.7%) | 35482 (32.6%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|             ca-mosquito |          00) original | Wed Sep 14 22:01:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+12hr) |              108947.0 | 77291 (70.9%) |            0.21 | 0.14 | 0.33 | 0.57 | 0.71 |   47466 (43.6%) | 31656 (29.1%) | 7786 (10.1%) | 27055 (57.0%) |OLD
  ;;|             ca-mosquito | 01) crowning disabled | Wed Sep 14 22:01:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+12hr) |              108947.0 | 72699 (66.7%) |            0.25 | 0.17 | 0.40 | 0.65 | 0.76 |   15640 (14.4%) | 36248 (33.3%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|                  ca-red |          00) original | Fri Sep 16 20:40:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+12hr) |                  44.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   44 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|                  ca-red | 01) crowning disabled | Fri Sep 16 20:40:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+12hr) |                  44.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   44 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|                  ca-red |          00) original | Fri Sep 16 20:40:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+12hr) |                  44.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   44 (100.0%) |        0 (-) |         0 (-) |OLD
  ;;|                  ca-red | 01) crowning disabled | Fri Sep 16 20:40:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+12hr) |                  44.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   44 (100.0%) |        0 (-) |         0 (-) |OLD


  ;;|                  ca-red |          00) original | Thu Sep 15 10:28:00 UTC 2022 | Thu Sep 15 17:34:00 UTC 2022 (+ 7hr) |                  97.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   97 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|                  ca-red | 01) crowning disabled | Thu Sep 15 10:28:00 UTC 2022 | Thu Sep 15 17:34:00 UTC 2022 (+ 7hr) |                  97.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   97 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|                  ca-red |          00) original | Thu Sep 15 10:28:00 UTC 2022 | Thu Sep 15 17:34:00 UTC 2022 (+ 7hr) |                  97.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   97 (100.0%) |        0 (-) |         0 (-) |OLD
  ;;|                  ca-red | 01) crowning disabled | Thu Sep 15 10:28:00 UTC 2022 | Thu Sep 15 17:34:00 UTC 2022 (+ 7hr) |                  97.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   97 (100.0%) |        0 (-) |         0 (-) |OLD


  ;;|                  ca-red |          00) original | Wed Sep 14 16:50:00 UTC 2022 | Thu Sep 15 10:28:00 UTC 2022 (+18hr) |                  96.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   96 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|                  ca-red | 01) crowning disabled | Wed Sep 14 16:50:00 UTC 2022 | Thu Sep 15 10:28:00 UTC 2022 (+18hr) |                  96.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   96 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|                  ca-red |          00) original | Wed Sep 14 16:50:00 UTC 2022 | Thu Sep 15 10:28:00 UTC 2022 (+18hr) |                  96.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   96 (100.0%) |        0 (-) |         0 (-) |OLD
  ;;|                  ca-red | 01) crowning disabled | Wed Sep 14 16:50:00 UTC 2022 | Thu Sep 15 10:28:00 UTC 2022 (+18hr) |                  96.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   96 (100.0%) |        0 (-) |         0 (-) |OLD


  ;;|              ca-rodgers |          00) original | Fri Sep 16 16:28:00 UTC 2022 | Sat Sep 17 09:49:00 UTC 2022 (+17hr) |                  40.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   40 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|              ca-rodgers | 01) crowning disabled | Fri Sep 16 16:28:00 UTC 2022 | Sat Sep 17 09:49:00 UTC 2022 (+17hr) |                  40.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   40 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|              ca-rodgers |          00) original | Fri Sep 16 16:28:00 UTC 2022 | Sat Sep 17 09:49:00 UTC 2022 (+17hr) |                  40.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   40 (100.0%) |        0 (-) |         0 (-) |OLD
  ;;|              ca-rodgers | 01) crowning disabled | Fri Sep 16 16:28:00 UTC 2022 | Sat Sep 17 09:49:00 UTC 2022 (+17hr) |                  40.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   40 (100.0%) |        0 (-) |         0 (-) |OLD


  ;;|               ca-summit |          00) original | Mon Sep 19 21:23:00 UTC 2022 | Tue Sep 20 08:54:00 UTC 2022 (+12hr) |                1396.0 |  1148 (82.2%) |            0.09 | 0.03 | 0.03 | 0.03 | 0.65 |        2 (0.1%) |   248 (17.8%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|               ca-summit | 01) crowning disabled | Mon Sep 19 21:23:00 UTC 2022 | Tue Sep 20 08:54:00 UTC 2022 (+12hr) |                1396.0 |  1148 (82.2%) |            0.09 | 0.03 | 0.03 | 0.03 | 0.65 |        2 (0.1%) |   248 (17.8%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|               ca-summit |          00) original | Mon Sep 19 21:23:00 UTC 2022 | Tue Sep 20 08:54:00 UTC 2022 (+12hr) |                1396.0 |  1146 (82.1%) |            0.08 | 0.03 | 0.03 | 0.03 | 0.59 |        2 (0.1%) |   250 (17.9%) |     0 (0.0%) |      0 (0.0%) |OLD
  ;;|               ca-summit | 01) crowning disabled | Mon Sep 19 21:23:00 UTC 2022 | Tue Sep 20 08:54:00 UTC 2022 (+12hr) |                1396.0 |  1146 (82.1%) |            0.08 | 0.03 | 0.03 | 0.03 | 0.59 |        2 (0.1%) |   250 (17.9%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|               ca-summit |          00) original | Mon Sep 26 09:32:00 UTC 2022 | Tue Sep 27 09:11:00 UTC 2022 (+24hr) |                  49.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   49 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|               ca-summit | 01) crowning disabled | Mon Sep 26 09:32:00 UTC 2022 | Tue Sep 27 09:11:00 UTC 2022 (+24hr) |                  49.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   49 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|               ca-summit |          00) original | Mon Sep 26 09:32:00 UTC 2022 | Tue Sep 27 09:11:00 UTC 2022 (+24hr) |                  49.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   49 (100.0%) |        0 (-) |         0 (-) |OLD
  ;;|               ca-summit | 01) crowning disabled | Mon Sep 26 09:32:00 UTC 2022 | Tue Sep 27 09:11:00 UTC 2022 (+24hr) |                  49.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   49 (100.0%) |        0 (-) |         0 (-) |OLD


  ;;|               ca-summit |          00) original | Sun Sep 18 10:21:00 UTC 2022 | Sun Sep 18 16:30:00 UTC 2022 (+ 6hr) |                2461.0 |   817 (33.2%) |            0.08 | 0.06 | 0.06 | 0.06 | 0.06 |        1 (0.0%) |  1644 (66.8%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|               ca-summit | 01) crowning disabled | Sun Sep 18 10:21:00 UTC 2022 | Sun Sep 18 16:30:00 UTC 2022 (+ 6hr) |                2461.0 |   817 (33.2%) |            0.08 | 0.06 | 0.06 | 0.06 | 0.06 |        1 (0.0%) |  1644 (66.8%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|               ca-summit |          00) original | Sun Sep 18 10:21:00 UTC 2022 | Sun Sep 18 16:30:00 UTC 2022 (+ 6hr) |                2461.0 |   811 (33.0%) |            0.06 | 0.06 | 0.06 | 0.06 | 0.06 |        0 (0.0%) |  1650 (67.0%) |     0 (0.0%) |         0 (-) |OLD
  ;;|               ca-summit | 01) crowning disabled | Sun Sep 18 10:21:00 UTC 2022 | Sun Sep 18 16:30:00 UTC 2022 (+ 6hr) |                2461.0 |   811 (33.0%) |            0.06 | 0.06 | 0.06 | 0.06 | 0.06 |        0 (0.0%) |  1650 (67.0%) |     0 (0.0%) |         0 (-) |OLD


  ;;|               ca-summit |          00) original | Sun Sep 25 09:49:00 UTC 2022 | Mon Sep 26 09:32:00 UTC 2022 (+24hr) |                 147.0 |    69 (46.9%) |            0.21 | 0.03 | 0.25 | 0.78 | 0.90 |        1 (0.7%) |    78 (53.1%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|               ca-summit | 01) crowning disabled | Sun Sep 25 09:49:00 UTC 2022 | Mon Sep 26 09:32:00 UTC 2022 (+24hr) |                 147.0 |    69 (46.9%) |            0.21 | 0.03 | 0.25 | 0.78 | 0.90 |        1 (0.7%) |    78 (53.1%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|               ca-summit |          00) original | Sun Sep 25 09:49:00 UTC 2022 | Mon Sep 26 09:32:00 UTC 2022 (+24hr) |                 147.0 |    62 (42.2%) |            0.07 | 0.03 | 0.03 | 0.03 | 0.61 |        0 (0.0%) |    85 (57.8%) |     0 (0.0%) |         0 (-) |OLD
  ;;|               ca-summit | 01) crowning disabled | Sun Sep 25 09:49:00 UTC 2022 | Mon Sep 26 09:32:00 UTC 2022 (+24hr) |                 147.0 |    62 (42.2%) |            0.07 | 0.03 | 0.03 | 0.03 | 0.61 |        0 (0.0%) |    85 (57.8%) |     0 (0.0%) |         0 (-) |OLD


  ;;|               ca-summit |          00) original | Thu Sep 22 21:18:00 UTC 2022 | Sat Sep 24 09:20:00 UTC 2022 (+36hr) |                  14.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   14 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|               ca-summit | 01) crowning disabled | Thu Sep 22 21:18:00 UTC 2022 | Sat Sep 24 09:20:00 UTC 2022 (+36hr) |                  14.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   14 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|               ca-summit |          00) original | Thu Sep 22 21:18:00 UTC 2022 | Sat Sep 24 09:20:00 UTC 2022 (+36hr) |                  14.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   14 (100.0%) |        0 (-) |         0 (-) |OLD
  ;;|               ca-summit | 01) crowning disabled | Thu Sep 22 21:18:00 UTC 2022 | Sat Sep 24 09:20:00 UTC 2022 (+36hr) |                  14.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   14 (100.0%) |        0 (-) |         0 (-) |OLD


  ;;|               ca-summit |          00) original | Tue Sep 27 09:11:00 UTC 2022 | Wed Sep 28 09:43:00 UTC 2022 (+25hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |NEW
  ;;|               ca-summit | 01) crowning disabled | Tue Sep 27 09:11:00 UTC 2022 | Wed Sep 28 09:43:00 UTC 2022 (+25hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |NEW
  ;;|               ca-summit |          00) original | Tue Sep 27 09:11:00 UTC 2022 | Wed Sep 28 09:43:00 UTC 2022 (+25hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |OLD
  ;;|               ca-summit | 01) crowning disabled | Tue Sep 27 09:11:00 UTC 2022 | Wed Sep 28 09:43:00 UTC 2022 (+25hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |OLD


  ;;|               ca-summit |          00) original | Wed Sep 21 10:15:00 UTC 2022 | Thu Sep 22 09:58:00 UTC 2022 (+24hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |NEW
  ;;|               ca-summit | 01) crowning disabled | Wed Sep 21 10:15:00 UTC 2022 | Thu Sep 22 09:58:00 UTC 2022 (+24hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |NEW
  ;;|               ca-summit |          00) original | Wed Sep 21 10:15:00 UTC 2022 | Thu Sep 22 09:58:00 UTC 2022 (+24hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |OLD
  ;;|               ca-summit | 01) crowning disabled | Wed Sep 21 10:15:00 UTC 2022 | Thu Sep 22 09:58:00 UTC 2022 (+24hr) |                   0.0 |         0 (-) |                 |      |      |      |      |           0 (-) |         0 (-) |        0 (-) |         0 (-) |OLD


  ;;|  id-caledonia-blackburn |          00) original | Sat Sep 17 09:47:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+11hr) |                 142.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  142 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|  id-caledonia-blackburn | 01) crowning disabled | Sat Sep 17 09:47:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+11hr) |                 142.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  142 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|  id-caledonia-blackburn |          00) original | Sat Sep 17 09:47:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+11hr) |                 142.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  142 (100.0%) |        0 (-) |         0 (-) |OLD
  ;;|  id-caledonia-blackburn | 01) crowning disabled | Sat Sep 17 09:47:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+11hr) |                 142.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  142 (100.0%) |        0 (-) |         0 (-) |OLD


  ;;|  id-caledonia-blackburn |          00) original | Wed Sep 14 09:52:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+72hr) |                 188.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  188 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|  id-caledonia-blackburn | 01) crowning disabled | Wed Sep 14 09:52:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+72hr) |                 188.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  188 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|  id-caledonia-blackburn |          00) original | Wed Sep 14 09:52:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+72hr) |                 188.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  188 (100.0%) |        0 (-) |         0 (-) |OLD
  ;;|  id-caledonia-blackburn | 01) crowning disabled | Wed Sep 14 09:52:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+72hr) |                 188.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  188 (100.0%) |        0 (-) |         0 (-) |OLD


  ;;|  id-columbus-bear-gulch |          00) original | Wed Sep 14 21:21:00 UTC 2022 | Sat Sep 17 10:36:00 UTC 2022 (+61hr) |                 662.0 |   406 (61.3%) |            0.22 | 0.01 | 0.46 | 0.74 | 0.83 |   3425 (517.4%) |   256 (38.7%) |   51 (12.6%) |   686 (20.0%) |NEW
  ;;|  id-columbus-bear-gulch | 01) crowning disabled | Wed Sep 14 21:21:00 UTC 2022 | Sat Sep 17 10:36:00 UTC 2022 (+61hr) |                 662.0 |   386 (58.3%) |            0.18 | 0.01 | 0.37 | 0.79 | 0.93 |   2288 (345.6%) |   276 (41.7%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|  id-columbus-bear-gulch |          00) original | Wed Sep 14 21:21:00 UTC 2022 | Sat Sep 17 10:36:00 UTC 2022 (+61hr) |                 662.0 |   425 (64.2%) |            0.16 | 0.01 | 0.38 | 0.41 | 0.42 | 19703 (2976.3%) |   237 (35.8%) |  131 (30.8%) | 12666 (64.3%) |OLD
  ;;|  id-columbus-bear-gulch | 01) crowning disabled | Wed Sep 14 21:21:00 UTC 2022 | Sat Sep 17 10:36:00 UTC 2022 (+61hr) |                 662.0 |   370 (55.9%) |            0.10 | 0.01 | 0.03 | 0.46 | 0.68 |   1357 (205.0%) |   292 (44.1%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|           id-deep-creek |          00) original | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:19:00 UTC 2022 (+11hr) |                 145.0 |    74 (51.0%) |            0.18 | 0.06 | 0.22 | 0.60 | 0.72 |    208 (143.4%) |    71 (49.0%) |     3 (4.1%) |    25 (12.0%) |NEW
  ;;|           id-deep-creek | 01) crowning disabled | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:19:00 UTC 2022 (+11hr) |                 145.0 |    74 (51.0%) |            0.20 | 0.06 | 0.32 | 0.65 | 0.72 |    159 (109.7%) |    71 (49.0%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|           id-deep-creek |          00) original | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:19:00 UTC 2022 (+11hr) |                 145.0 |    72 (49.7%) |            0.32 | 0.06 | 0.87 | 0.95 | 0.97 |      93 (64.1%) |    73 (50.3%) |   17 (23.6%) |    35 (37.6%) |OLD
  ;;|           id-deep-creek | 01) crowning disabled | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:19:00 UTC 2022 (+11hr) |                 145.0 |    65 (44.8%) |            0.16 | 0.06 | 0.06 | 0.88 | 0.96 |      28 (19.3%) |    80 (55.2%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|  id-isabella-lower-twin |          00) original | Mon Sep 19 10:49:00 UTC 2022 | Mon Sep 19 21:25:00 UTC 2022 (+11hr) |                 466.0 |   348 (74.7%) |            0.22 | 0.08 | 0.29 | 0.66 | 0.82 |     186 (39.9%) |   118 (25.3%) |    11 (3.2%) |    33 (17.7%) |NEW
  ;;|  id-isabella-lower-twin | 01) crowning disabled | Mon Sep 19 10:49:00 UTC 2022 | Mon Sep 19 21:25:00 UTC 2022 (+11hr) |                 466.0 |   348 (74.7%) |            0.22 | 0.08 | 0.29 | 0.66 | 0.82 |     168 (36.1%) |   118 (25.3%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|  id-isabella-lower-twin |          00) original | Mon Sep 19 10:49:00 UTC 2022 | Mon Sep 19 21:25:00 UTC 2022 (+11hr) |                 466.0 |   333 (71.5%) |            0.30 | 0.08 | 0.78 | 0.91 | 0.94 |     313 (67.2%) |   133 (28.5%) |   55 (16.5%) |   271 (86.6%) |OLD
  ;;|  id-isabella-lower-twin | 01) crowning disabled | Mon Sep 19 10:49:00 UTC 2022 | Mon Sep 19 21:25:00 UTC 2022 (+11hr) |                 466.0 |   320 (68.7%) |            0.24 | 0.08 | 0.08 | 0.92 | 0.95 |       26 (5.6%) |   146 (31.3%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|  id-isabella-lower-twin |          00) original | Sun Sep 18 08:37:00 UTC 2022 | Mon Sep 19 10:49:00 UTC 2022 (+26hr) |                 595.0 |    88 (14.8%) |            0.16 | 0.02 | 0.25 | 0.58 | 0.67 |    986 (165.7%) |   507 (85.2%) |     3 (3.4%) |   357 (36.2%) |NEW
  ;;|  id-isabella-lower-twin | 01) crowning disabled | Sun Sep 18 08:37:00 UTC 2022 | Mon Sep 19 10:49:00 UTC 2022 (+26hr) |                 595.0 |    88 (14.8%) |            0.17 | 0.02 | 0.27 | 0.58 | 0.70 |     372 (62.5%) |   507 (85.2%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|  id-isabella-lower-twin |          00) original | Sun Sep 18 08:37:00 UTC 2022 | Mon Sep 19 10:49:00 UTC 2022 (+26hr) |                 595.0 |    86 (14.5%) |            0.28 | 0.02 | 0.50 | 0.76 | 0.82 |   1065 (179.0%) |   509 (85.5%) |     6 (7.0%) |   488 (45.8%) |OLD
  ;;|  id-isabella-lower-twin | 01) crowning disabled | Sun Sep 18 08:37:00 UTC 2022 | Mon Sep 19 10:49:00 UTC 2022 (+26hr) |                 595.0 |    83 (13.9%) |            0.26 | 0.02 | 0.50 | 0.71 | 0.76 |     121 (20.3%) |   512 (86.1%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|  id-isabella-lower-twin |          00) original | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 08:56:00 UTC 2022 (+47hr) |                 370.0 |   183 (49.5%) |            0.22 | 0.19 | 0.34 | 0.51 | 0.62 |    642 (173.5%) |   187 (50.5%) |   25 (13.7%) |   115 (17.9%) |NEW
  ;;|  id-isabella-lower-twin | 01) crowning disabled | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 08:56:00 UTC 2022 (+47hr) |                 370.0 |   177 (47.8%) |            0.24 | 0.21 | 0.37 | 0.60 | 0.69 |    456 (123.2%) |   193 (52.2%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|  id-isabella-lower-twin |          00) original | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 08:56:00 UTC 2022 (+47hr) |                 370.0 |   187 (50.5%) |            0.23 | 0.24 | 0.31 | 0.35 | 0.72 |   1578 (426.5%) |   183 (49.5%) |   57 (30.5%) |   602 (38.1%) |OLD
  ;;|  id-isabella-lower-twin | 01) crowning disabled | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 08:56:00 UTC 2022 (+47hr) |                 370.0 |   166 (44.9%) |            0.35 | 0.31 | 0.71 | 0.82 | 0.84 |     210 (56.8%) |   204 (55.1%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|                id-katka |          00) original | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:12:00 UTC 2022 (+40hr) |                  12.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   12 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|                id-katka | 01) crowning disabled | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:12:00 UTC 2022 (+40hr) |                  12.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   12 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|                id-katka |          00) original | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:12:00 UTC 2022 (+40hr) |                  12.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   12 (100.0%) |        0 (-) |         0 (-) |OLD
  ;;|                id-katka | 01) crowning disabled | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:12:00 UTC 2022 (+40hr) |                  12.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   12 (100.0%) |        0 (-) |         0 (-) |OLD


  ;;|                id-katka |          00) original | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                 155.0 |   128 (82.6%) |            0.14 | 0.05 | 0.26 | 0.47 | 0.57 |    615 (396.8%) |    27 (17.4%) |   26 (20.3%) |    62 (10.1%) |NEW
  ;;|                id-katka | 01) crowning disabled | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                 155.0 |   128 (82.6%) |            0.22 | 0.07 | 0.37 | 0.63 | 0.69 |    519 (334.8%) |    27 (17.4%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|                id-katka |          00) original | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                 155.0 |   125 (80.6%) |            0.33 | 0.32 | 0.56 | 0.68 | 0.73 |    719 (463.9%) |    30 (19.4%) |   41 (32.8%) |   385 (53.5%) |OLD
  ;;|                id-katka | 01) crowning disabled | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                 155.0 |   119 (76.8%) |            0.35 | 0.32 | 0.60 | 0.71 | 0.76 |    205 (132.3%) |    36 (23.2%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|  id-kootenai-rv-complex |          00) original | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:04:00 UTC 2022 (+40hr) |                 488.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  488 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|  id-kootenai-rv-complex | 01) crowning disabled | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:04:00 UTC 2022 (+40hr) |                 488.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  488 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|  id-kootenai-rv-complex |          00) original | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:04:00 UTC 2022 (+40hr) |                 488.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  488 (100.0%) |        0 (-) |         0 (-) |OLD
  ;;|  id-kootenai-rv-complex | 01) crowning disabled | Fri Sep 16 20:02:00 UTC 2022 | Sun Sep 18 12:04:00 UTC 2022 (+40hr) |                 488.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |  488 (100.0%) |        0 (-) |         0 (-) |OLD


  ;;|  id-kootenai-rv-complex |          00) original | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                3567.0 |  2664 (74.7%) |            0.12 | 0.01 | 0.06 | 0.55 | 0.69 |  10064 (282.1%) |   903 (25.3%) |   127 (4.8%) |  2350 (23.4%) |NEW
  ;;|  id-kootenai-rv-complex | 01) crowning disabled | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                3567.0 |  2636 (73.9%) |            0.12 | 0.01 | 0.06 | 0.52 | 0.70 |   7647 (214.4%) |   931 (26.1%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|  id-kootenai-rv-complex |          00) original | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                3567.0 |  2652 (74.3%) |            0.17 | 0.01 | 0.25 | 0.60 | 0.69 |   8137 (228.1%) |   915 (25.7%) |  325 (12.3%) |  4668 (57.4%) |OLD
  ;;|  id-kootenai-rv-complex | 01) crowning disabled | Wed Sep 14 12:19:00 UTC 2022 | Fri Sep 16 20:02:00 UTC 2022 (+56hr) |                3567.0 |  2607 (73.1%) |            0.16 | 0.01 | 0.25 | 0.59 | 0.70 |    2989 (83.8%) |   960 (26.9%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|  id-kootenai-rv-complex |          00) original | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 02:03:00 UTC 2022 (+ 6hr) |                5185.0 |  3807 (73.4%) |            0.19 | 0.05 | 0.27 | 0.60 | 0.78 |    3121 (60.2%) |  1378 (26.6%) |   312 (8.2%) |  1471 (47.1%) |NEW
  ;;|  id-kootenai-rv-complex | 01) crowning disabled | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 02:03:00 UTC 2022 (+ 6hr) |                5185.0 |  3747 (72.3%) |            0.20 | 0.05 | 0.29 | 0.64 | 0.78 |    1221 (23.5%) |  1438 (27.7%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|  id-kootenai-rv-complex |          00) original | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 02:03:00 UTC 2022 (+ 6hr) |                5185.0 |  3903 (75.3%) |            0.16 | 0.05 | 0.19 | 0.47 | 0.63 |  10315 (198.9%) |  1282 (24.7%) |  639 (16.4%) |  6941 (67.3%) |OLD
  ;;|  id-kootenai-rv-complex | 01) crowning disabled | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 02:03:00 UTC 2022 (+ 6hr) |                5185.0 |  3747 (72.3%) |            0.20 | 0.05 | 0.29 | 0.64 | 0.78 |    1221 (23.5%) |  1438 (27.7%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|                id-lemhi |          00) original | Mon Sep 26 10:19:00 UTC 2022 | Tue Sep 27 09:09:00 UTC 2022 (+23hr) |                2002.0 |  1894 (94.6%) |            0.07 | 0.03 | 0.08 | 0.20 | 0.25 | 21625 (1080.2%) |    108 (5.4%) |    39 (2.1%) |  4690 (21.7%) |NEW
  ;;|                id-lemhi | 01) crowning disabled | Mon Sep 26 10:19:00 UTC 2022 | Tue Sep 27 09:09:00 UTC 2022 (+23hr) |                2002.0 |  1894 (94.6%) |            0.07 | 0.03 | 0.10 | 0.21 | 0.27 |  14950 (746.8%) |    108 (5.4%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|                id-lemhi |          00) original | Mon Sep 26 10:19:00 UTC 2022 | Tue Sep 27 09:09:00 UTC 2022 (+23hr) |                2002.0 |  1894 (94.6%) |            0.26 | 0.36 | 0.40 | 0.44 | 0.48 |  15492 (773.8%) |    108 (5.4%) |    78 (4.1%) |  4577 (29.5%) |OLD
  ;;|                id-lemhi | 01) crowning disabled | Mon Sep 26 10:19:00 UTC 2022 | Tue Sep 27 09:09:00 UTC 2022 (+23hr) |                2002.0 |  1894 (94.6%) |            0.27 | 0.37 | 0.42 | 0.51 | 0.55 |   9215 (460.3%) |    108 (5.4%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|                id-lemhi |          00) original | Sun Sep 25 21:12:00 UTC 2022 | Mon Sep 26 10:19:00 UTC 2022 (+13hr) |                1791.0 |   964 (53.8%) |            0.48 | 0.48 | 0.74 | 0.89 | 0.94 |     476 (26.6%) |   827 (46.2%) |    25 (2.6%) |   126 (26.5%) |NEW
  ;;|                id-lemhi | 01) crowning disabled | Sun Sep 25 21:12:00 UTC 2022 | Mon Sep 26 10:19:00 UTC 2022 (+13hr) |                1791.0 |   956 (53.4%) |            0.48 | 0.49 | 0.73 | 0.89 | 0.94 |     224 (12.5%) |   835 (46.6%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|                id-lemhi |          00) original | Sun Sep 25 21:12:00 UTC 2022 | Mon Sep 26 10:19:00 UTC 2022 (+13hr) |                1791.0 |   981 (54.8%) |            0.42 | 0.44 | 0.64 | 0.76 | 0.80 |   2213 (123.6%) |   810 (45.2%) |    90 (9.2%) |  1181 (53.4%) |OLD
  ;;|                id-lemhi | 01) crowning disabled | Sun Sep 25 21:12:00 UTC 2022 | Mon Sep 26 10:19:00 UTC 2022 (+13hr) |                1791.0 |   830 (46.3%) |            0.40 | 0.41 | 0.62 | 0.75 | 0.80 |      128 (7.1%) |   961 (53.7%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|                id-lemhi |          00) original | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+14hr) |                3684.0 |  3600 (97.7%) |            0.21 | 0.20 | 0.28 | 0.36 | 0.42 |  12302 (333.9%) |     84 (2.3%) |  732 (20.3%) |  2864 (23.3%) |NEW
  ;;|                id-lemhi | 01) crowning disabled | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+14hr) |                3684.0 |  3219 (87.4%) |            0.28 | 0.23 | 0.38 | 0.60 | 0.76 |   7717 (209.5%) |   465 (12.6%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|                id-lemhi |          00) original | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+14hr) |                3684.0 |  3600 (97.7%) |            0.19 | 0.19 | 0.27 | 0.33 | 0.38 |  13368 (362.9%) |     84 (2.3%) |  945 (26.3%) |  4785 (35.8%) |OLD
  ;;|                id-lemhi | 01) crowning disabled | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+14hr) |                3684.0 |  3152 (85.6%) |            0.26 | 0.22 | 0.36 | 0.54 | 0.67 |   5897 (160.1%) |   532 (14.4%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|              id-ross-fk |          00) original | Tue Sep 13 09:24:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+25hr) |                7284.0 |  2012 (27.6%) |            0.10 | 0.02 | 0.05 | 0.47 | 0.69 |    2695 (37.0%) |  5272 (72.4%) |   106 (5.3%) |   353 (13.1%) |NEW
  ;;|              id-ross-fk | 01) crowning disabled | Tue Sep 13 09:24:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+25hr) |                7284.0 |  2004 (27.5%) |            0.10 | 0.02 | 0.05 | 0.44 | 0.69 |    1936 (26.6%) |  5280 (72.5%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|              id-ross-fk |          00) original | Tue Sep 13 09:24:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+25hr) |                7284.0 |  1964 (27.0%) |            0.23 | 0.02 | 0.47 | 0.56 | 0.63 |    1229 (16.9%) |  5320 (73.0%) |    90 (4.6%) |   223 (18.1%) |OLD
  ;;|              id-ross-fk | 01) crowning disabled | Tue Sep 13 09:24:00 UTC 2022 | Wed Sep 14 09:54:00 UTC 2022 (+25hr) |                7284.0 |  1959 (26.9%) |            0.23 | 0.02 | 0.48 | 0.56 | 0.61 |     783 (10.7%) |  5325 (73.1%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|              id-tenmile |          00) original | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                 289.0 |   244 (84.4%) |            0.11 | 0.04 | 0.09 | 0.28 | 0.45 |   1041 (360.2%) |    45 (15.6%) |     5 (2.0%) |     26 (2.5%) |NEW
  ;;|              id-tenmile | 01) crowning disabled | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                 289.0 |   244 (84.4%) |            0.11 | 0.04 | 0.10 | 0.30 | 0.45 |    971 (336.0%) |    45 (15.6%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|              id-tenmile |          00) original | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                 289.0 |   242 (83.7%) |            0.21 | 0.04 | 0.42 | 0.55 | 0.60 |    775 (268.2%) |    47 (16.3%) |    13 (5.4%) |     46 (5.9%) |OLD
  ;;|              id-tenmile | 01) crowning disabled | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                 289.0 |   242 (83.7%) |            0.22 | 0.04 | 0.44 | 0.56 | 0.62 |    538 (186.2%) |    47 (16.3%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|          id-trail-ridge |          00) original | Sat Sep 17 10:23:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+10hr) |                   2.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    2 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|          id-trail-ridge | 01) crowning disabled | Sat Sep 17 10:23:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+10hr) |                   2.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    2 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|          id-trail-ridge |          00) original | Sat Sep 17 10:23:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+10hr) |                   2.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    2 (100.0%) |        0 (-) |         0 (-) |OLD
  ;;|          id-trail-ridge | 01) crowning disabled | Sat Sep 17 10:23:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+10hr) |                   2.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    2 (100.0%) |        0 (-) |         0 (-) |OLD


  ;;|             mt-billiard |          00) original | Wed Sep 14 09:52:00 UTC 2022 | Fri Sep 16 11:11:00 UTC 2022 (+49hr) |                 356.0 |   198 (55.6%) |            0.15 | 0.02 | 0.09 | 0.71 | 0.80 |   1554 (436.5%) |   158 (44.4%) |   28 (14.1%) |   296 (19.0%) |NEW
  ;;|             mt-billiard | 01) crowning disabled | Wed Sep 14 09:52:00 UTC 2022 | Fri Sep 16 11:11:00 UTC 2022 (+49hr) |                 356.0 |   192 (53.9%) |            0.12 | 0.02 | 0.08 | 0.66 | 0.75 |   1238 (347.8%) |   164 (46.1%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|             mt-billiard |          00) original | Wed Sep 14 09:52:00 UTC 2022 | Fri Sep 16 11:11:00 UTC 2022 (+49hr) |                 356.0 |   200 (56.2%) |            0.23 | 0.02 | 0.30 | 0.76 | 0.78 |  4757 (1336.2%) |   156 (43.8%) |   54 (27.0%) |  3225 (67.8%) |OLD
  ;;|             mt-billiard | 01) crowning disabled | Wed Sep 14 09:52:00 UTC 2022 | Fri Sep 16 11:11:00 UTC 2022 (+49hr) |                 356.0 |   191 (53.7%) |            0.20 | 0.02 | 0.29 | 0.71 | 0.79 |    726 (203.9%) |   165 (46.3%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|               mt-cannon |          00) original | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+59hr) |                 141.0 |   132 (93.6%) |            0.09 | 0.02 | 0.12 | 0.20 | 0.54 |  2120 (1503.5%) |      9 (6.4%) |     4 (3.0%) |    182 (8.6%) |NEW
  ;;|               mt-cannon | 01) crowning disabled | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+59hr) |                 141.0 |   132 (93.6%) |            0.09 | 0.02 | 0.12 | 0.20 | 0.55 |  1757 (1246.1%) |      9 (6.4%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|               mt-cannon |          00) original | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+59hr) |                 141.0 |   129 (91.5%) |            0.14 | 0.01 | 0.18 | 0.40 | 0.63 |  1545 (1095.7%) |     12 (8.5%) |    10 (7.8%) |   241 (15.6%) |OLD
  ;;|               mt-cannon | 01) crowning disabled | Thu Sep 15 09:34:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+59hr) |                 141.0 |   129 (91.5%) |            0.14 | 0.01 | 0.19 | 0.40 | 0.63 |   1122 (795.7%) |     12 (8.5%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|          mt-george-lake |          00) original | Tue Sep 13 09:22:00 UTC 2022 | Thu Sep 15 11:09:00 UTC 2022 (+50hr) |                1349.0 |   766 (56.8%) |            0.09 | 0.04 | 0.11 | 0.24 | 0.30 |   1395 (103.4%) |   583 (43.2%) |    71 (9.3%) |   172 (12.3%) |NEW
  ;;|          mt-george-lake | 01) crowning disabled | Tue Sep 13 09:22:00 UTC 2022 | Thu Sep 15 11:09:00 UTC 2022 (+50hr) |                1349.0 |   762 (56.5%) |            0.14 | 0.07 | 0.25 | 0.40 | 0.46 |     775 (57.4%) |   587 (43.5%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|          mt-george-lake |          00) original | Tue Sep 13 09:22:00 UTC 2022 | Thu Sep 15 11:09:00 UTC 2022 (+50hr) |                1349.0 |   778 (57.7%) |            0.22 | 0.21 | 0.27 | 0.71 | 0.80 |   2248 (166.6%) |   571 (42.3%) |  104 (13.4%) |   896 (39.9%) |OLD
  ;;|          mt-george-lake | 01) crowning disabled | Tue Sep 13 09:22:00 UTC 2022 | Thu Sep 15 11:09:00 UTC 2022 (+50hr) |                1349.0 |   757 (56.1%) |            0.28 | 0.24 | 0.44 | 0.77 | 0.81 |     311 (23.1%) |   592 (43.9%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|           mt-government |          00) original | Fri Sep 16 11:01:00 UTC 2022 | Sat Sep 17 11:20:00 UTC 2022 (+24hr) |                  89.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   89 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|           mt-government | 01) crowning disabled | Fri Sep 16 11:01:00 UTC 2022 | Sat Sep 17 11:20:00 UTC 2022 (+24hr) |                  89.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   89 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|           mt-government |          00) original | Fri Sep 16 11:01:00 UTC 2022 | Sat Sep 17 11:20:00 UTC 2022 (+24hr) |                  89.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   89 (100.0%) |        0 (-) |         0 (-) |OLD
  ;;|           mt-government | 01) crowning disabled | Fri Sep 16 11:01:00 UTC 2022 | Sat Sep 17 11:20:00 UTC 2022 (+24hr) |                  89.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   89 (100.0%) |        0 (-) |         0 (-) |OLD


  ;;|           mt-government |          00) original | Sat Sep 17 11:20:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+ 9hr) |                  96.0 |    60 (62.5%) |            0.09 | 0.04 | 0.10 | 0.27 | 0.45 |      63 (65.6%) |    36 (37.5%) |   18 (30.0%) |     7 (11.1%) |NEW
  ;;|           mt-government | 01) crowning disabled | Sat Sep 17 11:20:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+ 9hr) |                  96.0 |    60 (62.5%) |            0.14 | 0.04 | 0.27 | 0.47 | 0.53 |      40 (41.7%) |    36 (37.5%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|           mt-government |          00) original | Sat Sep 17 11:20:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+ 9hr) |                  96.0 |    60 (62.5%) |            0.26 | 0.04 | 0.82 | 0.84 | 0.86 |      81 (84.4%) |    36 (37.5%) |   28 (46.7%) |   81 (100.0%) |OLD
  ;;|           mt-government | 01) crowning disabled | Sat Sep 17 11:20:00 UTC 2022 | Sat Sep 17 20:21:00 UTC 2022 (+ 9hr) |                  96.0 |    53 (55.2%) |            0.08 | 0.04 | 0.04 | 0.04 | 0.83 |        0 (0.0%) |    43 (44.8%) |     0 (0.0%) |         0 (-) |OLD


  ;;|           mt-government |          00) original | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 09:28:00 UTC 2022 (+13hr) |                  96.0 |    63 (65.6%) |            0.14 | 0.03 | 0.07 | 0.74 | 0.97 |    147 (153.1%) |    33 (34.4%) |   21 (33.3%) |    32 (21.8%) |NEW
  ;;|           mt-government | 01) crowning disabled | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 09:28:00 UTC 2022 (+13hr) |                  96.0 |    60 (62.5%) |            0.09 | 0.03 | 0.17 | 0.27 | 0.32 |      86 (89.6%) |    36 (37.5%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|           mt-government |          00) original | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 09:28:00 UTC 2022 (+13hr) |                  96.0 |    87 (90.6%) |            0.19 | 0.07 | 0.34 | 0.47 | 0.49 |  1499 (1561.5%) |      9 (9.4%) |   44 (50.6%) |   965 (64.4%) |OLD
  ;;|           mt-government | 01) crowning disabled | Sat Sep 17 20:21:00 UTC 2022 | Sun Sep 18 09:28:00 UTC 2022 (+13hr) |                  96.0 |    60 (62.5%) |            0.09 | 0.03 | 0.17 | 0.27 | 0.32 |      76 (79.2%) |    36 (37.5%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|             mt-margaret |          00) original | Thu Sep 15 08:43:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+49hr) |                 679.0 |   661 (97.3%) |            0.25 | 0.18 | 0.41 | 0.69 | 0.79 |   2875 (423.4%) |     18 (2.7%) |    17 (2.6%) |   345 (12.0%) |NEW
  ;;|             mt-margaret | 01) crowning disabled | Thu Sep 15 08:43:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+49hr) |                 679.0 |   661 (97.3%) |            0.25 | 0.18 | 0.41 | 0.69 | 0.79 |   2192 (322.8%) |     18 (2.7%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|             mt-margaret |          00) original | Thu Sep 15 08:43:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+49hr) |                 679.0 |   648 (95.4%) |            0.34 | 0.26 | 0.70 | 0.84 | 0.92 |   2611 (384.5%) |     31 (4.6%) |   70 (10.8%) |   688 (26.4%) |OLD
  ;;|             mt-margaret | 01) crowning disabled | Thu Sep 15 08:43:00 UTC 2022 | Sat Sep 17 09:47:00 UTC 2022 (+49hr) |                 679.0 |   625 (92.0%) |            0.33 | 0.29 | 0.49 | 0.82 | 0.90 |    792 (116.6%) |     54 (8.0%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|          or-cedar-creek |          00) original | Fri Sep 16 09:15:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+24hr) |               19045.0 |  6552 (34.4%) |            0.33 | 0.26 | 0.60 | 0.81 | 0.89 |    3173 (16.7%) | 12493 (65.6%) |    23 (0.4%) |     21 (0.7%) |NEW
  ;;|          or-cedar-creek | 01) crowning disabled | Fri Sep 16 09:15:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+24hr) |               19045.0 |  6526 (34.3%) |            0.33 | 0.25 | 0.60 | 0.80 | 0.89 |    3124 (16.4%) | 12519 (65.7%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|          or-cedar-creek |          00) original | Fri Sep 16 09:15:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+24hr) |               19045.0 |  6322 (33.2%) |            0.42 | 0.51 | 0.67 | 0.83 | 0.90 |    1923 (10.1%) | 12723 (66.8%) |   288 (4.6%) |     67 (3.5%) |OLD
  ;;|          or-cedar-creek | 01) crowning disabled | Fri Sep 16 09:15:00 UTC 2022 | Sat Sep 17 08:58:00 UTC 2022 (+24hr) |               19045.0 |  6014 (31.6%) |            0.39 | 0.48 | 0.67 | 0.83 | 0.91 |     1646 (8.6%) | 13031 (68.4%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|         or-double-creek |          00) original | Mon Sep 19 09:09:00 UTC 2022 | Tue Sep 20 08:52:00 UTC 2022 (+24hr) |                1883.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1883 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|         or-double-creek | 01) crowning disabled | Mon Sep 19 09:09:00 UTC 2022 | Tue Sep 20 08:52:00 UTC 2022 (+24hr) |                1883.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1883 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|         or-double-creek |          00) original | Mon Sep 19 09:09:00 UTC 2022 | Tue Sep 20 08:52:00 UTC 2022 (+24hr) |                1883.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1883 (100.0%) |        0 (-) |         0 (-) |OLD
  ;;|         or-double-creek | 01) crowning disabled | Mon Sep 19 09:09:00 UTC 2022 | Tue Sep 20 08:52:00 UTC 2022 (+24hr) |                1883.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1883 (100.0%) |        0 (-) |         0 (-) |OLD


  ;;|         or-double-creek |          00) original | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                4588.0 |  1439 (31.4%) |            0.34 | 0.25 | 0.58 | 0.78 | 0.89 |    2635 (57.4%) |  3149 (68.6%) |   102 (7.1%) |   603 (22.9%) |NEW
  ;;|         or-double-creek | 01) crowning disabled | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                4588.0 |  1341 (29.2%) |            0.38 | 0.36 | 0.61 | 0.82 | 0.91 |     935 (20.4%) |  3247 (70.8%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|         or-double-creek |          00) original | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                4588.0 |  1396 (30.4%) |            0.47 | 0.48 | 0.72 | 0.86 | 0.92 |   5861 (127.7%) |  3192 (69.6%) |  161 (11.5%) |  1968 (33.6%) |OLD
  ;;|         or-double-creek | 01) crowning disabled | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                4588.0 |  1217 (26.5%) |            0.46 | 0.55 | 0.73 | 0.88 | 0.95 |     573 (12.5%) |  3371 (73.5%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|         or-double-creek |          00) original | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |               20434.0 |  3637 (17.8%) |            0.47 | 0.47 | 0.74 | 0.92 | 0.96 |       48 (0.2%) | 16797 (82.2%) |   296 (8.1%) |      0 (0.0%) |NEW
  ;;|         or-double-creek | 01) crowning disabled | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |               20434.0 |  3063 (15.0%) |            0.44 | 0.43 | 0.72 | 0.90 | 0.96 |       45 (0.2%) | 17371 (85.0%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|         or-double-creek |          00) original | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |               20434.0 |  2542 (12.4%) |            0.47 | 0.07 | 0.94 | 0.98 | 0.99 |        0 (0.0%) | 17892 (87.6%) |   227 (8.9%) |         0 (-) |OLD
  ;;|         or-double-creek | 01) crowning disabled | Wed Sep 28 08:50:00 UTC 2022 | Wed Sep 28 20:17:00 UTC 2022 (+11hr) |               20434.0 |  2256 (11.0%) |            0.14 | 0.07 | 0.07 | 0.07 | 0.96 |        0 (0.0%) | 18178 (89.0%) |     0 (0.0%) |         0 (-) |OLD


  ;;|         or-double-creek |          00) original | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 08:33:00 UTC 2022 (+12hr) |               25821.0 | 19136 (74.1%) |            0.22 | 0.04 | 0.34 | 0.70 | 0.84 |     1878 (7.3%) |  6685 (25.9%) |   200 (1.0%) |   276 (14.7%) |NEW
  ;;|         or-double-creek | 01) crowning disabled | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 08:33:00 UTC 2022 (+12hr) |               25821.0 | 19022 (73.7%) |            0.23 | 0.02 | 0.39 | 0.72 | 0.87 |     1028 (4.0%) |  6799 (26.3%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|         or-double-creek |          00) original | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 08:33:00 UTC 2022 (+12hr) |               25821.0 | 19722 (76.4%) |            0.25 | 0.12 | 0.44 | 0.72 | 0.84 |    4966 (19.2%) |  6099 (23.6%) |   596 (3.0%) |  1382 (27.8%) |OLD
  ;;|         or-double-creek | 01) crowning disabled | Wed Sep 28 20:17:00 UTC 2022 | Thu Sep 29 08:33:00 UTC 2022 (+12hr) |               25821.0 | 19011 (73.6%) |            0.23 | 0.02 | 0.38 | 0.71 | 0.86 |      995 (3.9%) |  6810 (26.4%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|             or-sturgill |          00) original | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                6854.0 |  5510 (80.4%) |            0.34 | 0.31 | 0.56 | 0.67 | 0.76 |    5652 (82.5%) |  1344 (19.6%) |   343 (6.2%) |  1108 (19.6%) |NEW
  ;;|             or-sturgill | 01) crowning disabled | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                6854.0 |  5147 (75.1%) |            0.34 | 0.27 | 0.54 | 0.78 | 0.87 |    2615 (38.2%) |  1707 (24.9%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|             or-sturgill |          00) original | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                6854.0 |  5505 (80.3%) |            0.39 | 0.44 | 0.50 | 0.56 | 0.68 |  15066 (219.8%) |  1349 (19.7%) |  723 (13.1%) |  5918 (39.3%) |OLD
  ;;|             or-sturgill | 01) crowning disabled | Tue Sep 27 10:49:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+22hr) |                6854.0 |  4906 (71.6%) |            0.45 | 0.49 | 0.69 | 0.86 | 0.92 |    1469 (21.4%) |  1948 (28.4%) |     3 (0.1%) |      0 (0.0%) |OLD


  ;;|             or-sturgill |          00) original | Wed Sep 14 09:54:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                 884.0 |   203 (23.0%) |            0.08 | 0.04 | 0.04 | 0.22 | 0.29 |     118 (13.3%) |   681 (77.0%) |     4 (2.0%) |      7 (5.9%) |NEW
  ;;|             or-sturgill | 01) crowning disabled | Wed Sep 14 09:54:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                 884.0 |   203 (23.0%) |            0.09 | 0.04 | 0.04 | 0.25 | 0.29 |     107 (12.1%) |   681 (77.0%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|             or-sturgill |          00) original | Wed Sep 14 09:54:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                 884.0 |   203 (23.0%) |            0.15 | 0.04 | 0.04 | 0.50 | 0.63 |     210 (23.8%) |   681 (77.0%) |     9 (4.4%) |    66 (31.4%) |OLD
  ;;|             or-sturgill | 01) crowning disabled | Wed Sep 14 09:54:00 UTC 2022 | Thu Sep 15 09:34:00 UTC 2022 (+24hr) |                 884.0 |   203 (23.0%) |            0.15 | 0.04 | 0.04 | 0.52 | 0.63 |       66 (7.5%) |   681 (77.0%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|           wa-bolt-creek |          00) original | Thu Sep 22 09:54:00 UTC 2022 | Thu Sep 22 20:29:00 UTC 2022 (+11hr) |                4179.0 |  3326 (79.6%) |            0.22 | 0.09 | 0.23 | 0.66 | 0.81 |    2683 (64.2%) |   853 (20.4%) |   233 (7.0%) |  1836 (68.4%) |NEW
  ;;|           wa-bolt-creek | 01) crowning disabled | Thu Sep 22 09:54:00 UTC 2022 | Thu Sep 22 20:29:00 UTC 2022 (+11hr) |                4179.0 |  3280 (78.5%) |            0.24 | 0.09 | 0.38 | 0.68 | 0.80 |     461 (11.0%) |   899 (21.5%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|           wa-bolt-creek |          00) original | Thu Sep 22 09:54:00 UTC 2022 | Thu Sep 22 20:29:00 UTC 2022 (+11hr) |                4179.0 |  3175 (76.0%) |            0.27 | 0.09 | 0.09 | 0.90 | 0.95 |     857 (20.5%) |  1004 (24.0%) |   260 (8.2%) |   794 (92.6%) |OLD
  ;;|           wa-bolt-creek | 01) crowning disabled | Thu Sep 22 09:54:00 UTC 2022 | Thu Sep 22 20:29:00 UTC 2022 (+11hr) |                4179.0 |  3074 (73.6%) |            0.13 | 0.09 | 0.09 | 0.09 | 0.76 |       14 (0.3%) |  1105 (26.4%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|           wa-goat-rocks |          00) original | Mon Sep 26 11:08:00 UTC 2022 | Tue Sep 27 10:00:00 UTC 2022 (+23hr) |                2916.0 |  2349 (80.6%) |            0.25 | 0.11 | 0.42 | 0.74 | 0.86 |    1700 (58.3%) |   567 (19.4%) |    45 (1.9%) |    126 (7.4%) |NEW
  ;;|           wa-goat-rocks | 01) crowning disabled | Mon Sep 26 11:08:00 UTC 2022 | Tue Sep 27 10:00:00 UTC 2022 (+23hr) |                2916.0 |  2349 (80.6%) |            0.25 | 0.13 | 0.42 | 0.74 | 0.86 |    1490 (51.1%) |   567 (19.4%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|           wa-goat-rocks |          00) original | Mon Sep 26 11:08:00 UTC 2022 | Tue Sep 27 10:00:00 UTC 2022 (+23hr) |                2916.0 |  2241 (76.9%) |            0.36 | 0.38 | 0.50 | 0.71 | 0.81 |    2110 (72.4%) |   675 (23.1%) |    92 (4.1%) |   623 (29.5%) |OLD
  ;;|           wa-goat-rocks | 01) crowning disabled | Mon Sep 26 11:08:00 UTC 2022 | Tue Sep 27 10:00:00 UTC 2022 (+23hr) |                2916.0 |  2204 (75.6%) |            0.36 | 0.38 | 0.50 | 0.70 | 0.81 |     879 (30.1%) |   712 (24.4%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|           wa-goat-rocks |          00) original | Sun Sep 25 09:45:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+11hr) |                   9.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    9 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|           wa-goat-rocks | 01) crowning disabled | Sun Sep 25 09:45:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+11hr) |                   9.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    9 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|           wa-goat-rocks |          00) original | Sun Sep 25 09:45:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+11hr) |                   9.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    9 (100.0%) |        0 (-) |         0 (-) |OLD
  ;;|           wa-goat-rocks | 01) crowning disabled | Sun Sep 25 09:45:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+11hr) |                   9.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |    9 (100.0%) |        0 (-) |         0 (-) |OLD


  ;;|           wa-goat-rocks |          00) original | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+14hr) |                1787.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1787 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|           wa-goat-rocks | 01) crowning disabled | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+14hr) |                1787.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1787 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|           wa-goat-rocks |          00) original | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+14hr) |                1787.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1787 (100.0%) |        0 (-) |         0 (-) |OLD
  ;;|           wa-goat-rocks | 01) crowning disabled | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+14hr) |                1787.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) | 1787 (100.0%) |        0 (-) |         0 (-) |OLD


  ;;|            wa-irving-pk |          00) original | Mon Sep 19 10:00:00 UTC 2022 | Tue Sep 20 08:50:00 UTC 2022 (+23hr) |                1621.0 |   619 (38.2%) |            0.30 | 0.19 | 0.50 | 0.70 | 0.84 |     735 (45.3%) |  1002 (61.8%) |    19 (3.1%) |     56 (7.6%) |NEW
  ;;|            wa-irving-pk | 01) crowning disabled | Mon Sep 19 10:00:00 UTC 2022 | Tue Sep 20 08:50:00 UTC 2022 (+23hr) |                1621.0 |   617 (38.1%) |            0.32 | 0.23 | 0.55 | 0.75 | 0.89 |     658 (40.6%) |  1004 (61.9%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|            wa-irving-pk |          00) original | Mon Sep 19 10:00:00 UTC 2022 | Tue Sep 20 08:50:00 UTC 2022 (+23hr) |                1621.0 |   605 (37.3%) |            0.53 | 0.48 | 0.65 | 0.81 | 0.87 |     805 (49.7%) |  1016 (62.7%) |    55 (9.1%) |   210 (26.1%) |OLD
  ;;|            wa-irving-pk | 01) crowning disabled | Mon Sep 19 10:00:00 UTC 2022 | Tue Sep 20 08:50:00 UTC 2022 (+23hr) |                1621.0 |   598 (36.9%) |            0.54 | 0.49 | 0.65 | 0.79 | 0.89 |     407 (25.1%) |  1023 (63.1%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|            wa-irving-pk |          00) original | Mon Sep 26 09:28:00 UTC 2022 | Tue Sep 27 09:07:00 UTC 2022 (+24hr) |                 661.0 |   423 (64.0%) |            0.14 | 0.02 | 0.15 | 0.43 | 0.78 |   2161 (326.9%) |   238 (36.0%) |    16 (3.8%) |   868 (40.2%) |NEW
  ;;|            wa-irving-pk | 01) crowning disabled | Mon Sep 26 09:28:00 UTC 2022 | Tue Sep 27 09:07:00 UTC 2022 (+24hr) |                 661.0 |   418 (63.2%) |            0.13 | 0.02 | 0.15 | 0.41 | 0.69 |     392 (59.3%) |   243 (36.8%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|            wa-irving-pk |          00) original | Mon Sep 26 09:28:00 UTC 2022 | Tue Sep 27 09:07:00 UTC 2022 (+24hr) |                 661.0 |   426 (64.4%) |            0.23 | 0.02 | 0.42 | 0.61 | 0.88 |   5493 (831.0%) |   235 (35.6%) |   66 (15.5%) |  3206 (58.4%) |OLD
  ;;|            wa-irving-pk | 01) crowning disabled | Mon Sep 26 09:28:00 UTC 2022 | Tue Sep 27 09:07:00 UTC 2022 (+24hr) |                 661.0 |   408 (61.7%) |            0.20 | 0.02 | 0.48 | 0.56 | 0.68 |     191 (28.9%) |   253 (38.3%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|            wa-irving-pk |          00) original | Sat Sep 17 09:47:00 UTC 2022 | Mon Sep 19 10:00:00 UTC 2022 (+48hr) |                1286.0 |   314 (24.4%) |            0.09 | 0.06 | 0.12 | 0.24 | 0.30 |   2531 (196.8%) |   972 (75.6%) |    15 (4.8%) |   403 (15.9%) |NEW
  ;;|            wa-irving-pk | 01) crowning disabled | Sat Sep 17 09:47:00 UTC 2022 | Mon Sep 19 10:00:00 UTC 2022 (+48hr) |                1286.0 |   314 (24.4%) |            0.12 | 0.08 | 0.13 | 0.27 | 0.53 |   1478 (114.9%) |   972 (75.6%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|            wa-irving-pk |          00) original | Sat Sep 17 09:47:00 UTC 2022 | Mon Sep 19 10:00:00 UTC 2022 (+48hr) |                1286.0 |   344 (26.7%) |            0.24 | 0.20 | 0.24 | 0.82 | 0.88 |   5712 (444.2%) |   942 (73.3%) |  107 (31.1%) |  2589 (45.3%) |OLD
  ;;|            wa-irving-pk | 01) crowning disabled | Sat Sep 17 09:47:00 UTC 2022 | Mon Sep 19 10:00:00 UTC 2022 (+48hr) |                1286.0 |   306 (23.8%) |            0.19 | 0.23 | 0.27 | 0.35 | 0.47 |     702 (54.6%) |   980 (76.2%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|            wa-irving-pk |          00) original | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                 678.0 |   288 (42.5%) |            0.16 | 0.04 | 0.26 | 0.44 | 0.52 |     240 (35.4%) |   390 (57.5%) |    16 (5.6%) |   103 (42.9%) |NEW
  ;;|            wa-irving-pk | 01) crowning disabled | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                 678.0 |   272 (40.1%) |            0.20 | 0.02 | 0.36 | 0.73 | 0.78 |       47 (6.9%) |   406 (59.9%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|            wa-irving-pk |          00) original | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                 678.0 |   290 (42.8%) |            0.07 | 0.03 | 0.08 | 0.14 | 0.17 |   2846 (419.8%) |   388 (57.2%) |   59 (20.3%) |  1740 (61.1%) |OLD
  ;;|            wa-irving-pk | 01) crowning disabled | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                 678.0 |   270 (39.8%) |            0.19 | 0.02 | 0.26 | 0.70 | 0.76 |       40 (5.9%) |   408 (60.2%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|            wa-irving-pk |          00) original | Thu Sep 15 10:26:00 UTC 2022 | Fri Sep 16 09:15:00 UTC 2022 (+23hr) |                  81.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   81 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|            wa-irving-pk | 01) crowning disabled | Thu Sep 15 10:26:00 UTC 2022 | Fri Sep 16 09:15:00 UTC 2022 (+23hr) |                  81.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   81 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|            wa-irving-pk |          00) original | Thu Sep 15 10:26:00 UTC 2022 | Fri Sep 16 09:15:00 UTC 2022 (+23hr) |                  81.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   81 (100.0%) |        0 (-) |         0 (-) |OLD
  ;;|            wa-irving-pk | 01) crowning disabled | Thu Sep 15 10:26:00 UTC 2022 | Fri Sep 16 09:15:00 UTC 2022 (+23hr) |                  81.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   81 (100.0%) |        0 (-) |         0 (-) |OLD


  ;;|            wa-irving-pk |          00) original | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2760.0 |  2087 (75.6%) |            0.10 | 0.01 | 0.07 | 0.28 | 0.73 |   4596 (166.5%) |   673 (24.4%) |   146 (7.0%) |   569 (12.4%) |NEW
  ;;|            wa-irving-pk | 01) crowning disabled | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2760.0 |  2070 (75.0%) |            0.11 | 0.01 | 0.12 | 0.29 | 0.67 |   3483 (126.2%) |   690 (25.0%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|            wa-irving-pk |          00) original | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2760.0 |  2180 (79.0%) |            0.17 | 0.02 | 0.08 | 0.73 | 0.82 |   9202 (333.4%) |   580 (21.0%) |  304 (13.9%) |  3574 (38.8%) |OLD
  ;;|            wa-irving-pk | 01) crowning disabled | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2760.0 |  2059 (74.6%) |            0.11 | 0.01 | 0.10 | 0.26 | 0.69 |    2470 (89.5%) |   701 (25.4%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|            wa-irving-pk |          00) original | Wed Sep 28 10:30:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+24hr) |                 926.0 |   657 (71.0%) |            0.10 | 0.02 | 0.13 | 0.23 | 0.44 |   1816 (196.1%) |   269 (29.0%) |    51 (7.8%) |   614 (33.8%) |NEW
  ;;|            wa-irving-pk | 01) crowning disabled | Wed Sep 28 10:30:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+24hr) |                 926.0 |   655 (70.7%) |            0.11 | 0.02 | 0.18 | 0.22 | 0.40 |     606 (65.4%) |   271 (29.3%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|            wa-irving-pk |          00) original | Wed Sep 28 10:30:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+24hr) |                 926.0 |   655 (70.7%) |            0.18 | 0.02 | 0.37 | 0.48 | 0.54 |   2802 (302.6%) |   271 (29.3%) |  112 (17.1%) |  1517 (54.1%) |OLD
  ;;|            wa-irving-pk | 01) crowning disabled | Wed Sep 28 10:30:00 UTC 2022 | Thu Sep 29 10:11:00 UTC 2022 (+24hr) |                 926.0 |   649 (70.1%) |            0.20 | 0.02 | 0.47 | 0.52 | 0.57 |     303 (32.7%) |   277 (29.9%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|                  wa-kid |          00) original | Tue Sep 13 20:51:00 UTC 2022 | Wed Sep 14 09:52:00 UTC 2022 (+13hr) |                  78.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   78 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|                  wa-kid | 01) crowning disabled | Tue Sep 13 20:51:00 UTC 2022 | Wed Sep 14 09:52:00 UTC 2022 (+13hr) |                  78.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   78 (100.0%) |        0 (-) |         0 (-) |NEW
  ;;|                  wa-kid |          00) original | Tue Sep 13 20:51:00 UTC 2022 | Wed Sep 14 09:52:00 UTC 2022 (+13hr) |                  78.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   78 (100.0%) |        0 (-) |         0 (-) |OLD
  ;;|                  wa-kid | 01) crowning disabled | Tue Sep 13 20:51:00 UTC 2022 | Wed Sep 14 09:52:00 UTC 2022 (+13hr) |                  78.0 |      0 (0.0%) |                 |      |      |      |      |        0 (0.0%) |   78 (100.0%) |        0 (-) |         0 (-) |OLD


  ;;|         wa-minnow-ridge |          00) original | Sun Sep 25 08:58:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+12hr) |                2408.0 |  2137 (88.7%) |            0.31 | 0.19 | 0.47 | 0.79 | 0.89 |     694 (28.8%) |   271 (11.3%) |    88 (4.1%) |   180 (25.9%) |NEW
  ;;|         wa-minnow-ridge | 01) crowning disabled | Sun Sep 25 08:58:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+12hr) |                2408.0 |  2108 (87.5%) |            0.31 | 0.20 | 0.47 | 0.81 | 0.92 |     432 (17.9%) |   300 (12.5%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|         wa-minnow-ridge |          00) original | Sun Sep 25 08:58:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+12hr) |                2408.0 |  2026 (84.1%) |            0.48 | 0.08 | 0.89 | 0.97 | 0.98 |     447 (18.6%) |   382 (15.9%) |  253 (12.5%) |   360 (80.5%) |OLD
  ;;|         wa-minnow-ridge | 01) crowning disabled | Sun Sep 25 08:58:00 UTC 2022 | Sun Sep 25 21:14:00 UTC 2022 (+12hr) |                2408.0 |  1899 (78.9%) |            0.32 | 0.08 | 0.88 | 0.95 | 0.98 |       30 (1.2%) |   509 (21.1%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|         wa-minnow-ridge |          00) original | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                2298.0 |  2183 (95.0%) |            0.17 | 0.06 | 0.25 | 0.58 | 0.72 |    1135 (49.4%) |    115 (5.0%) |    90 (4.1%) |   279 (24.6%) |NEW
  ;;|         wa-minnow-ridge | 01) crowning disabled | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                2298.0 |  2149 (93.5%) |            0.16 | 0.02 | 0.27 | 0.45 | 0.65 |     709 (30.9%) |    149 (6.5%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|         wa-minnow-ridge |          00) original | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                2298.0 |  2186 (95.1%) |            0.14 | 0.03 | 0.17 | 0.54 | 0.68 |   3105 (135.1%) |    112 (4.9%) |   186 (8.5%) |  1906 (61.4%) |OLD
  ;;|         wa-minnow-ridge | 01) crowning disabled | Sun Sep 25 21:14:00 UTC 2022 | Mon Sep 26 09:28:00 UTC 2022 (+12hr) |                2298.0 |  2148 (93.5%) |            0.16 | 0.02 | 0.27 | 0.44 | 0.63 |     598 (26.0%) |    150 (6.5%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|         wa-minnow-ridge |          00) original | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2184.0 |  2145 (98.2%) |            0.16 | 0.05 | 0.15 | 0.58 | 0.66 |   7045 (322.6%) |     39 (1.8%) |  324 (15.1%) |  1394 (19.8%) |NEW
  ;;|         wa-minnow-ridge | 01) crowning disabled | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2184.0 |  2121 (97.1%) |            0.22 | 0.11 | 0.34 | 0.68 | 0.79 |   4652 (213.0%) |     63 (2.9%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|         wa-minnow-ridge |          00) original | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2184.0 |  2149 (98.4%) |            0.12 | 0.03 | 0.08 | 0.62 | 0.72 |  16505 (755.7%) |     35 (1.6%) |  620 (28.9%) |  9368 (56.8%) |OLD
  ;;|         wa-minnow-ridge | 01) crowning disabled | Thu Sep 22 20:29:00 UTC 2022 | Sat Sep 24 09:15:00 UTC 2022 (+37hr) |                2184.0 |  2043 (93.5%) |            0.22 | 0.09 | 0.23 | 0.74 | 0.83 |   3132 (143.4%) |    141 (6.5%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|         wa-minnow-ridge |          00) original | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:17:00 UTC 2022 (+11hr) |                2157.0 |  1926 (89.3%) |            0.33 | 0.24 | 0.51 | 0.79 | 0.91 |    1433 (66.4%) |   231 (10.7%) |  203 (10.5%) |   584 (40.8%) |NEW
  ;;|         wa-minnow-ridge | 01) crowning disabled | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:17:00 UTC 2022 (+11hr) |                2157.0 |  1870 (86.7%) |            0.37 | 0.31 | 0.59 | 0.84 | 0.93 |     771 (35.7%) |   287 (13.3%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|         wa-minnow-ridge |          00) original | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:17:00 UTC 2022 (+11hr) |                2157.0 |  1790 (83.0%) |            0.63 | 0.87 | 0.91 | 0.96 | 0.98 |    1291 (59.9%) |   367 (17.0%) |  515 (28.8%) |  1136 (88.0%) |OLD
  ;;|         wa-minnow-ridge | 01) crowning disabled | Tue Sep 20 09:41:00 UTC 2022 | Tue Sep 20 20:17:00 UTC 2022 (+11hr) |                2157.0 |  1435 (66.5%) |            0.34 | 0.06 | 0.91 | 0.97 | 0.98 |       69 (3.2%) |   722 (33.5%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|         wa-minnow-ridge |          00) original | Tue Sep 27 09:07:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+24hr) |                1622.0 |  1327 (81.8%) |            0.31 | 0.28 | 0.44 | 0.72 | 0.82 |     933 (57.5%) |   295 (18.2%) |  288 (21.7%) |   108 (11.6%) |NEW
  ;;|         wa-minnow-ridge | 01) crowning disabled | Tue Sep 27 09:07:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+24hr) |                1622.0 |  1156 (71.3%) |            0.36 | 0.30 | 0.58 | 0.81 | 0.89 |     618 (38.1%) |   466 (28.7%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|         wa-minnow-ridge |          00) original | Tue Sep 27 09:07:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+24hr) |                1622.0 |  1363 (84.0%) |            0.45 | 0.46 | 0.52 | 0.68 | 0.80 |   4018 (247.7%) |   259 (16.0%) |  627 (46.0%) |  2532 (63.0%) |OLD
  ;;|         wa-minnow-ridge | 01) crowning disabled | Tue Sep 27 09:07:00 UTC 2022 | Wed Sep 28 08:50:00 UTC 2022 (+24hr) |                1622.0 |   950 (58.6%) |            0.51 | 0.57 | 0.73 | 0.87 | 0.93 |     226 (13.9%) |   672 (41.4%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|         wa-minnow-ridge |          00) original | Wed Sep 21 09:22:00 UTC 2022 | Thu Sep 22 10:43:00 UTC 2022 (+25hr) |                3105.0 |  3041 (97.9%) |            0.20 | 0.15 | 0.26 | 0.49 | 0.66 |   6224 (200.5%) |     64 (2.1%) |  537 (17.7%) |  1677 (26.9%) |NEW
  ;;|         wa-minnow-ridge | 01) crowning disabled | Wed Sep 21 09:22:00 UTC 2022 | Thu Sep 22 10:43:00 UTC 2022 (+25hr) |                3105.0 |  3031 (97.6%) |            0.32 | 0.27 | 0.50 | 0.73 | 0.84 |    2742 (88.3%) |     74 (2.4%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|         wa-minnow-ridge |          00) original | Wed Sep 21 09:22:00 UTC 2022 | Thu Sep 22 10:43:00 UTC 2022 (+25hr) |                3105.0 |  3008 (96.9%) |            0.38 | 0.41 | 0.47 | 0.54 | 0.65 |  13024 (419.5%) |     97 (3.1%) |  993 (33.0%) |  8462 (65.0%) |OLD
  ;;|         wa-minnow-ridge | 01) crowning disabled | Wed Sep 21 09:22:00 UTC 2022 | Thu Sep 22 10:43:00 UTC 2022 (+25hr) |                3105.0 |  2749 (88.5%) |            0.47 | 0.51 | 0.69 | 0.80 | 0.84 |    1043 (33.6%) |   356 (11.5%) |     0 (0.0%) |      0 (0.0%) |OLD


  ;;|             wa-siouoxon |          00) original | Sun Sep 25 11:28:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+24hr) |                 430.0 |   377 (87.7%) |            0.42 | 0.40 | 0.68 | 0.85 | 0.93 |    440 (102.3%) |    53 (12.3%) |     5 (1.3%) |      7 (1.6%) |NEW
  ;;|             wa-siouoxon | 01) crowning disabled | Sun Sep 25 11:28:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+24hr) |                 430.0 |   375 (87.2%) |            0.42 | 0.40 | 0.69 | 0.88 | 0.93 |     417 (97.0%) |    55 (12.8%) |     0 (0.0%) |      0 (0.0%) |NEW
  ;;|             wa-siouoxon |          00) original | Sun Sep 25 11:28:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+24hr) |                 430.0 |   340 (79.1%) |            0.43 | 0.45 | 0.56 | 0.73 | 0.80 |     271 (63.0%) |    90 (20.9%) |    29 (8.5%) |     13 (4.8%) |OLD
  ;;|             wa-siouoxon | 01) crowning disabled | Sun Sep 25 11:28:00 UTC 2022 | Mon Sep 26 11:08:00 UTC 2022 (+24hr) |                 430.0 |   291 (67.7%) |            0.46 | 0.49 | 0.66 | 0.79 | 0.84 |     184 (42.8%) |   139 (32.3%) |     0 (0.0%) |      0 (0.0%) |OLD

  ;; Main observations:
  ;; (1) Under "00) original", the new version causes much less overprediction than the old version.
  ;; (2) Under "01) crowning disabled", the new version causes much MORE overprediction than the old version.
  ;; So in all likelihood, something has changed to make the surface fire spread more aggressive, at least in these simulations.

  ;; Here is a curated list of the new branches which might have had an impact:

  ;; $ git br --merged HEAD --no-merge 85f1467
  ;  conflicts-96
  ;  draft-vwaeselynck-smoothed-supergrid-perturbation
  ;  fix-suppression-fc-max0
  ;  hotfix-fuel-moisture-get-band
  ;  kcheung-fix-suppression-override-config
  ;  kcheung-sdi
  ;  kcheung-sdi-spec-docs
  ;  kcheung-use-fuel-varying-multipliers
  ;  vwaeselynck-29-no-boxing-in-get-value-fn
  ;* vwaeselynck-330-gridfire-historical-fires
  ;  vwaeselynck-352-disable-crowning
  ;  vwaeselynck-353-directional-crowning-initiation
  ;  vwaeselynck-395-new-fuel-models
  ;  vwaeselynck-398-burn-period-from-sun-per-ignition
  ;  vwaeselynck-405-e2g-ignition-csv-override-layers
  ;  vwaeselynck-410--simplify-spotting-sampling
  ;  vwaeselynck-434-compiler-options-for-production-jar
  ;  vwaeselynck-436-gridfire-optimizations
  ;  vwaeselynck-fix-NaN-clock
  ;  vwaeselynck-fix-create-new-burn-vectors_test
  ;  vwaeselynck-fix-elmfire-merge-override-config
  ;  vwaeselynck-fix-missing-test-suites-metadata
  ;  vwaeselynck-fix-process-output-layers
  ;  vwaeselynck-fix-test-suites
  ;  vwaeselynck-fix-valid-fuel-range-spec-WUI-max=303
  ;  vwaeselynck-hash-determined-pixel-perturbation
  ;  vwaeselynck-refactor-ignition-sites-sampling
  ;  vwaeselynck-update-magellan-deps-2022-12
  ;  vwaeselynck-upgrade-magellan-2022.10.21
  ;  vwaeselynk-336-burn-period-from-sunrise-sunset

  *e)
