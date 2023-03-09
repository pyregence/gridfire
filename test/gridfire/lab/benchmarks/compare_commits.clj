;; [[file:../../../../org/GridFire.org::gridfire.lab.benchmarks.compare-commits][gridfire.lab.benchmarks.compare-commits]]
;; FIXME LP coverage
(ns gridfire.lab.benchmarks.compare-commits
  "A babashka script for benchmarking several commits."
  (:require [babashka.process]
            [clojure.java.shell :as sh]
            [clojure.string     :as str]))

(defn- shell-safe
  [& args]
  (let [shell-ret (apply sh/sh args)]
    (if (= 0 (:exit shell-ret))
      (:out shell-ret)
      (throw (ex-info (format "Shell command %s returned error: %s"
                              (pr-str (first args))
                              (:err shell-ret))
                      {::shell-args args
                       ::shell-ret shell-ret})))))

(defn git-checkout!
  [commit-sha]
  (shell-safe "git" "checkout" commit-sha))

(defn benchmark-commits!
  [config-path commit-shas]
  (let [current-commit (-> (shell-safe "git" "rev-parse" "--short" "HEAD") (subs 0 7))]
    (try
      (->> commit-shas
           (run! (fn [commit-sha]
                   (println "--------------------------------------------------")
                   (println "Benchmarking at code version:" (shell-safe "git" "show" "--no-patch" "--oneline" commit-sha))
                   (git-checkout! commit-sha)
                   (let [shell-cmd ["clojure" "-J-Dclojure.compiler.direct-linking=true" "-M:perf-testing" "-m" "gridfire.lab.benchmarks" "benchmark" config-path]]
                     ;; Applying Babashka recipe to print the child process' output: https://book.babashka.org/#_babashka_process
                     @(babashka.process/process (-> shell-cmd (doto prn))
                                                {:inherit true :shutdown babashka.process/destroy-tree}))
                   (println "--------------------------------------------------"))))
      (finally
        (git-checkout! current-commit)))))

(defn commits-range
  "Commits after shart-sha (exclusive) until end-sha (inclusive)."
  [start-sha end-sha]
  (->> (shell-safe "git" "log" "--pretty=format:%h" (str start-sha "..." end-sha))
       (str/split-lines)
       (reverse)
       (into [start-sha])))

(comment
  (commits-range "dafdcaa" "5e1eded")
  ["dafdcaa" "c88e8c9" "26c6676" "f2eae21" "d5565da" "34e9720" "d79e18d" "91722c4" "5e1eded"]

  (benchmark-commits! "test/gridfire/lab/benchmarks/rhino-input-deck/gridfire.edn"
                      ["dafdcaa" "c88e8c9" "26c6676" "f2eae21" "d5565da" "34e9720" "d79e18d" "91722c4" "5e1eded"])

  ;; Example:
  ;; bb test/gridfire/lab/benchmarks/compare_commits.clj "test/gridfire/lab/benchmarks/rhino-input-deck/gridfire.edn" "dafdcaa" "c88e8c9" "26c6676" "f2eae21" "d5565da" "34e9720" "d79e18d" "91722c4" "5e1eded" >> test/gridfire/lab/benchmarks/rhino_dafdcaa-5e1eded.txt

  *e)

(defn -main [config-path & commit-shas]
  (benchmark-commits! config-path commit-shas))

;; Applying Babashka recipe: https://book.babashka.org/#main_file
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
;; gridfire.lab.benchmarks.compare-commits ends here
