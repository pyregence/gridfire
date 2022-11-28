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
                   (let [shell-cmd ["clojure" "-J-Dclojure.compiler.direct-linking=true" "-M:mydev:gridfire-my-dev" "-m" "gridfire.lab.benchmarks" "benchmark" config-path]]
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
  (commits-range "161f1fc" "4b57a8b")
  ["161f1fc" "3b2f9bd" "eb3c6f3" "d5bb44e" "2d1ad41" "0104564" "766c57b" "3f8e13f" "4b57a8b"]

  (benchmark-commits! "test/gridfire/lab/benchmarks/rhino-input-deck/gridfire.edn"
                      ["161f1fc" "3b2f9bd" "eb3c6f3" "d5bb44e" "2d1ad41" "0104564" "766c57b" "3f8e13f" "4b57a8b"])

  ;; Example:
  ;; bb unvvv/gridfire/lab/benchmarks/compare_commits.clj "test/gridfire/lab/benchmarks/rhino-input-deck/gridfire.edn" "161f1fc"  "3b2f9bd" "eb3c6f3" "d5bb44e" "2d1ad41" "0104564" "766c57b" "3f8e13f" "4b57a8b"

  *e)

(defn -main [config-path & commit-shas]
  (benchmark-commits! config-path commit-shas))

;; Applying Babashka recipe: https://book.babashka.org/#main_file
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
