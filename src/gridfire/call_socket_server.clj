(ns gridfire.call-socket-server
  (:gen-class))

(defn -main [& args]
  (println "If everybody had an ocean\nAcross the U.S.A.\nThen everybody'd be surfin'\nLike Californi-a")
  (run! println args)
  (System/exit 0))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
