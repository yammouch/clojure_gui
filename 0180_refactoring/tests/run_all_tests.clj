(use '[clojure.java.shell :only (sh)])

(let [result (sh "java" "-cp" ".;../dut;../../../clojure-1.6.0.jar"
                 "clojure.main" "-i" "TestLogicElements.clj")]
  (println (:out result))
  (println (:err result)))

(let [result (sh "java" "-cp" ".;../dut;../../../clojure-1.6.0.jar"
                 "clojure.main" "-i" "LogicElements/geometries.clj")]
  (println (:out result))
  (println (:err result)))

(System/exit 0)
