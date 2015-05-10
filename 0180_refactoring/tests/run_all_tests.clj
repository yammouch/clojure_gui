(use '[clojure.java.shell :only (sh)])

(let [result (sh "java" "-cp" ".;../dut;../../../clojure-1.6.0.jar"
                 "clojure.main" "-i" "TestLogicElements.clj")]
  (println (:out result))
  (println (:err result)))

(let [result (sh "java" "-cp" ".;../dut;../../../clojure-1.6.0.jar"
                 "clojure.main" "-i" "LogicElements/geometries.clj")]
  (println (:out result))
  (println (:err result)))

(let [result (sh "java" "-cp" "..;../dut;../../../clojure-1.6.0.jar"
                 "clojure.main" "-i" "compile_tests.clj")]
  (println (:out result))
  (println (:err result)))

(let [result (sh "java" "-cp" "./classes;../../../clojure-1.6.0.jar"
                 "tests.TestSchemDialog")]
  (println (:out result))
  (println (:err result)))

(let [result (sh "java" "-cp" "./classes;../../../clojure-1.6.0.jar"
                 "tests.TestLogicElementsDraw")]
  (println (:out result))
  (println (:err result)))

(System/exit 0)
