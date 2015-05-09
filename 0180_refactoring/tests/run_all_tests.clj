(use '[clojure.java.shell :only (sh)])

(def result (sh "java" "-cp" ".;..;../../../clojure-1.6.0.jar"
                "clojure.main" "-i" "TestLogicElements.clj"))
(println (:out result))
(println (:err result))

(def result (sh "java" "-cp" ".;..;../../../clojure-1.6.0.jar"
                "clojure.main" "-i" "LogicElements/geometries.clj"))
(println (:out result))
(println (:err result))

(System/exit 0)
