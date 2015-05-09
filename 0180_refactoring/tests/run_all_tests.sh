CLOJURE=../../../clojure-1.6.0.jar
java -cp .:..:${CLOJURE} clojure.main -i TestLogicElements.clj
java -cp .:..:${CLOJURE} clojure.main -i LogicElements/geometries.clj
