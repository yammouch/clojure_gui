; do following
; $ java -cp .:/path/to/clojure-1.6.0.jar clojure.main -i compile_tests.clj
; $ java -cp ./classes_tests:/path/to/clojure-1.6.0.jar {testcase}
; (for DOS ...)
; > java -cp .;\path\to\clojure-1.6.0.jar clojure.main -i compile_tests.clj
; > java -cp classes_tests;\path\to\clojure-1.6.0.jar {testcase}

(def force-toolkit-init (javafx.embed.swing.JFXPanel.))
(binding [*compile-path* "classes_tests"]
  (compile 'SchemDialog)
  (compile 'tests.TestSchemDialog))
(System/exit 0)
