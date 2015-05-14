; do following
; $ java -cp .:/path/to/clojure-1.6.0.jar clojure.main -i compile.clj
; $ java -cp ./classes:/path/to/clojure-1.6.0.jar SchemRtl
; (for DOS ...)
; > java -cp .;\path\to\clojure-1.6.0.jar clojure.main -i compile.clj
; > java -cp classes;\path\to\clojure-1.6.0.jar SchemRtl

(use '[clojure.java.shell :only (sh)])

(def force-toolkit-init (javafx.embed.swing.JFXPanel.))
(compile 'LogicElements)
(compile 'LogicElementsDraw)
(compile 'SchemDialog)
(compile 'SchemRtl)

(sh "java" "-cp" "./classes;../../../clojure-1.6.0.jar" "SchemRtl")

(System/exit 0)
