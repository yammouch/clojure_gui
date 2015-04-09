; do following
; $ java -cp .:/path/to/clojure-1.6.0.jar clojure.main -i compile.clj
; $ java -cp ./classes:/path/to/clojure-1.6.0.jar SchemRtl
; (for DOS ...)
; > java -cp .;\path\to\clojure-1.6.0.jar clojure.main -i compile.clj
; > java -cp classes;\path\to\clojure-1.6.0.jar SchemRtl

(def force-toolkit-init (javafx.embed.swing.JFXPanel.))
(compile 'LogicElements)
(compile 'LogicElementsDraw)
(compile 'SchemDialog)
(compile 'SchemRtl)
(System/exit 0)
