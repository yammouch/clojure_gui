; do following
; $ mkdir classes
; $ java -cp .:/path/to/clojure-1.6.0.jar clojure.main -i compile.clj
; (on DOS prompt...)
; > java -cp classes;\path\to\clojure-1.6.0.jar SchemRtl

(def force-toolkit-init (javafx.embed.swing.JFXPanel.))
(compile 'SchemRtl)
(System/exit 0)
