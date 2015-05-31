; do following
; $ java -cp .:/path/to/clojure-1.6.0.jar clojure.main -i compile.clj
; $ java -cp ./classes:/path/to/clojure-1.6.0.jar canvas_size_show
; (for DOS ...)
; > java -cp .;\path\to\clojure-1.6.0.jar clojure.main -i compile.clj
; > java -cp classes;\path\to\clojure-1.6.0.jar canvas_size_show

(def force-toolkit-init (javafx.embed.swing.JFXPanel.))
(compile 'canvas_size_show)
(System/exit 0)
