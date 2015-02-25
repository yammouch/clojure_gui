; do following
; $ java -cp .:/path/to/clojure-1.6.0.jar clojure.main -i compile.clj
; $ java -cp ./classes:/path/to/clojure-1.6.0.jar no_canvas_change
; (for DOS ...)
; > java -cp .;\path\to\clojure-1.6.0.jar clojure.main -i compile.clj
; > java -cp classes;\path\to\clojure-1.6.0.jar no_canvas_change

(def force-toolkit-init (javafx.embed.swing.JFXPanel.))
(compile 'no_canvas_change)
(System/exit 0)
