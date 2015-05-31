; do following
; $ java -cp .:/path/to/clojure-1.6.0.jar clojure.main -i compile.clj
; $ java -cp ./classes:/path/to/clojure-1.6.0.jar menu_shortcut
; (for DOS ...)
; > java -cp .;\path\to\clojure-1.6.0.jar clojure.main -i compile.clj
; > java -cp classes;\path\to\clojure-1.6.0.jar menu_shortcut

(def force-toolkit-init (javafx.embed.swing.JFXPanel.))
(compile 'menu_shortcut)
(System/exit 0)
