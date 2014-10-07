; do following
; $ mkdir classes
; $ java -cp /path/to/clojure-1.6.0.jar clojure.main -i compile.clj
; $ java -cp classes:/path/to/clojure-1.6.0.jar parent

(compile 'parent)
(compile 'child)
(compile 'grand-child)
(compile 'subdir.subdir)
