To generate class file, follow the instruction below:
$ mkdir classes
$ java -cp /path/to/clojure-1.6.0.jar clojure.main
> (compile 'gui)
> C-c
$ java -cp classes:/path/to/clojure-1.6.0.jar gui
