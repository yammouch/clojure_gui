; $ java -cp /path/to/JOCL-x.x.x.jar:/path/to/clojure-x.x.x.jar -Djava.library.path="/path/to/JOCL-x.x.x-bin" clojure.main -i clInfo.clj

(require 'cl)

(def platforms (map cl/get-platform (cl/clGetPlatformIDs)))
(clojure.pprint/pprint platforms)
