(let [x (read-string (slurp "td000.txt"))]
  (print (apply str (interpose "\n"
                              (map (fn [p] (apply str (interpose "," p)))
                                   (apply concat (:body x))
			           )))))
