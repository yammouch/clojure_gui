(defn l2i [l]
  (format "%02X" (reduce #(+ (* 2 %1) %2) 0 l)))

(let [x (read-string (slurp "td000.txt"))]
  (print (apply str (interpose "\n"
                              (map (fn [row]
			             (apply str (interpose "," (map l2i row))))
                                   (:body x)
			           )))))
