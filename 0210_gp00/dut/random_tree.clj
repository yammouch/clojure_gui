(ns random-tree)

(def non-terminals
  '((:if 3)
    (:< 2)
    (:> 2)
    (:= 2)
    (:+ 2)
    (:- 2)
    (:* 2)
    (:/ 2)
    (:pos 1)
    (:nth 2)
    (:setx 2)
    (:sety 2)
    (:adjacents 1) 
    (:prog2 2)
    (:prog3 3)
    (boundary 0)
    (finish 0)))

(def rand-obj (java.util.Random.))

(defn distribute [i n]
  "Distributes i into n-element array. i must be > 0, n must be > 0."
  (let [rands (sort (concat [0 i]
                            (map (fn [_] (.nextInt rand-obj i))
                                 (range (dec n))
                                 )))]
    (map (fn [[x y]] (- y x)) (partition 2 1 rands))))

