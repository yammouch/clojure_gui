(ns utils)

(defn lcg [seed]
  (let [val (mod (+ (* seed 1103515245) 12345)
                 (bit-shift-left 1 32))]
    (cons val (lazy-seq (lcg val)))
    ))
