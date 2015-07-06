(ns roulette)

(defn sumup [l]
  "e.g. ((2 :a) (3 :b) (4 :c)) -> (9 ((2 :a) (5 :b) (9 :c)))"
  (loop [l l, sum 0, acc []] 
    (if (empty? l)
      [sum acc]
      (let [new-sum (+ sum (ffirst l))]
        (recur (next l) new-sum
               (conj acc [new-sum (nfirst l)])
               )))))

(def rand-obj (java.util.Random.))

(defn make-roulette-wheel-selector [l]
  "Return a function which generates random number with given probability
distribution. e.g. if argument if ((2 :a) (3 :b) (5 :c)), :a, :b and :c
occurs with the probability of 20%, 30% and 50% respectively by calling
returned function."
  (let [[sum sum-list] (sumup l)]
    (fn [] (fnext (find-if #(> (first %) (.nextInt sum))
                  sum-list)))
