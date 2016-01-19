(ns disentangler)

(require 'mlp)
(require 'schemanip)
(alias 'smp 'schemanip)

(def schem
  {:field (read-string (slurp "td000.txt"))
   :cmd   {:cmd :move-y :org [9 6] :dst 9}})

(prn (count (take 100 (take-while identity (iterate smp/slide-upper schem)))))
(prn (count (take 100 (take-while identity (iterate smp/slide-lower schem)))))
(prn (count (take 100 (take-while identity (iterate smp/slide-left schem)))))
(prn (count (take 100 (take-while identity (iterate smp/slide-right schem)))))
