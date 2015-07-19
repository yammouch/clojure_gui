(ns test.run-gene)

(require 'run-gene)
(alias 'rg 'run-gene)

(println "test gene-op")
(def test-cases [[[0  32767 32768]      0]
                 [[0  32768 32767]      1]
                 [[1  32767 32768]      1]
                 [[1  32768 32767]      0]
                 [[2  32767 32768]      0]
                 [[2  65537     1]      1]
                 [[3  32000   767]  32767]
                 [[3  32000   768] -32768]
                 [[3  32000   769] -32767]
                 [[4     -1 32766] -32767]
                 [[4     -1 32767] -32768]
                 [[4     -1 32768]  32767]
                 [[5    256   256]      0]
                 [[5    255   256]   -256]
                 [[6    256   256]      1]
                 [[6 131072     2]      0]])
  
(doseq [[[op x y] exp] test-cases]
  (let [[result _] (rg/gene-op [op x y] nil)]
    (if (= exp result)
      (print "[OK]")
      (print "[ER]" result))
    (print " test case ")
    (println [op x y])))

(def env-1
  {:nodes [[-5 -4]
           [ 3  2]
           [ 0  0]]
   :edges [[0 1]
           [1 2]]})

(def test-cases [[0 [-5 -4]]
                 [1 [ 3  2]]
                 [2 [ 0  0]]])

(doseq [[inst-id exp] test-cases]
  (let [[result _] (rg/gene-pos [inst-id] env-1)]
    (if (= exp result)
      (print "[OK]")
      (print "[ER]" result))
    (print " test case ")
    (println inst-id)))

(println "test gene-setx")

(def test-cases [[0  100 [[100 -4] [  3 2] [  0 0]]]
                 [1  200 [[ -5 -4] [200 2] [  0 0]]]
                 [2  300 [[ -5 -4] [  3 2] [300 0]]]
                 [3  400 [[400 -4] [  3 2] [  0 0]]]
                 [[] 500 [[500 -4] [  3 2] [  0 0]]]
                 [-1 600 [[ -5 -4] [  3 2] [600 0]]]
                 ])

(doseq [[inst-id x exp] test-cases]
  (let [[val {nodes :nodes}] (rg/gene-setx [inst-id x] env-1)]
    (if (and (= val 0) (= exp nodes))
      (print "[OK]")
      (print "[ER]" val nodes))
    (print " test case ")
    (println inst-id x)))

(println "test gene-nth")

(def test-cases [[:foo       6  0]
                 [[:a :b :c] 4 :b]
                 [[]         3  0]])

(doseq [[l n exp] test-cases]
  (let [[result _] (rg/gene-nth [l n] nil)]
    (if (= exp result)
      (print "[OK]")
      (print "[ER]" result))
    (print " test case ")
    (println l n)))

(println "test gene-adjacents")

(def test-cases [[0 [1  ]]
                 [1 [0 2]]
                 [2 [1  ]]])

(doseq [[n exp] test-cases]
  (let [[result _] (rg/gene-adjacents [n] env-1)]
    (if (= exp result)
      (print "[OK]")
      (print "[ER]" result))
    (print " test case ")
    (println n)))

(println "test gene-boundary")

(def test-cases [[env-1 [-5 -4 3 2]]])

(doseq [[env exp] test-cases]
  (let [[result _] (rg/gene-boundary [] env-1)]
    (if (= exp result)
      (print "[OK]")
      (print "[ER]" result))
    (print " test case ")
    (println env)))

(println "test eval-gene")

(def test-cases
 [[5 [5 env-1]]
  ['(:prog2 5 4) [4 env-1]]
  ['(:prog3 5 4 3) [3 env-1]]
  ['(:adjacents 1)
   [[0 2] env-1]]
  ['(:boundary) [[-3 -2 5 4] env-1]]
  ['(:* 2 4) [8 env-1]]
  ['(:/ 11 3) [3 env-1]]
  ['(:if (:< (:pos 0 1) 6)
      (:sety 1 (:+ (:pos 0 1) 1))
      (:sety 1 (:- (:pos 0 1) 1)))
   [0 {:nodes [[6 4] [-3 -2] [0 0]]
       :edges [[0 1] [1 2]]}]]
  ['(:if (:< (:pos 0 1) 2)
      (:sety 1 (:+ (:pos 0 1) 1))
      (:sety 1 (:- (:pos 0 1) 1)))
   [0 {:nodes [[4 4] [-3 -2] [0 0]]
       :edges [[0 1] [1 2]]}]]
  ['(:setx (:nth (:adjacents 1) 1)
           (:+ (:pos (:nth (:adjacents 1) 1) 0)
               1))
   [0 {:nodes [[5 4] [-3 -2] [1 0]]
       :edges [[0 1] [1 2]]}]]
  ['(:prog2 (:setx 0 1) (:sety 1 -1))
   [0 {:nodes [[1 -1] [-3 -2] [1 0]]
       :edges [[0 1] [1 2]]}]]
  ['(:prog3 (:setx 0 1) (:sety 0 -1) (:setx 1 -4))
   [0 {:nodes [[1 -1] [-4 -2] [1 0]]
       :edges [[0 1] [1 2]]
       }]]])

;(doseq [[gene exp] test-cases]
;  (let [result (rg/eval-gene gene env-1)]
;    (if (= exp result)
;      (print "[OK]")
;      (print "[ER]" result))
;    (print " test case ")
;    (println gene)))

