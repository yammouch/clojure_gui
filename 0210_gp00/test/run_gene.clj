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

(println "test gene-pos")
(def env-1
  {:nodes [[-5 -4]
           [ 3  2]
           [ 0  0]]
   :edges [[0 1]
           [1 2]]})

(def test-cases [[0 0 -5]
                 [0 1 -4]
                 [1 0  3]
                 [1 1  2]
                 [2 0  0]
                 [2 1  0]])

(doseq [[inst-id axes exp] test-cases]
  (let [[result _] (rg/gene-pos [inst-id axes] env-1)]
    (if (= exp result)
      (print "[OK]")
      (print "[ER]" result))
    (print " test case ")
    (println inst-id axes)))

(println "test gene-mov")

(def test-cases [[0  0 100 [[  95   -4] [  3 2] [  0 0]]]
                 [0  1 100 [[  -5   96] [  3 2] [  0 0]]]
                 [1  0 200 [[  -5   -4] [203 2] [  0 0]]]
                 [2  0 300 [[  -5   -4] [  3 2] [300 0]]]
                 [3  0 400 [[ 395   -4] [  3 2] [  0 0]]]
                 [-1 0 600 [[  -5   -4] [  3 2] [600 0]]]
                 ])

(doseq [[inst-id axes amount exp] test-cases]
  (let [[val {nodes :nodes}] (rg/gene-mov [inst-id axes amount] env-1)]
    (if (and (= val 0) (= exp nodes))
      (print "[OK]")
      (print "[ER]" val nodes))
    (print " test case ")
    (println inst-id axes amount)))

(println "test gene-adjacent")

(def test-cases [[0 0 1]
                 [0 1 1]
                 [1 0 0]
                 [1 1 2]
                 [1 2 0]
                 [2 0 1]
                 [4 0 0]
                 [4 1 2]])

(doseq [[inst-id i exp] test-cases]
  (let [[result _] (rg/gene-adjacent [inst-id i] env-1)]
    (if (= exp result)
      (print "[OK]")
      (print "[ER]" result))
    (print " test case ")
    (println inst-id i)))

(println "test gene-boundary")

(def test-cases [[0 0 -5]
                 [1 2 -4]
                 [2 1  3]])

(doseq [[axes dir exp] test-cases]
  (let [[result _] (rg/gene-boundary [axes dir] env-1)]
    (if (= exp result)
      (print "[OK]")
      (print "[ER]" result))
    (print " test case ")
    (println axes dir)))

(println "test eval-gene")
(def env-1
  {:nodes [[-5 -4]
           [ 3  2]
           [ 0  0]]
   :edges [[0 1]
           [1 2]]})

(def test-cases
 [[5 [5 env-1]]
  ['(:prog2 5 4) [4 env-1]]
  ['(:prog3 5 4 3) [3 env-1]]
  ['(:adjacent 1 1) [2 env-1]]
  ['(:boundary 1 1) [2 env-1]]
  ['(:op 5 2 4) [8 env-1]] ; op 5 -> *
  ['(:op 6 11 3) [3 env-1]] ; op 6 -> /
  ['(:op 0 (:pos 0 1) -6) [0 env-1]] ; op 0 -> <
  ['(:op 0 (:pos 0 1) -2) [1 env-1]] ; op 0 -> <
  ['(:if (:op 0 (:pos 0 1) -6) ; op 0 -> <
      (:mov 0 1  1)
      (:mov 0 1 -1))
   [0 {:nodes [[-5 -5] [3 2] [0 0]]
       :edges [[0 1] [1 2]]}]]
  ['(:if (:op 0 (:pos 0 1) -2)
      (:mov 0 1  1)
      (:mov 0 1 -1))
   [0 {:nodes [[-5 -3] [3 2] [0 0]]
       :edges [[0 1] [1 2]]}]]
  ['(:mov (:adjacent 1 1) 0 1)
   [0 {:nodes [[-5 -4] [3 2] [1 0]]
       :edges [[0 1] [1 2]]}]]
  ['(:prog2 (:mov 0 0 1) (:mov 1 1 -1))
   [0 {:nodes [[-4 -4] [3 1] [0 0]]
       :edges [[0 1] [1 2]]}]]
  ['(:prog3 (:mov 0 0 1) (:mov 1 1 -1) (:mov 1 1 -1))
   [0 {:nodes [[-4 -4] [3 0] [0 0]]
       :edges [[0 1] [1 2]]
       }]]])

(doseq [[gene exp] test-cases]
  (let [result (rg/eval-gene gene env-1)]
    (if (= exp result)
      (print "[OK]")
      (print "[ER]" result))
    (print " test case ")
    (println gene)))

