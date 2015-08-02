(ns test.utils)

(require 'utils)
(alias 'ut 'utils)

(println "test lcg")

(println (take 5 (ut/lcg 0)))
