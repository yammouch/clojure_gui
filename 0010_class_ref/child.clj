(ns child
  (:gen-class
   :init init
   :state state))

(require 'grand-child)

(defn -init []
  [[] (atom [])])

(defn do-body []
  (println "child is invoked.")
  (grand-child/do-body))
