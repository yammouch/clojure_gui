(ns grand-child
  (:gen-class
   :init init
   :state state))

(defn -init []
  [[] (atom [])])

(defn do-body []
  (println "grand-child is invoked."))
