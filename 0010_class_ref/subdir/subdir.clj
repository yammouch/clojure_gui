(ns subdir.subdir
  (:gen-class
   :init init
   :state state))

(defn -init []
  [[] (atom [])])

(defn do-body []
  (println "subdir is invoked."))
