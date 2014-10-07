(ns parent
  (:gen-class
   :init init
   :state state))

(require 'child)
(require 'subdir.subdir)

(defn -init []
  [[] (atom [])])

(defn -main []
  (child/do-body)
  (subdir.subdir/do-body))
