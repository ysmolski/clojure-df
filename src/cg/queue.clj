(ns cg.queue)

;;; Queues

(defn queue [& args]
  (-> (clojure.lang.PersistentQueue/EMPTY)
      (into args)))

(defmethod print-method clojure.lang.PersistentQueue
  [o, w]
  (print-method '<- w)
  (print-method (seq o) w)
  (print-method '-< w))
