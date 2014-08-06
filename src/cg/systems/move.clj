(ns cg.systems.move
  (:require [cg.comps :refer :all]
            [cg.ecs :refer :all]))

(defn move [e time]
  (let [v (:velocity e)
        t-norm (/ time 1000)
        dx (* (:x v) t-norm)
        dy (* (:y v) t-norm)]
    ;; (prn :move v t-norm [dx dy])
    (-> e
        (update-in [:position :x] + dx)
        (update-in [:position :y] + dy))))

(defn system-move [w time]
  (update-entities-by-cnames w (:move node) move time))
