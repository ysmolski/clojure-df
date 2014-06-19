(ns cg.systems.move
  [:use cg.ecs]
  [:use cg.comps])

(defn move [e time]
  (let [v (:velocity e)
        t-norm (/ time 1000)
        dx (* (:x v) t-norm)
        dy (* (:y v) t-norm)]
    (-> e
        (update-in [:position :x] + dx)
        (update-in [:position :y] + dy))))

(defn system-move [w time]
  (update-comps w (:move node) move time))
