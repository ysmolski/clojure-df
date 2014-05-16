(ns cg.astar)

(def world [[1 1 1 1 1]
            [9 9 9 9 1]
            [1 1 1 1 1]
            [1 9 9 9 9]
            [1 1 1 1 1]])

(def dirs [[-1 0]
           [-1 1]
           [0 1]
           [1 1]
           [1 0]
           [1 -1]
           [0 -1]
           [-1 -1]])

(defn neighbors
  ([size xy] (neighbors dirs size xy))
  ([deltas size xy]
     (filter (fn [new-xy]
               (every? #(< -1 % size) new-xy))
             (map #(map + xy %) deltas))))

(defn estimate-cost [step-cost-est [from-x from-y] [to-x to-y]]
  (* step-cost-est
     (+ (Math/abs (- to-x from-x))
        (Math/abs (- to-y from-y)))))

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
     (:cost cheapest-nbr 0)))

(defn total-cost [newcost step-cost-est from to]
  (+ newcost
     (estimate-cost step-cost-est from to)))

(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min this]
              (if (> (f min) (f this)) this min))
            coll)))

(defn path [step-est cell-costs from-xy to-xy]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (replicate size (vec (replicate size nil))))
           work-todo (sorted-set [0 from-xy])]
      (if (empty? work-todo)
        [(peek (peek routes)) :steps steps]
        (let [[_ xy :as work-item] (first work-todo)
              rest-work-todo (disj work-todo work-item)
              nbr-xys (neighbors size xy)
              cheapest-nbr (min-by :cost
                                   (keep #(get-in routes %) nbr-xys))
              newcost (path-cost (get-in cell-costs xy)
                                  cheapest-nbr)
              oldcost (:cost (get-in routes xy))]
          (prn work-item)
          (prn rest-work-todo)
          (prn nbr-xys)
          (prn cheapest-nbr)
          (prn newcost)
          (prn oldcost)
          (if (and oldcost (>= newcost oldcost))
            (recur (inc steps)
                   routes
                   rest-work-todo)
            (recur (inc steps)
                   (assoc-in routes xy
                             {:cost newcost
                              :xys (conj (:xys cheapest-nbr []) xy)})
                   (into rest-work-todo
                         (map (fn [w]
                                (let [[x y] w]
                                  [(total-cost newcost step-est [x y] to-xy) w]))
                              nbr-xys)))))))))

(prn "total" (total-cost 10 5 [0 0] [4 4]))

(prn "path" (path 4 world [0 0] [2 2]))



