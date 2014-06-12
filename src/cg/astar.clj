(ns cg.astar)

(def dirs [[-1 0]
           [-1 1]
           [0 1]
           [1 1]
           [1 0]
           [1 -1]
           [0 -1]
           [-1 -1]])

(defn neighbors
  "return vec of coords of neighbors of xy"
  ([size xy] (neighbors dirs size xy))
  ([deltas size xy]
     (filter (fn [new-xy]
               (every? #(< -1 % size) new-xy))
             (map #(map + xy %) deltas))))

(defn estimate-cost
  "H function"
  [step-cost-est [from-x from-y] [to-x to-y]]
  (* step-cost-est
     (+ (Math/abs (- to-x from-x))
        (Math/abs (- to-y from-y)))))

(defn path-cost
  "G function"
  [node-cost [x y] cheapest-nbr]
  (let [coef (if-let [[from-x from-y] (peek (:xys cheapest-nbr []))]
               (if (and (not= x from-x)
                        (not= y from-y))
                 1.4
                 1)
               1)]
    ;; (prn coef x y cheapest-nbr)
    (+ (int (* coef node-cost))
       (:cost cheapest-nbr 0))))

(defn total-cost
  "G + H function"
  [newcost step-cost-est from to]
  (+ newcost
     (estimate-cost step-cost-est from to)))

(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min this]
              (if (> (f min) (f this)) this min))
            coll)))

;;; this algorithm can be improved:
;;; * introducing timeout for an execution of the algo
;;; * returning the best estimation for getting to not possible destination

(defn path
  "a* path finding"
  [from-xy to-xy step-est cell-costs get-cell-cost filter-neighbor]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (replicate size (vec (replicate size nil))))
           work-todo (sorted-set [0 from-xy])]
      (if (empty? work-todo)
        {:xys []
         ;; :routes (min-by :cost (for [rx (range size)
                                 ;;     ry (range size)]
                                 ;; (get-in routes [rx ry])))
         :steps steps}
        (let [[_ xy :as work-item] (first work-todo)
              rest-work-todo (disj work-todo work-item)
              nbr-xys (filter (partial filter-neighbor cell-costs) (neighbors size xy))
              cheapest-nbr (min-by :cost
                                   (keep #(get-in routes %) nbr-xys))
              newcost (path-cost (get-cell-cost cell-costs xy)
                                 xy
                                 cheapest-nbr)
              oldcost (:cost (get-in routes xy))]
          (if (= xy to-xy)
            {:cost newcost
             :xys (conj (:xys cheapest-nbr []) xy)
             :steps steps} 
            (if (and oldcost (>= newcost oldcost))
              (recur (inc steps)
                     routes
                     rest-work-todo)
              (recur (inc steps)
                     (assoc-in routes xy
                               {:cost newcost
                                :xys (conj (:xys cheapest-nbr []) xy)})
                     (into rest-work-todo
                           (map (fn [[x y]]
                                  [(total-cost newcost step-est [x y] to-xy) [x y]])
                                nbr-xys))))))))))

(defn get-cell-cost [cells xy]
  (get-in cells xy))

(defn filter-neighbor [cells xy]
  true)

;; (prn "total" (total-cost 10 5 [0 0] [4 4]))
;; (time (prn "path" (path [0 0] [4 4] 5 world get-cell-cost filter-neighbor))) 
