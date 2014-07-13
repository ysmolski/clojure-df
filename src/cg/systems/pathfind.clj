(ns cg.systems.pathfind
  [:use cg.ecs]
  [:use cg.comps]
  [:require [cg.site :as s]]
  [:require [cg.astar :as astar]])

;;; PATH FIND SYSTEM

(defn get-cell-cost [cells xy] 10)

(defn filter-nbr [m xy]
  (s/passable? (s/place m xy)))


(defn path-find-add [e time map]
  (let [from-xy (round-coords (:position e))
        to-xy (round-coords (:move-to e))
        new-path (astar/path from-xy to-xy 11 map get-cell-cost filter-nbr)]
    (prn :path-found from-xy to-xy new-path)
    (if (empty? (:xys new-path))
      (-> e
          (rem-c :move-to)
          (set-c (failed-job)))
      (-> e
          (rem-c :move-to)
          (set-c (path (:xys new-path)))))))

(defn system-path-find [w time]
  (update-comps w (:path-find node) path-find-add time (:map w)))

