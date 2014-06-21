(ns cg.systems.pathfind
  [:use cg.ecs]
  [:use cg.comps]
  [:require [cg.site :as s]]
  [:require [cg.astar :as astar]])

;;; PATH FIND SYSTEM

(defn get-cell-cost [cells xy] 10)

(defn filter-nbr [m xy]
  (s/passable? (s/place m xy)))


(defn path-find-add [e time mp]
  (let [[ex ey] (round-coords (:position e))
        [x y] (round-coords (:destination e))
        new-path (astar/path [ex ey] [x y] 11 mp get-cell-cost filter-nbr)
        ;new-path {:xys [[x y]]}
        ]
    (prn :path-found x y ex ey new-path)
    (if (empty? (:xys new-path))
      (rem-c e :destination)
      (-> e
          (set-c (path (:xys new-path)))
          (rem-c :destination)))))

(defn system-path-find [w time]
  (update-comps w (:path-find node) path-find-add time (:map w)))

