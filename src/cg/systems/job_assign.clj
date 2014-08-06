(ns cg.systems.job-assign
  (:require [cg.comps :refer :all]
            [cg.ecs :refer :all]
            [cg.jobs :as j]
            [cg.map :as m]
            [cg.site :as s]))

(defn escape-walls
  "For any new found worker it tries to find matching job
  which is accessible and can have all needed materials"
  [e time w]
  (let [pos (round-coords (:position e))
        cell (m/place w pos)]
    (if (s/passable? cell)
      e
      (if-let [[x y] (m/passable-nbr w pos)]
        (do
          ;; (prn :escape-wall pos :to [x y])
          (j/enqueue e [(move-to x y)]))
        (do
          (prn :escape-stuck pos)
          e)))))

(defn system-escape-walls
  "take free workers and find next (closest?) jobs for them"
  [w time]
  (update-entities-by-cnames w [:want-job] escape-walls time w))




