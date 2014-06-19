(ns cg.systems.job-assign
  [:use cg.common]
  [:use cg.ecs]
  [:use cg.comps]
  [:require [cg.ecs :as e]]
  [:require [cg.site :as s]]
  [:require [cg.astar :as astar]])

(defn find-reachable-nbrs
  "finds all neighbour points of to-xy reachable from from-xy"
  [w [fx fy :as from-xy] to-xy]
  (->> to-xy
       (astar/neighbors (:map-size w))
       (filter (partial s/connected? (:map w) from-xy))
       (sort-by (fn [[tx ty]] (distance fx fy tx ty)))))

(defn find-reachable
  "Tries to find free cell next to entity specified by target-ids reachable from xy.
  Returns [id [x y] [tx ty]] where
  id - id of reachable target entity
  x y - coords of found free-cell.
  tx ty - coords of target entity
  Otherwise returns nil."
  [w xy target-ids]
  (when (seq target-ids)
    (let [id (first target-ids)
          target (get-e w id)
          txy (round-coords (target :position))]
      (let [reachable-nbrs (find-reachable-nbrs w xy txy)]
        (prn :reachable-nbrs reachable-nbrs)
        (if (empty? reachable-nbrs)
          (recur w xy (rest target-ids))
          [id (first reachable-nbrs) txy])))))

(defn assign-jobs
  [w time]
  (let [workers (get-cnames-ids w (node :free-worker))
        jobs    (get-cnames-ids w (node :free-job))]
    (if-not (or (empty? workers)
                (empty? jobs))
      (let [worker-id (first workers)
            xy (round-coords (get-c w worker-id :position))]
        ;; find unoccupied neighbors and check if worker can get to
        ;; them
        (if-let [[job-id [x y] [tx ty]] (find-reachable w xy jobs)]
          (let [job (get-e w job-id)]
            (prn :job-assigned job-id worker-id tx ty x y)
            (-> w 
                (update-entity job-id rem-c :free)
                (update-entity worker-id rem-c :job-ready)
                (update-entity worker-id set-c (job-dig tx ty job-id))
                (update-entity worker-id set-c (destination (float x) (float y))))))))))

(defn system-assign-jobs
  "take free workers and find next (closest?) jobs for them"
  [w time]
  (let [res (assign-jobs w time)]
    (if res
      res
      w)))

