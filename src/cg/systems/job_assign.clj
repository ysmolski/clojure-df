(ns cg.systems.job-assign
  [:use cg.common]
  [:use cg.ecs]
  [:use cg.comps]
  [:require [cg.ecs :as e]]
  [:require [cg.map :as m]]
  [:require [cg.site :as s]]
  [:require [cg.astar :as astar]])

(defn find-reachable-nbrs
  "finds all neighbour points of to-xy reachable from from-xy"
  [w [fx fy :as from-xy] to-xy]
  (->> to-xy
       (astar/neighbors (:map-size w))
       (filter (partial s/connected? (:map w) from-xy))
       (sort-by (fn [[tx ty]] (distance fx fy tx ty)))))

;;; FIX: this is super slow when number of targets is high
(defn sort-by-nearest [w from-xy targets]
  (let [[fx fy] from-xy]
    (time (sort-by (fn [[_ e]]
                     (let [[tx ty] (coords (:position e))]
                       (distance fx fy tx ty)))
                   targets))))

(defn sort-by-id [w from-xy targets]
  (let [[fx fy] from-xy]
    (sort-by (fn [[id _]] id) targets)))

(defn find-reachable
  "Tries to find free cell next to entity specified by targetreachable from xy.
  Returns [id [x y] [tx ty]] where
  id - id of reachable target entity
  x y - coords of found free-cell.
  tx ty - coords of target entity
  Otherwise returns nil."
  [w xy ids]
  (loop [w w
         xy xy
         ;;targets (sort-by-nearest w xy (get-e-many w ids))
         targets (get-e-many w ids)
         ]
    (when (seq targets)
        (let [[id target] (first targets)
            txy (round-coords (:position target))]
        (let [reachable-nbrs (find-reachable-nbrs w xy txy)]
          ;;(prn :reachable-nbrs reachable-nbrs)
          (if (empty? reachable-nbrs)
            (recur w xy (rest targets))
            [id (first reachable-nbrs) txy]))))))

(defn get-nbrs-jobs [w xy free-jobs]
  (let [nbrs (map :ids (filter s/diggable? (map #(m/place w %) (astar/neighbors (:map-size w) xy))))
        all-ids (apply clojure.set/union nbrs)
        ids (clojure.set/intersection all-ids free-jobs)
        id (first ids)]
    (when id
      (prn :found-close id (round-coords (:position (get-e w id))))
      [id (round-coords (:position (get-e w id)))])))

(defn assign-jobs
  "For any new found worker it tries to find matching job
  first by looking at neughbour cells and then by looking at other cells"
  [w t]
  (let [workers (get-cnames-ids w (:free-worker node))]
    (if (seq workers)
      (let [jobs (get-cnames-ids w (:free-job node))]
        (if (seq jobs)
          (let [worker-id (first workers)
                xy (round-coords (get-c w worker-id :position))]
            ;; find unoccupied neighbors and check if worker can get to
            ;; them
            (if-let [[job-id [tx ty]] (get-nbrs-jobs w xy jobs)]
              (-> w 
                  (update-entity job-id rem-c :free)
                  (update-entity worker-id rem-c :job-ready)
                  (update-entity worker-id set-c (job-dig tx ty job-id)))
              (if-let [[job-id [x y] [tx ty]] (find-reachable w xy jobs)]
                (let [job (get-e w job-id)]
                  (prn :job-assigned job-id worker-id tx ty x y)
                  (-> w 
                      (update-entity job-id rem-c :free)
                      (update-entity worker-id rem-c :job-ready)
                      (update-entity worker-id set-c (job-dig tx ty job-id))
                      (update-entity worker-id set-c (destination (float x) (float y)))))))))))))

(defn system-assign-jobs
  "take free workers and find next (closest?) jobs for them"
  [w time]
  (if-let [res (assign-jobs w time)]
    res
    w))

