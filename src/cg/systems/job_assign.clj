(ns cg.systems.job-assign
  [:use cg.common]
  [:use cg.ecs]
  [:use cg.comps]
  (:require [cg.map :as m]
            [cg.site :as s]
            [cg.astar :as astar]
            [cg.jobs :as j]))

(defn find-reachable-nbrs
  "finds all neighbour points of to-xy reachable from from-xy"
  [w [fx fy :as from-xy] to-xy]
  (->> to-xy
       (astar/neighbors (:map-size w))
       (filter (partial s/connected? (:map w) from-xy))
       (sort-by (fn [[tx ty]] (distance fx fy tx ty)))))

;;; FIX: this is super slow when number of targets is high
(defn sort-by-nearest
  "returns list of ids and entity values sorted by the distance from-xy to target"
  [w from-xy targets]
  (let [[fx fy] from-xy
        start (timer)
        res (sort-by (fn [[_ e]]
                       (let [[tx ty] (coords (:position e))]
                         (distance fx fy tx ty)))
                     targets)
        elapsed (timer-end start)]
    (prn :sort-by-nearest elapsed from-xy (map #(% 0) targets))
    res))

(defn sort-by-nearest-cell
  "returns list of ids and entity values sorted by the distance from-xy to target"
  [w from-xy targets]
  (let [[fx fy] from-xy
        start (timer)
        res (sort-by (fn [[tx ty]]
                       (distance fx fy tx ty))
                     targets)
        elapsed (timer-end start)]
    (prn :sort-by-nearest-cell elapsed from-xy targets)
    res))

(defn sort-by-id [w from-xy targets]
  (let [[fx fy] from-xy]
    (sort-by (fn [[id _]] id) targets)))

(defn find-reachable-nbr
  "Tries to find free cell next to one of ids reachable from xy.
  Returns [id [x y]] where
  id - id of reachable target entity
  x y - coords of found neighbour free-cell.
  Otherwise returns nil."
  [w from-xy ids]
  (loop [w w
         targets (sort-by-nearest w from-xy (get-e-many w ids))
         ;;targets (get-e-many w ids)
         ]
    (when (seq targets)
      (let [[id target] (first targets)
              to-xy (round-coords (:position target))]
        (let [reachable-nbrs (find-reachable-nbrs w from-xy to-xy)]
          ;;(prn :reachable-nbrs reachable-nbrs)
          (if (empty? reachable-nbrs)
            (recur w (rest targets))
            [id (first reachable-nbrs)]))))))

(defn find-reachable
  "Tries to find one reachable entity of ids from xy.
  Returns id of reachable target entity.
  Otherwise returns nil."
  [w from-xy ids]
  (loop [w w
         targets (sort-by-nearest w from-xy (get-e-many w ids))
         ;;targets (get-e-many w ids)
         ]
    (when (seq targets)
      (let [[id target] (first targets)
            to-xy (round-coords (:position target))]
        ;; (prn :reachable from-xy target)
        (if-not (s/connected? (:map w) from-xy to-xy)
          (recur w (rest targets))
          [id to-xy])))))

(defn find-reachable-cell
  "Tries to find one reachable target identified by pair of coordinates from xy.
  Returns [x y] of reachable cell.
  Otherwise returns nil."
  [w from-xy cells]
  (loop [w w
         targets (sort-by-nearest-cell w from-xy cells)
         ;;targets (get-e-many w ids)
         ]
    (when (seq targets)
      (let [to-xy (first targets)]
        ;; (prn :reachable from-xy target)
        (if-not (s/connected? (:map w) from-xy to-xy)
          (recur w (rest targets))
          to-xy)))))

(defn get-nbrs-dig-jobs [w xy free-jobs]
  (let [nbrs (map :ids (filter s/diggable? (map #(m/place w %)
                                                (astar/neighbors (:map-size w) xy))))
        all-ids (apply clojure.set/union nbrs)
        ids (clojure.set/intersection all-ids free-jobs)
        id (first ids)]
    (when id
      (prn :found-close id (round-coords (:position (get-e w id))))
      id)))

(defn assign-dig-task
  "For any new found worker it tries to find matching job
  first by looking at neughbour cells and then by looking at other cells"
  [w t]
  (let [workers (get-cnames-ids w (:free-digger node))]
    (if (seq workers)
      (let [jobs (get-cnames-ids w (:free-dig node))]
        (if (seq jobs)
          (let [worker-id (first workers)
                xy (round-coords (get-c w worker-id :position))]
            ;; find unoccupied neighbors and check if worker can get to
            ;; them
            (if-let [job-id (get-nbrs-dig-jobs w xy jobs)]
              (-> w 
                  (update-entity job-id rem-c :free)
                  (j/enqueue worker-id [(job-dig job-id)]))
              (if-let [[job-id [x y]] (find-reachable-nbr w xy jobs)]
                (let []
                  (prn :dig-assigned job-id worker-id  x y)
                  (-> w 
                      (update-entity job-id rem-c :free)
                      (j/enqueue worker-id [(move-to x y)
                                            (job-dig job-id)])))))))))))

(defn system-assign-dig-tasks
  "take free workers and find next (closest?) jobs for them"
  [w time]
  (if-let [res (assign-dig-task w time)]
    res
    w))


(defn assign-build-task
  "For any new found worker it tries to find matching job
  which is accessible and can have all needed materials"
  [w t]
  (let [workers (get-cnames-ids w (:free-builder node))]
    (if (seq workers)
      (let [jobs (get-cnames-ids w (:free-build-wall node))]
        (if (seq jobs)
          (let [stones (get-cnames-ids w (:free-stone node))]
            (if (seq stones)
              (let [worker-id (first workers)
                    xy (round-coords (get-c w worker-id :position))]
                (if-let [[task-id [tx ty]] (find-reachable-nbr w xy jobs)]
                  (if-let [[stone-id [sx sy]] (find-reachable w [tx ty] stones)]
                    (let []
                      (prn :build-assigned :tid task-id :sid stone-id :to worker-id :t [tx ty] :s [sx sy])
                      (-> w 
                          (update-entity task-id rem-c :free)
                          (update-entity stone-id rem-c :free)
                          (j/enqueue worker-id
                                     [(move-to sx sy)
                                      (pickup stone-id)
                                      (move-to tx ty)
                                      (job-build-wall task-id stone-id)])))))))))))))

(defn system-assign-build-tasks
  "take free workers and find next (closest?) jobs for them"
  [w time]
  (if-let [res (assign-build-task w time)]
    res
    w))

(defn passable-nbr
  "finds passable neighbour next to x y"
  [w xy]
  (->> xy
       (astar/neighbors (:map-size w))
       (some #(if (s/passable? (m/place w %)) %))))

(defn escape-walls
  "For any new found worker it tries to find matching job
  which is accessible and can have all needed materials"
  [e time w]
  (let [pos (round-coords (:position e))
        cell (m/place w pos)]
    (if (s/passable? cell)
      e
      (if-let [[x y] (passable-nbr w pos)]
        (do
          (prn :escape-wall pos :to [x y])
          (j/enqueue e [(move-to x y)]))
        (do
          (prn :escape-stuck pos)
          e)))))

(defn system-escape-walls
  "take free workers and find next (closest?) jobs for them"
  [w time]
  (update-comps w [:want-job] escape-walls time w))



(defn get-free-storages
  "returns list of free cells in storages"
  [w]
  (let [stores (get-cnames-ids w [:store])]
    (reduce #(into %1 (map first
                           (filter (fn [x] (nil? (second x)))
                                   (-> (get-e w %2) :store :cells))))
            []
            stores)))

(defn assign-haul-task
  "For any new found worker it tries to find matching job
  which is accessible and can have all needed materials"
  [w t]
  (let [workers (get-cnames-ids w (:free-hauler node))]
    (if (seq workers)
      (let [cells (get-free-storages w)]
        ;; (prn :assign-haul cells)
        (if (seq cells)
          (let [items (get-cnames-ids w (:free-stone node) :exclude [:stored])]
            (if (seq items)
              (let [worker-id (first workers)
                    xy (round-coords (get-c w worker-id :position))]
                (if-let [[tx ty] (find-reachable-cell w xy cells)]
                  (if-let [[item-id [sx sy]] (find-reachable w [tx ty] items)]
                    (let [store-id (m/storage w [tx ty])]
                      (prn :haul-assigned worker-id :id item-id :to store-id :cell [tx ty] :item [sx sy])
                      (-> w 
                          (update-entity store-id j/reserve-storage [tx ty] item-id)
                          (update-entity item-id rem-c :free)
                          (j/enqueue worker-id
                                     [(move-to sx sy)
                                      (pickup item-id)
                                      (move-to tx ty)
                                      (put item-id)
                                      (job-haul store-id item-id)])))))))))))))

(defn system-assign-haul-tasks
  "take free workers and find next (closest?) jobs for them"
  [w time]
  (if-let [res (assign-haul-task w time)]
    res
    w))
