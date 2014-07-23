(ns cg.systems.job-assign
  [:use cg.common]
  [:use cg.ecs]
  [:use cg.comps]
  (:require [cg.map :as m]
            [cg.site :as s]
            [cg.astar :as astar]
            [cg.jobs :as j]))

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
            (if-let [job-id (m/get-nbr-jobs w xy jobs)]
              (-> w 
                  (rem-c job-id :free)
                  (j/enqueue worker-id [(job-dig job-id)]))
              (if-let [[job-id [x y]] (m/find-reachable-nbr w xy jobs)]
                (do
                  (prn :dig-assigned job-id worker-id  x y)
                  (-> w 
                      (rem-c job-id :free)
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
                (if-let [[task-id [tx ty]] (m/find-reachable-nbr w xy jobs)]
                  (if-let [[stone-id [sx sy]] (m/find-reachable w [tx ty] stones)]
                    (let []
                      (prn :build-assigned :tid task-id :sid stone-id :to worker-id :t [tx ty] :s [sx sy])
                      (-> w 
                          (rem-c task-id :free)
                          (rem-c stone-id :free)
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
          ;; (prn :escape-wall pos :to [x y])
          (j/enqueue e [(move-to x y)]))
        (do
          (prn :escape-stuck pos)
          e)))))

(defn system-escape-walls
  "take free workers and find next (closest?) jobs for them"
  [w time]
  (update-entities-by-cnames w [:want-job] escape-walls time w))



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
                (if-let [[tx ty] (m/find-reachable-cell w xy cells)]
                  (if-let [[item-id [sx sy]] (m/find-reachable w [tx ty] items)]
                    (let [store-id (m/storage w [tx ty])]
                      (prn :haul-assigned worker-id :id item-id :to store-id :cell [tx ty] :item [sx sy])
                      (-> w 
                          (j/reserve-storage store-id [tx ty] item-id)
                          (rem-c item-id :free)
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
