(ns cg.systems.haul
  (:require [cg.comps :refer :all]
            [cg.ecs :refer :all]
            [cg.jobs :as j]
            [cg.map :as m]))

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

(defn try-finish-haul
  "takes world and id of worker who has a job-build-wall and tries to perform the job.
   if job is completed then remove job property from worker and destroy job entity"
  [w id job-name time]
  (let [e (get-e w id)
        e-xy (round-coords (e :position))
        store-id (:store (job-name e))
        item-id (:item (job-name e))
        ]
    ;; (prn :build-do e-xy :o occupied :t task)
    (if true
      (-> w
          (j/complete id job-name)
          (set-c item-id (free))
          (set-c item-id (stored store-id)))
      w)))

(defn system-finish-hauls
  [w time]
  (let [ids (get-cnames-ids w [:job-haul])]
    (reduce #(try-finish-haul %1 %2 :job-haul time) w ids)))
