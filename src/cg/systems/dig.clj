(ns cg.systems.dig
  [:use cg.common]
  [:use cg.ecs]
  [:use cg.comps]
  (:require [clojure.math.numeric-tower :as math]
            [cg.map :as m]
            [cg.site :as s]
            [cg.astar :as astar]
            [cg.jobs :as j]
            [cg.units :as u]))

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


(defn try-dig
  "takes world and id of worker who has a job-dig and tries to perform the job.
   if job is completed then remove job property from worker and destroy job entity"
  [w id job-name task-name time]
  (let [e (get-e w id)
        e-xy (round-coords (e :position))
        job-id (:id (job-name e))
        job (get-e w job-id)
        job-xy (round-coords (:position job))
        progress (-> job task-name :progress)]
    ;;(prn :dig-do job-name task-name e-xy job)
    (if (contacting? e-xy job-xy)
      (if (neg? progress)
        (-> w
            (j/complete id job-name)
            (rem-e job-id)
            (m/dig job-xy)
            (add-with-prob 0.5 u/add-stone (job-xy 0) (job-xy 1)))
        (update-comp w job-id [task-name :progress] - (math/round time)))
      ;; remove job from id and report failed job for entity
      (-> w
          (j/fail id job-name)))))

(defn system-dig
  [w time]
  (let [ids (get-cnames-ids w [:job-dig])]
    (reduce #(try-dig %1 %2 :job-dig :task-dig time) w ids)))



