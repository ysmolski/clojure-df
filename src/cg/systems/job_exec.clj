(ns cg.systems.job-exec
  [:use cg.common]
  [:use cg.ecs]
  [:use cg.comps]
  (:require [clojure.math.numeric-tower :as math]
            [cg.map :as m]
            [cg.units :as u]
            [cg.astar :as astar]
            [cg.jobs :refer :all]))

;; EXECUTE JOBS

(defn add-with-prob [w probability f & args]
  (if (< (rand) probability)
    (apply f w args)
    w))

(defn try-job
  "takes world and id of worker who has a job-dig and tries to perform the job.
   if job is completed then remove job property from worker and destroy job entity"
  [w id job-name task-name time]
  (let [e (get-e w id)
        e-xy (round-coords (e :position))
        job-id (:id (job-name e))
        job (get-e w job-id)
        job-xy (round-coords (:position job))
        progress (-> job task-name :progress)]
    ;;(prn :job-do job-name task-name e-xy job)
    (if (contacting? e-xy job-xy)
      (if (neg? progress)
        (-> w
            (complete-job id job-name (done-job))
            (rem-e job-id)
            (m/dig job-xy)
            (add-with-prob 0.5 u/add-stone (job-xy 0) (job-xy 1)))
        (update-entity w job-id #(update-in %1 [task-name :progress] - (math/round time))))
      ;; remove job from id and report failed job for entity
      (-> w
          (complete-job id job-name (failed-job))))))

(defn system-dig
  [w time]
  ;;(update-comps w [:job-dig] try-dig time w)
  (let [ids (get-cnames-ids w [:job-dig])]
    (reduce #(try-job %1 %2 :job-dig :task-dig time) w ids)))
