(ns cg.systems.job-exec
  [:use cg.common]
  [:use cg.ecs]
  [:use cg.comps]
  (:require [clojure.math.numeric-tower :as math]
            [cg.map :as m]
            [cg.units :as u]
            [cg.astar :as astar]))

;; EXECUTE JOBS

(defn add-with-prob [w probability f & args]
  (if (< (rand) probability)
    (apply f w args)
    w))

(defn try-job
  "takes world and id of worker who has a job-dig and tries to perform the job.
   if job is completed then remove job property from worker and destroy job entity"
  [w id job-kind time]
  (let [e (get-e w id)
        e-xy (round-coords (e :position))
        job-xy (coords (e job-kind))
        {job-id :id
         progress :progress} (job-kind e)]
    #_(prn :job-do job-kind e-xy job-xy progress)
    (if (contacting? e-xy job-xy)
      (if (neg? progress)
        (-> w
            (update-entity id rem-c job-kind)
            (update-entity id set-c (done-job))
            (rem-e job-id)
            (m/dig job-xy)
            (add-with-prob 0.5 u/add-stone (job-xy 0) (job-xy 1)))
        (update-entity w id #(update-in %1 [job-kind :progress] - (math/round time))))
      ;; remove job from id and report failed job for entity
      (-> w
          (update-entity id rem-c job-kind)
          (update-entity id set-c (failed-job))))))

(defn system-dig
  [w time]
  ;;(update-comps w [:job-dig] try-dig time w)
  (let [ids (get-cnames-ids w [:job-dig])]
    (reduce #(try-job %1 %2 :job-dig time) w ids)))
