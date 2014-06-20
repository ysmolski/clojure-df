(ns cg.systems.job-exec
  [:use cg.common]
  [:use cg.ecs]
  [:use cg.comps]
  [:require [cg.ecs :as e]]
  [:require [cg.map :as m]]
  [:require [cg.units :as u]]
  [:require [cg.astar :as astar]])

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
            (update-entity id set-c (job-ready))
            (rem-e job-id)
            (m/dig job-xy)
            (add-with-prob 0.1 u/add-stone (job-xy 0) (job-xy 1)))
        (update-entity w id #(update-in %1 [job-kind :progress] - time)))
      w)))

(defn system-dig
  [w time]
  ;;(update-comps w [:job-dig] try-dig time w)
  (let [ids (get-cnames-ids w [:job-dig])]
    (reduce #(try-job %1 %2 :job-dig time) w ids)))
