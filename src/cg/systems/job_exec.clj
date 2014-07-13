(ns cg.systems.job-exec
  [:use cg.common]
  [:use cg.ecs]
  [:use cg.comps]
  (:require [clojure.math.numeric-tower :as math]
            [cg.map :as m]
            [cg.inv :as i]
            [cg.units :as u]
            [cg.astar :as astar]
            [cg.jobs :refer :all]))

;; EXECUTE JOBS

(defn add-with-prob [w probability f & args]
  (if (< (rand) probability)
    (apply f w args)
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
    (reduce #(try-dig %1 %2 :job-dig :task-dig time) w ids)))

(defn try-build
  "takes world and id of worker who has a job-build-wall and tries to perform the job.
   if job is completed then remove job property from worker and destroy job entity"
  [w id job-name task-name time]
  (let [e (get-e w id)
        e-xy (round-coords (e :position))
        task-id (:id (job-name e))
        stone-id (:stone (job-name e))
        task (get-e w task-id)
        stone (get-e w stone-id)
        task-xy (round-coords (:position task))
        stone-xy (round-coords (:position stone))
        progress (-> task task-name :progress)
        occupied (m/ids w task-xy :real)]
    ;; (prn :build-do job-name task-name e-xy :o occupied :t task)
    (if (and (empty? occupied)
             (contacting? e-xy task-xy)
             (contacting? stone-xy task-xy))
      (if (neg? progress)
        (-> w
            (complete-job id job-name (done-job))
            (i/uncontain id stone-id)
            (rem-e task-id)
            (rem-e stone-id)
            (m/put-construction task-xy :wall)
            (u/add-wall (task-xy 0) (task-xy 1)))
        (update-entity w task-id #(update-in %1 [task-name :progress] - (math/round time))))
      ;; remove job from id and report failed job for entity
      (-> w
          (i/uncontain id stone-id)
          (update-entity stone-id set-c (free))
          (update-entity task-id set-c (free))
          (complete-job id job-name (failed-job))))))

(defn system-build
  [w time]
  ;;(update-comps w [:job-dig] try-dig time w)
  (let [ids (get-cnames-ids w [:job-build-wall])]
    (reduce #(try-build %1 %2 :job-build-wall :task-build-wall time) w ids)))
