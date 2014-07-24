(ns cg.systems.build
  [:use cg.common]
  [:use cg.ecs]
  [:use cg.comps]
  (:require [clojure.math.numeric-tower :as math]
            [cg.inv :as i]
            [cg.map :as m]
            [cg.site :as s]
            [cg.astar :as astar]
            [cg.jobs :as j]
            [cg.units :as u]))

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
    ;; (prn :build-do e-xy :o occupied :t task)
    (if (and (empty? occupied)
             (contacting? e-xy task-xy)
             (contacting? stone-xy task-xy))
      (if (neg? progress)
        (-> w
            (j/complete id job-name)
            (i/uncontain id stone-id)
            (rem-e task-id)
            (rem-e stone-id)
            (m/put-construction task-xy :wall)
            (u/add-wall (task-xy 0) (task-xy 1)))
        (update-comp w task-id [task-name :progress] - (math/round time)))
      ;; remove job from id and report failed job for entity
      (do
        (prn :build-abort occupied e-xy (job-name e))
        (-> w
            (j/fail id job-name)
            (j/abort (job-name e)))))))

(defn system-build
  [w time]
  (let [ids (get-cnames-ids w [:job-build-wall])]
    (reduce #(try-build %1 %2 :job-build-wall :task-build-wall time) w ids)))

