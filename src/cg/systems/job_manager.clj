(ns cg.systems.job-manager
  [:use cg.common]
  [:use cg.ecs]
  [:use cg.comps]
  (:require [cg.map :as m]
            [cg.site :as s]
            [cg.astar :as astar]
            [cg.jobs :as j]))

(defn pr-done-job
  "tries to setup next job for an entity from job-queue component"
  [e time]
  (let [jobs (-> e :job-queue :jobs)
        next-job (peek jobs)]
    ;;(prn :next-job jobs)
    (if (nil? next-job)
      (-> e
          (rem-c :done-job)
          (rem-c :job-queue)
          (set-c (want-job)))
      (-> e
          (rem-c :done-job)
          (update-in [:job-queue :jobs] pop)
          (set-c next-job)))))

(defn abort-jobs-queue [w jobs]
  (let [next (peek jobs)]
    ;; (prn :abort next)
    (if (nil? next)
      w
      (recur (j/abort w next) (pop jobs)))))

(defn pr-failed-job
  "removes jobs queue and releases all capture resources in there"
  [w id time]
  (let [e (get-e w id)
        jobs (-> e :job-queue :jobs)]
    (prn :failed-job jobs)
    (-> w
        (abort-jobs-queue jobs)
        (update-entity id rem-c :failed-job)
        (update-entity id rem-c :job-queue)
        (update-entity id set-c (want-job)))))

(defn system-done-job [w time]
  (update-entities-by-cnames w (:done-job node) pr-done-job time))

(defn system-failed-job [w time]
  (let [ids (get-cnames-ids w (:failed-job node))]
    (reduce #(pr-failed-job %1 %2 time) w ids)))

