(ns cg.systems.next-job
  [:use cg.common]
  [:use cg.ecs]
  [:use cg.comps]
  (:require [cg.map :as m]
            [cg.site :as s]
            [cg.astar :as astar]
            [cg.jobs :refer :all]))

(defn next-job
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

(defn fail-job
  "tries to setup next job for an entity from job-queue component"
  [e time]
  (let [jobs (-> e :job-queue :jobs)]
    (prn :fail-job jobs)
    (-> e
        (rem-c :fail-job)
        (rem-c :job-queue)
        (set-c (want-job)))))

(defn system-next-job [w time]
  (update-comps w (:done-job node) next-job time))

(defn system-fail-job [w time]
  (update-comps w (:failed-job node) fail-job time))

