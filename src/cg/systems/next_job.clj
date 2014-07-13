(ns cg.systems.next-job
  [:use cg.common]
  [:use cg.ecs]
  [:use cg.comps]
  (:require [cg.map :as m]
            [cg.site :as s]
            [cg.astar :as astar]
            [cg.jobs :refer :all]))

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

(defn pr-failed-job
  "tries to setup next job for an entity from job-queue component"
  [e time]
  (let [jobs (-> e :job-queue :jobs)]
    (prn :failed-job jobs)
    (-> e
        (rem-c :failed-job)
        (rem-c :job-queue)
        (set-c (want-job)))))

(defn system-done-job [w time]
  (update-comps w (:done-job node) pr-done-job time))

(defn system-failed-job [w time]
  (update-comps w (:failed-job node) pr-failed-job time))

