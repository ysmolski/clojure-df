(ns cg.jobs
  (:require [clojure.math.numeric-tower :as math]
            [cg.ecs :refer :all]
            [cg.comps :refer :all]))

(defn queue-jobs
  ([w worker-id jobs]
     (-> w
         (update-entity worker-id rem-c :want-job)
         (update-entity worker-id set-c (done-job))
         (update-entity worker-id set-c (job-queue jobs))))
  ([e jobs]
     (-> e
         (rem-c :want-job)
         (set-c (done-job))
         (set-c (job-queue jobs)))))

(defn complete-job [w id job-name result-comp]
  (prn :complete-job id job-name result-comp)
  (-> w
      (update-entity id rem-c job-name)
      (update-entity id set-c result-comp)))
