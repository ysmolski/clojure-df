(ns cg.jobs
  (:require [clojure.math.numeric-tower :as math]
            [cg.ecs :refer :all]
            [cg.comps :refer :all]))

(defn queue-jobs [w worker-id jobs]
  (-> w
      (update-entity worker-id rem-c :want-job)
      (update-entity worker-id set-c (done-job))
      (update-entity worker-id set-c (job-queue jobs))))
