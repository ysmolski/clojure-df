(ns cg.jobs
  (:require [clojure.math.numeric-tower :as math]
            [cg.ecs :refer :all]
            [cg.comps :refer :all]
            [cg.inv :as i]))

(defn enqueue
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

(defn complete [w id job-name]
  (prn :complete-job id job-name)
  (-> w
      (update-entity id rem-c job-name)
      (update-entity id set-c (done-job))))

(defn fail [w id job-name]
  (prn :fail-job id job-name)
  (-> w
      (update-entity id rem-c job-name)
      (update-entity id set-c (failed-job))))


(defmulti abort (fn [w comp] (:cname comp)))

(defmethod abort :job-dig [w comp]
  w)

(defmethod abort :move-to [w comp]
  w)

(defmethod abort :pickup [w comp]
  (update-entity w (:id comp) set-c (free)))

(defmethod abort :job-build-wall [w comp]
  (let [stone-id (:stone comp)
        stone (get-e w stone-id)
        container-id (-> stone :contained :id)]
    ;; (prn stone-id stone )
    (-> w
        (i/uncontain container-id stone-id)
        (update-entity stone-id set-c (free))
        (update-entity (:id comp) set-c (free)))))


