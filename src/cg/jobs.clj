(ns cg.jobs
  (:require [cg.comps :refer :all]
            [cg.ecs :refer :all]
            [cg.inv :as i]))

(defn enqueue
  ([w worker-id jobs]
     (-> w
         (rem-c worker-id :want-job)
         (set-c worker-id (done-job))
         (set-c worker-id (job-queue jobs))))
  ([e jobs]
     (-> e
         (rem-c :want-job)
         (set-c (done-job))
         (set-c (job-queue jobs)))))

(defn complete [w id job-name]
  (prn :complete-job id job-name)
  (-> w
      (rem-c id job-name)
      (set-c id (done-job))))

(defn fail [w id job-name]
  (prn :fail-job id job-name)
  (-> w
      (rem-c id job-name)
      (set-c id (failed-job))))


(defmulti abort (fn [w comp] (:cname comp)))

(defmethod abort :job-dig [w comp]
  w)

(defmethod abort :move-to [w comp]
  w)

(defmethod abort :pickup [w comp]
  (set-c w (:id comp) (free)))

(defmethod abort :job-build-wall [w comp]
  (let [stone-id (:stone comp)
        stone (get-e w stone-id)
        container-id (-> stone :contained :id)]
    ;; (prn stone-id stone )
    (-> w
        (i/uncontain container-id stone-id)
        (set-c stone-id (free))
        (set-c (:id comp) (free)))))

(defn reserve-storage [w store-id [x y] item-id]
  (update-entity w
                 store-id
                 #(assoc-in % [:store :cells [x y]] item-id)))


