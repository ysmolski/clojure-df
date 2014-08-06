(ns cg.systems.inventory
  (:require [cg.common :refer :all]
            [cg.comps :refer :all]
            [cg.ecs :refer :all]
            [cg.inv :as i]
            [cg.jobs :as j]
            [clojure.math.numeric-tower :as math]))

;; EXECUTE JOBS

(defn try-pickup
  "tries to pickup object and put it into entity inventory"
  [w id job-name time]
  (let [e (get-e w id)
        e-xy (round-coords (e :position))
        job (job-name e)
        item-id (:id job)
        item (get-e w item-id)
        item-xy (round-coords (:position item))
        store-id (-> item :stored :store)]
    ;;(prn :job-do job-name task-name e-xy job)
    (if (contacting? e-xy item-xy)
      (if (neg? (:progress job))
        (-> (if store-id
              (-> w
                  (rem-c item-id :stored)
                  (j/reserve-storage store-id item-xy nil))
              w)
            (j/complete id job-name)
            (i/contain id item-id))
        (update-comp w id [job-name :progress] - (math/round time)))
      ;; remove job from id and report failed job for entity
      (-> w
          (j/fail id job-name)
          (j/abort w job)))))

(defn system-pickup
  [w time]
  (let [ids (get-cnames-ids w [:pickup])]
    (reduce #(try-pickup %1 %2 :pickup time) w ids)))


(defn try-put
  "tries to pickup object and put it into entity inventory"
  [w id job-name time]
  (let [e (get-e w id)
        e-xy (round-coords (e :position))
        job (job-name e)
        item-id (:id job)
        item (get-e w item-id)
        item-xy (round-coords (:position item))]
    ;;(prn :job-do job-name task-name e-xy job)
    (if (contacting? e-xy item-xy)
      (if (neg? (:progress job))
        (-> w
            (j/complete id job-name)
            (i/uncontain id item-id))
        (update-comp w id [job-name :progress] - (math/round time)))
      ;; remove job from id and report failed job for entity
      (-> w
          (j/fail id job-name)
          (j/abort w job)))))

(defn system-put
  [w time]
  (let [ids (get-cnames-ids w [:put])]
    (reduce #(try-put %1 %2 :put time) w ids)))


(defn move-contained [e w time]
  (let [id (-> e :contained :id)
        pos (:position (get-e w id))]
    ;; (prn :move e :to id pos)
    (assoc e :position pos)))

(defn system-move-contained [w time]
  (update-entities-by-cnames w [:contained :position] move-contained w time))

