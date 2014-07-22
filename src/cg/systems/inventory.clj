(ns cg.systems.inventory
  [:use cg.common]
  [:use cg.ecs]
  [:use cg.comps]
  (:require [clojure.math.numeric-tower :as math]
            [cg.inv :as i]
            [cg.map :as m]
            [cg.units :as u]
            [cg.astar :as astar]
            [cg.jobs :as j]))

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
                  (update-entity item-id rem-c :stored)
                  (update-entity store-id j/reserve-storage item-xy nil))
              w)
            (j/complete id job-name)
            (i/contain id item-id))
        (update-entity w id #(update-in %1 [job-name :progress] - (math/round time))))
      ;; remove job from id and report failed job for entity
      (-> w
          (j/fail id job-name)
          (j/abort w job)))))

(defn system-pickup
  [w time]
  ;;(update-comps w [:job-dig] try-dig time w)
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
        (update-entity w id #(update-in %1 [job-name :progress] - (math/round time))))
      ;; remove job from id and report failed job for entity
      (-> w
          (j/fail id job-name)
          (j/abort w job)))))

(defn system-put
  [w time]
  ;;(update-comps w [:job-dig] try-dig time w)
  (let [ids (get-cnames-ids w [:put])]
    (reduce #(try-put %1 %2 :put time) w ids)))


(defn move-contained [e w time]
  (let [id (-> e :contained :id)
        pos (:position (get-e w id))]
    ;; (prn :move e :to id pos)
    (assoc e :position pos)))

(defn system-move-contained [w time]
  (update-comps w [:contained :position] move-contained w time))

