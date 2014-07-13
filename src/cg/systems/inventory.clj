(ns cg.systems.inventory
  [:use cg.common]
  [:use cg.ecs]
  [:use cg.comps]
  (:require [clojure.math.numeric-tower :as math]
            [cg.inv :as i]
            [cg.map :as m]
            [cg.units :as u]
            [cg.astar :as astar]
            [cg.jobs :refer :all]))

;; EXECUTE JOBS

(defn try-pickup
  "tries to pickup object and put it into entity inventory"
  [w id jobname time]
  (let [e (get-e w id)
        e-xy (round-coords (e :position))
        item-id (-> e jobname :id)
        item (get-e w item-id)
        item-xy (round-coords (:position item))
        progress (-> e jobname :progress)]
    ;;(prn :job-do job-name task-name e-xy job)
    (if (contacting? e-xy item-xy)
      (if (neg? progress)
        (-> w
            (complete-job id jobname (done-job))
            (i/contain id item-id))
        (update-entity w id #(update-in %1 [jobname :progress] - (math/round time))))
      ;; remove job from id and report failed job for entity
      (-> w
          (complete-job id jobname (failed-job))))))

(defn system-pickup
  [w time]
  ;;(update-comps w [:job-dig] try-dig time w)
  (let [ids (get-cnames-ids w [:pickup])]
    (reduce #(try-pickup %1 %2 :pickup time) w ids)))


(defn move-contained [e w time]
  (let [id (-> e :contained :id)
        pos (:position (get-e w id))]
    ;; (prn :move e :to id pos)
    (assoc e :position pos)))

(defn system-move-contained [w time]
  (update-comps w [:contained :position] move-contained w time))

