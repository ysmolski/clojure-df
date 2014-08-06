(ns cg.inv
  (:require [cg.comps :refer :all]
            [cg.ecs :refer :all]))

(defn contain [w container-id item-id]
  (-> w
      (set-c item-id (contained container-id))
      (update-comp container-id [:inventory :backpack] conj item-id)))

(defn uncontain [w container-id item-id]
  (-> w
      (rem-c item-id :contained)
      (update-comp container-id [:inventory :backpack] disj item-id)))
