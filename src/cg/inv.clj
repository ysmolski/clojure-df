(ns cg.inv
  (:require [clojure.math.numeric-tower :as math]
            [cg.ecs :refer :all]
            [cg.comps :refer :all]))

(defn contain [w container-id item-id]
  (-> w
      (set-c item-id (contained container-id))
      (update-comp container-id [:inventory :backpack] conj item-id)))

(defn uncontain [w container-id item-id]
  (-> w
      (rem-c item-id :contained)
      (update-comp container-id [:inventory :backpack] disj item-id)))
