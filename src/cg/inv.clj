(ns cg.inv
  (:require [clojure.math.numeric-tower :as math]
            [cg.ecs :refer :all]
            [cg.comps :refer :all]))

(defn contain [w container-id item-id]
  (-> w
      (update-entity item-id set-c (contained container-id))
      (update-entity container-id #(update-in %1 [:inventory :backpack] conj item-id))))

(defn uncontain [w container-id item-id]
  (-> w
      (update-entity item-id rem-c :contained)
      (update-entity container-id #(update-in %1 [:inventory :backpack] disj item-id))))
