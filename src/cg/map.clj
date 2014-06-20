(ns cg.map
  "High level operations on ECS (both map and ecs entities)"
  (:require [cg.ecs :refer :all]
            [cg.site :as s]))

;; visible system

(defn init-visible
  "updates cells visibility from point [x y] using region->cells hash"
  [w xy]
  (let [r (s/region (:map w) xy)
        cells (s/rc-cells (:rc w) r)]
    (update-in w [:map] s/add-visibles cells)))

(defn attach-to-ecs [ecs map rc]
  (assoc ecs :map map :map-size (count map) :rc rc))

;; dig system

(defn add-region
  "Sets cell [x y] to the region r and sets visible for it"
  [ecs [x y] r]
  ;; (prn (place ecs [x y]))
  (-> ecs
      (update-in [:map] s/add-visible (:map-size ecs) [x y])
      (region [x y] r)
      (update-in [:rc] s/rc-add r [x y])))

(defn move-region
  "Adds region old-r to region new-r and updates map.
  If region old-r visible then it makes new-r visible and vise versa."
  [ecs old-r new-r]
  (let [cells (s/rc-cells (:rc ecs) old-r)
        hidden-cells (if (s/visible (:map ecs) (first cells))
                       (s/rc-cells (:rc ecs) new-r)
                       cells)]
    (-> (reduce #(region %1 %2 new-r) ecs cells)
        (update-in [:map] s/add-visibles hidden-cells)
        (update-in [:rc] s/rc-move old-r new-r)
        (rem-ents-in-cells [:job] hidden-cells))))

(defn update-region
  "Adds region for cell xy based on neighbour cells.
  Chooses biggest neighbour region and renames other regions to it.
  Sets :visibible for all Neighbour regions"
  [ecs [x y]]
  (let [rs (s/nbrs-regions (:map ecs) (:map-size ecs) [x y])]
    (if (= 1 (count rs))
      (add-region ecs [x y] (first rs))
      (let [sorted-regions (s/rc-biggest-area (:rc ecs) rs)
            big (first sorted-regions)
            other (rest sorted-regions)]
        (prn :update-region sorted-regions)
        (-> (reduce #(move-region %1 %2 big) ecs other)
            (add-region [x y] big))))))


(defn dig
  "Digs cell in position and puts floor into the place.
  Also updates region of newly dug cell."
  [ecs xy]
  (-> ecs
      (form xy :floor)
      (update-region xy)))


