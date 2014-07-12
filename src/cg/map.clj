(ns cg.map
  "High level operations on ECS (both map and ecs entities)"
  (:require [cg.comps :refer :all]
            [cg.ecs :refer :all]
            [cg.site :as s]))


(defn place [ecs [x y]]
  (get-in ecs [:map (int x) (int y)]))

(defn form [ecs [x y] val]
  (update-in ecs [:map] s/form [(int x) (int y)] val))

(defn region [ecs [x y] val]
  (update-in ecs [:map] s/region [(int x) (int y)] val))


(defn rem-if-in-cell
  "Removes from ECS entities found in cell xy for which (pred entity-id) is true"
  [pred ecs xy]
  (let [cell-ids (:ids (place ecs xy))]
    ;; (prn :rem-if-in-cell xy cell-ids)
    (reduce #(if (pred %2) (rem-e %1 %2) %1) ecs cell-ids)))

;; FIX: add check if the job is actuall job dig
(defn rem-ents-in-cells
  "Gets ids which have component cnames and if those ids are found in cells, removes them
  from map and ecs completely."
  [ecs cnames cells]
  (let [ids (get-cnames-ids ecs cnames)
        f (partial rem-if-in-cell #(contains? ids %))]
    (reduce f ecs cells)))

;; visible system

(defn init-visible
  "updates cells visibility from point [x y] using region->cells hash"
  [w xy]
  (let [r (s/region (:map w) xy)
        cells (s/rc-cells (:rc w) r)]
    (update-in w [:map] s/add-visibles cells)))

(defn map-add-id [ecs [x y] id]
  (update-in ecs [:map (int x) (int y) :ids] conj id))

(defn map-rem-id [ecs [x y] id]
  (update-in ecs [:map (int x) (int y) :ids] disj id))

(defn- update-position [ecs id e1 e2]
  (let [p1 (:position e1)
        p2 (:position e2)]
    (-> ecs
        (#(if p1 (map-rem-id % (round-coords p1) id) %))
        (#(if p2 (map-add-id % (round-coords p2) id) %)))))

(defn attach-to-ecs [ecs map rc]
  (-> ecs
      (assoc
          :map map
          :map-size (count map)
          :rc rc)
      (add-update-hook update-position)))

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
        (rem-ents-in-cells [:task-dig] hidden-cells))))

(defn update-region
  "Adds region for cell xy based on neighbour cells.
  Chooses biggest neighbour region and renames other regions to it.
  Sets :visibible for all Neighbour regions"
  [ecs [x y]]
  (let [rs (s/nbrs-regions (:map ecs) (:map-size ecs) [x y])]
    ;; (prn :update-region [x y])
    (if (= 1 (count rs))
      (add-region ecs [x y] (first rs))
      (let [sorted-regions (s/rc-biggest-area (:rc ecs) rs)
            big (first sorted-regions)
            other (rest sorted-regions)]
        ;; (prn :update-region sorted-regions)
        (-> (reduce #(move-region %1 %2 big) ecs other)
            (add-region [x y] big))))))


(defn dig
  "Digs cell in position and puts floor into the place.
  Also updates region of newly dug cell."
  [ecs xy]
  (-> ecs
      (form xy :floor)
      (update-region xy)))


