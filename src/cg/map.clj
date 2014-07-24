(ns cg.map
  "High level operations on ECS (both map and ecs entities)"
  (:require
   [cg.astar :as astar]
   [cg.common :refer :all]
   [cg.comps :refer :all]
   [cg.ecs :refer :all]
   [cg.site :as s]))


(defn place [ecs [x y]]
  (get-in ecs [:map (int x) (int y)]))

(defn ids
  ([ecs [x y]]
     (get-in ecs [:map (int x) (int y) :ids]))
  ([ecs xy cname]
     (ids-with-comp ecs (ids ecs xy) cname)))

(defn form [ecs [x y] val]
  (update-in ecs [:map] s/form [(int x) (int y)] val))

(defn region
  ([ecs [x y]]
     (s/region (:map ecs) [(int x) (int y)]))
  ([ecs [x y] val]
     (update-in ecs [:map] s/region [(int x) (int y)] val)))

(defn storage
  ([ecs [x y]]
     (get-in ecs [:map (int x) (int y) :storage]))
  ([ecs [x y] id]
     (assoc-in ecs [:map (int x) (int y) :storage] id)))


(defn passable-nbr
  "finds passable neighbour next to x y"
  [w xy]
  (->> xy
       (astar/neighbors (:map-size w))
       (some #(if (s/passable? (place w %)) %))))

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

(defn remove-region
  "Sets cell [x y] to the region r and sets visible for it"
  [ecs [x y]]
  (let [r (region ecs [x y])]
    (prn :rem-region [x y] r)
    (-> ecs
        (region [x y] nil)
        (update-in [:rc] s/rc-remove r [x y]))))

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

(defn put-construction
  "Put construction into map and recalculates regions"
  [ecs xy what]
  (-> ecs
      (form xy what)
      (remove-region xy)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finders 

(defn find-reachable-nbrs
  "finds all neighbour points of to-xy reachable from from-xy"
  [w [fx fy :as from-xy] to-xy]
  (->> to-xy
       (astar/neighbors (:map-size w))
       (filter (partial s/connected? (:map w) from-xy))
       (sort-by (fn [[tx ty]] (distance fx fy tx ty)))))

;;; FIX: this is super slow when number of targets is high
(defn sort-by-nearest
  "returns list of ids and entity values sorted by the distance from-xy to target"
  [w from-xy targets]
  (let [[fx fy] from-xy
        start (timer)
        res (sort-by (fn [[_ e]]
                       (let [[tx ty] (coords (:position e))]
                         (distance fx fy tx ty)))
                     targets)
        elapsed (timer-end start)]
    (prn :sort-by-nearest elapsed from-xy (map #(% 0) targets))
    res))

(defn sort-by-nearest-cell
  "returns list of ids and entity values sorted by the distance from-xy to target"
  [w from-xy targets]
  (let [[fx fy] from-xy
        start (timer)
        res (sort-by (fn [[tx ty]]
                       (distance fx fy tx ty))
                     targets)
        elapsed (timer-end start)]
    (prn :sort-by-nearest-cell elapsed from-xy targets)
    res))

(defn sort-by-id [w from-xy targets]
  (let [[fx fy] from-xy]
    (sort-by (fn [[id _]] id) targets)))

(defn find-reachable-nbr
  "Tries to find free cell next to one of ids reachable from xy.
  Returns [id [x y]] where
  id - id of reachable target entity
  x y - coords of found neighbour free-cell.
  Otherwise returns nil."
  [w from-xy ids]
  (loop [w w
         targets (sort-by-nearest w from-xy (get-e-many w ids))
         ;;targets (get-e-many w ids)
         ]
    (when (seq targets)
      (let [[id target] (first targets)
              to-xy (round-coords (:position target))]
        (let [reachable-nbrs (find-reachable-nbrs w from-xy to-xy)]
          ;;(prn :reachable-nbrs reachable-nbrs)
          (if (empty? reachable-nbrs)
            (recur w (rest targets))
            [id (first reachable-nbrs)]))))))

(defn find-reachable
  "Tries to find one reachable entity of ids from xy.
  Returns id of reachable target entity.
  Otherwise returns nil."
  [w from-xy ids]
  (loop [w w
         targets (sort-by-nearest w from-xy (get-e-many w ids))
         ;;targets (get-e-many w ids)
         ]
    (when (seq targets)
      (let [[id target] (first targets)
           to-xy (round-coords (:position target))]
        ;; (prn :reachable from-xy target)
        (if-not (s/connected? (:map w) from-xy to-xy)
          (recur w (rest targets))
          [id to-xy])))))

(defn find-reachable-cell
  "Tries to find one reachable target identified by pair of coordinates from xy.
  Returns [x y] of reachable cell.
  Otherwise returns nil."
  [w from-xy cells]
  (loop [w w
         targets (sort-by-nearest-cell w from-xy cells)
         ;;targets (get-e-many w ids)
         ]
    (when (seq targets)
      (let [to-xy (first targets)]
        ;; (prn :reachable from-xy target)
        (if-not (s/connected? (:map w) from-xy to-xy)
          (recur w (rest targets))
          to-xy)))))


(defn get-nbr-jobs [w xy free-jobs]
  (let [nbrs (map :ids (map #(place w %)
                            (astar/neighbors (:map-size w) xy)))
        all-ids (apply clojure.set/union nbrs)
        ids (clojure.set/intersection all-ids free-jobs)
        id (first ids)]
    (when id
      (prn :found-close id (round-coords (:position (get-e w id))))
      id)))

