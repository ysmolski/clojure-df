(ns cg.ecs
  (:use [clojure.pprint :only [pprint]])
  (:use [clojure.set :only [intersection difference]])
  (:use [cg.queue])
  (:require [cg.site :as s]))

;; all intercations with ecs should go via 

(declare get-cnames
         get-cname-ids
         rem-c
         get-c
         round-coords
         coords
         map-add-id
         map-rem-id)

(defrecord Ecs [id etoc ctoe map map-size rc])

(defn new-ecs [[map region->cells]]
  (Ecs. 0 {} {} map (if map (count map) 0) region->cells))

;; (defn new-ecs-ref []
;;   (ref (new-ecs)))

;;;; 1st level
;;;; internal representation should be hidden

;; FIX: rethink how to handle ::names more properly
(defn e-name [e]
  (::name e))

(defn no-name [e]
  (dissoc e ::name))

(defn last-id
  "returns id of last added entity"
  [ecs]
  (dec (:id ecs)))

(defn inc-id
  "returns ECS with increased id counter"
  [ecs]
  (update-in ecs [:id] inc))

(defn dissoc-in
  "removes key in the nested assoc structure using ks as vector of keys"
  [ecs ks key]
  (update-in ecs ks dissoc key))


;;;; 2nd level
;;;; interface for ECS

(defn add-e
  "adds entity to the ECS. added id should be extracted using last-id function"
  [ecs entity-name]
  (let [id (:id ecs)]
    (-> ecs
        (inc-id)
        (assoc-in [:etoc id] {::name entity-name}))))

(defn rem-e
  "removes entity along with components"
  [ecs entity-id]
  (dissoc-in (reduce #(rem-c %1 entity-id %2)
                     ecs
                     (get-cnames ecs entity-id))
             [:etoc]
             entity-id))

(defn set-c
  "adds component to entity"
  ([entity comp]
     (let [cname (::name comp)]
       ;; (when (= cname :position)
       ;;   (prn :setc entity))
       (assoc entity cname (dissoc comp ::name))))
  ([ecs entity-id c]
     (let [cname (::name c)
           c-without-name (dissoc c ::name)]
       (-> (if (= cname :position)
             (map-add-id ecs [(:x c) (:y c)] entity-id)
             ecs)
           (assoc-in [:etoc entity-id cname] c-without-name)
           (assoc-in [:ctoe cname entity-id] 1)))))

(defn rem-c
  "removes component from entity or ECS"
  ([entity cname]
     (dissoc entity cname))
  ([ecs entity-id cname]
     (-> (if (= cname :position)
           (let [c (get-c ecs entity-id cname)]
             ;;(prn entity-id c)
             (map-rem-id ecs [(:x c) (:y c)] entity-id))
             ecs)
         (dissoc-in [:etoc entity-id] cname)
         (dissoc-in [:ctoe cname] entity-id))))

(defn has?
  "checks is entity contains component"
  [ecs entity-id cname]
  (contains? (get-in ecs [:etoc entity-id]) cname))

(defn get-e-name
  "returns name of entity by id"
  [ecs id]
  (get-in ecs [:etoc id ::name]))

(defn get-e
  "returns entity by id"
  [ecs entity-id]
  (get-in ecs [:etoc entity-id]))

(defn get-c
  "returns component `cname` in entity by `id`"
  [ecs id cname]
  (get-in ecs [:etoc id cname]))

(defn get-val
  "returns entity-component value"
  [ecs id cname k]
  (get-in ecs [:etoc id cname k]))

(defn get-cnames
  "returns keys of components of entity"
  [ecs id]
  (keys (get-in ecs [:etoc id])))

(defn get-cname-ids
  "Returns ids of entities which have the component
  specified in cname"
  [ecs cname]
  (keys (get-in ecs [:ctoe cname])))

(defn get-cnames-ids
  "Returns ids of entities which have all the components
  specified in sequence cnames"
  [ecs cnames]
  (apply intersection (map #(set (get-cname-ids ecs %)) cnames)))

(defn get-cnames-ents
  "return entities which have components specified by seq cnames"
  [ecs cnames]
  (map #(get-e ecs %) (get-cnames-ids ecs cnames)))
  
;; not sure. is it needed???
(defn update-comp
  "updates whole component using function f and args"
  [ecs id cname f & args]
  (apply update-in ecs [:etoc id cname] f args))

(defn update-val
  "updates value in the component using function f and args"
  [ecs id cname k f & args]
  (apply update-in ecs [:etoc id cname k] f args))

(defn removed-added [a b]
  (let [a (set a)
        b (set b)]
    [(difference a b)
     (difference b a)]))

(defn ctoe-rem-id-cnames
  [ctoe id cnames]
  (reduce #(dissoc-in %1 [%2] id) ctoe cnames))

(defn ctoe-add-id-cnames
  [ctoe id cnames]
  (reduce #(assoc-in %1 [%2 id] 1) ctoe cnames))

(defn- update-e [ecs id entity]
  (assoc-in ecs [:etoc id] entity))

(defn update-map-position [ecs id e1 e2]
  (let [[x1 y1] (round-coords (:position e1))
        [x2 y2] (round-coords (:position e2))]
    (if (or (not= x1 x2)
            (not= y1 y2))
      (-> ecs
          (map-rem-id [x1 y1] id)
          (map-add-id [x2 y2] id))
      ecs)))


;; public interfaces

(defn update-entity
  "Takes entity by id and calls function f with entity and args as parameters.
  Returned entity is updated into ECS"
  [ecs id f & args]
  (let [e (get-e ecs id)
        r (apply f e args)
        ke (keys e)
        kr (keys r)
        ecs (update-map-position ecs id e r)]
    ;; (prn :update-e id f (count args))
    (if (not= ke kr)
      (let [[removed added] (removed-added ke kr)]
        ;; (prn ke '-> kr 'rem removed 'add added)
        (-> ecs 
            (assoc :ctoe (-> (:ctoe ecs)
                             (ctoe-rem-id-cnames id removed)
                             (ctoe-add-id-cnames id added)))
            (update-e id r)))
      (update-e ecs id r))))

(defn update-entities
  [ecs ids f & args]
  (reduce #(apply update-entity %1 %2 f args) ecs ids))

(defn update-comps
  "update entities in ECS which has comp-names with function entity-update-fn"
  [ecs comp-names entity-update-fn & args]
  (let [ids (get-cnames-ids ecs comp-names)]
    (apply update-entities ecs ids entity-update-fn args)))

;; (defn set-val
;;   "sets value in the component"
;;   [ecs id cname k val]
;;   (assoc-in ecs [:etoc id cname k] val))

;; Performance note:
;; We should update tree on the highest possible level, if you perform
;; operations on entity then update it at once, then if possible
;; component, or the worst case: value in component.

;;;; Components

(defmacro defcomp [name params & r]
  `(defn ~name ~params
     (hash-map ::name ~(keyword (clojure.core/name name)) ~@r)))

;;;; Systems

;; TODO: add functions for storing systems fns in the ECS and fn for
;; running them

;;;; Util

(defn load-entity
  "Loads anentity of `ename` and vector of components into ECS"
  [ecs ename comps]
  (let [s (add-e ecs ename)
        id (last-id s)]
    (reduce #(set-c %1 id %2) s comps)))

(defn load-scene
  "Loads into ECS a scene, which is hashmap, where keys are names
  of entities and values are vectors containing components"
  [ecs scene]
  (reduce (fn [ecs [ename comps]]
            (load-entity ecs ename comps))
          ecs
          (seq scene)))


;;; MAP operations
;;; --------------------------------------------------------------------------------

(defn round-coords [c]
  [(Math/round (:x c))
   (Math/round (:y c))])

(defn coords [c]
  [(:x c)
   (:y c)])

(defn place [ecs [x y]]
  (get-in ecs [:map (int x) (int y)]))

(defn form [ecs [x y] val]
  (update-in ecs [:map] s/form [(int x) (int y)] val))

(defn region [ecs [x y] val]
  (update-in ecs [:map] s/region [(int x) (int y)] val))

(defn map-add-id [ecs [x y] id]
  (update-in ecs [:map (int x) (int y) :ids] conj id))

(defn map-rem-id [ecs [x y] id]
  (update-in ecs [:map (int x) (int y) :ids] disj id))

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


(defn map-dig
  "Digs cell in position and puts floor into the place.
  Also updates region of newly dug cell."
  [ecs xy]
  (-> ecs
      (form xy :floor)
      (update-region xy)))


;;;; -------------------------

(comment
  (defn test-run [s cycles]
    (loop [s s
           n cycles]
      (if (> n 0)
        (recur
         (-> s
             (set-val 0 :health :count 1))
         (dec n))
        s)))

  (let [s (-> (new-ecs)
              (add-e :player)
              (add-e :mob)
              (set-c 0 (health))
              (set-c 1 (health)))]
    (prn (time (prn (test-run s 1000000))))))
