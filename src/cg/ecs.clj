(ns cg.ecs
  "Entity component system"
  (:use [clojure.pprint :only [pprint]])
  (:use [clojure.set :only [intersection difference]])
  (:use [cg.queue])
  (:require [cg.site :as s]))

;;; All interactions with ecs should go via high order functions
;;; keeping ECS in consistency

;;; Performance note:
;;; We should update tree on the highest possible level, if you perform
;;; operations on entity then update it at once, then if possible
;;; component, or the worst case: value in component.

(declare 
 rem-c
 get-c
 round-coords
 coords
 map-add-id
 map-rem-id)

(defrecord ECS [id etoc ctoe])

(defn new-ecs []
  (ECS. 0 {} {}))

;;;; Components

(defmacro defcomp [name params & r]
  `(defn ~name ~params
     (hash-map ::name ~(keyword (clojure.core/name name)) ~@r)))

;;;; 1st level
;;;; internal representation should be hidden

(defn- id [ecs]
  (:id ecs))

(defn last-id
  "returns id of last added entity"
  [ecs]
  (dec (:id ecs)))

(defn- inc-id
  "returns ECS with increased id counter"
  [ecs]
  (update-in ecs [:id] inc))

(defn- dissoc-in
  "removes key in the nested assoc structure using ks as vector of keys"
  [ecs ks key]
  (update-in ecs ks dissoc key))

(defn- pre-set [s val]
  (if-not s
    #{val}
    (conj s val)))

(defn- ctoe-rem-id-cnames
  [ctoe id cnames]
  (reduce #(update-in %1 [%2] disj id) ctoe cnames))

(defn- ctoe-add-id-cnames
  [ctoe id cnames]
  (reduce #(update-in %1 [%2] pre-set id) ctoe cnames))

(defn- update-e [ecs id entity]
  (assoc-in ecs [:etoc id] entity))

;;;; 2nd level
;;;; read interface for ECS

(defn e-name [e]
  "")

(defn no-name [e]
  e)

;;; getters

(defn has?
  "Checks if entity-id has component cname"
  [ecs entity-id cname]
  (contains? (get-in ecs [:etoc entity-id]) cname))

(defn get-e
  "returns entity by id"
  [ecs entity-id]
  (get-in ecs [:etoc entity-id]))

(defn get-c
  "returns component `cname` in entity by `id`"
  [ecs id cname]
  (get-in ecs [:etoc id cname]))

;; (defn get-val
;;   "returns entity-component value"
;;   [ecs id cname k]
;;   (get-in ecs [:etoc id cname k]))

(defn get-e-cnames
  "returns keys of components of entity"
  [ecs id]
  (keys (get-in ecs [:etoc id])))

(defn get-cname-ids
  "Returns ids of entities which have the component
  specified in cname"
  [ecs cname]
  (get-in ecs [:ctoe cname]))

(defn get-cnames-ids
  "Returns ids of entities which have all the components
  specified in sequence cnames"
  [ecs cnames]
  (apply intersection (map #(set (get-cname-ids ecs %)) cnames)))

(defn get-cnames-ents
  "return entities which have components specified by seq cnames"
  [ecs cnames]
  (map #(get-e ecs %) (get-cnames-ids ecs cnames)))

;;; updaters

(defn add-e
  "adds entity to the ECS. added id should be extracted using last-id function"
  [ecs entity-name]
  (let [i (id ecs)]
    (-> ecs
        (inc-id)
        (assoc-in [:etoc i] {}))))

(defn rem-e
  "removes entity along with components"
  [ecs entity-id]
  (dissoc-in (reduce #(rem-c %1 entity-id %2)
                     ecs
                     (get-e-cnames ecs entity-id))
             [:etoc]
             entity-id))

(defn set-c
  "adds component to entity. Two-Arguments version should be passed to update-entity only"
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
           (update-in [:ctoe cname] pre-set entity-id)))))

(defn rem-c
  "Removes component from entity. Two-args version should be passed to update-entity only"
  ([entity cname]
     (dissoc entity cname))
  ([ecs entity-id cname]
     (-> (if (= cname :position)
           (let [c (get-c ecs entity-id cname)]
             ;;(prn entity-id c)
             (map-rem-id ecs [(:x c) (:y c)] entity-id))
           ecs)
         (dissoc-in [:etoc entity-id] cname)
         (update-in [:ctoe cname] disj entity-id))))


(defn- removed-added [a b]
  (let [a (set a)
        b (set b)]
    [(difference a b)
     (difference b a)]))


(defn map-add-id [ecs [x y] id]
  (update-in ecs [:map (int x) (int y) :ids] conj id))

(defn map-rem-id [ecs [x y] id]
  (update-in ecs [:map (int x) (int y) :ids] disj id))

(defn- update-map-position [ecs id e1 e2]
  (if (and (contains? e1 :position)
           (contains? e2 :position))
    (let [[x1 y1] (round-coords (:position e1))
          [x2 y2] (round-coords (:position e2))]
      (if (or (not= x1 x2)
              (not= y1 y2))
        (-> ecs
            (map-rem-id [x1 y1] id)
            (map-add-id [x2 y2] id))
        ecs))
    ecs))


;;; 3rd level
;;; Public 

(defn- update-ctoe
  "Updates ctoe according changes happened in entity id from state a to state b"
  [ecs id a b]
  (let [a (keys a)
        b (keys b)]
    (if (= a b)
      ecs
      (let [[removed added] (removed-added a b)]
        (-> ecs 
            (update-in [:ctoe] ctoe-rem-id-cnames id removed)
            (update-in [:ctoe] ctoe-add-id-cnames id added))))))

(defn update-entity
  "Takes entity by id and calls function (f entity & args).
  Returned entity is updated into ECS.
  If components were added/removed then update ctoe as well."
  [ecs id f & args]
  (let [e (get-e ecs id)
        result (apply f e args)
        ecs (update-map-position ecs id e result)]
    #_(prn :update-e id f (count args))
    (-> ecs
        (update-ctoe id e result)
        (update-e id result))))

(defn update-entities
  "Update multiple entities (ids) using function (f entity & args)"
  [ecs ids f & args]
  (reduce #(apply update-entity %1 %2 f args) ecs ids))

(defn update-comps
  "update entities which have components-names using (f entity & args)"
  [ecs comp-names f & args]
  (let [ids (get-cnames-ids ecs comp-names)]
    (apply update-entities ecs ids f args)))

;; DEPRECATED: not sure. is it needed???
;; (defn set-val
;;   "sets value in the component"
;;   [ecs id cname k val]
;;   (assoc-in ecs [:etoc id cname k] val))
;; (defn update-comp
;;   "updates whole component using function f and args"
;;   [ecs id cname f & args]
;;   (apply update-in ecs [:etoc id cname] f args))
;; (defn update-val
;;   "updates value in the component using function f and args"
;;   [ecs id cname k f & args]
;;   (apply update-in ecs [:etoc id cname k] f args))


;;;; Systems

;; TODO: add functions for storing systems fns in the ECS and fn for
;; running them

;;;; Util

(defn load-entity
  "Loads anentity of `ename` and vector of components into ECS"
  [ecs ename & comps]
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
;;; move them to cg.map

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
