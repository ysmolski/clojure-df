(ns cg.ecs
  "Entity component system"
;;  (:use midje.sweet)
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

(declare rem-c get-c)

(defrecord ECS [id etoc ctoe hooks])

(defn new-ecs []
  (ECS. 0 {} {} {:update #{} :set #{} :rem #{}}))

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

;;; getters

(defn has?
  "Checks if entity-id has component cname"
  [ecs entity-id cname]
  (contains? (get-in ecs [:etoc entity-id]) cname))

(defn get-e
  "returns entity by id"
  [ecs entity-id]
  (get-in ecs [:etoc entity-id]))

(defn get-e-many
  [ecs entity-ids]
  (map #(vector % (get-e ecs %)) entity-ids))

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
  (apply intersection (map #(get-cname-ids ecs %) cnames)))

(defn get-cnames-ents
  "return entities which have components specified by seq cnames"
  [ecs cnames]
  (map #(get-e ecs %) (get-cnames-ids ecs cnames)))

;;; event handlers

(defn add-update-hook
  "Add function which will be called everytime update-entity is happening.
  It is called in such way: (f ecs id a b) where id is of entity,
  a, b - entity before and after update fn."
  [ecs f]
  (update-in ecs [:hooks :update] conj f))

(defn get-update-fns [ecs]
  (get-in ecs [:hooks :update]))

(defn- run-fns
  [ecs fns & args]
  (reduce #(apply %2 %1 args) ecs fns))

;; (fact "run-fns"
;;  (run-fns 0 [#(inc %) #(* 3 %)]) => 3)

(defn- apply-update-fns
  [ecs & args]
  (apply run-fns ecs (get-update-fns ecs) args))

;;; updaters

;;; 3rd level
;;; Public 

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
       (assoc entity cname (dissoc comp ::name))))
  ([ecs entity-id c]
     (let [cname (::name c)
           c-without-name (dissoc c ::name)
           e (get-e ecs entity-id)]
       (-> (apply-update-fns ecs entity-id nil e)
           #_(if (= cname :position)
               (map-add-id ecs [(:x c) (:y c)] entity-id)
               ecs)
           (update-in [:etoc entity-id] assoc cname c-without-name)
           (update-in [:ctoe cname] pre-set entity-id)))))

(defn rem-c
  "Removes component from entity. Two-args version should be passed to update-entity only"
  ([entity cname]
     (dissoc entity cname))
  ([ecs entity-id cname]
     (-> #_(if (= cname :position)
           (let [c (get-c ecs entity-id cname)]
             ;;(prn entity-id c)
             (map-rem-id ecs [(:x c) (:y c)] entity-id))
           ecs)
         (apply-update-fns ecs entity-id (get-e ecs entity-id) nil)
         (dissoc-in [:etoc entity-id] cname)
         (update-in [:ctoe cname] disj entity-id))))


(defn- removed-added [a b]
  (let [a (set a)
        b (set b)]
    [(difference a b)
     (difference b a)]))


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
        ecs (apply-update-fns ecs id e result)]
    ;; (prn :update-e id (str f) (count args))
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

