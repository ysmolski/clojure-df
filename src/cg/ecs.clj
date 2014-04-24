(ns cg.ecs
  (:use [clojure.pprint :only [pprint]])
  (:use [clojure.set :only [intersection]]))

(defrecord Ecs [id etoc ctoe])

(defn new-ecs []
  (Ecs. 0 {} {}))

;; (defn new-ecs-ref []
;;   (ref (new-ecs)))

;;;; 1st level
;;;; internal representation should be hidden

(defn last-id
  "returns id of last added entity"
  [ecs]
  (dec (get ecs :id)))

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
(declare get-id-cnames get-cname-ids rem-c)

(defn add-e
  "adds entity to the ECS. added id should be extracted using last-id function"
  [ecs name]
  (let [id (:id ecs)]
    (-> ecs
        (inc-id)
        (assoc-in [:etoc id] {::name name}))))

(defn rem-e
  "removes entity along with components"
  [ecs ent]
  (-> (reduce #(rem-c %1 ent %2)
              ecs
              (get-id-cnames ecs ent))
      (dissoc-in [:etoc] ent)))

(defn add-c
  "adds component to entity"
  [ecs ent c]
  (let [cname (c ::name)
        c-without-name (dissoc c ::name)]
    (-> ecs
        (assoc-in [:etoc ent cname] c-without-name)
        (assoc-in [:ctoe cname ent] 1))))

(defn rem-c
  "removes component from ECS"
  [ecs ent cname]
  (-> ecs
      (dissoc-in [:etoc ent] cname)
      (dissoc-in [:ctoe cname] ent)))

(defn has?
  "checks is entity contains component"
  [ecs ent cname]
  (contains? (get-in ecs [:etoc ent]) cname))

(defn get-e-name
  "returns name of entity by id"
  [ecs id]
  (get-in ecs [:etoc id ::name]))

(defn get-e
  "returns entity by id"
  [ecs id]
  (get-in ecs [:etoc id]))

(defn get-comp
  "returns component `cname` in entity by `id`"
  [ecs id cname]
  (get-in ecs [:etoc id cname]))

(defn get-val
  "returns entity-component value"
  [ecs id cname k]
  (get-in ecs [:etoc id cname k]))

(defn get-id-cnames
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

(defn update-entity
  "Takes entity by id and calls function f with entity and args as parameters.
  Returned entity is updated into ECS"
  [ecs id f & args]
  (let [r (apply f (get-e ecs id) args)]
    (assoc-in ecs [:etoc id] r)))

(defn set-val
  "sets value in the component"
  [ecs id cname k val]
  (assoc-in ecs [:etoc id cname k] val))

;; Performance note:
;; We should update tree on the highest possible level, if you perform
;; operations on entity then update it at once, then if possible
;; component, or the worst case: value in component.

;;;; mutable operations on ECS
;;;; DON'T USE THEM

;; (defn add-e! [s name]
;;   (dosync
;;    (alter s add-e name)
;;    (dec (:id @s))))

;; (defn add-c! [s ent c]
;;   (dosync
;;    (alter s add-c ent c)))

;; (defn rem-c! [s ent cname]
;;   (dosync
;;    (alter s rem-c ent cname)))

;; (defn update-val! [s id cname k val]
;;   (dosync
;;    (alter s update-val id cname k val)))

;;;; Components

(defmacro defcomp [name params & r]
  `(defn ~name ~params
     (hash-map ::name ~(keyword (clojure.core/name name)) ~@r)))

;;;; Systems


;;;; Util


; load the entity into ECS

(defn load-entity
  "Loads into ECS an Entity of `ename` and vector of components"
  [ecs ename comps]
  (let [s (add-e ecs ename)
        id (last-id s)]
    (reduce #(add-c %1 id %2) s comps)))

; load hash-map of entities into ECS

(defn load-scene
  "Loads into ECS a scene, which is hashmap, where keys are names of entities and values are vectors containing components"
  [ecs scene]
  (reduce (fn [ecs [ename comps]]
            (load-entity ecs ename comps))
          ecs
          (seq scene)))

;;;; -------------------------

(comment
  (prn "test")
  (def s (new-ecs-ref))
  (prn (add-e! s :player))
  (pprint s)

  (prn (add-e! s :player))
  (pprint s)

  (pprint (add-c! s 0 (health)))
  (pprint (add-c! s 1 (health)))
  (pprint (add-c! s 0 (keyboard)))
  ;;(prn (rem-c! s 0 :speed))

  (defn foo
    "I don't do a whole lot."
    [x]
    (println x "Hello, World!"))


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
              (add-c 0 (health))
              (add-c 1 (health)))]
    (prn (time (prn (test-run s 1000000))))))
