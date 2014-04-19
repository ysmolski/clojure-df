(ns cg.ecs
  (:use [clojure.pprint :only [pprint]]))

(defrecord Ecs [id etoc ctoe])

(defn new-ecs []
  (Ecs. 0 {} {}))

(defn new-ecs-ref []
  (ref (new-ecs)))

;;;; 1st level
;;;; internal representation should be hidden

(defn inc-id [s]
  (update-in s [:id] inc))

(defn dissoc-in [s ks key]
  (update-in s ks dissoc key))


;;;; 2nd level
;;;; interface for ECS
(declare rem-c)

(defn all-c [s ent]
  (keys (get-in s [:etoc ent])))

(defn all-e [s cname]
  (keys (get-in s [:ctoe cname])))

(defn add-e [s name]
  (let [id (:id s)]
    (-> s
        (inc-id)
        (assoc-in [:etoc id] {::name name}))))

(defn rem-e [s ent]
  (-> (reduce #(rem-c %1 ent %2)
              s
              (all-c s ent))
      (dissoc-in [:etoc] ent)))

(defn add-c [s ent c]
  (let [cname (c ::name)
        c-without-name (dissoc c ::name)]
    (-> s
        (assoc-in [:etoc ent cname] c-without-name)
        (assoc-in [:ctoe cname ent] 1))))

(defn rem-c
  [s ent cname]
  (-> s
      (dissoc-in [:etoc ent] cname)
      (dissoc-in [:ctoe cname] ent)))

(defn has? [s ent cname]
  (contains? (get-in @s [:etoc ent]) cname))

(defn get-e-name [s id]
  (get-in s [:etoc id ::name]))

(defn get-e [s id]
  (get-in s [:etoc id]))

(defn get-comp [s id cname]
  (get-in s [:etoc id cname]))

;; not sure. is it needed???
(defn update-comp [s id cname f & args]
  (apply update-in s [:etoc id cname] f args))

(defn update-val [s id cname k f & args]
  (apply update-in s [:etoc id cname k] f args))

(defn set-val [s id cname k val]
  (assoc-in s [:etoc id cname k] val))

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

(defcomp health []
  :dead false
  :count 0)

(defcomp keyboard [])
(defcomp renderable [])

(defcomp container []
  :items []
  :weight 0
  :size 0)

;;;; Systems


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
    (println x "Hello, World!")))


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
  (prn (time (prn (test-run s 1000000)))))
