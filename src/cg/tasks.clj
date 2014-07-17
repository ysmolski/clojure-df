(ns cg.tasks
  "Functions for adding new entities into the world"
  (:require [cg.comps :refer :all]
            [cg.ecs :refer :all]
            [cg.map :as m]
            [cg.site :as s]))

(defmulti add-task (fn [world kind [x y]] kind))

(defmethod add-task :task-dig [w kind x y]
  (load-entity w kind
               (task-dig)
               (position x y)
               (renderable :dig :yellow)
               (free)))

(defmethod add-task :task-build-wall [w kind x y]
  (load-entity w kind
               (task-build-wall)
               (position x y)
               (renderable :wall :yellow)
               (free)))


;;(defmulti designate-task (fn [world action abs-x abs-y] action))
(defmulti cell-taskable? (fn [world action abs-x abs-y] action))

(defmethod cell-taskable? :task-dig [world action x y]
  (let [c (m/place world [x y])
        tasks (ids-with-comp world (:ids c) action)]
    (and (empty? tasks)
         (or (not (s/visible? c))
             (s/diggable? c)))))

(defmethod cell-taskable? :task-build-wall [world action x y]
  (let [c (m/place world [x y])
        tasks (ids-with-comp world (:ids c) action)]
    (and (empty? tasks)
         (s/passable? c))))

(defn designate-task [world action x y]
  (if (cell-taskable? world action x y)
    (add-task world action [x y])
    world))


;; (defmethod designate-task :task-build-wall [world action x y]
;;   (if (cell-taskable? world action x y)
;;     (add-task world action [x y])
;;     world))


