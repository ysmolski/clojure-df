(ns cg.tasks
  "Functions for adding new entities into the world"
  (:require [cg.comps :refer :all]
            [cg.ecs :refer :all]
            [cg.map :as m]
            [cg.site :as s]))

(defmulti add-task (fn [world kind [x y]] kind))

(defmethod add-task :task-dig [w kind [x y]]
  (load-entity w kind
               (task-dig)
               (position x y)
               (renderable :dig :yellow)
               (free)))

(defmethod add-task :task-build-wall [w kind [x y]]
  (load-entity w kind
               (task-build-wall)
               (position x y)
               (renderable :wall :yellow)
               (free)))


;;(defmulti designate-task (fn [world action abs-x abs-y] action))
(defmulti cell-taskable? (fn [world action [abs-x abs-y]] action))

(defmethod cell-taskable? :task-dig [world action [x y]]
  (let [c (m/place world [x y])]
    (and (or (not (s/visible? c))
             (s/diggable? c))
         (empty? (ids-with-comp world (:ids c) action)))))

(defmethod cell-taskable? :task-build-wall [world action [x y]]
  (let [c (m/place world [x y])]
    (and (s/passable? c)
         (empty? (ids-with-comp world (:ids c) action)))))

(defmethod cell-taskable? :task-storage [world action [x y]]
  (let [c (m/place world [x y])]
    (and (s/passable? c)
         (empty? (ids-with-comp world (:ids c) action)))))

(defn designate-task [world action [x y]]
  (if (cell-taskable? world action [x y])
    (add-task world action [x y])
    world))

(defn designate-storage [world action cells]
  (let [world (load-entity world
                           :storage
                           (renderable :wood :white)
                           (store cells))
        id (last-id world)]
    (reduce #(m/storage %1 %2 id) world cells)))

(defmulti designate-area
  (fn [world action xy1 xy2] action))

(defmethod designate-area :task-storage
  [world action [x1 y1] [x2 y2]]
  (let [cells (for [x (range x1 (inc x2))
                    y (range y1 (inc y2))]
                [x y])
        cells (filter #(cell-taskable? world action %) cells)]
    (prn :designate-storage (count cells))
    (time (designate-storage world action cells))))

(defmethod designate-area :default
  [world action [x1 y1] [x2 y2]]
  (let [cells (for [x (range x1 (inc x2))
                    y (range y1 (inc y2))]
                [x y])]
    (prn :designate-area (count cells))
    (time (reduce #(designate-task %1 action %2) world cells))))


;; (defmethod designate-task :task-build-wall [world action x y]
;;   (if (cell-taskable? world action x y)
;;     (add-task world action [x y])
;;     world))


