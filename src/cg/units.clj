(ns cg.units
  "Functions for adding new entities into the world"
  (:require [cg.comps :refer :all]
            [cg.ecs :refer :all]))

(defn add-task-dig [w x y]
  (load-entity w :task-dig
               (task-dig)
               (position x y)
               (renderable :dig :yellow)
               (free)))

(defn add-task-build-wall [w x y]
  (load-entity w :task-build-wall
               (task-build-wall)
               (position x y)
               (renderable :wall :yellow)
               (free)))

(defn add-stone [w x y]
  (load-entity w :stone
               (stone :gabbro)
               (position x y)
               (renderable :stone :white)
               (free)
               (real)))

(defn add-wall [w x y]
  (load-entity w :wall
               (position x y)
               (renderable :wall :white)
               (wall)
               (real)))

(defn add-player [w [x y]]
  (load-entity w :unit
               (speed 10)
               (position (float x) (float y))
               (worker)
               (renderable :char :white)
               (real)
               (inventory #{})
               (can-dig)
               (can-build)
               (can-haul)
               (want-job)))

