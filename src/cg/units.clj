(ns cg.units
  "Functions for adding new entities into the world"
  (:require [cg.comps :refer :all]
            [cg.ecs :refer :all]))

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

