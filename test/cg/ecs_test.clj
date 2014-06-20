(ns cg.ecs-test
  (:use midje.sweet)
  (:require [cg.ecs :refer :all]
           )
  (:import [cg.ecs ECS]))

(defcomp c1 [a]
  :a a)

(defcomp c2 [b]
  :b b)

(fact
 "about generating ecs"
 (new-ecs) => (ECS. 0 {} {})
 (last-id (add-e (new-ecs) :u)) => 0)

(fact
 "about loading entity"
 (:ctoe (load-entity (new-ecs) :u
                     (c1 0)
                     (c2 10)))
 => {:c1 #{0} :c2 #{0}})

(fact
 "about updating entity"
 (update-entity (load-entity
                 (new-ecs) :u
                 (c1 0)
                 (c2 10))
                0
                (fn [e] (rem-c e :c1)))
 => (contains {:ctoe {:c1 #{} :c2 #{0}}
               :etoc {0 {:c2 {:b 10}, :cg.ecs/name :u}}}))

