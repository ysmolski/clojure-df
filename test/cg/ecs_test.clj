(ns cg.ecs-test
  (:use midje.sweet)
  (:require [cg.ecs :refer :all]
           )
  (:import [cg.ecs ECS]))

(defcomp c1 [a] :a a)
(defcomp c2 [a] :a a)
(defcomp c3 [a] :a a)

(fact
 "about generating ecs"
 (new-ecs) => (ECS. 0 {} {})
 (last-id (add-e (new-ecs) :u)) => 0)

(fact
 "about loading entity"
 (:ctoe (load-entity (new-ecs) :u
                     (c1 0)
                     (c2 0)))
 => {:c1 #{0} :c2 #{0}})

(let [e (load-entity (new-ecs) :u (c1 0) (c2 0))]
  (fact
   "about updating entity - removing component"
   (update-entity e 0
                  (fn [e] (rem-c e :c1)))
   => (contains {:ctoe {:c1 #{} :c2 #{0}}
                 :etoc {0 {:c2 {:a 0}}}}))
  (fact
   "about updating entity - adding component"
   (update-entity e 0
                  (fn [e] (set-c e (c3 0))))
   => (contains {:ctoe {:c1 #{0}
                        :c2 #{0}
                        :c3 #{0}}
                 :etoc {0 {:c1 {:a 0}
                           :c2 {:a 0}
                           :c3 {:a 0}}}})))

(let [e (-> (new-ecs)
            (load-entity :u (c1 0) (c2 0))
            (load-entity :p (c1 1) (c2 1)))]
  (fact
   "adding component directly using set-c"
   (set-c e 0 (c3 0)) => (contains {:ctoe {:c1 #{0 1} :c2 #{0 1} :c3 #{0}}
                                    :etoc {0 {:c1 {:a 0} :c2 {:a 0} :c3 {:a 0}}
                                           1 {:c1 {:a 1} :c2 {:a 1}}}}))
  (fact
   "removing entity directly using rem-e"
   (rem-e e 0) => (contains {:ctoe {:c1 #{1} :c2 #{1}}
                             :etoc {1 {:c1 {:a 1} :c2 {:a 1}}}}))
  (fact
   "removing component directly using rem-c"
   (rem-c e 0 :c2) => (contains {:ctoe {:c1 #{0 1} :c2 #{1}}
                                 :etoc {0 {:c1 {:a 0}}
                                        1 {:c1 {:a 1} :c2 {:a 1}}}}))
  (fact
   "about getting ids of entities by set of comp-names"
   (get-cnames-ids e [:c1 :c2]) => #{0 1}))
