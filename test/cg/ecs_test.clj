(ns cg.ecs-test
  (:use midje.sweet)
  (:require [cg.ecs :refer :all]
           )
  (:import [cg.ecs ECS]))

(defcomp c1 [a] :a a)
(defcomp c2 [a] :a a)
(defcomp c3 [a] :a a)

(fact "about generating ecs"
 (keys (new-ecs)) => [:id :etoc :ctoe :fn]
 (last-id (add-e (new-ecs) :u)) => 0)

(fact "about loading entity"
 (:ctoe (load-entity (new-ecs) :u
                     (c1 0)
                     (c2 0)))
 => {:c1 #{0} :c2 #{0}})

(let [e (load-entity (new-ecs) :u (c1 0) (c2 0))]
  (fact "about updating entity - removing component"
   (update-entity e 0
                  (fn [e] (rem-c e :c1)))
   => (contains {:ctoe {:c1 #{} :c2 #{0}}
                 :etoc {0 {:c2 {:a 0}}}}))
  (fact "about updating entity - adding component"
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
  (fact "adding component directly using set-c"
   (set-c e 0 (c3 0)) => (contains {:ctoe {:c1 #{0 1} :c2 #{0 1} :c3 #{0}}
                                    :etoc {0 {:c1 {:a 0} :c2 {:a 0} :c3 {:a 0}}
                                           1 {:c1 {:a 1} :c2 {:a 1}}}}))
  (fact "removing entity directly using rem-e"
   (rem-e e 0) => (contains {:ctoe {:c1 #{1} :c2 #{1}}
                             :etoc {1 {:c1 {:a 1} :c2 {:a 1}}}}))
  (fact "removing component directly using rem-c"
   (rem-c e 0 :c2) => (contains {:ctoe {:c1 #{0 1} :c2 #{1}}
                                 :etoc {0 {:c1 {:a 0}}
                                        1 {:c1 {:a 1} :c2 {:a 1}}}}))
  (fact "about getting ids of entities by set of comp-names"
   (get-cnames-ids e [:c1 :c2]) => #{0 1})

  (let [e (assoc e :test 0)
        e (add-update-fn e (fn [ecs id a b]
                             (update-in ecs [:test] inc)))]
    (fact "about updating entity with attached update handler"
     (update-entity e 0 (fn [e] e)) => (contains {:test 1}))
    (fact "about set-c with attached update handler"
     (-> e
         (set-c 0 (c1 0))
         (set-c 1 (c1 0))) => (contains {:test 2}))
    (fact "about rem-c with attached update handler"
     (-> e
         (rem-c 0 (c1 0))
         (rem-c 1 (c1 0))) => (contains {:test 2}))))


