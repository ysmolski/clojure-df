(ns cg.ecs-test
  (:require [clojure.test :refer :all]
            [cg.ecs :refer :all])
  (:import [cg.ecs Ecs]))

(deftest a-test
  (testing "create"
    (is (= (new-ecs) (Ecs. 0 {} {})))))
