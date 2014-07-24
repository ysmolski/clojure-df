(ns cg.systems.job-exec
  [:use cg.common]
  [:use cg.ecs]
  [:use cg.comps]
  (:require [clojure.math.numeric-tower :as math]
            [cg.map :as m]
            [cg.inv :as i]
            [cg.units :as u]
            [cg.astar :as astar]
            [cg.jobs :as j]))



