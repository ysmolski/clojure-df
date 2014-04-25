(ns cg.comps
  (:use [cg.ecs :only [defcomp]])
  (:use cg.queue))

(def node {:move [:position :velocity]
           :guide [:position :destination :velocity :speed]
           :render [:position :renderable]})

(defcomp health []
  :dead false
  :count 100)

(defcomp destination [x y]
  :x x
  :y y)

(defcomp path [points]
  :p (apply queue points))

(defcomp velocity [x y]
  :x x
  :y y)

;;; how fast is the item can be. it can be recalculated depending in
;;; the weight
(defcomp speed [pix-per-sec]
  :s pix-per-sec)

(defcomp position [x y]
  :x x
  :y y)

(defcomp controllable [])

(defcomp keyboard [])
(defcomp renderable [s]
  :char s)

(defcomp container []
  :items []
  :weight 0
  :size 0)
