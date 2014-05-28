(ns cg.comps
  (:use [cg.ecs :only [defcomp]])
  (:use cg.queue))

(def node {:move [:position :velocity]
           :guide [:position :path :speed]
           :path-find [:position :destination]
           :render [:position :renderable]
           :free-worker [:controllable :job-ready]
           :free-job [:job :free]
           })

(defcomp health []
  :dead false
  :count 100)

(defcomp destination [x y]
  :x x
  :y y)

(defcomp path [points]
  :points (apply queue points))

(defcomp velocity [x y]
  :x x
  :y y)

;;; how fast is the item can be. it can be recalculated depending in
;;; the weight
(defcomp speed [pix-per-sec]
  :pixsec pix-per-sec)

(defcomp position [x y]
  :x x
  :y y)

(defcomp controllable [])

(defcomp job [kind]
  :kind kind)

(defcomp free [])

(defcomp assigned [id]
  :id id)

(defcomp job-ready [])

(defcomp job-dig [x y id]
  :x x
  :y y
  :id id
  :progress 1000) ;; from 1000 to 0

(defcomp stone [kind]
  :kind kind)

(defcomp keyboard [])
(defcomp renderable [char]
  :char char)

(defcomp container []
  :items []
  :weight 0
  :size 0)
