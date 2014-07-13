(ns cg.comps
  (:require [clojure.math.numeric-tower :as math])
  (:use [cg.ecs :only [defcomp]])
  (:use cg.queue))

;; Aspects of the system. each aspect is used by one system
;;

(def node {:move [:position :velocity]
           :guide [:position :path :speed]
           :path-find [:position :move-to]
           :render [:position :renderable]
           
           :done-job [:job-queue :done-job]
           :failed-job [:job-queue :failed-job]

           :free-digger [:worker :want-job :can-dig]
           :free-builder [:worker :want-job :can-build]

           :free-dig [:task-dig :free]
           :free-build-wall [:task-build-wall :free]
           :free-stone [:stone :free]
           })

(defn round-coords
  "returns coordinates comp as vector of rounded values"
  [c]
  [(math/round (:x c))
   (math/round (:y c))])

(defn coords
  "returns coordinates comp as vector"
  [c]
  [(:x c)
   (:y c)])

(defcomp move-to [x y]
  :x (float x)
  :y (float y))

(defcomp path [points]
  :points (apply queue points))

(defcomp velocity [x y]
  :x (float x)
  :y (float y))

;;; how fast is the item can be. it can be recalculated depending in
;;; the weight
(defcomp speed [pix-per-sec]
  :pixsec pix-per-sec)

(defcomp position [x y]
  :x (float x)
  :y (float y))

;;;;

(defcomp stone [kind]
  :kind kind)

(defcomp renderable [t color]
  :texture t
  :color color)

(defcomp container []
  :items []
  :weight 0
  :size 0)

;; means that it's real object in world with boundaries
(defcomp real [])

;;;;
;; tasks, jobs and management of them

(defcomp worker [])

;; specify is worker can do particular set of jobs
(defcomp can-dig [])
(defcomp can-build [])
(defcomp can-haul [])

(defcomp task-dig []
  :progress 800)

(defcomp task-build-wall []
  :progress 500)

;; means that task can be assigned for the worker
(defcomp free [])

;; means that task was assigned to worker with id
(defcomp assigned [worker-id]
  :id worker-id)

;; All jobs are put into job-queue and extracted by next-job system
;; when unit has "done-job". If unit has "failed-job" then all stack of
;; jobs is flushed out

;; what are next jobs worker should proceed with
(defcomp job-queue [jobs]
  :jobs (apply queue jobs))

;; means that worker is ready for another job to be assigned
(defcomp want-job [])

;; indicates that current job was finished and job-manager-system can
;; assign another job for worker
(defcomp done-job [])

;; if last job has failed, system put this component in entity and
;; next-job system flushes out whole queue of jobs
(defcomp failed-job [])

;; various possible tasks unit can perform
(defcomp job-dig [task-id]
  :id task-id)

(defcomp job-build-wall [task-id stone-id]
  :id task-id
  :stone stone-id)

;;;;
;;; Inventory

;; if items is contained in somewhere then put this component to
;; indicate where it belongs to
(defcomp contained [id-where]
  :id id-where)

;; for now inventory is simple abstract backpack
(defcomp inventory [ids]
  :backpack ids)

;; pickup might take some time. that's okay
(defcomp pickup [id]
  :id id
  :progress 100)

(defcomp put [id]
  :id id
  :progress 100)
