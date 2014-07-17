(ns cg.core
  (:use cg.common)
  (:use cg.ecs)
  (:use cg.comps)
  (:use cg.camera)
  (:use cg.systems.move)
  (:use cg.systems.guide)
  (:use cg.systems.pathfind)
  (:use cg.systems.job-assign)
  (:use cg.systems.job-manager)
  (:use cg.systems.job-exec)
  (:use cg.systems.inventory)

  (:require [clojure.math.numeric-tower :as math]
            [clojure.pprint :as pp]
            [cg.astar :as astar]
            [cg.map :as m]
            [cg.site :as s]
            [cg.units :as u]
            [cg.jobs :as j]
            [cg.rendering :as r]
            [cg.tasks :as t]
            [play-clj.core :as g]
            [play-clj.g2d :refer :all]
            [play-clj.utils :as gu])
  )

;; (set! *warn-on-reflection* true) 
;; (set! *unchecked-math* true)


(def running (atom true))

(defn new-player [w]
  (let [xy (s/random-place (:map w) s/passable? 40 40)
        ;;xy (first (s/rc-cells (:rc w) (first (s/rc-smallest-area (:rc w)))))
        ]
    (u/add-player w xy)))

(defn new-spawn [w]
  (-> (apply-times w 10 new-player)
      (m/init-visible (s/random-place (:map w) s/passable? 40 40))))

;;; State

;;; paused - if the game is paused
;;; mouse-actions - what does action of mouse have effect on. possible
;;; values: :move-to :dig :build-wall

(def map-size 120)

(def game (atom {:world nil
                 :paused false
                 :update-time 0
                 :frame 0
                 :frame-time 0
                 :mouse-action :task-dig
                 :mouse-start nil
                 :mouse-pos [0 0]}))


(defn dig-all [w]
  (loop [w w
         cells (s/range-2d 1 (dec (count (:map w))))]
    (if-let [fc (first cells)]
      (let [rc (rest cells)
            c (m/place w fc)
            [x y] fc]
        (if (and (s/visible? c)
                 (s/diggable? c))
          (recur (t/add-task w :task-dig x y) rc)
          (recur w rc)))
      w)))

(defn new-world [w]
  (let [[site rc] (s/generate map-size s/new-cell)]
    (-> (new-ecs)
        (m/attach-to-ecs site rc)
        (new-spawn)
        #_(dig-all))))

;;; Handle input

(defn bound-view
  [camera [dx dy]]
  (let [[x y] (:pos camera)
        [w h] (:size camera)]
    (assoc camera :pos [(bound (- map-size w) (+ x dx))
                        (bound (- map-size h) (+ y dy))])))

(def key-to-scroll {47 [0 -1]
                    51 [0 1]
                    29 [-1 0]
                    32 [1 0]})

(defn on-key
  "Handles key presses. Returns new state of the world"
  [game key]
  ;;(set-val w 0 :health :count key)
  (condp = key
    (g/key-code :space) (update-in game [:paused] not)
    (g/key-code :escape) (assoc game :mouse-start nil)
    (g/key-code :f) (assoc game :mouse-action :task-dig)
    (g/key-code :g) (assoc game :mouse-action :move-to)
    (g/key-code :b) (assoc game :mouse-action :task-build-wall)
    (let [delta (map #(* % (ui :scroll-amount))
                     (key-to-scroll key [0 0]))]
      (update-in game [:camera] bound-view delta)
      ;;(prn delta @(game :viewport))
      )))

;; FIX: Handler contains code for logic and presentation
(defn on-mouse
  [game x y e]
  ;; (prn x y e)
  (let [w (:world game)
        action (:mouse-action game)
        [width height] (-> game :camera :size)
        [rel-x rel-y] (pix->relative [x y])
        [abs-x abs-y] (relative->absolute [rel-x rel-y] (:camera game))]
    (prn [x y] [abs-x abs-y] e action)
    (if (in-bound? rel-x rel-y width height)
      (cond
       (= action :move-to)
       (update-in game [:world]
                  update-entities
                  (get-cnames-ids w [:worker])
                  j/enqueue [(move-to abs-x abs-y)])

       (#{:task-dig :task-build-wall} action)
       (assoc game :mouse-start [x y])
       #_(update-in game [:world] t/designate-task action abs-x abs-y)
       
       :else game)
      game)))

;; (defn mouse-released
;;   [game x y e xy1 xy2]
;;   ;; (prn x y e)
;;   (if-let [[x1 y1] (:mouse-start game)]
;;     (let [[x2 y2] (:mouse-pos game)
;;           w (:world game)
;;           action (:mouse-action game)
;;           [width height] (-> game :camera :size)
;;           rel1 (pix->relative)
;;           [abs-x abs-y] (relative->absolute [rel-x rel-y] (:camera game))]
;;       (prn [x y] [abs-x abs-y] e action)
;;       (if (in-bound? rel-x rel-y width height)
;;         (cond
;;          (= action :move-to)
;;          (update-in game [:world]
;;                     update-entities
;;                     (get-cnames-ids w [:worker])
;;                     j/enqueue [(move-to abs-x abs-y)])

;;          (#{:task-dig :task-build-wall} action)
;;          (assoc game :mouse-start [x y])
;;          #_(update-in game [:world] t/designate-task action abs-x abs-y)
         
;;          :else game)
;;         game))))

(def systems [system-move
              system-guide
              system-path-find
              system-escape-walls
              system-assign-dig-tasks
              system-assign-build-tasks
              system-pickup
              system-move-contained
              system-done-job
              system-failed-job
              system-dig
              system-build])

(defn on-tick
  "Handles ticks of the world, delta is the time passes since last tick"
  [w time]
  (let [[ts w] (reduce (fn [[ts w] s]
                         (let [t (timer)
                               w (s w time)
                               t (timer-end t)]
                           [(conj ts t) w]))
                       [[] w] systems)]
    (when (> (reduce + ts) 10)
      (prn ts))
    w))

;;; ticks thread

(defn averager [& args]
  (int (/ (apply + args) (count args))))

(defn updating []
  (loop []
    (let [paused (:paused @game)
          update-sleep-ms (/ 1000 (float (ui :ups-cap)))
          start (timer)
          new-game (if paused
                     game
                     (do
                       (swap! game update-in [:world] on-tick update-sleep-ms)
                       (comment (prn :on-tick (inc (:frame @game)) )
                                (swap! game update-in [:frame] inc)
                                (swap! game assoc :frame-time (timer)))))
          elapsed (timer-end start)]
      (when-not paused
        (swap! game update-in [:update-time] averager (/ (float 1000) (max update-sleep-ms elapsed))))
      (if (> elapsed update-sleep-ms)
        (prn "timeout:" elapsed)
        (Thread/sleep (- update-sleep-ms elapsed)))
      (when @running
        (recur))))
  (prn :updating-exited))


(g/defscreen main-screen
  :on-show
  (fn [screen _]
    (g/update! screen :renderer (g/stage) :camera (g/orthographic))
    (g/graphics! :set-v-sync false)
    (swap! game update-in [:world] new-world)
    (swap! game r/add-renderers)
    (swap! game r/add-camera)
    (.start (Thread. updating))
    nil)
  
  :on-render
  (fn [screen _]
    (g/clear!)
    (r/render-tick @game)
    nil)
  
  :on-key-down
  (fn [{:keys [key]} _]
    (swap! game on-key key))

  :on-mouse-moved
  (fn [screen _]
    (swap! game assoc-in [:mouse-pos] (r/unproject-input screen)))

  :on-touch-down 
  (fn [screen _]
    (let [[x y] (r/unproject-input screen)]
      (swap! game on-mouse x y :down)))
  
  :on-touch-up
  (fn [screen _]
    ;; (swap! game mouse-released)
    (swap! game assoc-in [:mouse-start] nil))

  :on-touch-dragged
  (fn [screen _]
    (let [[x y] (r/unproject-input screen)]
      (swap! game assoc :mouse-pos [x y])
      #_(swap! game on-mouse x y :down))))

(g/defgame cg-game
    :on-create
    (fn [this]
      (g/set-screen! this main-screen)))
