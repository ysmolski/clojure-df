(ns cg.core
  [:use cg.ecs]
  [:use quil.core]
  [:require [quil.applet :as qa]])


(def update-sleep-ms 1000)
(def running (atom true))

(def scene {:dw [(health)
                 (movable 10 10 20)
                 (controllable)
                 (target false)]
            :beast [(health)]})

(def world (atom (load-scene (new-ecs) scene)))

(defn on-key
  "Handles key presses. Returns new state of the world"
  [w key]
  (set-val w 0 :health :count key))

(defn on-tick
  "Handles ticks of the world, delta is the time passes since last tick"
  [w delta]
  (do
    (prn w)
    (update-val w 1 :health :count inc)))

(defn on-mouse
  [w x y e]
  (do
    (prn x y e)
    w))

(def updater (agent nil))

(defn updating [_]
  (when @running
    (send-off *agent* #'updating))
  (swap! world on-tick update-sleep-ms)
  (. Thread (sleep update-sleep-ms))
  nil)


;; -----

(def params
  {:big-text-size 20
   :background-color 25
   :foreground-color 200})

(defn setup []
  (smooth)
  (no-stroke)
  )

(defn draw-world [w]
  (text (str (get-comp w 0 :health)) 60 60))

(defn draw
  []
  (background-float (params :background-color))
  (stroke-weight 20)
  (stroke-float 10)
  (fill (params :foreground-color))
  (text-size (params :big-text-size))
  (let [w @world]
    (draw-world w)))

(defn key-press []
  (swap! world on-key (raw-key)))

(defn mouse
  "Possible events: :down :up :drag :move :enter :leave"
  [event]
  (swap! world on-mouse (mouse-x) (mouse-y) event))

; start thread for ticks
(send-off updater updating)

(qa/applet
  :title "ECS prototype"
  :size [800 400]
  :renderer :opengl
  :setup setup
  :draw draw
;  :key-typed key-press
  :mouse-pressed #(mouse :down)
  :on-close (fn [] (do
                     (reset! running false)
                     )))

