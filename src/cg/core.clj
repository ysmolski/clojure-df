(ns cg.core
  [:use cg.ecs]
  [:use quil.core]
  [:require [quil.applet :as qa]])


(def update-sleep-ms 100)
(def running (atom true))

(def scene {:dw [(health)
                 (movable 10 10 20)
                 (controllable)
                 (renderable "D")
                 (target 0 0)]
            :beast [(health)
                    (movable 100 100 15)
                    (renderable "b")]})

(def world (atom (load-scene (new-ecs) scene)))

;;; Systems

(defn mover [e]
  (-> e
      (update-in [:movable :x] inc)
      (update-in [:movable :y] inc)
      ))

(defn system-movement [w]
  (let [mid (get-cname-ids w :movable)
        cid (get-cname-ids w :controllable)
        ids (clojure.set/intersection (set mid) (set cid))
        ]
;    (prn mid cid ids)
    (reduce #(update-entity %1 %2 mover) w ids)
    ))

;;; Quil handlers

(defn on-key
  "Handles key presses. Returns new state of the world"
  [w key]
  (set-val w 0 :health :count key))

(defn on-tick
  "Handles ticks of the world, delta is the time passes since last tick"
  [w delta]
  (do
 ;   (prn w)
    (-> w
        (update-val 1 :health :count inc)
        (system-movement))))

(defn on-mouse
  [w x y e]
  (do
    (prn x y e)
    w))



;; -----

(def params
  {:big-text-size 20
   :small-text-size 10
   :background-color 25
   :foreground-color 200})

(defn setup []
  (smooth)

  )

(defn draw-objs [ents]
  (doseq [e ents]
    (let [m (e :movable)
          r (e :renderable)]
      (text (r :char) (m :x) (+ 10 (m :y))))
    )
  )

(defn draw-world [w]
  (text (str (get-cname-ids w :renderable)) 10 390)
  (draw-objs (get-cname-ents w :renderable))
  )

(defn draw
  []
  (background-float (params :background-color))
  (stroke-weight 10)
  (stroke-float 10)
  (fill (params :foreground-color))
  (text-size (params :big-text-size))
  (let [w @world]
    (draw-world w)))

(defn key-press []
  (swap! world on-key (raw-key)))

(defn mouse
  "Possible events: :down :up :drag :move :enter :leave."
  [event]
  (swap! world on-mouse (mouse-x) (mouse-y) event))

(def updater (agent nil))

(defn updating [_]
  (when @running
    (send-off *agent* #'updating))
  (swap! world on-tick update-sleep-ms)
  (. Thread (sleep update-sleep-ms))
  nil)

;; start thread for ticks
(send-off updater updating)

(sketch 
  :title "ECS prototype"
  :size [800 400]
;  :renderer :opengl
  :setup setup
  :draw draw
  :key-typed key-press
  :mouse-pressed #(mouse :down)
  :on-close (fn [] (do
                     (reset! running false)
                     )))

