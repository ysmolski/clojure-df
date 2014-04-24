(ns cg.core
  [:use cg.ecs]
  [:use cg.comps]
  [:require [quil.core :as q]])


(def update-sleep-ms 100)
(def running (atom true))

(def scene {:dw [(health)
                 (speed 50)
                 (position 10 10)
                 (velocity 0.02 0.02)
                 (controllable)
                 (renderable "D")
                 (destination 200 100)
                 (path [[100 10]
                        [350 180]])]
            :beast [(health)
                    (position 100 100)
                    (velocity 0 0)
                    (renderable "b")]})

(def world (atom (load-scene (new-ecs) scene)))

;;; Systems

(defn move [e time]
  (let [v (e :velocity)
        t-norm (/ time 1000)
        dx (* (v :x) t-norm)
        dy (* (v :y) t-norm)]
    (-> e
        (update-in [:position :x] + dx)
        (update-in [:position :y] + dy))))

(defn system-move [w time]
  (let [ids (get-cnames-ids w (node :render))]
    (reduce #(update-entity %1 %2 move time) w ids)))


;;; Guide System

(defn distance
  "distance between points"
  ([x1 y1 x2 y2]
     (let [dx (- x2 x1)
           dy (- y2 y1)]
       (distance dx dy)))
  ([dx dy]
     (Math/sqrt (+ (* dx dx)
                   (* dy dy)))))

(defn project-speed
  "calculates projected speed on x and y from x1, y2 to x2, y2 and absolute speed"
  [x1 y1 x2 y2 speed]
  (let [dx (- x2 x1)
        dy (- y2 y1)
        dist (distance dx dy)]
    (if (< dist speed)
      [0 0]
      (let [relation (/ (float speed)
                        dist)
            vx (* relation dx)
            vy (* relation dy)]
        [vx vy]))))

(defn update-destination
  "Gets first point from path if exists and set destination to it.
  Otherwise - no destination"
  [e]
  (if-let [[x y] (first (-> e :path :p))]
        (-> e
            (assoc :destination (destination x y)) ; refactor
            (update-in [:path :p] rest))
        (dissoc e :destination))) ; you cannot do that. you need to
                                  ; call to higher level operator on
                                  ; ECS structure :(

(defn guide
  "calculates velocity based on position, destination and speed"
  [e time]
  (let [p (e :position)
        d (e :destination)
        s (-> e :speed :s)
        [vx vy] (project-speed (p :x) (p :y) (d :x) (d :y) s)]
    (if (or (= vx 0)
            (= vy 0))
      (update-destination e)
      (assoc e :velocity (velocity vx vy)))))  ; refactor

(defn system-guide [w time]
  (let [ids (get-cnames-ids w (node :guide))]
    (reduce #(update-entity %1 %2 guide time) w ids)))

;;; Path Finding
(defn system-path-finding [w time]
  (let [ids (get-cnames-ids w (node :guide))]
    (reduce #(update-entity %1 %2 guide time) w ids)))

;;; Quil handlers

(defn on-key
  "Handles key presses. Returns new state of the world"
  [w key]
  (set-val w 0 :health :count key))

(defn on-tick
  "Handles ticks of the world, delta is the time passes since last tick"
  [w time]
  (do
    ;; (prn w)
    (-> w
        (update-val 1 :health :count inc)
        (system-guide time)
        (system-move time))))

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
  (q/smooth)
  (q/frame-rate 60))

(defn draw-objs [ents]
  (doseq [e ents]
    (let [m (e :position)
          r (e :renderable)]
      (q/text (r :char) (m :x) (+ 10 (m :y))))
    )
  )

(defn draw-world [w]
  (q/text (str (get-cname-ids w :renderable)) 10 390)
  (draw-objs (get-cnames-ents w (node :render)))
  )

(defn draw
  []
  (q/background-float (params :background-color))
  (q/stroke-weight 10)
  (q/stroke-float 10)
  (q/fill (params :foreground-color))
  (q/text-size (params :big-text-size))
  (let [w @world]
    (draw-world w)))

(defn key-press []
  (swap! world on-key (q/raw-key)))

(defn mouse
  "Possible events: :down :up :drag :move :enter :leave."
  [event]
  (swap! world on-mouse (q/mouse-x) (q/mouse-y) event))

(def updater (agent nil))

(defn updating [_]
  (when @running
    (send-off *agent* #'updating))
  (swap! world on-tick update-sleep-ms)
  (. Thread (sleep update-sleep-ms))
  nil)

;; start thread for ticks
(send-off updater updating)

(q/sketch
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

