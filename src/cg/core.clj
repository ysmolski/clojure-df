(ns cg.core
  [:use cg.ecs]
  [:use cg.comps]
  [:require [cg.site :as s]]
  [:require [cg.astar :as astar]]
  [:require [quil.core :as q]])

(def ui
  {:window-border 10
   :tile-size 19
   :text-size 15
   :char-color 200
   :ui-color 40
   :wall-color 50
   :background-color 25
   :foreground-color 200
   })

(declare pos2pix pix2pos epos2pix pos-middle tiles)

(def update-sleep-ms 20)
(def running (atom true))

(def scene {:dw [(health)
                 (speed 10)
                 (position 1.0 1.0)
                 (velocity 0.0 0.0)
                 (controllable)
                 (renderable "D")
                 (path [])]
            :beast [(health)
                    (position 15 15)
                    (renderable "b")]})

(def world (atom (load-scene (new-ecs) scene)))

;;; MAP management

(def site-size 60)
(def site (s/generate site-size 0.4))

(s/form! site [1 1] :floor)
(s/form! site [32 32] :floor)

;; (s/smooth-list site site-size)
(s/smooth 1 site site-size)

;;; path finding

(defn get-cell-cost [cells xy] 1)

(defn filter-nbr [xy]
  (s/passable? @(s/place site xy)))

(s/form! site [1 1] :floor)
(s/form! site [32 32] :floor)

;;; just for test
;; (time (prn "path" (astar/path [1 1] [32 32] 1 site get-cell-cost filter-nbr)))

;;; create view port

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
  (update-comps w (node :move) move time))

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
    (if (< dist 0.1)
      [0 0]
      (let [relation (/ (float speed)
                        dist)
            vx (* relation dx)
            vy (* relation dy)]
        [vx vy]))))

;;; TODO refactor this piece
(defn guide
  "calculates velocity based on position, destination and speed"
  [e time]
  (let [d (peek (-> e :path :p))]
    (if (nil? d)
      e
      (let [p (e :position)
            s (-> e :speed :s)
            [vx vy] (project-speed (p :x) (p :y) (d 0) (d 1) s)]
        (if (= vx 0)
          (-> e
              (update-in [:path :p] pop)
              (set-c (velocity 0 0))
              (set-c (destination 0 0)))
          (set-c e (velocity vx vy)))))))

(defn system-guide [w time]
  (update-comps w (node :guide) guide time))

;;; Path Finding
;; (defn system-path-finding [w time]
;;   (let [ids (get-cnames-ids w (node :guide))]
;;     (reduce #(update-entity %1 %2 guide time) w ids)))

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
        (system-move time)
        (system-guide time)
        )))

(defn path-find-add [e x y]
  (let [ex (Math/round (-> e :position :x))
        ey (Math/round (-> e :position :y))
        x (Math/round x)
        y (Math/round y)
        path (astar/path [ex ey] [x y] 5 site get-cell-cost filter-nbr)
        e2 (update-in e [:path :p] conj [x y])]
    (if (empty? (path :xys))
      (do
        (prn "no path to" x y path)
        e)
      (do
        (prn "path found" ex ey path)
        (update-in e [:path :p] into (path :xys))))))

(defn on-mouse
  [w x y e]
  (prn x y e)
  (let [ids (get-cnames-ids w [:controllable])
        x (pos-middle (pix2pos x))
        y (pos-middle (pix2pos y))
        w-tiles (tiles (q/width))
        h-tiles (tiles (q/height))]
    (prn x y e)
    (if (and (>= x 0)
             (>= y 0)
             (< x w-tiles)
             (< y h-tiles))
      (update-entities w ids path-find-add x y)
      w)))

;;; RENDERING STUFF

(defn tiles [size]
  (let [tile-size (ui :tile-size)]
    (quot (- size (* tile-size 2))
          tile-size)))

(defn pos2pix [position]
  (+ (ui :tile-size) (* position (ui :tile-size))))

(defn epos2pix
  "converts entity position to pixels on the screen"
  [position]
  (pos2pix (+ 0.5 position)))

(defn pix2pos [pixel]
  (/ (float (- pixel (ui :tile-size)))
     (ui :tile-size)))

(defn pos-middle [position]
  (quot position 1))

(defn draw-ents [ents]
  (doseq [e ents]
    (let [m (e :position)
          r (e :renderable)
          x (epos2pix (m :x))
          y (epos2pix (m :y))]
      (q/text (r :char)
              (- x 4)
              (+ y 6)))))

(defn draw-tile-bg [passable x y]
  (when (not passable)
    (q/rect (pos2pix x)
            (pos2pix y)
            (ui :tile-size)
            (ui :tile-size)))
  )

(defn draw-site []
  (doseq [x (range (tiles (q/width)))
          y (range (tiles (q/height)))]
    (let [c @(s/place site [x y])]
      (draw-tile-bg (s/passable? c) x y))))

(defn draw-world [w]
  (q/text (str (get-cname-ids w :renderable)) 10 390)
  (q/fill (ui :wall-color))
  (draw-site)
  (q/fill (ui :char-color))
  (draw-ents (get-cnames-ents w (node :render)))
  )

(defn draw
  []
  (let [w-tiles (tiles (q/width))
        h-tiles (tiles (q/height))]
    (q/background-float (ui :background-color))

    ;; draw grid
    (q/stroke-weight 1)
    (q/stroke-float (ui :ui-color))
    (doseq [x (range (+ 1 w-tiles))]
      (q/line (pos2pix x) (pos2pix 0)
              (pos2pix x) (pos2pix h-tiles)))
    (doseq [y (range (+ 1 h-tiles))]
      (q/line (pos2pix 0) (pos2pix y)
              (pos2pix w-tiles) (pos2pix y)))

    ;; (q/text-size (ui :text-size))
    (q/text-font (q/state :font-monaco))
    (let [w @world]
      (draw-world w))))

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

(defn setup []
  (q/set-state! :font-monaco (q/create-font "Monaco" (ui :text-size) true))
  (q/smooth)
  (q/frame-rate 60))

(q/sketch
 :title "ECS prototype"
 :size [700 700]
                                        ;  :renderer :opengl
 :setup setup
 :draw draw
 :key-typed key-press
 :mouse-pressed #(mouse :down)
 :on-close (fn [] (do
                    (reset! running false)
                    )))
