(ns cg.core
  [:use cg.ecs]
  [:use cg.comps]
  [:require [cg.ecs :as e]]
  [:require [cg.site :as s]]
  [:require [cg.astar :as astar]]
  [:require [quil.core :as q]])

(def ui
  {:window-width 1000
   :window-height 700
   :right-panel-width 300
   :window-border 10
   :tile-size 17
   :text-size 15
   :text-size-small 10
   :char-color 200
   :ui-color 40
   :wall-color 50
   :background-color 25
   :foreground-color 200
   :scroll-amount 10
   })

(declare pos2pix pix2pos epos2pix pos-middle tiles)

(def update-sleep-ms 33)
(def running (atom true))

(def scene {
            :beast [;;(health)
                    (position 15 15)
                    (renderable "b")]})

(defn new-job [w kind x y char]
  (load-entity w kind [(job kind)
                       (position x y)
                       (renderable char)
                       (free)]))

(defn new-stone [w x y]
  (load-entity w :stone [(stone :gabbro)
                         (position x y)
                         (renderable "✶")]))

(defn new-player [w]
  (let [[x y] (s/random-place (:map w) s/passable? 40 40)]
    (load-entity w :pl [(speed 10)
                        (position (float x) (float y))
                        (controllable)
                        (renderable "D")
                        (job-ready)
                        ])))

;;; State

;;; paused - if the game is paused
;;; mouse-actions - what does action of mouse have effect on. possible
;;; values: :move-to :dig :build-wall

(def map-size 100)
(def game {:world (atom (load-scene (new-ecs (s/generate map-size s/new-cell))
                                    scene))
           :viewport (atom [0 0])
           :paused (atom false)
           :mouse-action (atom :move-to)
           :update-time (atom 0)
           :mouse-pos (atom [0 0])})

;;; path finding

(defn get-cell-cost [cells xy] 10)

(defn filter-nbr [cells xy]
  (s/passable? (s/place cells xy)))


;;; view port

(defn floor
  "drops factional part"
  [position]
  (quot position 1))

(defn tiles
  "how many tiles can fit in size"
  [size]
  (let [tile-size (ui :tile-size)]
    (quot (- size (* tile-size 2))
          tile-size)))

(defn vp-width []
  (tiles (- (q/width) (ui :right-panel-width))))
  
(defn vp-height []
  (tiles (q/height)))

(defn viewport []
  (let [[vp-x vp-y] @(game :viewport)]
    [vp-x
     vp-y
     (vp-width)
     (vp-height)]))

(defn in-viewport? [x y]
  (and (>= x 0)
       (>= y 0)
       (< x (vp-width))
       (< y (vp-height))))

(defn bound
  "returns n bound to limit [0-b]"
  [b n]
  (if (neg? n)
    0
    (if (>= n b)
      b
      n)))

(defn bordering? [[x1 y1] [x2 y2]]
  (and (>= 1 (Math/abs (- x1 x2)))
       (>= 1 (Math/abs (- y1 y2)))))

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
    (if (< dist 0.2)
      [0 0]
      (let [relation (/ (float speed)
                        dist)
            vx (* relation dx)
            vy (* relation dy)]
        [vx vy]))))

;;; TODO refactor this piece
(defn guide
  "calculates velocity based on position, next point and speed"
  [e time]
  (let [points (-> e :path :points)
        next-point (peek points)]
    (if (nil? next-point)
      (-> e
          (rem-c :velocity)
          (rem-c :path))
      (let [p (e :position)
            s (-> e :speed :pixsec)
            [vx vy] (project-speed (p :x) (p :y) (next-point 0) (next-point 1) s)]
        (if (zero? vx)
          (-> e
              (update-in [:path :points] pop)
              (set-c (velocity 0.0 0.0)))
          (set-c e (velocity vx vy)))))))

(defn system-guide [w time]
  (update-comps w (node :guide) guide time))

(defn path-find-add [e time mp]
  (let [[ex ey] (round-coords e :position)
        [x y] (round-coords e :destination)
        new-path (astar/path [ex ey] [x y] 11 mp get-cell-cost filter-nbr)
        ;new-path {:xys [[x y]]}
        ]
    (prn "path" x y ex ey new-path)
    (if (empty? (new-path :xys))
      (rem-c e :destination)
      (-> e
          (set-c (path (new-path :xys)))
          (rem-c :destination)))))

(defn system-path-find [w time]
  (update-comps w (node :path-find) path-find-add time (:map w)))



(defn find-reachable
  "tries to find free cell next to cells specified by ids and return [id [x y]]
   where x,y - coords of found free cell. otherwise returns nil"
  [w ids entity]
  (when (seq ids)
    (let [id (first ids)
          target (get-e w id)
          [tx ty] (round-coords target :position)]
      (let [free-nbrs (filter (partial filter-nbr (:map w)) (astar/neighbors map-size [tx ty]))]
        (if (empty? free-nbrs)
          (recur w (rest ids) entity)
          [id (first free-nbrs) [tx ty]])))))

(defn assign-jobs
  [w time]
  (let [ids (get-cnames-ids w (node :free-worker))
        jobs (get-cnames-ids w (node :free-job))]
    (if-not (or (empty? ids)
                (empty? jobs))
      (let [worker-id (first ids)
            worker (get-e w worker-id)]
        ;; find unoccupied neighbors and check if worker can get to
        ;; them
        ;; TODO: check if worker and target coord are connected
        (if-let [[job-id [x y] [tx ty]] (find-reachable w jobs worker)]
          (let [job (get-e w job-id)]
            (do
              (prn :job-assigned job-id worker-id tx ty x y)
              (-> w 
                  (update-entity job-id rem-c :free)
                  (update-entity worker-id rem-c :job-ready)
                  (update-entity worker-id set-c (job-dig tx ty job-id))
                  (update-entity worker-id set-c (destination (float x) (float y)))))))))))

(defn system-assign-jobs
  "take free workers and find next (closest?) jobs for them"
  [w time]
  (let [res (assign-jobs w time)]
    (if res
      res
      w)))

(defn add-with-prob [w probability f & args]
  (if (< (rand) probability)
    (apply f w args)
    w))

(defn try-job
  "takes world and id of worker who has a job-dig and tries to perform the job.
   if job is completed then remove job property from worker and destroy job entity"
  [w id job-kind time]
  (let [e (get-e w id)
        e-xy (round-coords e :position)
        job-xy (coords e job-kind)
        {job-id :id
         progress :progress} (job-kind e)]
    ;; (prn :job-do job-kind e-xy job-xy progress)
    (if (bordering? e-xy job-xy)
      (if (neg? progress)
        (do
          (-> w
              (map-dig job-xy)
              (update-entity id rem-c job-kind)
              (update-entity id set-c (job-ready))
              (rem-e job-id)
              (add-with-prob 0.5 new-stone (job-xy 0) (job-xy 1))))
        (update-entity w id #(update-in %1 [job-kind :progress] - time)))
      w)))

(defn system-dig
  [w time]
  ;;(update-comps w [:job-dig] try-dig time w)
  (let [ids (get-cnames-ids w [:job-dig])]
    (reduce #(try-job %1 %2 :job-dig time) w ids)))



;;; Convert stuff

(defn pos2pix
  "converts relative position (in tile) to position in pixels"
  [tile-pos]
  (+ (ui :tile-size)
     (* tile-pos (ui :tile-size))))

(defn epos2pix
  "converts entity position to pixels on the screen.
  shifts coord of entity to place entity in the middle of tile"
  [position]
  (pos2pix (+ 0.5 position)))

(defn pix2pos [pixel]
  (/ (float (- pixel (ui :tile-size)))
     (ui :tile-size)))

(defn pix->relative
  "converts position in pixels to position relative (viewport) in tiles"
  [xy]
  (map #(int (floor (pix2pos %))) xy))

(defn relative->absolute
  "converts relative (viewport) position to absolute (map) position"
  [[x y]]
  (let [[vp-x vp-y _ _] (viewport)]
    [(+ vp-x x)
     (+ vp-y y)]))

(def pix->absolute (comp relative->absolute pix->relative))

;;; Events handlers

(load "handlers")

;;; RENDERING STUFF

(defn draw-ents [[vp-x vp-y w h] ents]
  (doseq [e ents]
    (let [m (e :position)
          r (e :renderable)
          x (- (m :x) vp-x)
          y (- (m :y) vp-y)]
      (if (and (< 0 x w)
               (< 0 y h))
        (q/text (r :char)
                (- (epos2pix x) 4)
                (+ (epos2pix y) 6))))))

(defn draw-tile-bg [passable x y]
  (when-not passable
    (q/rect (pos2pix x)
            (pos2pix y)
            (ui :tile-size)
            (ui :tile-size))))

(defn draw-site [w [vp-x vp-y width height]]
  (doseq [x (range width)
          y (range height)]
    (let [cell (place w [(+ vp-x x) (+ vp-y y)])]
      (draw-tile-bg (s/passable? cell) x y))))

(defn draw-world [w viewport]
  ;(q/text (str (get-cname-ids w :renderable)) 10 390)
  (q/fill (ui :wall-color))
  (draw-site w viewport)
  (q/fill (ui :char-color))
  (draw-ents viewport (get-cnames-ents w (node :render)))
  )

(defn entity-info-str [w id]
  (let [e (get-e w id)
        n (::e/name e)
        en (sort-by #(nth % 0) (vec (dissoc e ::e/name)))]
    (apply str (interpose "\n " (into [n] en)))))

(defn draw-info
  "display info for the cell by abs position: x, y"
  [w [x y :as xy]]
  (let [[x y] (pix->relative [x y])
        abs (relative->absolute [x y])]
    (when (in-viewport? x y)
      ;; (prn :draw-info x y abs)
      (let [cell (place w abs)
            ids (keys (:ids cell))
            entities (map #(entity-info-str w %) ids)]
        (q/text (str (abs 0) " " (abs 1) "\n"
                     (:form cell) "\n"
                     ids "\n"
                     (apply str (interpose "\n" entities)) "\n")
                (pos2pix (inc (vp-width)))
                (pos2pix 1))))))

(defn on-draw
  []
  (let [world @(game :world)
        width (vp-width)
        height (vp-height)
        viewport (viewport)
        mouse-pos @(game :mouse-pos)]
    (q/background-float (ui :background-color))
    
    ;; draw grid
    (q/stroke-weight 1)
    (q/stroke-float (ui :ui-color))
    (doseq [x (range (inc width))]
      (q/line (pos2pix x) (pos2pix 0)
              (pos2pix x) (pos2pix height)))
    (doseq [y (range (inc height))]
      (q/line (pos2pix 0) (pos2pix y)
              (pos2pix width) (pos2pix y)))

    ;; (q/text-size (ui :text-size))
    (q/text-font (q/state :font-monaco) (ui :text-size))

    (when @(game :paused)
      (q/text "pause" (pos2pix 0) (pos2pix (inc height))))

    (q/text (str @(game :update-time)) (pos2pix 6) (pos2pix (inc height)))
    (q/text (str @(game :mouse-action)) (pos2pix 9) (pos2pix (inc height)))
    (q/text (str mouse-pos) (pos2pix 16) (pos2pix (inc height)))

    (draw-world world viewport)

    (q/text-font (q/state :font-monaco) (ui :text-size-small))

    (draw-info world mouse-pos)))



;;; ticks thread

(defn averager [v1 v2]
  (int (/ (+ v1 v2) 2)))

(defn err-handler-fn [ag ex]
  (println "agent error: " ex 
           "\nvalue " @ag))
 
(def updater (agent nil :error-handler err-handler-fn))

(defn updating [_]
  (when @running
    (send-off *agent* #'updating))
  (let [start (System/nanoTime)
        new-world (if @(game :paused)
                    (game :world)
                    (swap! (game :world) on-tick update-sleep-ms))
        elapsed (/ (double (- (System/nanoTime) start)) 1000000.0)]

    (swap! (game :update-time) averager (/ (float 1000) (max update-sleep-ms elapsed)))
    
    (if (> elapsed update-sleep-ms)
      (prn "elapsed:" elapsed)
      (Thread/sleep (- update-sleep-ms elapsed))))
  nil)

(send-off updater updating)



;;; quil handlers 

(defn key-press []
  (swap! (game :world) on-key (q/raw-key)))

(defn mouse
  "Possible events: :down :up :drag :move :enter :leave."
  [event]
  (swap! (game :world) on-mouse (q/mouse-x) (q/mouse-y) event))



(defn setup []
  (swap! (game :world) on-start)
  (q/set-state! :font-monaco (q/create-font "Monaco" (ui :text-size) true))
  (q/smooth)
  (q/frame-rate 60))

(q/sketch
 :title "ECS prototype"
 :size [(ui :window-width) (ui :window-height)]
 :setup setup
 :draw on-draw
 :key-pressed key-press
 :mouse-pressed #(mouse :down)
 :mouse-moved (fn [] (reset! (game :mouse-pos) [(q/mouse-x) (q/mouse-y)]))
 :on-close (fn [] (reset! running false)))
