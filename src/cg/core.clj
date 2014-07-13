(ns cg.core
  (:use cg.common)
  (:use cg.ecs)
  (:use cg.comps)
  (:use cg.systems.move)
  (:use cg.systems.guide)
  (:use cg.systems.pathfind)
  (:use cg.systems.job-assign)
  (:use cg.systems.next-job)
  (:use cg.systems.job-exec)
  (:use cg.systems.inventory)
  (:import [com.badlogic.gdx.graphics Texture]
           [com.badlogic.gdx.graphics.glutils ShapeRenderer ShapeRenderer$ShapeType]
           [com.badlogic.gdx.graphics.g2d SpriteBatch BitmapFont])

  (:require [clojure.math.numeric-tower :as math]
            [clojure.pprint :as pp]
            [cg.astar :as astar]
            [cg.map :as m]
            [cg.site :as s]
            [cg.units :as u]
            [play-clj.core :as g]
            [play-clj.g2d :refer :all]
            [play-clj.utils :as gu])
  )

;; (set! *warn-on-reflection* true) 
;; (set! *unchecked-math* true)

(def ui
  {:window-width 1000
   :window-height 700
   :right-panel-width 300
   :window-border 10
   :tile-size 16
   :text-size 15
   :text-size-small 10
   :char-color 250
   :ui-color 40
   :wall-color 80
   :background-color 25
   :foreground-color 200
   :scroll-amount 10
   :fps-cap 60
   :ups-cap 60
   })

(declare pos2pix pix2pos epos2pix pos-middle tiles)

(def running (atom true))

;; (def scene {:beast [(position 15 15)
;;                     (renderable "b")]})

(defn new-player [w]
  (let [xy (s/random-place (:map w) s/passable? 40 40)
        ;;xy (first (s/rc-cells (:rc w) (first (s/rc-smallest-area (:rc w)))))
        ]
    (u/add-player w xy)))

(defn new-spawn [w]
  (-> (apply-times w 20 new-player)
      (m/init-visible (s/random-place (:map w) s/passable? 40 40))))

;;; State

;;; paused - if the game is paused
;;; mouse-actions - what does action of mouse have effect on. possible
;;; values: :move-to :dig :build-wall

(def map-size 120)

(def game (atom {:world nil
                 :paused false
                 :mouse-action :dig
                 :update-time 0
                 :mouse-pos [0 0]}))

(def colors {:white (g/color :white)
             :yellow (g/color 1 1 0 0.7)})

;;; view port

(defn tiles
  "how many tiles can fit in size"
  [size]
  (let [tile-size (ui :tile-size)]
    (quot (- size (* tile-size 2))
          tile-size)))

(defn in-camera? [x y width height]
  (and (>= x 0)
       (>= y 0)
       (< x width)
       (< y height)))


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
  [[x y] camera]
  (let [[vp-x vp-y] (:pos camera)]
    [(+ vp-x x)
     (+ vp-y y)]))

;;(def pix->absolute (comp relative->absolute pix->relative))

(defn dig-all [w]
  (loop [w w
         cells (s/range-2d 1 (dec (count (:map w))))]
    (if-let [fc (first cells)]
      (let [rc (rest cells)
            c (m/place w fc)
            [x y] fc]
        (if (and (s/visible? c)
                 (s/diggable? c))
          (recur (u/add-task-dig w x y) rc)
          (recur w rc)))
      w)))

;;; Events handlers

(defn new-world [w]
  (let [[site rc] (s/generate map-size s/new-cell)]
    (-> (new-ecs)
        (m/attach-to-ecs site rc)
        (new-spawn)
        #_(dig-all))))

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
    (g/key-code :f) (assoc game :mouse-action :dig)
    (g/key-code :g) (assoc game :mouse-action :move-to)
    (g/key-code :b) (assoc game :mouse-action :build-wall)
    (let [delta (map #(* % (ui :scroll-amount))
                     (key-to-scroll key [0 0]))]
      (update-in game [:camera] bound-view delta)
      ;;(prn delta @(game :viewport))
      )))

(defmulti on-mouse-designate (fn [_ action _ _] action))

(defmethod on-mouse-designate :dig [w action x y]
  (let [c (m/place w [x y])
        tasks (ids-with-comp w (:ids c) :task-dig)]
    (if (and (empty? tasks)
             (or (not (s/visible? c))
                 (s/diggable? c)))
      (u/add-task-dig w x y)
      w)))

(defmethod on-mouse-designate :build-wall [w action x y]
  (let [c (m/place w [x y])
        tasks (ids-with-comp w (:ids c) :task-build-wall)]
    (if (and (empty? tasks)
             (s/passable? c))
      (u/add-task-build-wall w x y)
      w)))

(defn on-mouse
  [game x y e]
  ;; (prn x y e)
  (let [w (:world game)
        action (:mouse-action game)
        [width height] (-> game :camera :size)
        ids (get-cnames-ids w [:worker])
        [x y] (pix->relative [x y])
        [abs-x abs-y] (relative->absolute [x y] (:camera game))]
    ;; (prn abs-x abs-y e action)
    (if (in-camera? x y width height)
      (cond
       (= action :move-to)
       (update-in game [:world] update-entities ids set-c (move-to abs-x abs-y))

       (#{:dig :build-wall} action)
       (update-in game [:world] on-mouse-designate action abs-x abs-y)
       
       :else game)
      game)))

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

;;; RENDERING STUFF

;; TODO: remove magic numbers
(defn text
  "x and y are tiles coordinates"
  [font batch s x y]
  (.draw font batch s
         (float (- (epos2pix x) 4))
         (float (+ (epos2pix y) 7))))

(defn text-wrapped
  "x and y are tiles coordinates"
  [font batch s x y wrap-width]
  (.drawWrapped font batch s
         (float (- (epos2pix x) 4))
         (float (+ (epos2pix y) 7))
         wrap-width))

(defn image
  "x and y are tiles coordinates"
  [batch t color x y]
  (.setColor batch (color colors))
  (.draw batch
         t
         (float (+ (epos2pix (dec x)) 8))
         (float (+ (epos2pix (dec y)) 8))))

(defn draw-ents [batch [vp-x vp-y w h] ents tiles font]
  (doseq [e ents]
    (let [[x y] (coords (e :position))
          r (e :renderable)
          texture (:texture r)
          color (:color r)
          x (- x vp-x)
          y (- y vp-y)]
      (if (and (< 0 (math/round x) w)
               (< 0 (math/round y) h))
        (image batch (texture tiles) color x y)))))

(defn draw-tile [batch cell x y tiles]
  (when (s/visible? cell)
    (do
      (when (s/diggable? cell)
        (image batch (:rock tiles) :white x y))
      (when (s/floor? cell)
        (image batch (:grass tiles) :white x y)))))

(defn entity-info-str [w id]
  (let [e (get-e w id)
        en (sort-by #(nth % 0) (vec e))]
    (apply str (interpose "\n " (into [id] en)))))

(defn draw-info
  "display info for the cell by abs position: x, y"
  [font batch game]
  (let [[x y] (pix->relative (:mouse-pos game))
        [width height] (-> game :camera :size)
        abs (relative->absolute [x y] (:camera game))]
    (when (in-camera? x y width height)
      ;; (prn :draw-info x y abs)
      (let [w (:world game)
            cell (m/place w abs)
            ids (:ids cell)
            entities (map #(entity-info-str w %) ids)]
        (text font batch
              (str (abs 0) " " (abs 1)
                   " " (:form cell)
                   " " (:visible cell)
                   " " (:region cell)
                   " " ids)
              (inc width) height)
        (when (pos? (count entities))
          (text-wrapped font batch
                        (apply str (interpose " " entities))
                        (inc width)
                        (dec height)
                        (:right-panel-width ui)))))))

(defn render-tick [game]
  (let [w (:world game)
        c (:camera game)
        r (:renderer game)
        m (:map w)
        batch (:batch r)
        font (:font r)
        tx (:textures r)
        [cx cy] (:pos c)
        [width height] (:size c)
        text (str (g/game :fps) " " (* width height))]
;;    (prn width height (.maxSpritesInBatch batch) (.totalRenderCalls batch))
    (.begin batch)
    (doseq [x (range width)
            y (range height)]
      (draw-tile batch
                 (s/place m [(+ x cx)
                             (+ y cy)])
                 x y tx))
    (.setColor batch (float 1) (float 1) (float 0) (float 1.0))
    (draw-ents batch
               [cx cy width height]
               (get-cnames-ents w (node :render))
               tx
               font)
    (.setColor batch (float 1) (float 1) (float 1) (float 1.0))
    (.draw font batch text 10 15)
    (.draw font batch (str (:mouse-pos game)) 70 15)
    (.draw font batch (str (-> game :camera :pos)) 190 15)
    (.draw font batch (str (count (-> game :world :etoc))) 240 15)
    (.draw font batch (str (-> game :update-time)) 290 15)
    (.draw font batch (str (-> game :mouse-action)) 350 15)
    (draw-info font batch game)
    (.end batch)))

;;; ticks thread

(defn averager [& args]
  (int (/ (apply + args) (count args))))

(defn updating []
  (loop []
    (let [update-sleep-ms (/ 1000 (float (ui :ups-cap)))
          start (timer)
          new-game (if (:paused @game)
                     game
                     (swap! game update-in [:world] on-tick update-sleep-ms))
          elapsed (timer-end start)]
      (swap! game update-in [:update-time] averager (/ (float 1000) (max update-sleep-ms elapsed)))
      (if (> elapsed update-sleep-ms)
        (prn "timeout:" elapsed)
        (Thread/sleep (- update-sleep-ms elapsed)))
      ;;(Thread/sleep 200)
      (when @running
        (recur))))
  (prn :updating-exited))


(defonce asset-manager (g/asset-manager))
(g/set-asset-manager! asset-manager)

(defn textures [file]
  (let [sheet (texture file)
        tiles (texture! sheet :split 16 16)]
    {:grass (:object (texture (aget tiles 5 2)))
     :rock (:object (texture (aget tiles 1 6)))
     :char (:object (texture (aget tiles 6 0)))
     :dig (:object (texture (aget tiles 6 5)))
     :wall (:object (texture (aget tiles 1 5)))
     :stone (:object (texture (aget tiles 6 6)))}))

(defn add-renderers [game]
  (assoc game :renderer
         {:shape (ShapeRenderer.)
          :batch (SpriteBatch. 5000)
          :font  (BitmapFont.)
          :textures (textures "tiles.png")}))

(defn add-camera [game]
  (assoc game :camera
         {:pos [0 0]
          :size [(tiles (- (g/game :width)
                           (:right-panel-width ui)))
                 (tiles (g/game :height))]}))

(defn unproject [screen]
  [(:input-x screen)
   (- (g/game :height)
      (:input-y screen))])

(g/defscreen main-screen
  :on-show
  (fn [screen _]
    ;;(prn screen)
    ;;(swap! s add-renderers)
    (g/update! screen :renderer (g/stage) :camera (g/orthographic))
    (g/graphics! :set-v-sync false)
    (swap! game update-in [:world] new-world)
    (swap! game add-renderers)
    (swap! game add-camera)
    (.start (Thread. updating))
    nil)
  
  :on-render
  (fn [screen _]
    (g/clear!)
    ;;(render-bench-tick @s)
    (render-tick @game)
    nil)
  
  :on-key-down
  (fn [{:keys [key]} _]
    (swap! game on-key key))

  :on-mouse-moved
  (fn [screen _]
    (swap! game assoc-in [:mouse-pos] (unproject screen)))

  :on-touch-down 
  (fn [screen _]
    (let [[x y] (unproject screen)]
      (swap! game on-mouse x y :down)))

  :on-touch-dragged
  (fn [screen _]
    (let [[x y] (unproject screen)]
      (swap! game on-mouse x y :down))))

(g/defgame cg-game
    :on-create
    (fn [this]
      (g/set-screen! this main-screen)))
