(ns cg.rendering
  (:use cg.common)
  (:use cg.ecs)
  (:use cg.comps)
  (:use cg.camera)
  (:import [com.badlogic.gdx.graphics Texture]
           [com.badlogic.gdx.graphics.glutils ShapeRenderer ShapeRenderer$ShapeType]
           [com.badlogic.gdx.graphics.g2d SpriteBatch BitmapFont])

  (:require [clojure.math.numeric-tower :as math]
            [clojure.pprint :as pp]
            [cg.map :as m]
            [cg.site :as s]
            [cg.tasks :as t]
            [play-clj.core :as g]
            [play-clj.g2d :refer :all]
            [play-clj.utils :as gu]))

(def colors {:white (g/color :white)
             :black (g/color :black)
             :yellow (g/color 1 1 0 0.7)
             :selection (g/color 1 1 0 0.8)
             :selection-weak (g/color 1 1 0 0.3)})

(defn tiles
  "how many tiles can fit in size"
  [size]
  (let [tile-size (ui :tile-size)]
    (quot (- size (* tile-size 2))
          tile-size)))

(defn add-camera [game]
  (assoc game :camera
         {:pos [0 0]
          :size [(tiles (- (g/game :width)
                           (:right-panel-width ui)))
                 (tiles (g/game :height))]}))

(defn unproject-input [screen]
  [(:input-x screen)
   (- (g/game :height)
      (:input-y screen))])

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
     :stone (:object (texture (aget tiles 6 6)))
     :wood (:object (texture (aget tiles 5 9)))}))

(defn add-renderers [game]
  (assoc game :renderer
         {:shape (ShapeRenderer.)
          :batch (SpriteBatch. 5000)
          :font  (BitmapFont.)
          :textures (textures "tiles.png")}))

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

(defn square
  "x and y are tiles coordinates"
  [r color x y]
  (.setColor r (color colors))
  (.rect r
         (float (+ (epos2pix (dec x)) 8))
         (float (+ (epos2pix (dec y)) 8))
         (:tile-size ui)
         (:tile-size ui)))

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
      (if (s/storage? cell)
        (image batch (:wood tiles) :white x y)
        (do
          (when (s/diggable? cell)
            (image batch (:rock tiles) :white x y))
          (when (s/floor? cell)
            (image batch (:grass tiles) :white x y)))))))

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
    (when (in-bound? x y width height)
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
                   " " (:storage cell)
                   " " ids)
              (inc width) height)
        (when (s/storage? cell)
          (text font batch
                (str "storage #" (:storage cell)
                     " " (:cells (:store (get-e w (:storage cell)))))
                (inc width) (dec height)))        
        (when (pos? (count entities))
          (text-wrapped font batch
                        (apply str (interpose " " entities))
                        (inc width)
                        (- height 2)
                        (:right-panel-width ui)))))))

(defn get-task-filter
  "returns function predicate which check if cell with relative coordinates is taskable"
  [world action camera]
  (fn [x y]
    (let [[absx absy] (relative->absolute [x y] camera)]
      (t/cell-taskable? world action [absx absy]))))

(defn render-selected-tiles
  [shape pred [x1 y1] [x2 y2] camera]
  (let [[w h] (:size camera)
        [[x1 y1] [x2 y2]] (bound-rect [x1 y1] [x2 y2] (dec w) (dec h))]
    (doseq [x (range x1 (inc x2))
            y (range y1 (inc y2))]
      (when (pred x y)
        (doto shape
          (square :selection x y))))))

(defn render-tick [game]
  (let [w (:world game)
        c (:camera game)
        r (:renderer game)
        m (:map w)
        batch (:batch r)
        font (:font r)
        shape (:shape r)
        tx (:textures r)
        action (:action game)
        taskable? (get-task-filter w action c)
        [cx cy] (:pos c)
        [width height] (:size c)
        text (str (g/game :fps) " " (* width height))]
    ;; (prn width height (.maxSpritesInBatch batch) (.totalRenderCalls batch))
    ;; (prn :render-tick (:frame game) (timer-end (:frame-time game)))
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
    (.draw font batch (str (-> game :action)) 350 15)
    (.draw font batch (str (-> game :first-click)) 410 15)
    (.draw font batch (str (-> game :mouse-abs)) 470 15)
    (draw-info font batch game)
    (.end batch)
    
    (if-let [xy1 (:first-click game)]
      (let [xy2 (:mouse-abs game)
            [xy1 xy2] (abs->rel xy1 xy2 c)
            [[relx1 rely1] [relx2 rely2]] (norm-rect xy1 xy2)]
        ;; (prn xy1 xy2 [relx1 rely1] [relx2 rely2])
        (g/gl! :gl-enable (g/gl :gl-blend))
        (g/gl! :gl-blend-func (g/gl :gl-src-alpha) (g/gl :gl-one-minus-src-alpha))
        (doto shape
          (.begin ShapeRenderer$ShapeType/Line)
          ;; (.setColor (:black colors))
          ;; (.rect (inc x1) (dec y1) w h)
          ;; (.setColor (:selection-weak colors))
          ;; (.rect x1 y1 w h)
          (render-selected-tiles taskable? [relx1 rely1] [relx2 rely2] c)
          (.end))
        (g/gl! :gl-disable (g/gl :gl-blend))))))

