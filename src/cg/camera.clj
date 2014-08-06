(ns cg.camera
  (:require [cg.common :refer :all]))

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
    [(+ x vp-x)
     (+ y vp-y)]))

(defn norm-rect
  [[x1 y1] [x2 y2]]
  (let [ax1 (min x1 x2)
        ax2 (max x1 x2)
        ay1 (min y1 y2)
        ay2 (max y1 y2)]
    [[ax1 ay1]
     [ax2 ay2]]))

(defn abs->rel
  ([[x y] camera]
     (let [[vp-x vp-y] (:pos camera)]
       [(- x vp-x)
        (- y vp-y)]))
  ([xy1 xy2 camera]
     (let [xy1 (abs->rel xy1 camera)
           xy2 (abs->rel xy2 camera)]
       [xy1 xy2])))

;;(def pix->absolute (comp relative->absolute pix->relative))
