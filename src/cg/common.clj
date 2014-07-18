(ns cg.common
  (:require [clojure.math.numeric-tower :as math]))

;; constants

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
   :ups-cap 100
   })

;; utils

(defn timer [] (System/nanoTime))

(defn timer-end [start]
  (/ (double (- (System/nanoTime)
                start))
     1000000.0))

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

(defn floor
  "drops fractional part"
  [position]
  (math/floor position))

(defn bound
  "returns n bound to limit [0-b]"
  [b n]
  (if (neg? n)
    0
    (if (>= n b)
      b
      n)))

(defn bound-rect
  [[x1 y1] [x2 y2] w h]
  [[(bound w x1) (bound h y1)]
   [(bound w x2) (bound h y2)]])

(defn in-bound?
  "returns true if 0 <= x < width and 0 <= y < height"
  [x y width height]
  (and (>= x 0)
       (>= y 0)
       (< x width)
       (< y height)))

(defn contacting? [[x1 y1] [x2 y2]]
  (and (>= 1 (math/abs (- x1 x2)))
       (>= 1 (math/abs (- y1 y2)))))

(defn apply-times
  "N time applies w&args to f and then result is applied as w"
  [w n f & args]
  (reduce (fn [w _] (apply f w args)) w (range n)))
