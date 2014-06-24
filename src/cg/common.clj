(ns cg.common)

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

(defn floor
  "drops fractional part"
  [position]
  (quot position 1))

(defn bound
  "returns n bound to limit [0-b]"
  [b n]
  (if (neg? n)
    0
    (if (>= n b)
      b
      n)))

(defn contacting? [[x1 y1] [x2 y2]]
  (and (>= 1 (Math/abs (- x1 x2)))
       (>= 1 (Math/abs (- y1 y2)))))

(defn apply-times
  "N time applies w&args to f and then result is applied as w"
  [w n f & args]
  (reduce (fn [w _] (apply f w args)) w (range n)))
