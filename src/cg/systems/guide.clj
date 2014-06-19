(ns cg.systems.guide
  [:use cg.common]
  [:use cg.ecs]
  [:use cg.comps])

;;; Guide System

(defn guide
  "calculates velocity based on position, next point and speed"
  [e time]
  (let [points (-> e :path :points)
        next-point (peek points)]
    (if (nil? next-point)
      (-> e
          (rem-c :velocity)
          (rem-c :path))
      (let [p (:position e)
            s (-> e :speed :pixsec)
            [vx vy] (project-speed (:x p) (:y p) (next-point 0) (next-point 1) s)]
        (if (zero? vx)
          (-> e
              (update-in [:path :points] pop)
              (set-c (velocity 0 0)))
          (set-c e (velocity vx vy)))))))

(defn system-guide [w time]
  (update-comps w (:guide node) guide time))

