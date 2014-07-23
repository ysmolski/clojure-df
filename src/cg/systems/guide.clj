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
    ;; (prn :guide points)
    (if (nil? next-point)
      (-> e
          (rem-c :velocity)
          (rem-c :path)
          (set-c (done-job)))
      (let [p (:position e)
            s (-> e :speed :pixsec)
            [vx vy] (project-speed (:x p) (:y p) (next-point 0) (next-point 1) s)]
        ;; (prn :guide p s [vx vy])
        (if (and (zero? vx)
                 (zero? vy))
          (-> e
              (update-in [:path :points] pop)
              (set-c (velocity 0 0)))
          (set-c e (velocity vx vy)))))))

(defn system-guide [w time]
  (update-entities-by-cnames w (:guide node) guide time))

