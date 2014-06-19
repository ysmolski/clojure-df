(in-ns 'cg.core)

(defn on-start [w]
  (-> w
      (new-spawn)))

(defn bound-viewport
  [[x y] [dx dy]]
  (let [w (vp-width)
        h (vp-height)]
    [(bound (- map-size w) (+ x dx))
     (bound (- map-size h) (+ y dy))]))

(def key-to-scroll {\w [0 -1]
                    \s [0 1]
                    \a [-1 0]
                    \d [1 0]})

(defn on-key
  "Handles key presses. Returns new state of the world"
  [w key]
  ;;(set-val w 0 :health :count key)
  (condp = key
    \space (swap! (game :paused) not)
    \f (reset! (game :mouse-action) :dig)
    \g (reset! (game :mouse-action) :move-to)
    \b (reset! (game :mouse-action) :build-wall)
    (let [delta (map #(* % (ui :scroll-amount))
                     (key-to-scroll key [0 0]))]
      (swap! (game :viewport) bound-viewport delta)
      ;;(prn delta @(game :viewport))
      ))
  w)

(defmulti on-mouse-designate (fn [_ action _ _] action))

(defmethod on-mouse-designate :dig [w action x y]
  (let [c (place w [x y])]
    (if (or (not (s/visible? c))
            (s/diggable? c))
      (u/add-job w :dig x y "X")
      w)))

(defmethod on-mouse-designate :build-wall [w action x y]
  (if (s/passable? (place w [x y]))
    (u/add-job w :build-wall x y "□")
    w))

(defn on-mouse
  [w x y e]
  ;; (prn x y e)
  (let [ids (get-cnames-ids w [:controllable])
        [x y] (pix->relative [x y])
        [abs-x abs-y] (relative->absolute [x y])
        action @(game :mouse-action)]
    (prn abs-x abs-y e action)
    (if (in-viewport? x y)
      (cond
       (= action :move-to) (update-entities w ids set-c (destination abs-x abs-y))
       (#{:dig :build-wall} action) (on-mouse-designate w action abs-x abs-y)
       :else w))))

(defn on-tick
  "Handles ticks of the world, delta is the time passes since last tick"
  [w time]
  (-> w
      (system-move time)
      (system-guide time)
      (system-path-find time)
      (system-assign-jobs time)
      (system-dig time)
      ))
