(ns cg.site)

;;; World consists of Cells wrapped into atoms. Each cell has IDs of
;;; entities inside of it and details (wall/floor/passable).

(defrecord Cell [form ids])

(def cell-forms [:floor :wall :door])

(defn gen-wall [probability]
  (if (> (rand) probability)
    :floor
    :wall))

(defn place [site [x y]]
  (-> site (nth x) (nth y)))

(defn passable? [cell]
  (not= (:form cell) :wall))

(defn form! [site xy form]
  (swap! (place site xy) assoc :form form))

(defn add-borders [site size]
  (doseq [x (range size)]
    (form! site [x 0] :wall)
    (form! site [0 x] :wall)
    (form! site [x (dec size)] :wall)
    (form! site [x (dec size)] :wall)))

(def cut-low 2)
(def cut-high 5)

(defn smooth-list [site size]
  (for [x (range 1 (- size 2))
        y (range 1 (- size 2))]
    (let [occupied (reduce + (for [dx (range -1 2)
                                   dy (range -1 2)
                                   :when (not (and (= dx 0) (= dy 0)))
                                   ]
                               (if-not (passable? @(place site [(+ x dx) (+ y dy)])) 1 0)))
          old-form (:form @(place site [x y]))
          new-form (if (= old-form :wall)
                     (if (< occupied 3)
                       :floor
                       :wall)
                     (if (> occupied 4)
                       :wall
                       :floor))]
      [x y new-form])))

(defn smooth [times site size]
  (dotimes [n times]
    (doseq [[x y form] (smooth-list site size)]
      (form! site [x y] form))))

(defn generate [size wall-probability]
  (let [site (apply vector (map (fn [_] (apply vector (map (fn [_] (atom (Cell. (gen-wall wall-probability) {})))
                                                          (range size))))
                                (range size)))]
    (do
      (add-borders site size)
      (smooth 2 site size)
      site)))
