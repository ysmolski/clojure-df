(ns cg.site)

;;; World consists of Cells wrapped into atoms. Each cell has IDs of
;;; entities inside of it and details (wall/floor/passable).

(defrecord Cell [form ids])

(def cell-forms [:floor :wall :door :diggable])

(defn gen-wall [probability]
  (if (> (rand) probability)
    :floor
    :diggable))

(defn place [site [x y]]
  (-> site (nth x) (nth y)))

(defn passable? [cell]
  (= (:form cell) :floor))

(defn diggable? [cell]
  (= (:form cell) :diggable))

(defn form! [site xy form]
  (swap! (place site xy) assoc :form form))

(defn form-if-previous! [site xy form previous-form]
  (swap! (place site xy) #(if (= previous-form (:form %))
                            (assoc % :form form)
                            %)))

(defn dig! [site xy]
  (form-if-previous! site xy :floor :diggable))

(defn add-borders [site size]
  (doseq [x (range size)]
    (form! site [x 0] :wall)
    (form! site [0 x] :wall)
    (form! site [x (dec size)] :wall)
    (form! site [(dec size) x] :wall)))

(def cut-low 2)
(def cut-high 5)

(defn smooth-list [site size]
  (for [x (range 1 (- size 1))
        y (range 1 (- size 1))]
    (let [occupied (reduce + (for [dx (range -1 2)
                                   dy (range -1 2)
                                   :when (not (and (= dx 0) (= dy 0)))
                                   ]
                               (if-not (passable? @(place site [(+ x dx) (+ y dy)])) 1 0)))
          old-form (:form @(place site [x y]))
          new-form (if (= old-form :diggable)
                     (if (< occupied 3)
                       :floor
                       :diggable)
                     (if (> occupied 4)
                       :diggable
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
