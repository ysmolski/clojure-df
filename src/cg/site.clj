(ns cg.site)

;;; World consists of Cells. Each cell has IDs of
;;; entities inside of it and details (wall/floor/passable).

(defrecord Cell [form ids])

(def cell-forms [:floor :wall :door :diggable])

(defn place [m [x y]]
  (-> m (nth x) (nth y)))

(defn passable? [cell]
  (= (:form cell) :floor))

(defn diggable? [cell]
  (= (:form cell) :diggable))

(defn form [m [x y] form]
  (assoc-in m [x y :form] form))

(defn form-if-previous [m [x y] form previous-form]
  (update-in m [x y] #(if (= previous-form (:form %))
                        (assoc % :form form)
                        %)))

(defn dig [m [x y]]
  (form-if-previous m [x y] :floor :diggable))

;; generation of map

(def wall-probability 0.4)

(defn new-cell []
  (Cell. (if (> (rand) wall-probability)
           :floor
           :diggable)
         {}))

(defn add-borders [m]
  (let [size (count m)]
    (reduce #(-> %1
                 (form [%2 0] :wall)
                 (form [0 %2] :wall)
                 (form [%2 (dec size)] :wall)
                 (form [(dec size) %2] :wall))
            m
            (range size))))

(def cut-low 3)
(def cut-high 4)

(defn smooth-list [site size]
  (for [x (range 1 (dec size))
        y (range 1 (dec size))]
    (let [occupied (reduce + (for [dx (range -1 2)
                                   dy (range -1 2)
                                   :when (not (and (zero? dx) (zero? dy)))
                                   ]
                               (if-not (passable? (place site [(+ x dx) (+ y dy)]))
                                 1 0)))
          old-form (:form (place site [x y]))
          new-form (if (= old-form :diggable)
                     (if (< occupied cut-low)
                       :floor
                       :diggable)
                     (if (> occupied cut-high)
                       :diggable
                       :floor))]
      [x y new-form])))

(defn smooth [m]
  (let [size (count m)]
    (reduce (fn [m [x y new-form]] (form m [x y] new-form))
            m
            (smooth-list m size))))

(defn smooth-times [site times]
  (nth (iterate smooth site)
       times))

(defn generate [size cell-fn]
  (let [m (apply vector (map (fn [_] (apply vector (map (fn [_] (cell-fn))
                                                       (range size))))
                             (range size)))]
    (-> m
        (add-borders)
        (smooth-times 2))))

