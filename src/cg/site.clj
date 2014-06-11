(ns cg.site
  (:use [cg.astar :only [neighbors]])
  (:require [jordanlewis.data.union-find :as u]))

;; tools
(defn mapv-2d [f vecofvec]
  (mapv #(mapv f %) vecofvec))


;;; World consists of Cells. Each cell has IDs of
;;; entities inside of it and details (wall/floor/passable).

(defrecord Cell [form ids region])

(def cell-forms [:floor :wall :door :diggable])

(defn place [m [x y]]
  (-> m (nth x) (nth y)))

(defn passable? [cell]
  (= (:form cell) :floor))

(defn diggable? [cell]
  (= (:form cell) :diggable))

(defn form [m [x y] form]
  (assoc-in m [x y :form] form))

(defn region [m [x y] region]
  (assoc-in m [x y :region] region))

(defn form-if-previous [m [x y] form previous-form]
  (update-in m [x y] #(if (= previous-form (:form %))
                        (assoc % :form form)
                        %)))

(defn dig [m [x y]]
  (form-if-previous m [x y] :floor :diggable))

(defn random-place
  "returns random coords on a map for which (pred cell) is true"
  [m pred max-x max-y]
  (loop [x (rand-int max-x)
         y (rand-int max-y)]
    (if (pred (place m [x y]))
      [x y]
      (recur (rand-int max-x) (rand-int max-y)))))

;; generation of map

(def wall-probability 0.4)

(defn new-cell []
  (Cell. (if (> (rand) wall-probability)
           :floor
           :diggable)
         {}
         nil))

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

(defn smooth
  "sets new forms from vec [x y form] into map m"
  [m]
  (let [size (count m)]
    (reduce (fn [m [x y new-form]] (form m [x y] new-form))
            m
            (smooth-list m size))))

(defn smooth-times [m times]
  (nth (iterate smooth m)
       times))

(defn vec-2d [size cell-fn]
  (apply vector (map (fn [_] (apply vector (map (fn [_] (cell-fn))
                                               (range size))))
                     (range size))))

(defn generate [size cell-fn]
  (let [m (vec-2d size cell-fn)]
    (-> m
        (add-borders)
        (smooth-times 2))))

(def conn-dirs [[-1  0]
                [-1 -1]
                [ 0 -1]
                [ 1 -1]])

(defn union-regions
  [uf regions]
  (reduce (partial apply u/union)
          uf
          (partition 2 1 regions)))

(defn nbrs-labels [m size xy]
  (let [nbrs (neighbors conn-dirs size xy)]
    (distinct (keep #(:region (get-in m %)) nbrs))))

(defn range-2d
  "generates vector of vectors [x y] where x and y go through mentioned range"
  ([start end]
      (for [x (range start end)
            y (range start end)]
        [x y]))
  ([end]
     (for [x (range end)
           y (range end)]
        [x y])))

(defn add-connectivity-first [m]
  (let [size (count m)]
    (loop [m m
           cells (range-2d 1 (dec size))
           uf (u/union-find)
           last-region 0
           passables []]
      (if-let [xy (first cells)] 
        (let [cell (place m xy)
              more (rest cells)]
          (if (passable? cell)
            (let [labels (nbrs-labels m size xy)
                  passables (conj passables xy)]
              (prn xy labels uf)
              (if (empty? labels)
                (recur (region m xy last-region) more (conj uf last-region) (inc last-region) passables)
                (let [min-label (apply min labels)]
                  (recur (region m xy min-label) more (union-regions uf labels) last-region passables))))
            (recur m more uf last-region passables)))
        m))))

