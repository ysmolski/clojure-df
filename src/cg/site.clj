(ns cg.site
  (:require [cg.astar :as a])
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

(defn connected? [m xy1 xy2]
  (let [c1 (place m xy1)
        c2 (place m xy2)]
    (and (passable? c1)
         (= (:region c1)
            (:region c2)))))

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

(defn vec-2d [size cell-fn]
  (apply vector (map (fn [_] (apply vector (map (fn [_] (cell-fn))
                                               (range size))))
                     (range size))))

(def uf-scan-dirs [[-1  0]
                   [-1 -1]
                   [ 0 -1]
                   [ 1 -1]])

(defn nbrs-regions
  ([m size xy]
     (nbrs-regions m size xy a/all-dirs))
  ([m size xy dirs]
     (let [nbrs (a/neighbors dirs size xy)]
       (distinct (keep #(:region (get-in m %)) nbrs)))))

(defn range-2d
  "Generates vector of vectors [x y] where x and y go through mentioned range
  Iterate through each element of the data by column, then by row (Raster Scanning)"
  ([start end]
      (for [x (range start end)
            y (range start end)]
        [y x]))
  ([end]
     (for [x (range end)
           y (range end)]
        [y x])))

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

;;; CONNECTED REGIONS

(defn union-all
  [uf regions]
  (reduce (partial apply u/union)
          uf
          (partition 2 1 regions)))

(defn rc-add
  [rc r xy]
  (if (contains? rc r)
    (assoc-in rc [r xy] 1)
    (assoc rc r {xy 1})))

(defn rc-cells [rc r]
  (keys (rc r)))

(defn rc-regions [rc]
  (keys rc))

(defn rc-biggest
  "Returns list of regions sorted by the area they occupy in descending order"
  [rc regions]
  (map #(nth % 0)
       (sort-by #(nth % 1)
                >
                (map (fn [r]
                       [r (count (rc-cells rc r))])
                     regions))))

(defn rc-move
  [rc old-r new-r]
  (let [old (rc old-r)]
    (prn :rc-move old-r (keys old) :to new-r)
    (-> rc
        (dissoc old-r)
        (update-in [new-r] merge old))))

(defn- scan-regions
  "performs first scan of passables and assigns regions using union-find
  http://en.wikipedia.org/wiki/Connected-component_labeling#Two-pass"
  [m]
  (let [size (count m)]
    (loop [m m
           cells (range-2d 1 (dec size))
           uf (u/union-find)
           last-region 0
           passables '()]
      (if-let [xy (first cells)] 
        (let [cell (place m xy)
              more (rest cells)]
          (if (passable? cell)
            (let [labels (nbrs-regions m size xy uf-scan-dirs)
                  passables (conj passables xy)]
              ;;(prn xy labels uf)
              (if (empty? labels)
                (recur (region m xy last-region) more (conj uf last-region) (inc last-region) passables)
                (let [min-label (apply min labels)]
                  (recur (region m xy min-label) more (union-all uf labels) last-region passables))))
            (recur m more uf last-region passables)))
        [m uf passables]))))

(defn add-regions
  "returns map with cells being marked with same numbers
  if they belong to the same connected region"
  [m]
  (let [[m uf passables] (scan-regions m)]
    (prn uf (count passables))
    (loop [m m
           cells passables
           rc {}]
      (if-let [xy (first cells)]
        (let [r (uf (:region (place m xy)))]
          ;;(prn xy r (if (< (count (rc r)) 8) (rc r) []))
          (recur (region m xy r) (rest cells) (rc-add rc r xy)))
        [m rc]))))

(defn generate [size cell-fn]
  (let [m (vec-2d size cell-fn)]
    (-> m
        (add-borders)
        (smooth-times 2)
        (add-regions))))

