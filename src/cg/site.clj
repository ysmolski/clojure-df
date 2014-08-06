(ns cg.site
  (:require [cg.astar :as a]
            [jordanlewis.data.union-find :as u]))

;; tools
(defn mapv-2d [f vecofvec]
  (mapv #(mapv f %) vecofvec))


;;; World consists of Cells. Each cell has IDs of
;;; entities inside of it and details (wall/floor/passable).

(defrecord Cell [form ids region visible])

(def cell-forms [:floor :wall :door :diggable])

(defn place [m [x y]]
  (-> m (nth x) (nth y)))

(defn floor? [cell]
  (= (:form cell) :floor))

(defn passable? [cell]
  (floor? cell))

(defn diggable? [cell]
  (= (:form cell) :diggable))

(defn connected? [m xy1 xy2]
  (let [c1 (place m xy1)
        c2 (place m xy2)]
    (and (passable? c1)
         (= (:region c1)
            (:region c2)))))

(defn visible? [cell]
  (:visible cell))

(defn form [m [x y] val]
  (assoc-in m [x y :form] val))

(defn region
  ([m [x y] val]
     (assoc-in m [x y :region] val))
  ([m [x y]]
     (get-in m [x y :region])))

(defn visible
  ([m [x y] val]
     (if (visible m [x y])
       m
       (assoc-in m [x y :visible] val)))
  ([m [x y]]
     (get-in m [x y :visible])))

(defn storage
  ([m [x y] val]
     (assoc-in m [x y :storage] val))
  ([m [x y]]
     (get-in m [x y :storage])))


(defn storage? [cell]
  (:storage cell))

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
         #{}
         nil
         false))

(defn add-borders [m]
  (let [size (count m)
        b :diggable]
    (reduce #(-> %1
                 (form [%2 0] b)
                 (form [0 %2] b)
                 (form [%2 (dec size)] b)
                 (form [(dec size) %2] b))
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

(defn rc-new []
  {})

(defn rc-add
  [rc r xy]
  (if (contains? rc r)
    (update-in rc [r] conj xy)
    (assoc rc r #{xy})))

(defn rc-remove
  [rc r xy]
  (update-in rc [r] disj xy))

(defn rc-cells [rc r]
  (rc r))

(defn rc-regions [rc]
  (keys rc))

(defn rc-area [rc r]
  (count (rc-cells rc r)))

(defn rc-sort-by-area
  "Returns list of regions sorted by the area they occupy in comp order"
  ([comp rc]
     (rc-sort-by-area comp rc (rc-regions rc)))
  ([comp rc regions]
     (sort-by #(rc-area rc %) comp regions)))

(def rc-biggest-area (partial rc-sort-by-area >))
(def rc-smallest-area (partial rc-sort-by-area <))

(defn rc-move
  [rc old-r new-r]
  (let [old (rc old-r)]
    (prn :rc-move old-r old :to new-r)
    (-> rc
        (dissoc old-r)
        (update-in [new-r] clojure.set/union old))))

(defn- scan-regions
  "performs first scan of passables and assigns regions using union-find
  http://en.wikipedia.org/wiki/Connected-component_labeling#Two-pass"
  [m]
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
            (let [labels (nbrs-regions m size xy uf-scan-dirs)
                  passables (conj passables xy)]
              ;;(prn xy labels uf)
              (if (empty? labels)
                (recur (region m xy last-region) more (conj uf last-region) (inc last-region) passables)
                (let [min-label (apply min labels)]
                  (recur (region m xy min-label) more (union-all uf labels) last-region passables))))
            (recur m more uf last-region passables)))
        [m uf passables]))))

(defn- init-regions
  "Returns [map regions->cells] with cells being marked with same numbers
  if they belong to the same connected region"
  [m]
  (let [[m uf passables] (scan-regions m)
        rc (rc-new)
        u-fn (fn [[m rc] xy]
               (let [r (uf (region m xy))]
                 [(region m xy r) (rc-add rc r xy)]))]
    (reduce u-fn [m rc] passables)))

(defn add-visible [m size xy]
  (let [nbrs (a/neighbors a/all-dirs size xy)]
    (-> (reduce #(visible %1 %2 true) m nbrs)
        (visible xy true))))

;; FIX: generate expanded area by 1 tile efficiently 
(defn add-visibles
  [m cells]
  (let [size (count m)]
    (reduce #(add-visible %1 size %2) m cells)))

(defn generate
  "returns [map rc]"
  [size cell-fn]
  (let [m (vec-2d size cell-fn)]
    (-> m
        (add-borders)
        (smooth-times 2)
        (init-regions))))

