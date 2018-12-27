(ns adventofcode18.aoc11)

(def grid-serial-number 8979)

(defn hundreds [x]
  (-> x
      (mod 1000)
      (unchecked-divide-int 100)))

(comment
  (hundreds 1203)
  (hundreds 341663))

(defn power-level
  [sn [x y]]
  (let [rack-id (+ x 10)
        power (* rack-id (+ sn (* y rack-id)))]
    (- (hundreds power) 5)))

(comment
  (power-level 8 [3 5]))                                    ;4

(defn grid
  ([size]
   (grid 1 1 size))
  ([x y size]
   (for [row (range x (+ x size))
         col (range y (+ y size))]
     (vector row col))))

(defn square-3x3 [x y] (grid x y 3))

(defn power-grid-levels [sn coords]
  "Returns a map of cell coordinates and their power levels."
  (reduce (fn [m c] (assoc m c (power-level sn c))) {} coords))

(defn powers-at-square
  "Powers at the 3x3 square starting at given coordinates."
  [[x y] grid]
  (map grid (square-3x3 x y)))

(defn power-of-square-area-at
  "Returns sum of power levels of cells in a 3x3 square, starting at coords."
  [coords grid]
  (let [powers (take-while int? (powers-at-square coords grid))]
    (if (< (count powers) 9)
      -99
      (reduce + powers))))

(defn powers
  [sn grid-size]
  (let [cells (power-grid-levels sn (grid grid-size))]
    (->> cells
         (keys)
         ;pmap: < 400 ms, map: > 800 ms
         (pmap (fn [coords] [coords (power-of-square-area-at coords cells)]))
         (into {}))))

(defn max-power-square
  "Part I solution"
  [sn grid-size]
  (let [square-powers (powers sn grid-size)]
    (apply max-key val square-powers)))

(comment
  (grid 1 5 5)
  (power-grid-levels grid-serial-number (grid 3))
  (square-3x3 2 2)
  (power-of-square-area [2 2] (power-grid-levels grid-serial-number (grid 5)))
  (powers grid-serial-number 5)
  ;Part I
  ;Example
  ;=> [[21 61] 30]
  (max-power-square 42 300);
  ;Solution for input 8979
  (time (max-power-square grid-serial-number 300)))

;Part II
(defn fits-grid?
  "Returns true if a square of square-size with upper-left corner at x,y fits in a square of grid-size.
   Coordinates are counted from 1."
  [square-size grid-size x y]
  (<= square-size (inc (Math/min (unchecked-subtract-int grid-size x) (unchecked-subtract-int grid-size y)))))

(defn square-grid-with-fitting-square-sizes [size]
  (for [[x y] (grid size)
        square-size (range 1 (inc size))
        :while (fits-grid? square-size size x y)]
    (vector-of :int x y square-size)))

(comment
  (fits-grid? 2 2 1 1)
  (square-grid-with-fitting-square-sizes 3))

(defn power-with-sn [sn]
  (partial power-level sn))

(defn- down [[x y]] [x (inc y)])
(defn- right [[x y]] [(inc x) y])
(defn- get-or-0 [table cell] (get table cell 0))

(defn sum-area-table
  [size pwr-fn]
  (loop [cells (reverse (grid size))
         table {}]
    (if-let [cell (first cells)]
      (recur
        (rest cells)
        (assoc table cell
                     (+
                       (get-or-0 table (down cell))
                       (get-or-0 table (right cell))
                       (- (get-or-0 table (down (right cell))))
                       (pwr-fn cell))))
      table)))

(defn- range-from-1 [n] (range 1 (inc n)))
(defn- upper-diagonal [x]
  (for [xy (range-from-1 x)]
    (vector-of :int xy (- (inc x) xy))))
(defn- lower-diagonal [d n]
  (map (fn [[x y]] [(- (inc n) x) (- (inc n) y)]) (upper-diagonal d)))

(defn- diagonals [size]
  (concat (for [l (range 1 size)] (lower-diagonal l size))
          (for [d (reverse (range-from-1 size))] (upper-diagonal d))))

(defn sum-area-table-array
  "Experimental WIP version using an array just to see the perf. impact of a map.
  So far the serial version does nothing while pmap fills the array with random stuff."
  [size pwr-fn]
  (let [table (make-array Integer/TYPE (inc size) (inc size))
        get-or-0 (fn [x y] (if (some #(> % size) [x y]) 0 (aget table x y)))
        diagonals (diagonals size)]
    (doall
      (for [diagonal diagonals]
        (map
          (fn [[x y]]
            (aset-int table x y
                      (+ (pwr-fn [x y])
                         (get-or-0 x (inc y))
                         (get-or-0 (inc x) y)
                         (- (get-or-0 (inc x) (inc y))))))
          diagonal)))
    table))

(defn area [x y s table]
  (let [a [x y]
        ar [(+ s x) y]
        ad [x (+ s y)]
        ard [(+ s x) (+ s y)]]
    (+ (table a)
       (table ard 0)
       (- (table ar 0))
       (- (table ad 0)))))

(defn format-result [[[x y size] _]]
  (str x \, y \, size))

(defn max-power-area
  "Part II"
  [size]
  (let [table (sum-area-table size (power-with-sn grid-serial-number))
        grid-3d (square-grid-with-fitting-square-sizes size)]
    (->> grid-3d
         (pmap (fn [[x y s]] [[x y s] (area x y s table)]))
         (apply max-key second)
         (format-result))))

(comment
  (upper-diagonal 3)

  (sum-area-table 8 (power-with-sn grid-serial-number))
  (sum-area-table-array 8 (constantly 1))
  (reduce + (vals (sum-area-table 8 (power-with-sn grid-serial-number))))
  (pp/pp)

  (square-grid-with-fitting-square-sizes 3)
  (power-grid-levels grid-serial-number (grid 3))
  ;Solution
  (time (max-power-area 6))
  ;"Elapsed time: 26042.721391 msecs" //This is with a sorted map. The default implementation cuts the time by half.
  ;=> [[235 118 14] 166]
  (time (max-power-area 300)))


