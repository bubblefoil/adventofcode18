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
  [[x y] sn]
  (let [rack-id (+ x 10)
        power (* rack-id (+ sn (* y rack-id)))]
    (- (hundreds power) 5)))

(comment
  (power-level [3 5] 8))                                    ;4

(defn grid
  ([size]
   (grid 0 0 size))
  ([x y size]
   (for [row (range x (+ x size))
         col (range y (+ y size))]
     (vector-of :int row col))))

(defn square-3x3 [x y] (grid x y 3))

(defn power-grid-levels [sn coords]
  "Returns a map of cell coordinates and their power levels."
  (reduce (fn [m c] (assoc m c (power-level c sn))) {} coords))

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
  (let [cells (power-grid-levels sn (grid 1 1 grid-size))]
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
  (power-of-square-area-at [2 2] (power-grid-levels grid-serial-number (grid 1 1 5)))
  (powers grid-serial-number 5)
  ;Part I
  ;Example
  ;=> [[21 61] 30]
  (max-power-square 42 300);
  ;Solution for input 8979
  (time (max-power-square grid-serial-number 300)))
