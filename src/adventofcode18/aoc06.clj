(ns adventofcode18.aoc06
  (:require [clojure.java.io :as io]
            [adventofcode18.file :as f]
            [adventofcode18.util :as u]))

(defn parse-point [s]
  (map (comp f/to-int) (re-seq #"\d+" s)))

(defn read-points []
  (map parse-point (f/lines-of (io/resource "./aoc06.txt"))))

(defn left-most [points]
  (reduce min (map first points)))
(defn top-most [points]
  (reduce min (map second points)))
(defn right-most [points]
  (reduce max (map first points)))
(defn down-most [points]
  (reduce max (map second points)))

(def bbox
  (memoize
    (fn
      [points]
      [(left-most points)
       (top-most points)
       (right-most points)
       (down-most points)])))

(defn rectangle
  "Returns list of all coordinates in a rectangle."
  [[x1 y1 x2 y2]]
  (for [x (range x1 (inc x2))
        y (range y1 (inc y2))]
    [x y]))

(defn manhattan [[x1 y1] [x2 y2]]
  (+
    (Math/abs ^int (- x1 x2))
    (Math/abs ^int (- y1 y2))))

(defn closest
  "Find all the closest points to p."
  [p points]
  (->> points
       ;map to [dist point]
       (pmap #(vector (manhattan % p) %))
       (sort-by first)
       ;slice to sequences of points with the same distance
       (partition-by first)
       ;take the partition of the closest points
       (first)
       ;just take the points
       (map second)))

(defn one-or-none
  "Returns the only element in collection, or nil if there are more."
  [coll]
  (if (empty? (rest coll))
    (first coll)
    nil))

(defn fill-grid
  "Returns points mapped to grid coordinates by the lowest Manhattan distance.
  {[x y]->[px py]}"
  [points]
  (let [canvas (rectangle (bbox points))]
    (reduce
      (fn [acc p] (assoc acc p (one-or-none (closest p points))))
      {}
      canvas)))

(defn at-perimeter?
  [[px py] points]
  (let [[l t r d] (bbox points)]
    (or
      (= px l)
      (= px r)
      (= py t)
      (= py d))))

(def perimeter
  "Returns a set of the points whose areas touch the perimeter of the grid."
  (memoize
    (fn
      [grid]
      (let [coordinates (keys grid)]
        (->> grid
             (filter #(at-perimeter? (key %) coordinates))
             (map second)
             (set))))))

(defn infinite-area?
  "Returns true if the area touches the perimeter of the BBox."
  [p grid]
  (let [perimeter (perimeter grid)]
    (some #(= % p) perimeter)))

(defn largest-area
  "Find the largest area closest to a single point."
  [points]
  (let [grid (fill-grid points)
        finite? (complement (fn [[point _]] (infinite-area? point grid)))]
    (->> grid
         ;just the points
         (vals)
         ;remove equidistant
         (keep identity)
         ;sum up the areas
         (frequencies)
         ;ignore infinite areas
         (filter finite?)
         ;the largest area
         (u/max-by second)
         (second))))

(defn plot
  "Transform map of points to a 2d array."
  [grid]
  (->> grid
       (sort)
       (partition-by (fn [[[x _] _]] x))
       (map (fn [row]
              (map (fn [point] (second point)) row)))))

(comment
  (def e1 ["1, 1"
           "1, 6"
           "8, 3"
           "3, 4"
           "5, 5"
           "8, 9"])
  (def points (parse e1))
  ;(def points (read-points))
  (bbox points)
  (count (perimeter (keep identity (vals (fill-grid points)))))
  (rectangle (bbox points))
  (manhattan [1 4] [2 0])
  (closest [1 4] points)
  (clojure.pprint/pprint (plot (fill-grid points)))
  (largest-area points)
  (time (fill-grid (read-points)))
  ;Part I
  (largest-area (read-points)))                             ;3722
;Part II
