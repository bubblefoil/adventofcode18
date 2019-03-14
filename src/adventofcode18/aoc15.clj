(ns adventofcode18.aoc15
  (:require [adventofcode18.file :as f]
            [adventofcode18.util :as u]))

(defn read-tile
  [c]
  (case c
    \. {:tile :empty}
    \# {:tile :wall}
    \E {:tile :elf :hp 200 :round 0}
    \G {:tile :goblin :hp (rand-int 5) :round (rand-int 5)}))

(defn read-dungeon-map
  [res]
  (->> res
       f/lines-of-resource
       (f/map-chars-indexed
         (fn [c li ci]
           [[ci li] (read-tile c)]))
       (into {})))

(defn sort-by-round
  [units]
  (sort-by (juxt (comp :round second) first) units))

(defn alive-unit?
  [tile]
  (-> tile
      second
      :hp
      (some-> % pos-int?)))

(defn units
  [dungeon]
  (->> dungeon
       (filter alive-unit?)
       (sort-by-round)))

(defn enemy-pred
  "Creates a predicate that checks whether another unit is an enemy of given unit."
  [unit]
  (comp (complement #{unit}) :tile second))

(defn enemies
  [[_ {unit :tile}] dungeon]
  (->> dungeon
       units
       (filter (enemy-pred unit))
       (sort-by first)))

(defn adjacent
  [[[x y] _]]
  (list [(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)]))

(defn neighbors
  [unit dungeon]
  (select-keys (adjacent unit) dungeon))


(defn next-char
  [dungeon]
  (->> dungeon
       units
       first))

(comment
  (read-dungeon-map "aoc15ex.txt")
  (next-char (read-dungeon-map "aoc15ex.txt")))