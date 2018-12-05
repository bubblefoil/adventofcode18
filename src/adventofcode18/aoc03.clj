(ns adventofcode18.aoc03
  (:require [clojure.java.io :as io]
            [clojure.set :as s]
            [adventofcode18.file :as f]))

(defn read-ids [res-name]
  (f/lines-of (io/resource res-name)))

(def claims (read-ids "./aoc03.txt"))

(def claim-pattern (re-pattern "#(\\d+)\\s@\\s(\\d+),(\\d+):\\s(\\d+)x(\\d+)"))

(defn parse-rectangle
  "Claim to rectangle"
  [claim]
  (let [group-coords (fn [[id & coords]] (vector id coords))]
    (->> (re-find claim-pattern claim)
         (rest)
         (map f/to-int)
         (group-coords))))

(defn inches-in-rectangle
  "Returns all square inches within a rectangle."
  [[id [left top width height]]]
  (let [horizontal (range left (+ left width))
        vertical (range top (+ top height))]
    (->> vertical
         (mapcat
           (fn [col]
             (map
               (fn [row] (vector id [row col]))
               horizontal))))))

(defn parse-claims
  "Reads claims into a map of coordinates and lists of claim ids."
  [claims]
  (->> claims
       (map parse-rectangle)
       (mapcat inches-in-rectangle)
       ;the key is the [coords] in [id [coords]]
       (group-by second)
       ;update all vals to contain just ids without coords.
       (reduce-kv
         (fn [m k v] (assoc m k (map first v)))
         {})))

(defn more-than-one
  "Returns true if coll contains more than 1 elements."
  [coll]
  (> (count coll) 1))

(defn count-overlapping-claims
  "Part I"
  [claims]
  (->> claims
       (parse-claims)
       (vals)
       (filter more-than-one)
       (count)))

(comment
  (def example1 ["#1 @ 1,3: 4x4"
                 "#2 @ 3,1: 4x4"
                 "#3 @ 5,5: 2x2"])
  (parse-rectangle (first example1))                        ;12
  (inches-in-rectangle [1 [3 1 2 2]])
  ;Part I Solution:
  (count-overlapping-claims claims))                        ;116491

;Part II
(defn find-intact-claims
  [claims]
  (let [remove-overlapping (fn [{single false overlapping true}]
                             (s/difference single overlapping))]
    (->> claims
         (parse-claims)
         (vals)
         (group-by more-than-one)
         ;collect nested lists of ids in both map values into sets.
         (reduce-kv
           (fn [m k id-colls]
             (assoc m k (set (flatten id-colls))))
           {})
         (remove-overlapping))))

(comment
  "Part II"
  (find-intact-claims example1)                             ;#{3}
  (find-intact-claims claims))                              ;#{707}
