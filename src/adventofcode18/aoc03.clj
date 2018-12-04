(ns adventofcode18.aoc03
  (:require [clojure.java.io :as io]
            [clojure.set :as s]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [adventofcode18.file :as f]))

(defn read-ids [res-name]
  (f/lines-of (io/resource res-name)))

(def claims (read-ids "./aoc03.txt"))

(def claim-pattern (re-pattern "@\\s(\\d+),(\\d+):\\s(\\d+)x(\\d+)"))

(defn parse-rectangle
  "Claim to rectangle"
  [claim]
  (->> (re-find claim-pattern claim)
       (rest)
       (map f/to-int)))

(defn inches-in-rectangle
  "Returns all square inches within a rectangle."
  [[left top width height]]
  (let [horizontal (range left (+ left width))
        vertical (range top (+ top height))]
    (->> vertical
         (mapcat
           (fn [col]
             (map
               (fn [row] (vector row col))
               horizontal))))))

(defn claim-inch
  [claimed inch]
  (merge-with + claimed {inch 1}))

(defn claims-per-inch
  "Finds rectangle overlaps"
  [rectangles]
  (->> rectangles
       (mapcat inches-in-rectangle)
       (reduce claim-inch {})))

(defn overlaps
  [claims]
  (->> claims
       (vals)
       (filter #(> % 1))
       (count)))

(defn find-overlaps
  "Part I"
  [claims]
  (->> claims
       (map parse-rectangle)
       (claims-per-inch)
       (overlaps)))

(comment
  (def example1 ["#1 @ 1,3: 4x4"
                 "#2 @ 3,1: 4x4"
                 "#3 @ 5,5: 2x2"])
  (parse-rectangle (first example1))              ;12
  (inches-in-rectangle [3 1 2 2])
  (def claim-pi (claims-per-inch (map parse-rectangle example1)))
  (overlaps claim-pi)
  ;Solution:
  (find-overlaps claims))

;Part II