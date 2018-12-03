(ns adventofcode18.aoc02
  (:require [clojure.java.io :as io]
            [clojure.set :as s]
            [clojure.math.combinatorics :as combo]
            [adventofcode18.file :as f]))

(defn read-ids [res-name]
  (f/lines-of (io/resource res-name)))

(def ids (read-ids "./aoc02.txt"))

(defn distinct-char-counts
  "Returns map of counts of distinct character frequencies in a word."
  [str]
  (-> str
      (frequencies)
      (s/map-invert)
      (keys)
      (frequencies)))

(defn checksum
  "Calculates IDs checksum."
  [coll]
  (let [counts (map distinct-char-counts coll)
        count-sums (apply merge-with + counts)]
    (* (get count-sums 2 0) (get count-sums 3 0))))

(comment
  (def example1 ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])
  (println (checksum example1))                                  ;12
  (println (checksum ids)))

;Part II
(defn same-sub
  "Returns a string consisting of only the characters that are same in a pair of given strings."
  ([l r]
   (->> [l r]
        (apply interleave)
        (partition 2)
        (filter #(apply = %))
        (map first)
        (apply str))))

(defn close-enough
  "Returns true if given strings differ by exactly one character."
  [[left right]]
  (and
    (= (count left) (count right))
    (= 1 (- (count left) (count (same-sub left right))))))

(defn find-similar
  [strings]
  (let [strings (combo/combinations strings 2)]
    (->> strings
         (filter close-enough)
         (first)
         (apply same-sub))))

(comment
  (def example2 ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])
  (same-sub "fool" "foot")
  (find-similar example2)                                  ;fgij
  (find-similar ids))