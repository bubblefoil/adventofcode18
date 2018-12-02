(ns adventofcode18.aoc02
  (:require [clojure.java.io :as io]
            [clojure.set :as s]
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