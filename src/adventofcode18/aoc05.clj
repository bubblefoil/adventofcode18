(ns adventofcode18.aoc05
  (:require [clojure.java.io :as io]))

(def polymer (slurp (io/resource "./aoc05.txt")))

(defn upper? [c] (Character/isUpperCase ^char c))
(defn upper [c] (Character/toUpperCase ^char c))
(defn lower? [c] (Character/isLowerCase ^char c))
(defn lower [c] (Character/toLowerCase ^char c))

(defn opposite-case? [c1 c2]
  (or
    (and (upper? c1) (lower? c2) (= (lower c1) c2))
    (and (upper? c2) (lower? c1) (= (lower c2) c1))))

(defn react
  [polymer]
  (->> polymer
       (reduce
         (fn [acc s]
           (if
             (and
               (not-empty acc)
               (opposite-case? (peek acc) s))
             (pop acc)
             (conj acc s)))
         [])
       (reduce str)))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def alphabet (char-range \a \z))

(defn lower-upper-pattern [c]
  (re-pattern (str \[ (lower c) (upper c) \])))

(def remove-patterns (map lower-upper-pattern alphabet))

(defn generate-filtered-polymers
  [patterns polymer]
  (->> patterns
       (map #(clojure.string/replace polymer % ""))))

(defn shortest-polymer
  "Part II"
  [polymer]
  (->> polymer
       (generate-filtered-polymers remove-patterns)
       (map react)
       (map count)
       (reduce min)))

(comment
  (opposite-case? \A \A)
  (opposite-case? \a \A)
  (opposite-case? \A \a)
  (opposite-case? \A \b)
  (opposite-case? \A \b)
  (def e1 "dabAcCaCBAcCcaDA")
  (react e1)
  (react "AbBa")
  ;Part I
  (count (react polymer))
  (def e1-result "dabCBAcaDA")
  ;Part II
  (shortest-polymer polymer))
