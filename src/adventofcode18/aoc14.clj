(ns adventofcode18.aoc14
  (:require [adventofcode18.util :as u]))

(defn mix [one two]
  (let [mixture (+ one two)]
    (if (> mixture 9)
      [1 (- mixture 10)]
      [mixture])))

(defn count-ones [coll]
  (count (filter #{1} coll)))

(defn cook
  [kitchen]
  (let [{:keys [one two recipes]} kitchen
        new-recipes (mix (recipes one) (recipes two))
        all-recipes (apply conj recipes new-recipes)]
    (-> kitchen
        (assoc :recipes all-recipes)
        (update :one + (recipes one) 1)
        (update :one mod (count all-recipes))
        (update :two + (recipes two) 1)
        (update :two mod (count all-recipes)))))

(defn make-recipes
  [n kitchen]
  (loop [last-kitchen kitchen]
    (if (< (count (:recipes last-kitchen)) n)
      (recur (cook last-kitchen))
      last-kitchen)))

(def starter {:recipes (vector 3 7)
              :one     0
              :two     1})

(def score-length 10)

(defn as-str [v] (apply str v))

(defn score-after [n]
  (-> n
      (+ score-length)
      (make-recipes starter)
      (:recipes)
      (subvec n (+ n score-length))
      (as-str)))

(defn part-1 [] (score-after 290431))

(comment
  (mix 1 9)
  (mix 9 4)
  (mix 4 1)
  (count-ones [1])
  (cook starter)
  (u/loops 15 cook starter)
  (score-after 5);0124515891
  (score-after 18);9251071085
  (score-after 2018);5941429882
  ;"Elapsed time: 275.3957 msecs"
  ;=> "1776718175"
  (time (part-1)))

