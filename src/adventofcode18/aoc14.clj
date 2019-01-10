(ns adventofcode18.aoc14
  (:require [adventofcode18.util :as u]
            [adventofcode18.file :as f]))

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
  (u/loop-while #(< (count (:recipes %)) n) cook kitchen))

(def starter {:recipes (vector 3 7)
              :one     0
              :two     1})

(def score-length 10)

(comment
  (u/loop-while #(< (count (:recipes %)) 9) cook starter))

(defn as-str [v] (apply str v))

(defn score-after [n]
  (-> n
      (+ score-length)
      (make-recipes starter)
      (:recipes)
      (subvec n (+ n score-length))
      (as-str)))

(def input 290431)

(defn part-1 [] (score-after input))

(comment
  (mix 1 9)
  (mix 9 4)
  (mix 4 1)
  (count-ones [1])
  (cook starter)
  (u/loops 15 cook starter)
  (score-after 5)                                           ;0124515891
  (score-after 18)                                          ;9251071085
  (score-after 2018)                                        ;5941429882
  ;"Elapsed time: 275.3957 msecs"
  ;=> "1776718175"
  (time (part-1)))

;Part II
(defn score-digits [x] (vec (map (comp f/to-int str) (str x))))

(defn almost-ends-with
  "Returns true if the second last items in vector v equal to tail."
  [tail v]
  (let [nv (count v)
        i (- nv (count tail))]
    (and (pos-int? i)
         (= tail
            (subvec v (dec i) (dec nv))))))

(defn ends-with
  "Returns true if the last items in vector v equal to tail."
  [tail v]
  (let [nv (count v)
        i (- nv (count tail))]
    (and (>= i 0)
         (= tail
            (subvec v i)))))

(defn contains-score
  "Returns true if the target score elements are near the end of v."
  [score v]
  (or (ends-with score v)
      (almost-ends-with score v)))

(defn find-target-score
  [kitchen target-score]
  (u/loop-while
    #(not (contains-score (score-digits target-score) (:recipes %)))
    cook
    kitchen))

(defn recipes-before-score [kitchen target-score]
  (let [recipes (:recipes (find-target-score kitchen target-score))
        score (score-digits target-score)
        score-length (if (ends-with score recipes) (count score) (inc (count score)))]
    (-> recipes
        (count)
        (- score-length))))

(defn part-2
  "The solution."
  []
  (recipes-before-score starter input))

(comment
  (contains-score [1 2] [6 5 6 1 2])
  (contains-score `(1 2) [6 5 6 1 2])
  (contains-score [6 1] [6 5 6 1 2])
  (contains-score [1 2] [6 5 6 2])
  (map (comp f/to-int str) (str input))
  (time (find-target-score starter 51589))
  (recipes-before-score starter 51589)
  (recipes-before-score starter 59414)
  ;"Elapsed time: 36025.2844 msecs"
  ;=> 20220949
  (time (part-2)))