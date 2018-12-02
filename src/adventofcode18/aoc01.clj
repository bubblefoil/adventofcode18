(ns adventofcode18.aoc01
  (:require [clojure.java.io :as io]
            [adventofcode18.file :as f]))

(defn read-changes [res-name]
  (map f/to-int
       (f/lines-of (io/resource res-name))))

(comment
  (println "result:" (reduce + (read-changes "./aoc01.txt"))))

;Part II
(def changes (read-changes "./aoc01.txt"))

(defn find-first-repetition
  "The first repeating frequency."
  ([coll] (find-first-repetition #{} (first coll) (rest coll)))
  ([frequencies last-freq coll] (let [new-freq (+ last-freq (first coll))]
                                  (if (contains? frequencies new-freq)
                                    new-freq
                                    (recur (conj frequencies new-freq) new-freq (rest coll))))))

(comment
  ((deftest part2
            (is (= (find-first-repetition (cycle [+3 +3 +4 -2 -4])) 10))))

  (println
    "01 - Part II:"
    (find-first-repetition (cycle changes))))