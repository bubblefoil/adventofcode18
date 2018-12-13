(ns adventofcode18.aoc08
  (:require [adventofcode18.file :as f]
            [clojure.java.io :as io]))

(defn read-licence []
  (f/line-to-ints (slurp (io/resource "./aoc08.txt"))))

(defn ->tree [coll]
  (letfn [(->metadata [n {:keys [rem metadata]}]
            ;(println "metadata:" n rem metadata)
            {:metadata (concat metadata (take n rem))
             :rem (drop n rem)})
          (->node [licence]
            ;(println "node:" licence)
            (let [rem (:rem licence)
                  [children metadata] (take 2 rem)]
              (->metadata
                metadata
                (->children children (merge licence {:rem (drop 2 rem)})))))
          (->children [n licence]
            ;(println "children:" n licence)
            (loop [i n
                   acc licence]
              (if (> i 0)
                (recur (dec i) (->node acc))
                acc)))]
    (->node {:rem coll :metadata []})))

(defn sum-metadata
  "Part I"
  [licence]
  (->> licence
       (->tree)
       (:metadata)
       (reduce +)))

(comment
  (def in (f/line-to-ints "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"))
  ;2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
  ;A----------------------------------
  ;    B----------- C-----------
  ;                     D-----
  ;1+1+2+10+11+12+2+99=138

  (->tree in)
  (sum-metadata in)
  ;Part I
  (sum-metadata (read-licence)))