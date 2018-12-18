(ns adventofcode18.aoc08
  (:require [adventofcode18.file :as f]
            [clojure.java.io :as io]
            [adventofcode18.util :as u]))

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

(defn sum-by-idx
  [licence-key metadata]
  ;(println "sum-licence-key: lk=" licence-key "md=" metadata)
  (->> metadata
       (map #(nth licence-key (dec %) 0))
       (reduce +)))

(defn ->licence [coll]
  (letfn [(->metadata [n-children n-metadata {:keys [rem licence]}]
            ;(println "metadata: ch=" n-children "md=" n-metadata "rem=" rem "l=" licence)
            (let [metadata (take n-metadata rem)
                  remaining (drop n-metadata rem)
                  licence-key (if (= 0 n-children)
                                (conj licence (reduce + metadata))
                                (conj
                                  (subvec licence 0 (- (count licence) n-children))
                                  ;(u/spy "sum-by-idx"
                                  (sum-by-idx (take-last n-children licence) metadata)))]
              {:licence licence-key
               :rem     remaining}))
          (->node [licence]
            ;(println "node:" licence)
            (let [rem (:rem licence)
                  [children metadata] (take 2 rem)]
              (->metadata
                children
                metadata
                (->children children (merge licence {:rem (drop 2 rem)})))))
          (->children [n licence]
            ;(println "children:" n licence)
            (loop [i n
                   acc licence]
              (if (> i 0)
                (recur (dec i) (->node acc))
                acc)))]
    (->node {:rem coll :licence []})))

(defn sum-metadata
  "Part I"
  [licence]
  (->> licence
       (->tree)
       (:metadata)
       (reduce +)))

(defn calc-licence
  "Part II"
  [licence]
  (->> licence
       (->licence)
       (:licence)))


(comment
  (def in (f/line-to-ints "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"))
  ;2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
  ;A----------------------------------
  ;    B----------- C-----------
  ;                     D-----
  ;1+1+2+10+11+12+2+99=138

  ;[2 3] (1 1 2)
  ;--[0 3] (10 11 12)
  ;--[1 1] (2)
  ;----[0 1] (99)
  (->tree in)
  (sum-metadata in)
  ;Part I
  (sum-metadata (read-licence))
  ;Part II
  (calc-licence in)
  (calc-licence (read-licence)))