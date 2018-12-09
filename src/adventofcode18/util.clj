(ns adventofcode18.util)

(defn max-by
  [f coll]
  (reduce (fn ([x y] (max-key f x y)) ([] nil)) coll))

(defn min-by
  [f coll]
  (reduce (fn ([x y] (min-key f x y)) ([] nil)) coll))