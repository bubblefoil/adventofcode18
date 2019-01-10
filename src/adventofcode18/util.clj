(ns adventofcode18.util)

(defn max-by
  [f coll]
  (reduce (fn ([x y] (max-key f x y)) ([] nil)) coll))

(defn min-by
  [f coll]
  (reduce (fn ([x y] (min-key f x y)) ([] nil)) coll))

(defn loops
  [n f x]
  (reduce (fn [fx _] (f fx)) x (range n)))

(defn loop-while
  "Repeatedly applies f to x, then to f(x) etc., until pred xn returns false."
  [pred f x]
  (loop [fx x]
    (if (pred fx)
      (recur (f fx))
      fx)))

(defn spy
  "Prints and returns x"
  ([x] (println x) x)
  ([what x] (println what x) x))