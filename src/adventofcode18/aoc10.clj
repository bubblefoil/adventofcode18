(ns adventofcode18.aoc10
  (:require [adventofcode18.file :as f]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]))

(defn parse-star
  [star]
  (->> star
       (re-seq #"[-\d]+")
       (map f/to-int)))

(defn parse-input
  [lines]
  (->> lines
       (map parse-star)))

(defn min-max-vec2 [[min-x min-y max-x max-y] [x y]]
  (vector (min min-x x) (min min-y y) (max max-x x) (max max-y y)))

(defn bbox [stars]
  (->> stars
       (map #(take 2 %))
       (reduce min-max-vec2 [1000 1000 -1000 -1000])))

(defn subtract-v [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn move-to-positive-coords [stars]
  "Moves all the stars according to their BB so they all have positive coordinates."
  (let [[min-x min-y _ _] (bbox stars)]
    (map #(subtract-v % [min-x min-y]) stars)))

(defn make-canvas [[x1 y1 x2 y2]]
  (make-array Character/TYPE (inc (- y2 y1)) (inc (- x2 x1))))

(defn stringify [canvas]
  "Transforms 2d array of chars into 1d array of Strings."
  (areduce canvas idx ret []
           (conj ret
                 (apply str (aget canvas idx)))))

(defn plot [stars]
  (let [bb (bbox stars)
        canvas (make-canvas bb)
        points (set (move-to-positive-coords stars))
        [min-x min-y max-x max-y] bb]
    (doall
      (for [row (range 0 (inc (- max-y min-y)))
            col (range 0 (inc (- max-x min-x)))]
        (aset-char canvas row col (if (contains? points [col row]) \# \.))))
    (pp/pprint (stringify canvas))))

(defn ax+b [a x b] (+ (* a x) b))

(defn move [[x y vx vy] dt]
  (vector (ax+b dt vx x) (ax+b dt vy y) vx vy))

(defn animate
  "Returns stars with positions at given time delta."
  [stars dt]
  (map #(move % dt) stars))

(defn read-stars
  "Loads stars from the input."
  [res]
  (parse-input (f/lines-of-resource res)))

(defn area [[min-x min-y max-x max-y]]
  (*
    (- max-x min-x)
    (- max-y min-y)))

(defn densest-constellation-time [stars]
  (loop [bb (bbox stars) t 0]
    (let [next-t (inc t)
          new-bb (bbox (animate stars next-t))]
     (if (> (area new-bb) (area bb))
       t
       (recur new-bb next-t)))))

(defn part1
  "The Part I solution"
  []
  (let [stars (read-stars "./aoc10.txt")
        t (densest-constellation-time stars)]
    (plot (animate stars t))))

(defn part2
  "The Part II solution"
  []
  (-> "./aoc10.txt"
      (read-stars)
      (densest-constellation-time)))

(comment
  (move [3 -2 -1 1] 1.5)
  (parse-star "position=< 3, -2> velocity=<-1,  1>")
  (def example ["position=< 9,  1> velocity=< 0,  2>"
                "position=< 7,  0> velocity=<-1,  0>"
                "position=< 3, -2> velocity=<-1,  1>"
                "position=< 6, 10> velocity=<-2, -1>"
                "position=< 2, -4> velocity=< 2,  2>"
                "position=<-6, 10> velocity=< 2, -2>"
                "position=< 1,  8> velocity=< 1, -1>"
                "position=< 1,  7> velocity=< 1,  0>"
                "position=<-3, 11> velocity=< 1, -2>"
                "position=< 7,  6> velocity=<-1, -1>"
                "position=<-2,  3> velocity=< 1,  0>"
                "position=<-4,  3> velocity=< 2,  0>"
                "position=<10, -3> velocity=<-1,  1>"
                "position=< 5, 11> velocity=< 1, -2>"
                "position=< 4,  7> velocity=< 0, -1>"
                "position=< 8, -2> velocity=< 0,  1>"
                "position=<15,  0> velocity=<-2,  0>"
                "position=< 1,  6> velocity=< 1,  0>"
                "position=< 8,  9> velocity=< 0, -1>"
                "position=< 3,  3> velocity=<-1,  1>"
                "position=< 0,  5> velocity=< 0, -1>"
                "position=<-2,  2> velocity=< 2,  0>"
                "position=< 5, -2> velocity=< 1,  2>"
                "position=< 1,  4> velocity=< 2,  1>"
                "position=<-2,  7> velocity=< 2, -2>"
                "position=< 3,  6> velocity=<-1, -1>"
                "position=< 5,  0> velocity=< 1,  0>"
                "position=<-6,  0> velocity=< 2,  0>"
                "position=< 5,  9> velocity=< 1, -2>"
                "position=<14,  7> velocity=<-2,  0>"
                "position=<-3,  6> velocity=< 2, -1>"])
  (def stars (parse-input example))
  (min-max-vec2 [0 0 0 0] [1 2])
  (bbox stars)
  (move-to-positive-coords stars)
  (pp/pprint (make-canvas (bbox stars)))
  (plot stars)
  (stringify (plot stars))
  (def make-canvas (plot stars))
  (animate stars 3)
  (densest-constellation-time stars)
  (animate stars 5)
  ;Part I
  (def input (read-stars "./aoc10.txt"))
  (plot (animate input 10391))
  (part1)
  ;Part II
  (densest-constellation-time input));10391

