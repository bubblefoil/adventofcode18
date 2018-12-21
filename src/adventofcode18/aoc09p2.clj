(ns adventofcode18.aoc09p2
  (:require [adventofcode18.file :as f]
            [adventofcode18.circle :as c]
            [clojure.pprint :as pp]))

;Updated solution which uses a custom mutable array-based double-linked list.

(defn parse-input [s]
  (->> s
       (re-seq #"\d+")
       (map f/to-int)))

(defn place-marble
  "Returns game with a marble placed to the circle relative to the current marble
   and updated current marble index."
  [game marble]
  (-> game
      (update :circle #(c/clockwise %))
      (update :circle #(c/insert % marble))))

(defn score-points
  "Returns game with points added to the score of the current player."
  [game points]
  (update-in game [:score (first (:players game))] + points))

(comment
  (score-points {:players [0 1] :score {0 50 1 60}} 8)
  (score-points {:players [1 0] :score {0 50 1 60}} 8))

(defn take-marble
  "Removes the current marble and adds its value to the current player's score."
  [game]
  (let [new-circle (c/extract (:circle game))
        removed-marble (:last-removed new-circle)]
    (-> game
        (assoc :circle new-circle)
        (score-points removed-marble))))

(comment
  ;removes m3, adds it to 50, score of p0
  (take-marble {:players [0 1]
                :score   {0 50 1 60}
                :circle  [0 1 2 3]}))

(defn multiple? [x y]
  (and (not= 0 y) (not= 0 x) (integer? (/ y x))))

(defn turn [game]
  ;(pp/pprint (-> game (update :marbles count)(update :players first)))
  (let [marble (first (:marbles game))]
    (if (multiple? 23 marble)
      (-> game
          (score-points marble)
          (update :circle #(c/rotate % -7))
          (take-marble))
      (-> game
          (place-marble marble)))))

(defn play
  [game]
  (loop [g (update game :players cycle)]
    (if-not (empty? (:marbles g))
      (recur (-> g
                 (turn)
                 (update :players rest)
                 (update :marbles rest)))
      (:score g))))

(defn init [[player-count last-marble]]
  (let [players (range player-count)
        marbles (range 1 (inc last-marble))
        score (into [] (repeat player-count 0))]
    {:players      players
     :marbles      marbles
     :circle       (c/insert (c/->circle last-marble) 0)
     :score        score}))

(defn part1
  [setup]
  (->> setup
       (init)
       (play)
       (apply max)))

(comment
  (def example "9 players; last marble is worth 25 points:")
  (part1 [9 25])                                            ;32
  (part1 [10 1618])                                         ;8317
  (part1 [13 7999])                                         ;146373
  (time (part1 [60 16000]))
  (time (part1 [60 30000]))
  (part1 [17 1104])                                         ;2764
  (def game (parse-input in))
  (part1 game)
  ;Solution
  ;Part I
  ;"Elapsed time: 3915.391073 msecs"
  ;=> 373597
  (time (part1 (parse-input "486 players; last marble is worth 70833 points")))
  ;Part II
  ;"Elapsed time: 391766.57113 msecs"
  ;=> 2954067253
  (time (part1 [486 (* 100 70833)])))
