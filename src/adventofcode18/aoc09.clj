(ns adventofcode18.aoc09
  (:require [adventofcode18.file :as f]))

(defn init
  [[player-count last-marble]]
  (let [players (range player-count)
        marbles (range 1 (inc last-marble))
        current-marble 0
        score (into [] (repeat player-count 0))]
    {:players        players
     :marbles        marbles
     :current-marble current-marble
     :marble-count   1
     :circle         (vector-of :int 0)
     :score          score}))

(defn parse-input
  [s]
  (->> s
       (re-seq #"\d+")
       (map f/to-int)))


(def step-forward 1)
(def step-backwards 7)
(def every-nth-to-take 23)

(defn next-marble [current-marble]
  (+ current-marble step-forward))

(defn next-marble-idx [{pos :current-marble m-count :marble-count}]
  (inc
    (mod
      (next-marble pos)
      m-count)))

(defn prev-marble [current-marble]
  (- current-marble step-backwards))

(defn wrap-current-marble-idx [game position]
  (mod position (:marble-count game)))

(defn update-current-marble [game fn]
  (update game :current-marble (comp #(wrap-current-marble-idx game %) fn)))

(defn place-marble-at
  "Returns circle with a marble added to position pos."
  [circle marble pos]
  (let [placement-idx pos]
    (concat
      (take placement-idx circle)
      [marble]
      (nthrest circle placement-idx))))

(comment
  (place-marble-at [1 2 3 4] 9 0)
  (place-marble-at [1 2 3 4] 9 2)
  (place-marble-at [1 2 3 4] 9 4)
  ;unexpected result:
  (place-marble-at [1 2 3 4] 9 5)
  (place-marble-at (place-marble-at [1 2 3 4] 9 4) 10 4))

(defn place-marble
  "Returns game with a marble placed to the circle relative to the current marble
   and updated current marble index."
  [game marble]
  (let [
        placement-idx (next-marble-idx game)]
    (-> game
        (assoc :current-marble placement-idx)
        (update :circle #(place-marble-at % marble placement-idx))
        (update :marble-count inc))))

(defn remove-marble
  "Removes a marble at position.
   Returns new circle and value of the removed marble."
  [circle position]
  ;(println "remove marble" position "from" circle)
  (let [circle-position (mod position (count circle))
        removed-marble (nth circle circle-position)
        new-circle (into []
                         (concat
                           (take circle-position circle)
                           (nthrest circle (inc circle-position))))]
    {:circle new-circle
     :marble removed-marble}))

(comment
  (remove-marble [1 2 3 4] 0)
  (remove-marble [1 2 3 4] 2)
  (remove-marble [1 2 3 4] 3)
  (remove-marble [1 2 3 4] 4)
  (remove-marble (:circle (remove-marble [1 2 3 4] 4)) 4))

(defn current-player [game]
  (first (:players game)))

(defn score-points
  "Returns game with points added to the score of the current player."
  [game points]
  (update-in game [:score (current-player game)] + points))

(comment
  (score-points {:players [0 1]
                 :score   {0 50 1 60}} 8)
  (score-points {:players [1 0]
                 :score   {0 50 1 60}} 8))

(defn take-marble-at
  "Removes the marble at position and adds its value to the current player's score."
  [game position]
  (let [{removed-marble :marble
         new-circle     :circle} (remove-marble (:circle game) position)]
    (-> game
        (update :marble-count dec)
        (assoc :circle new-circle)
        (score-points removed-marble))))

(comment
  ;removes m3, adds it to 50, score of p0
  (take-marble-at {:players [0 1]
                   :score   {0 50 1 60}
                   :circle  [0 1 2 3]} 7))

(defn take-marble
  "Removes the current marble and adds its value to the current player's score."
  [game]
  (take-marble-at game (:current-marble game)))

(comment
  ;removes m3, adds it to 50, score of p0
  (take-marble {:players        [0 1]
                :score          {0 50 1 60}
                :circle         [0 1 2 3]
                :current-marble 7}))

(defn multiple? [x y]
  (and (not= 0 y) (not= 0 x) (integer? (/ y x))))

; Then, each Elf takes a turn placing the lowest-numbered remaining marble into the circle
; between the marbles that are 1 and 2 marbles clockwise of the current marble.
; (When the circle is large enough, this means that there is one marble between the marble
; that was just placed and the current marble.)
; The marble that was just placed then becomes the current marble.
;
;However, if the marble that is about to be placed has a number which is a multiple of 23,
; something entirely different happens.
; First, the current player keeps the marble they would have placed, adding it to their score.
; In addition, the marble 7 marbles counter-clockwise from the current marble is removed from the circle
; and also added to the current player's score.
; The marble located immediately clockwise of the marble that was removed becomes the new current marble.
(defn turn
  [game]
  ;(println "turn" (-> game (update :marbles count)(update :players first)))
  (let [marble (first (:marbles game))]
    (if (multiple? every-nth-to-take marble)
      (-> game
          (score-points marble)
          (update-current-marble prev-marble)
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
  (part1 (parse-input "486 players; last marble is worth 70833 points")))
