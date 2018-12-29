(ns adventofcode18.aoc12
  (:require [adventofcode18.file :as f]))

;Rules in a map [^char]=>^char
;State in [^char]
;customized get subvec fn which pads for x < 2 or x+2 > size
;get map subvec \.
;pmap state, recur

(defn rule? [s] (re-matches #"[.#]{5} =\> [\.#]" s))

(defn parse-initial [s]
  (->> s
       (re-find #"([#.])+")
       (first)
       (map-indexed (fn [i c] [i c]))
       (into (sorted-map))))

(defn parse-rule [s]
  [(vec (subs s 0 5)) (last s)])

(defn parse-rules [rules]
  (->> rules
       (filter rule?)
       (map parse-rule)
       (into {})))

(defn left [x] (- x 2))
(defn right [x] (+ x 2))

(defn padding [pots] (repeat (- 5 (count pots)) \.))
(defn pad-l [pots] (concat (padding pots) pots))
(defn pad-r [pots] (concat pots (padding pots)))

(defn neighborhood
  "Get a sequence of 5 values around idx from a sorted map."
  [pots idx]
  (let [neighborhood (vals (subseq pots >= (left idx) <= (right idx)))]
    (cond-> neighborhood
            (< (left idx) (key (first pots))) (pad-l)
            (> (right idx) (key (last pots))) (pad-r))))

(defn- first-full-idx [pots]
  (key (first (filter #(= \# (val %)) pots))))

(defn trim-empty
  "Removes empty pots at the head and tail of a sorted map."
  [pots]
  (let [first-full (first-full-idx pots)
        last-full (first-full-idx (reverse pots))]
    (into (sorted-map) (subseq pots >= first-full <= last-full))))

(comment
  (first-full-idx (parse-initial "....###...###"))
  (first-full-idx (reverse (parse-initial "###...")))
  (trim-empty (parse-initial "..###..")))

(defn next-generation [rules state]
  (let [head (first (keys state))
        tail (last (keys state))]
    (->> (range (left head) (right tail))
         (pmap (fn [i] [i (get rules (neighborhood state i) \.)]))
         (into (sorted-map))
         (trim-empty))))

(defn nth-generation [n rules state]
  (loop [new-state state
         i n]
    (if (pos-int? i)
      (recur (next-generation rules new-state) (dec i))
      new-state)))

(defn grow [gens in]
  (let [state (parse-initial (first in))
        rules (parse-rules (rest in))]
    (nth-generation gens rules state)))

(defn sum
  "Returns the sum of numbers of full pots."
  [pots]
  (->> pots
       (filter #(= \# (val %)))
       (map key)
       (reduce +)))

(defn part-1 []
  (->> (f/lines-of-resource "./aoc12.txt")
       (grow 20)
       (sum)))

(comment
  (pad-l [1 3])
  (neighborhood [1 2 3 4 5] 1)
  (neighborhood [1 2 3 4 5] 4)

  (def state (vec "#..#.#..##......###...###"))
  (def s "...## => #")
  (def example ["initial state: #..#.#..##......###...###"
                ""
                "...## => #"
                "..#.. => #"
                ".#... => #"
                ".#.#. => #"
                ".#.## => #"
                ".##.. => #"
                ".#### => #"
                "#.#.# => #"
                "#.### => #"
                "##.#. => #"
                "##.## => #"
                "###.. => #"
                "###.# => #"
                "####. => #"])
  (parse-rules example)
  (parse-initial (first example))
  (get (parse-rules example) [\. \. \# \. \.])
  (get (parse-rules example) (neighborhood [\# \. \. \# \. \. \. \. \# \. \.] 0))
  (grow 20 example)
  (sum (grow 20 example))
  ;Part I
  (part-1))

