(ns adventofcode18.aoc07
  (:require [adventofcode18.file :as f]))

(defn parse-line
  [s]
  (rest (re-find #"Step (\w+) must be finished before step (\w+) can begin." s)))

(defn read-instructions
  [lines]
  (->> lines
       (map parse-line)))

(defn get-steps
  [instructions]
  (->> instructions
       (vec)
       (flatten)
       (set)))

(defn ready?
  "Returns true if instructions do not contain any prerequisite for step."
  [step instructions]
  (->> instructions
       (map second)
       (not-any? #{step})))

(defn ready-step [steps-to-do instructions]
  ;(println steps-to-do instructions)
  (->> steps-to-do
       (filter #(ready? % instructions))
       (sort)
       (first)))

(defn remove-step
  "Returns instructions without a specific step."
  [step instructions]
  (filter (fn [instr] (not= step (first instr))) instructions))

(defn building-steps-order
  [full-instructions]
  (loop [steps-left (get-steps full-instructions)
         instructions-left full-instructions
         steps-done []]
    (if-let [next-step (ready-step steps-left instructions-left)]
      (recur
        (remove #{next-step} steps-left)
        (remove-step next-step instructions-left)
        (conj steps-done next-step))
      steps-done)))

(comment
  (def in ["Step C must be finished before step A can begin."
           "Step C must be finished before step F can begin."
           "Step A must be finished before step B can begin."
           "Step A must be finished before step D can begin."
           "Step B must be finished before step E can begin."
           "Step D must be finished before step E can begin."
           "Step F must be finished before step E can begin."])
  ;-->A--->B--
  ;/    \      \
  ;C      -->D----->E
  ;\           /
  ;---->F-----
  (def instructions (read-instructions in))
  (get-steps instructions)
  (ready-step (get-steps instructions) instructions)
  (remove-step "C" instructions)
  ;Part I
  (building-steps-order (read-instructions in));=> ["C" "A" "B" "D" "F" "E"]
  (clojure.string/join (building-steps-order (read-instructions (f/lines-of-resource "./aoc07.txt")))))
;=> ["C" "A" "B" "D" "F" "E"]
