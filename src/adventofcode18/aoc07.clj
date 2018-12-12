(ns adventofcode18.aoc07
  (:require [adventofcode18.file :as f]
            [adventofcode18.util :as u]
            [clojure.set :as s]))

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

(defn ready-steps
  [steps-to-do instructions]
  (->> steps-to-do
       (filter #(ready? % instructions))
       (sort)))

(defn remove-step-from-instructions
  "Returns instructions without a specific step."
  [step instructions]
  (filter (fn [instr] (not= step (first instr))) instructions))

(defn building-steps-order
  [full-instructions]
  (loop [steps-left (get-steps full-instructions)
         instructions-left full-instructions
         steps-done []]
    (if-let [next-step (first (ready-steps steps-left instructions-left))]
      (recur
        (remove #{next-step} steps-left)
        (remove-step-from-instructions next-step instructions-left)
        (conj steps-done next-step))
      steps-done)))

;Part II

;(def step-fixed-duration 0)
;(def workers 2)
(def step-fixed-duration 60)
(def workers 4)

(defn duration
  "Returns step duration"
  [step]
  (let [c (first step)
        step-duration (- (inc (int c)) (int \A))]
    (+ step-fixed-duration step-duration)))

(defn steps->durations
  "Groups steps by their durations."
  [time-offset steps]
  (map (fn [step] {:step-name step :step-finish-time (+ time-offset (duration step))}) steps))

(defn next-step-finish-time
  "Returns the step with nearest finish time or alphabetically lower name."
  [step-durations]
  (->> step-durations
       (map :step-finish-time)
       (reduce min)))

(defn remove-prerequisite-steps-from-instructions
  [steps instructions]
  (remove #((set steps) (first %)) instructions))

(defn wip-steps [wip]
  (->> wip
       (map :step-name)
       (set)))

(defn next-ready-steps
  [steps-done wip instructions]
  (let [steps (s/difference (get-steps instructions) steps-done)
        remaining-instructions (remove-prerequisite-steps-from-instructions steps-done instructions)]
    (->> steps
         (filter #(ready? % remaining-instructions))
         (remove (wip-steps wip))
         (sort))))

(defn building-steps-tree
  [instructions]
  (loop [steps-done #{}
         wip []
         elapsed-time 0]
    ;(println "time:" elapsed-time steps-done wip)
    (let [wip-done (filter #(<= (:step-finish-time %) elapsed-time) wip)
          wip-left (remove (set wip-done) wip)
          steps-done-now (s/union steps-done (wip-steps wip-done))
          idle-worker-count (- workers (count wip-left))
          more-work (take idle-worker-count (next-ready-steps steps-done-now wip-left instructions))
          next-wip (concat wip-left (steps->durations elapsed-time more-work))]
      (if (not-empty next-wip)
        (recur
          steps-done-now
          next-wip
          (next-step-finish-time next-wip))
        elapsed-time))))

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
  (remove-step-from-instructions "C" instructions)
  (ready-steps (get-steps instructions) (remove-step-from-instructions "C" instructions))
  ;Part I
  (building-steps-order (read-instructions in))             ;=> ["C" "A" "B" "D" "F" "E"]
  (clojure.string/join (building-steps-order (read-instructions (f/lines-of-resource "./aoc07.txt"))))
  ;Part II
  (remove-prerequisite-steps-from-instructions ["C"] instructions)
  (steps->durations 0 ["A" "C" "E"])
  (building-steps-tree (read-instructions in))              ;15, 2w, 0s
  (building-steps-tree (read-instructions (f/lines-of-resource "./aoc07.txt")))) ;980, 4w, 60s
