(ns adventofcode18.aoc04
  (:require [clojure.java.io :as io]
            [adventofcode18.file :as f]))

(defn read-ids [res-name]
  (f/lines-of (io/resource res-name)))

(def shifts (read-ids "./aoc04.txt"))

(defn read-minute
  "Parses the minute from a shift record."
  [shift]
  (f/to-int (subs shift 15 17)))

(defn minutes-asleep
  [guard-awakeness-records]
  (->> guard-awakeness-records
       (map read-minute)
       (partition 2)
       (map #(apply range %))
       (flatten)))

(def guard-id-pattern (re-pattern #"#(\d+)"))

(defn parse-guard-id
  [str]
  (f/to-int (second (re-find guard-id-pattern str))))

(defmulti process-shift-record
          (fn [_ record] (subs record 19 24)))

(defmethod process-shift-record "Guard" [shifts record]
  (let [guard-id (parse-guard-id record)]
    (-> shifts
        (assoc :current-guard guard-id)
        (update :shifts (fn [guard-shifts]
                          (merge {guard-id {:guard guard-id :actions []}} guard-shifts))))))

(defmethod process-shift-record :default [shifts record]
  (let [guard-id (:current-guard shifts)]
    (update-in shifts [:shifts guard-id :actions]
               (fn [actions]
                 (conj actions record)))))

(defn update-values
  [map f]
  (reduce-kv (fn [m k v] (assoc m k (f v))) {} map))

(defn max-by
  [f coll]
  (reduce (fn [x y] (max-key f x y)) coll))

(defn process-shift-actions
  "Transforms grouped shift records."
  [{guard-awakeness-records :actions}]
  {:minutes-asleep (minutes-asleep guard-awakeness-records)})

(defn actions-to-minutes-asleep
  [shifts]
  (update-values shifts process-shift-actions))

(defn read-shifts
  "Parses and sorts guard shift records."
  [shifts]
  (->> shifts
       (sort)
       (reduce process-shift-record {:shifts {} :current-guard nil})
       (:shifts)
       (actions-to-minutes-asleep)))

(defn find-drowsiest-minute
  "Returns a vector containing the number of the guard's drowsiest minute
   and total count of days on which the guard was asleep during that minute."
  [minutes-asleep]
  (->> minutes-asleep
       (frequencies)
       (max-by val)
       ;get the key from the max entry
       (first)))

(defn find-drowsiest-guard
  "Finds the guard who sleeps the most."
  [shifts]
  (->> shifts
       (max-by (comp count :minutes-asleep val))))

(defn drowsiness-key
  "Multiplies if of the guard who sleeps the most by the minute during which they sleep most often."
  [shifts]
  (let [[id {minutes-asleep :minutes-asleep}] (find-drowsiest-guard shifts)
        drowsiest-minute (find-drowsiest-minute minutes-asleep)]
    (* id drowsiest-minute)))

(comment
  (def example1 ["[1518-11-01 00:00] Guard #10 begins shift"
                 "[1518-11-01 00:05] falls asleep"
                 "[1518-11-01 00:25] wakes up"
                 "[1518-11-01 00:30] falls asleep"
                 "[1518-11-01 00:55] wakes up"])
  (def example2 ["[1518-06-28 00:03] Guard #1033 begins shift"
                 "[1518-03-29 23:58] Guard #1987 begins shift"
                 "[1518-03-30 00:34] falls asleep"
                 "[1518-03-30 00:35] wakes up"
                 "[1518-03-30 00:44] falls asleep"
                 "[1518-03-30 00:47] wakes up"
                 "[1518-03-30 00:51] falls asleep"
                 "[1518-03-30 00:53] wakes up)"])
  (def example ["[1518-11-01 00:00] Guard #10 begins shift"
                "[1518-11-01 00:05] falls asleep"
                "[1518-11-01 00:25] wakes up"
                "[1518-11-01 00:30] falls asleep"
                "[1518-11-01 00:55] wakes up"
                "[1518-11-01 23:58] Guard #99 begins shift"
                "[1518-11-02 00:40] falls asleep"
                "[1518-11-02 00:50] wakes up"
                "[1518-11-03 00:05] Guard #10 begins shift"
                "[1518-11-03 00:24] falls asleep"
                "[1518-11-03 00:29] wakes up"
                "[1518-11-04 00:02] Guard #99 begins shift"
                "[1518-11-04 00:36] falls asleep"
                "[1518-11-04 00:46] wakes up"
                "[1518-11-05 00:03] Guard #99 begins shift"
                "[1518-11-05 00:45] falls asleep"
                "[1518-11-05 00:55] wakes up"])
  (read-shifts example1)
  (read-shifts example)
  (read-shifts shifts)
  (def the-drowsiest (find-drowsiest-guard (read-shifts shifts))) ;
  (find-drowsiest-minute (get (read-shifts shifts) (:guard the-drowsiest))) ;
  (drowsiness-key (read-shifts example))                    ;
  ;Part II solution
  (drowsiness-key (read-shifts shifts)))                    ;