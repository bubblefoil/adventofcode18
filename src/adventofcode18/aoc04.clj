(ns adventofcode18.aoc04
  (:require [clojure.java.io :as io]
            [adventofcode18.file :as f])
  (:import (java.time.format DateTimeFormatter)
           (java.time.temporal TemporalAccessor)
           (java.time LocalDateTime)))

(defn read-ids [res-name]
  (f/lines-of (io/resource res-name)))

(def shifts (read-ids "./aoc04.txt"))

(defn read-shift-record
  "Parses a shift record."
  [shift]
  (let [ts (subs shift 1 17)
        minute (f/to-int (subs shift 15 17))
        action (subs shift 19)]
    {:ts ts :minute minute :action action}))

(defn parse-instant
  [instant]
  (let [df (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")]
    (LocalDateTime/parse instant df)))

(defn align-to-midnight
  "Aligns an instant to the closest midnight."
  [^LocalDateTime temporal]
  (-> temporal
      (.plusHours 1)
      (.toLocalDate)))

(defn get-day
  [instant]
  (-> instant
      (parse-instant)
      (align-to-midnight)))

(defn minutes-asleep
  [guard-awakeness-records]
  (->> guard-awakeness-records
       (map :minute)
       (partition 2)
       (map #(apply range %))
       (flatten)))
;(set)))

(def guard-id-pattern (re-pattern #"#(\d+)"))

(defn parse-guard-id
  [str]
  (f/to-int (second (re-find guard-id-pattern str))))

(defn process-shift
  "Transforms grouped shift records."
  [[[shift-beginning] guard-awakeness-records]]
  {:day            (get-day (:ts shift-beginning))
   :guard          (parse-guard-id (:action shift-beginning))
   :minutes-asleep (minutes-asleep guard-awakeness-records)})

(defn is-shift-change
  "Returns true if given record is a beginning of a new shift."
  [record]
  (clojure.string/starts-with? (:action record) "Guard"))

(defn read-shifts
  "Parses and sorts guard shift records."
  [shifts]
  (->> shifts
       (sort)
       (map read-shift-record)
       (partition-by is-shift-change)
       (partition 2)
       (map process-shift)
       (group-by :guard)))

(defn count-minutes
  [guard-shifts]
  (->> guard-shifts
       (map :minutes-asleep)
       (map count)
       (reduce +)))

(defn find-drowsiest-minute
  "Returns a vector containing the number of the guard's drowsiest minute
   and total count of days on which the guard was asleep during that minute."
  [guard-shifts]
  (->> guard-shifts
       (map :minutes-asleep)
       (map vec)
       (reduce concat)
       (frequencies)
       (reduce #(max-key val %1 %2))))

(defn find-drowsiest-guard
  "Finds the guard who sleeps the most."
  [shifts]
  (->> shifts
       (reduce-kv
         (fn [acc k v] (conj acc {:guard k :minutes-asleep-total (count-minutes v)}))
         [])
       (reduce (fn [acc x] (max-key :minutes-asleep-total acc x)))))

(defn drowsiness-key
  "Multiplies if of the guard who sleeps the most by the minute during which they sleep most often."
  [shifts]
  (let [the-drowsiest (find-drowsiest-guard shifts)
        drowsiest-minute (find-drowsiest-minute (get shifts (:guard the-drowsiest)))]
    (* (:guard the-drowsiest) (first drowsiest-minute))))


(comment
  (def example1 ["[1518-11-01 00:00] Guard #10 begins shift"
                 "[1518-11-01 00:05] falls asleep"
                 "[1518-11-01 00:25] wakes up"
                 "[1518-11-01 00:30] falls asleep"
                 "[1518-11-01 00:55] wakes up"])
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
  (def awakeness [{:ts "1518-11-21 00:00", :minute 0, :action "falls asleep"}
                  {:ts "1518-11-21 00:38", :minute 38, :action "wakes up"}
                  {:ts "1518-11-21 00:43", :minute 43, :action "falls asleep"}
                  {:ts "1518-11-21 00:46", :minute 46, :action "wakes up"}])
  (minutes-asleep awakeness)
  (re-find (re-pattern #"\[()]"))
  (read-shift-record (first example))
  (read-shifts example1)
  (read-shifts example)
  (read-shifts shifts)
  (def the-drowsiest (find-drowsiest-guard (read-shifts shifts))) ;240
  (find-drowsiest-minute (get (read-shifts shifts) (:guard the-drowsiest))) ;
  (drowsiness-key (read-shifts example))                    ;
  ;Part II solution
  (drowsiness-key (read-shifts shifts)))                    ;