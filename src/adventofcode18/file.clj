(ns adventofcode18.file
  (:use [clojure.string :only (split)])
  (:require [clojure.java.io :as io]))

(defn lines-of
  "Loads lines from a file, optionally transformed by given function."
  ([path] (lines-of path identity))
  ([path fn] (with-open [rdr (io/reader path)]
               (doall (map fn (line-seq rdr))))))

(defn map-chars-indexed
  "Loads lines from a file, optionally transformed by given function."
  ([path] (lines-of path identity))
  ([path fn] (with-open [rdr (io/reader path)]
               (doall (map fn (line-seq rdr))))))

(defn map-chars-indexed
  "Returns a seq of results of applying f
   to each character of every line, followed by its line and column number."
  [f lines]
  (->> lines
       (map-indexed
         (fn [line-idx line]
           (map-indexed
             (fn [char-idx c] (f c line-idx char-idx))
             line)))
       (apply concat)))

(defn lines-of-resource [res]
  (lines-of (io/resource res)))

(defn words [str] (split str #"\s"))

(def to-int #(Integer/parseInt %))

(defn line-to-ints
  [str]
  (map to-int
       (words str)))

(defn read-table-of-ints [path] (lines-of path line-to-ints))
