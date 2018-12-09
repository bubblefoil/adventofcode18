(ns adventofcode18.file
  (:use [clojure.string :only (split)])
  (:require [clojure.java.io :as io]))

(defn lines-of
  "Loads lines from a file, optionally transformed by given function."
  ([path] (lines-of path identity))
  ([path fn] (with-open [rdr (io/reader path)]
               (doall (map fn (line-seq rdr))))))

(defn lines-of-resource [res]
  (lines-of (io/resource res)))

(defn words [str] (split str #"\s"))

(def to-int #(Integer/parseInt %))

(defn line-to-ints
  [str]
  (map to-int
       (words str)))

(defn read-table-of-ints [path] (lines-of path line-to-ints))
