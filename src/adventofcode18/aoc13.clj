(ns adventofcode18.aoc13
  (:require [clojure.java.io :as io]
            [adventofcode18.file :as f]))

(defn map-chars
  [f lines]
  (->> lines
       (map-indexed
         (fn [line-idx line]
           (map-indexed
             (fn [char-idx c] (f line-idx char-idx c))
             line)))
       (flatten)))

(defn read-tile
  [il ic c]
  (condp contains? c
    #{\| \- \\ \/ \+} {:tracks {[ic il] c}}
    #{\^ \v} {:tracks {[ic il] \|}
              :carts  {[ic il] {:direction c :next-turn :left}}}
    #{\< \>} {:tracks {[ic il] \-}
              :carts  {[ic il] {:direction c :next-turn :left}}}
    {}))

(def dir-cycle [\^ \> \v \< \^])
(def turn-cycle {:left     :straight
                 :straight :right
                 :right    :left})

(defn- forward [cart]
  (case (:direction cart)
    \^ (update-in cart [:location :y] dec)
    \v (update-in cart [:location :y] inc)
    \< (update-in cart [:location :x] dec)
    \> (update-in cart [:location :x] inc)))

(comment
  (forward {:direction \^, :location {:x 3, :y 9}})
  (forward {:direction \v, :location {:x 3, :y 9}})
  (forward {:direction \>, :location {:x 3, :y 9}})
  (forward {:direction \<, :location {:x 3, :y 9}}))

(defn first-after
  "Finds the first item matching pred and returns the following one."
  [pred coll]
  (second (drop-while (complement pred) coll)))

(defn choose-turn [cart]
  (let [current-direction (:direction cart)]
    (case (:next-turn cart)
      :straight current-direction
      :right (first-after #{current-direction} dir-cycle)
      :left (first-after #{current-direction} (reverse dir-cycle)))))

(comment
  (choose-turn {:direction \^, :next-turn :straight})
  (choose-turn {:direction \^, :next-turn :left})
  (choose-turn {:direction \^, :next-turn :right})
  (choose-turn {:direction \>, :next-turn :right}))

(defn take-turn [cart]
  (-> cart
      (assoc :direction (choose-turn cart))
      (update :next-turn turn-cycle)))

(defn next-direction [direction track]
  (case [direction track]
    [\^ \\] \<
    [\^ \/] \>
    [\v \\] \<
    [\v \/] \>
    [\< \\] \^
    [\< \/] \v
    [\> \\] \v
    [\> \/] \^
    direction))

(defn- position->vec [cart]
  ((juxt :x :y) (:location cart)))

(defn update-direction [cart {tracks :tracks}]
  (let [track (get tracks (position->vec cart))]
    (if (= \+ track)
      (take-turn cart)
      (update cart :direction #(next-direction % track)))))

(defn ride
  "Returns cart with updated location after moving along the track."
  [railway cart]
  (-> cart
      (forward)
      (update-direction railway)))

(defn move-cart
  "Returns railway with updated cart location after moving it."
  [railway cart]
  (-> railway
      (update :carts #(remove #{cart} %))
      (update :carts #(conj % (ride railway cart)))))

;todo detect collisions
(defn tick [railway]
  (loop [last-railway railway
         carts (sort-by position->vec (:carts railway))]
    (if-let [cart (first carts)]
      (recur
        (move-cart last-railway cart)
        (rest carts))
      last-railway)))

(defn ticks
  [n railway]
  (reduce (fn [r _] (tick r)) railway (range n)))

(defn keys-to-vals
  ([m sym]
   (keys-to-vals m identity sym))
  ([m f sym]
   (map
     (fn [[k v]] (assoc v sym (f k)))
     m)))

(defn- vec->pos [v]
  (zipmap [:x :y] v))

(defn read-map [lines]
  (let [tiles (map-chars read-tile lines)
        railroad (apply merge-with merge tiles)]
    (update railroad :carts #(keys-to-vals % vec->pos :location))))

(comment
  (def example (f/lines-of-resource "./aoc13ex.txt"))
  (apply merge-with merge (map-chars read-tile example))
  (vec->pos [1 2])
  (read-map example)
  (:carts (read-map example))
  (:carts (tick (read-map example)))
  (:carts (ticks 14 (read-map example)))
  (:carts (tick (tick (read-map example)))))

