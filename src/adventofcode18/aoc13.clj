(ns adventofcode18.aoc13
  (:require [clojure.java.io :as io]
            [adventofcode18.file :as f]))

;Parser
(defn map-chars
  "Returns a seq of results of applying f
   to each character of every line, followed by its line and column number."
  [f lines]
  (->> lines
       (map-indexed
         (fn [line-idx line]
           (map-indexed
             (fn [char-idx c] (f c line-idx char-idx))
             line)))
       (flatten)))

(defn read-tile
  [c il ic]
  (condp contains? c
    #{\| \- \\ \/ \+} {:tracks {[ic il] c}}
    #{\^ \v} {:tracks {[ic il] \|}
              :carts  {[ic il] {:direction c :next-turn :left}}}
    #{\< \>} {:tracks {[ic il] \-}
              :carts  {[ic il] {:direction c :next-turn :left}}}
    {}))

(defn keys-to-vals
  "Returns a sequence of map values with the original keys associated to it.
  The values must be associative structures."
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

;The logic
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
    [\v \\] \>
    [\v \/] \<
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

(defn get-cart-queue
  "Returns a seq of carts in proper movement order for the next tick."
  [railway]
  (sort-by position->vec (:carts railway)))

(defn tick [railway]
  "Return railway with cart arrangement after one tick.
  For testing purposes."
  (loop [last-railway railway
         carts (get-cart-queue railway)]
    (if-let [cart (first carts)]
      (recur
        (move-cart last-railway cart)
        (rest carts))
      last-railway)))

(defn ticks
  [n railway]
  (reduce (fn [r _] (tick r)) railway (range n)))

(defn- collide? [cart1 cart2]
  (= (:location cart1) (:location cart2)))

(defn- detect-collision
  [{carts :carts} cart]
  (first
    (filter
      (partial collide? cart)
      (remove #{cart} carts))))

(defn find-first-collision-location [railway]
  (loop [last-railway railway
         carts (get-cart-queue railway)]
    (if-let [cart (first carts)]
      (let [next-railway (move-cart last-railway cart)
            collision (detect-collision next-railway cart)]
        (if (not collision)
          (recur
            next-railway
            (rest carts))
          (:location collision)))
      (recur
        last-railway
        (get-cart-queue last-railway)))))

(defn print-location [{x :x y :y}] (str x \, y))

(defn part-1 []
  (->> (f/lines-of-resource "./aoc13.txt")
       (read-map)
       (find-first-collision-location)
       (print-location)))

(comment
  (def example (f/lines-of-resource "./aoc13ex.txt"))
  (read-map example)
  (:carts (read-map example))
  (:carts (read-map (f/lines-of-resource "./aoc13.txt")))
  (:carts (tick (read-map example)))
  (:carts (ticks 14 (read-map example)))
  (time (:carts (ticks 200 (read-map example))))
  (find-first-collision-location (read-map example))
  (find-first-collision-location (read-map (f/lines-of-resource "./aoc13.txt")))
  (part-1))

