(ns adventofcode18.circle
  (:require [clojure.pprint :as pp])
  (:import (java.util Arrays)))

(defn ->circle [size]
  (assert (pos-int? size))
  {:cursor (long 0) :circle (make-array Long/TYPE size 3) :length (long 0)})

(def prev-idx (int 0))
(def val-idx (int 1))
(def next-idx (int 2))

(defn- ensure-capacity [circle]
  (let [a (:circle circle)
        n (:length circle)]
    (if (= (alength a) n)
      (assoc circle :circle (Arrays/copyOf a (* 2 (alength a))))
      circle)))

(defn- update-array [f]
  (fn [a]
    (f a)
    a))

(defn insert [circle ^long val]
  (let [{tail   :length
         cursor :cursor} circle]
    (if (zero? (:length circle))
      (do
        (aset (:circle circle) 0 (long-array [0 val 0]))
        (update circle :length inc))
      (letfn [(add-new-val [a] (aset a tail (long-array [cursor val (aget a cursor next-idx)])))
              (connect-next-to-new [a] (aset-long a (aget a cursor next-idx) prev-idx tail))
              (connect-prev-to-new [a] (aset-long a cursor next-idx tail))]
        (-> circle
            (ensure-capacity)
            (update :circle (update-array add-new-val))
            (update :circle (update-array connect-next-to-new))
            (update :circle (update-array connect-prev-to-new))
            (update :length inc)
            (assoc :cursor tail))))))

(defn extract [circle]
  (if (pos-int? (:length circle))
    (let [{length :length
           cursor :cursor
           c      :circle} circle
          prev-i (aget c cursor prev-idx)
          val (aget c cursor val-idx)
          next-i (aget c cursor next-idx)
          tail (dec length)]
      (-> circle
          ;Rebind neighbors
          (update :circle (update-array #(aset-long % prev-i next-idx next-i)))
          (update :circle (update-array #(aset-long % next-i prev-idx prev-i)))
          ;Move the tail element to the emptied cell
          (update :circle (update-array #(aset % cursor (aget % tail))))
          (update :circle (update-array #(aset % tail nil)))
          ;Rebind the moved tail element's neighbors
          (update :circle (update-array #(aset-long % (aget % cursor prev-idx) next-idx cursor)))
          (update :circle (update-array #(aset-long % (aget % cursor next-idx) prev-idx cursor)))
          ;Move to the next element after the removed one
          (assoc :cursor next-i)
          (update :length dec)
          (assoc :last-removed val)))))

(defn clockwise [circle]
  (update circle :cursor #(aget (:circle circle) % next-idx)))

(defn counter-clockwise [circle]
  (update circle :cursor #(aget (:circle circle) % prev-idx)))

(defn rotate [circle x]
  (let [move (if (neg-int? x) counter-clockwise clockwise)]
    (loop [n (Math/abs ^long x)
           c circle]
      (if (pos-int? n)
        (recur (dec n) (move c))
        c))))

(comment
  (pp/pp)
  (pp/pprint (->circle 1))
  (insert (->circle 1) 3)
  (pp/pprint
    (-> (->circle 3)
        (insert 10)
        (insert 20)
        (insert 30)))
  (pp/pprint
    (-> (->circle 3)
        (insert 10)
        (insert 20)
        (insert 30)
        (rotate -1)
        (insert 40)))
  (pp/pprint
    (-> (->circle 3)
        (insert 10)
        (insert 20)
        (insert 30)
        (rotate 2)
        (extract)
        (rotate 1)
        (insert 40)))
  (pp/pprint (insert circle 1 10)))