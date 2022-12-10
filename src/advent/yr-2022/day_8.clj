(ns advent.yr-2022.day-8
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (get-input 2022 8))

(def rows
  (let [lines (str/split-lines input)]
    (mapv (fn [line]
            (mapv (comp edn/read-string str) line))
          lines)))


(defn scan-r [row]
  (loop [[tree & trees] row
         i 0
         last-highest -1
         visible-indices []]
    (if (not tree)
      visible-indices
      (if (> tree last-highest)
        (recur trees
               (inc i)
               tree
               (conj visible-indices i))
        (recur trees
               (inc i)
               last-highest
               visible-indices)))))

(comment
  (scan-r [1 1 2 1 4 3 2 5 1 0])
  ;; => [0 2 4 7]
  )

(defn invert-indices [indices max-val]
  (mapv #(- max-val %) indices))

(comment
  (invert-indices [0 1 2 4] 5)
  ;; => [5 4 3 1]

  )

(defn visible-x [row y-value]
  (let [visible-r (scan-r row)
        row-reversed (reverse row)
        visible-l-reversed (scan-r row-reversed)
        visible-l (invert-indices visible-l-reversed (dec (count row)))
        unique (distinct (concat visible-r visible-l))]
    (mapv #(vector % y-value) unique)))

(comment
  (visible-x [1 2 4 5 1 1 5 5 3 1] 4)
  ;;          0 1 2 3 4 5 6 7 8 9
  ;; => [[0 4] [1 4] [2 4] [3 4] [9 4] [8 4] [7 4]]

  )

(defn transpose [rows]
  (apply map vector rows))

(comment
  (get-col [[1 2 3]
            [4 5 6]
            [7 8 9]] 2)
  ;; => [3 6 9]
  )

(defn visible-y [col x-value]
  (mapv reverse (visible-x col x-value)))

(comment
  (visible-y [1 2 4 5 1 1 5 5 3 1] 4)
  ;;          0 1 2 3 4 5 6 7 8 9
  ;; => [(4 0) (4 1) (4 2) (4 3) (4 9) (4 8) (4 7)]
  )

(defn visible-rows [rows]
  (apply concat
         (map-indexed (fn [y row]
                        (visible-x row y))
                      rows)))

(defn visible-cols [rows]
  (let [transposed (transpose rows)]
    (apply concat
           (map-indexed (fn [x col]
                          (visible-y col x))
                        transposed))))


(defn visible [rows]
  (let [visibles-x (visible-rows rows)
        visibles-y (visible-cols rows)]
    (count (distinct (concat visibles-x visibles-y)))))

(defn answer-1 []
  (visible rows))

(defn scan-r-le [row start]
  (loop [[tree & trees] row
         visible-count 0]
    (if (not tree)
      visible-count
      (if (< tree start)
        (recur trees
               (inc visible-count))
        (inc visible-count)))))

(defn scan-from-centre [row start]
  (let [[before after] (split-at start row)
        start-height (nth row start)
        after-count (scan-r-le (drop 1 after) start-height)
        before-count (scan-r-le (reverse before) start-height)]
    (* (max 1 before-count) (max 1 after-count))))

(comment
  (scan-from-centre [3 3 5 4 9] 2)
  (scan-from-centre [3 3 5 4 9] 0)
  )


(defn visible-trees-x-y [rows cols x y]
  (let [row (nth rows y)
        col (nth cols x)
        score-x (scan-from-centre row x)
        score-y (scan-from-centre col y)]
    (* (max 1 score-x) (max 1 score-y))))

(comment
  (visible-trees-x-y test-rows (transpose test-rows)
                     0 0)
  ;; => 9
  )


(defn answer-2 []
  (let [cols (transpose rows)
        scores (for [x (range (count cols))
                     y (range (count rows))]
                 (visible-trees-x-y rows cols x y))]
    (apply max scores)))
