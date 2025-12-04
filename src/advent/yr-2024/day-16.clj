(ns advent.yr-2024.day-16
  (:require [clojure.string :as str]
            [advent.core :refer [get-input]]))

(def test-input
  "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############")


(defn parse-input [input]
  (let [lines (str/split-lines input)
        ]
    (into {}
          (for [y (range (count lines))
                x (range (count (first lines)))]
            {[x y] (get (get lines y) x)}))))

(defn neighbours [grid centre? diagonals? skip-cells]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :when (not (skip-cells (get (get grid y) x)))]
    [[x y]
     (for [dx [-1 0 1]
           dy [-1 0 1]
           :let [xx (+ x dx)
                 yy (+ y dy)
                 v (get (get grid yy) xx)]
           :when (not (or (and (not centre?)    (= dx dy 0))
                          (and (not diagonals?) (= dx dy))
                          (not v)
                          (skip-cells v)))]
       [[(+ x dx) (+ y dy)] v])]))


(defn initialise-distances [input]
  (into {}
        (map (fn [[coords ns]]
          ;; coords
          [coords
           (into {} (->> ns
                         (map first)
                         (map #(vector % Integer/MAX_VALUE))))]

          )
        input)))
