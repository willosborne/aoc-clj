(ns advent.yr-2024.day-6
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [advent.core :refer [get-input]]
            [advent.utils :as utils]
            ))

(def input (str/split-lines (get-input 2024 6)))

(defn is-obstacle? [grid x y]
  (= \# (get (get grid y) x)))

(defn is-outside? [grid x y]
  (not (get (get grid y) x)))

(defn rotate-90 [dx dy]
  [(- dy) dx])

(comment
  (map (partial apply rotate-90) [[1 0] [0 1] [-1 0] [0 -1]])
  )




(defn ray-hits-loop [grid sx sy dx dy visited]
  ;; (loop [x sx
  ;;        y sy]
  ;;   (let [nx (+ x dx)
  ;;         ny (+ y dy)]
  ;;     (if-not (is-outside? grid nx ny)
  ;;       (if (some #{[nx ny dx dy]} visited) ;; [x y] in visited
  ;;         true
  ;;         (recur nx ny)))))

  (loop [x sx
         y sy
         dx dx
         dy dy
         ]
    (if-let [[nx ny ndx ndy visited'] (step grid x y dx dy visited :skip)]
      (if (some #{[nx ny ndx ndy]} visited)
        true
        (recur nx ny ndx ndy))))
  )


;; (comment
;;   (ray-hits-visited-obstruction grid 0 1 1 0 [[4 1]])
;;   )

(defn step [grid x y dx dy visited obs]
  ;; (println x y dx dy)
  (let [new-x (+ x dx)
        new-y (+ y dy)
        [ndx ndy] (rotate-90 dx dy)]
    (cond
      (is-outside? grid new-x new-y) nil
      (is-obstacle? grid new-x new-y) [x y ndx ndy visited obs]
      :else [new-x
             new-y
             dx
             dy
             (conj visited [new-x new-y dx dy])
             ;; if the turn to the right would eventually hit a visited obstruction
             (if (and (not= obs :skip)
                      (ray-hits-loop grid x y ndx ndy visited))
               (conj obs [new-x new-y])
               obs)
             ]
      )))

(def grid
  ["...#..."
   ".....#."
   "..#^..."
   "......."])


;; (assert (= (step grid 0 0 1 0 [])
           ;; [1 0 1 0 [[1 0 1 0]]]))
;; (assert (= (step grid 2 0 1 0 [])
;;            [2 0 0 1 []]))
;; (assert (= (step grid 0 0 0 -1 [])
;;            nil))
(comment
  (step grid 3 2 0 -1 #{} #{})
  )


(defn walk-guard [grid sx sy start-dx start-dy]
  (loop [x sx
         y sy
         dx start-dx
         dy start-dy
         visited #{[sx sy start-dx start-dy]}
         obs #{}]
    (if-let [[nx ny ndx ndy visited' obs'] (step grid x y dx dy visited obs)]
      (recur nx ny ndx ndy visited' obs')
      [visited obs])))

(comment
  (walk-guard grid 3 2 0 -1)
  )

(defn find-guard [grid]
  )

(defn answer1 []
  (let [[sx sy] (utils/find-in-grid input "^")
        [visited obs] (walk-guard input sx sy 0 -1)
        unique (distinct visited)]
    (count unique)))


(def sample-input
  (str/split-lines
   "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."))

(defn answer2 []
  (let [input input
        [sx sy] (utils/find-in-grid input "^")
        [visited obs] (walk-guard input sx sy 0 -1)
        unique (distinct obs)]
    (count obs)))
