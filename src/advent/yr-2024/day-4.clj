(ns advent.yr-2024.day-4
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [advent.core :refer [get-input]]
            ))

(defn count-single
  ([line regex]
   (+ (count (re-seq regex line))
      (count (re-seq regex (str/reverse line)))))
  ([line]
   (count-single line #"xmas")))

(comment
  (count-single "xmas")
  )

(defn count-horizontals [grid]
  (reduce + (map count-single grid)))

(defn count-horizontals-2 [grid]
  (reduce + (map #(count-single % #"MAS") grid)))

(comment
  (count-horizontals ["xmas"
                      "samx"])
  )

(defn transpose [grid]
  (apply mapv str grid))


(comment
  (transpose ["xmas"
              "xmas"
              "xmas"])
  )


(defn get-diagonal [grid start-x start-y]
  (for [i (iterate inc 0)
        :let [x (+ start-x i)
              y (+ start-y i)]
        :while (get (get grid y) x)]
    (get (get grid y) x)
    )


  )

(defn diagonals [grid]
  (mapv #(apply str %) (concat (for [y (range (count grid))]
             (get-diagonal grid 0 y))
           (for [x (range 1 (count (get grid 0)))]
             (get-diagonal grid x 0))))
  )

(def test-grid
  ["abc"
   "def"
   "ghi"])

(comment
  (diagonals test-grid)
  )

(defn rotate [grid]
  (mapv str/reverse (transpose grid)))

(comment
  (rotate ["abc"
           "def"
           "ghi"])
  (diagonals (rotate test-grid))
  )

(defn counts [grid]
  (let [horiz (count-horizontals grid)
        verts (count-horizontals (transpose grid))
        diags (diagonals grid)
        diags-count (count-horizontals diags)
        rotated (diagonals (rotate grid))
        diags-rotated-count (count-horizontals rotated)
        ]
    (+ horiz verts diags-count diags-rotated-count)
    )
  )

(comment

  (counts ["XMASXXXX"
           "MASXXMXX"
           "ASXMXXAX"
           "SXMAXXXS"])
  )

(defn answer1 []
  (counts (str/split-lines (get-input 2024 4)))
  )


(defn sub-grids [grid]
  (let [row-slices (into [] (partition 3 1 grid))
        transposed (map transpose row-slices)
        col-slices-transposed (apply concat (map #(apply vector (partition 3 1 %)) transposed))
        final (map transpose col-slices-transposed)]
    final
    ))


(comment
  (sub-grids ["12345"
              "54321"
              "67890"
              "23456"])
  )


(defn x-mas-count [grid]
  (quot (+ (count-horizontals-2 (diagonals grid))
           (count-horizontals-2 (diagonals (rotate grid))))
        2))


(comment
  (x-mas-count ["M M"
                " A "
                "S S"])
  )

(defn answer2 []
  (let [grid (str/split-lines (get-input 2024 4))
        sgs (sub-grids grid)
        counts (map x-mas-count sgs)]
    (reduce + counts)))
