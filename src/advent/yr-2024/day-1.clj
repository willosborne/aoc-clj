(ns advent.yr-2024.day-1
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (get-input 2024 1))

(defn parse [input]
  (map #(str/split % #"   ") (str/split-lines input)))


(defn diff [a b]
  (Math/abs (- a b)))

(defn answer1 []
  (let [lines (parse (get-input 2024 1))
        left (map (comp edn/read-string first) lines)
        right (map (comp edn/read-string second) lines)
        left-s (sort left)
        right-s (sort right)
        diffs (map diff left-s right-s)
        ]
    (reduce + diffs)
       ))



(defn similarity [num freqs]
  (* num (or (get freqs num) 0)))

(defn answer2 []
  (let [lines (parse (get-input 2024 1))
        left (map (comp edn/read-string first) lines)
        right (map (comp edn/read-string second) lines)
        counts (frequencies right)
        scores (map #(similarity % counts) left)
        ]

    (reduce + scores)
    ;; (reduce + (filter some? scores))
    )

  )
