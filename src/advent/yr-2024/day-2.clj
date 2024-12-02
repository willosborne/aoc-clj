(ns advent.yr-2024.day-2
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [advent.core :refer [get-input]]
            ))

(defn parse-report [line]
  (mapv edn/read-string (str/split line #" ")))


(defn permutations-1-removed [report]
  (for [i (range (count report))]
    (into
          (subvec report 0 i)
          (subvec report (inc i)))))

(comment
  (permutations-1-removed [0 1 2 3 4])
  )

(defn monotonic? [report]
  (let [sorted (sort report)]
    (or (= report sorted)
        (= report (reverse sorted)))))


(comment

  (monotonic? [1 2 2 3 4 10])
  (monotonic? [10 9 8 6 1 0])
  (monotonic? [1 7 2 3 2 0])
  )

(defn diff-vectors [v1 v2]
  (filter some?
          (map (fn [e1 e2]
                 (and e1 e2 (Math/abs (- e1 e2))))
               v1 v2)))

(comment
  (diff-vectors [1 2 3] [4 5 6])
  (diff-vectors [1 2 3] [nil 1 2])
  (diff-vectors [1 2 3] [2 3])
  )

(defn safe-diff? [report min-diff max-diff num-bad]
  (let [shifted-left (drop 1 report)
        diffs-left (diff-vectors report shifted-left)]
    (every? #(<= min-diff % max-diff) diffs-left)))

(comment
  (safe-diff? [1 2 3 4] 1 3)
  (safe-diff? [1 10 9 7] 1 3)
  (safe-diff? [1 2 5 0] 1 3)

  )

(defn valid? [report]
  (and (monotonic? report)
       (safe-diff? report 1 3)))

(defn answer1 []
  (let [input (get-input 2024 2)
        reports (map parse-report (str/split-lines input))]
    (count
     (filter valid?
             reports))))


(defn answer2 []
  (let [input (get-input 2024 2)
        reports (map parse-report (str/split-lines input))]
    (count
     (filter (fn [report]
               (or (valid? report)
                   (some valid? (permutations-1-removed report))))
             reports))))
