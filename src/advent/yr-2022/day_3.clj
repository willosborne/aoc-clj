(ns advent.yr-2022.day-3
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]
            [clojure.set :as set]))


(def input (get-input 2022 3))

(def lines (str/split-lines input))

(defn get-compartments [line]
  (let [len (count line)
        half (/ len 2)]
    [(subs line 0 half)
     (subs line half)]))

(defn common-letters [one two]
  (let [set1 (into #{} one)
        set2 (into #{} two)]
    (set/intersection set1 set2)))

(defn score-letter [letter]
  (let [val (int letter)
        scaled (- val 96)]
    (if (> scaled 0)
      scaled
      (+ scaled 32 26))))

(defn process-line [line]
  (let [[a b] (get-compartments line)
        common (common-letters a b)]
    (apply score-letter common)))

(defn answer-1 []
  (reduce + 0 (map process-line lines)))


(defn get-group-badge [group]
  (let [sets (mapv #(into #{} %) group)
        common (apply set/intersection sets)]
    (first common)))

(defn answer-2 []
  (let [groups (partition-all 3 lines)
        badges (mapv get-group-badge groups)
        scores (mapv score-letter badges)]
    (apply + scores)))
