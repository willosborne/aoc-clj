(ns advent.yr-2022.day-2
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]))

(def input (get-input 2022 2))

(def lines (str/split-lines input))

(defn parse-move [letter]
  (case letter
    "A" :rock
    "B" :paper
    "C" :scissors
    "X" :rock
    "Y" :paper
    "Z" :scissors))

(defn calc-result [move other-move]
  (if (= move other-move)
    :draw
    (case move
      :rock (case other-move
              :paper :lose
              :scissors :win)
      :paper (case other-move
               :rock :win
               :scissors :lose)
      :scissors (case other-move
                  :rock :lose
                  :paper :win))))

(defn calc-score [move other-move]
  (let [result (calc-result move other-move)
        result-score (case result
                       :win 6
                       :draw 3
                       :lose 0)
        shape-score (case move
                      :rock 1
                      :paper 2
                      :scissors 3)]
    (+ result-score shape-score)))

(defn parse-input [line]
  (mapv parse-move (str/split line #" ")))

(defn run-input [line]
  (apply calc-score (reverse (parse-input line))))

(defn answer-1 []
  (reduce + 0 (mapv run-input lines)))


;; part-2
(defn calc-move-target [other-move target]
  (case target
    :draw other-move
    :win (case other-move
           :rock :paper
           :paper :scissors
           :scissors :rock)
    :lose (case other-move
            :rock :scissors
            :paper :rock
            :scissors :paper)))

(defn parse-outcome [outcome]
  (case outcome
    "X" :lose
    "Y" :draw
    "Z" :win))

(defn parse-move-2 [line]
  (let [[fst snd] (str/split line #" ")
        other-move (convert-move fst)
        outcome (parse-outcome snd)
        my-move (calc-move-target other-move outcome)]
    [my-move other-move])) 

(defn run-input-2 [line]
  (apply calc-score (parse-move-2 line)))

(defn answer-2 []
  (reduce + 0 (mapv run-input-2 lines)))
