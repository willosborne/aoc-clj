(ns advent.yr-2022.day-6
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]))

(def input (get-input 2022 6))

(defn all-different?
  ([arr n]
   (= n (count (into #{} arr))))
  ([arr]
   (all-different? arr 4)))

(comment
  (all-different? [1 2 3 4])
  (all-different? [1 1 3 4])
  )

(defn first-unique-window [n]
  (let [windows (partition n 1 input)
        with-indices (map vector (map #(+ n %)(range)) windows)
        matching (filter (fn [pair] (all-different? (second pair) n))
                         with-indices)]
    (first matching)))

(defn answer1 []
  (first-unique-window 4))

(defn answer2 []
  (first-unique-window 14))
