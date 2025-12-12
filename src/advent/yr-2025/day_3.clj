(ns advent.yr-2025.day-3
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (get-input 2025 3))

(defn parse-line [line]
  (mapv #(Integer/parseInt (str %)) line))

(defn get-joltage [numbers]
  (let [num1 (apply max (drop-last numbers))
        i1 (.indexOf numbers num1)
        other (drop (inc i1) numbers)
        num2 (apply max other)]
    (Integer/parseInt (str num1 num2))))

(def test-input
  "987654321111111
811111111111119
234234234234278
818181911112111")

(defn ans1 [input]
  (reduce +
          (map (comp get-joltage parse-line) (str/split-lines input))))

(defn shift-left [numbers left right]
  "Returns [largest index-of-largest] of input vector given left and right indices"
  (let [options (subvec numbers (inc left) (inc right))
        largest (apply max options)
        i (.indexOf options largest)
        i-shifted (+ left i 1)]
    ;; (tap> {:fn `shift-left
    ;;        :left left
    ;;        :right right
    ;;        :largest largest
    ;;        :i i
    ;;        :i-shifted i-shifted
    ;;        :options options})
    [largest i-shifted]))

(comment
  (shift-left [9 8 7 6 5 4 1 1 1 1] 1 6)
  )

(defn starting-indices [len nslots]
  (vec (drop (- len nslots) (range len))))

(defn shift-index [indices numbers slot]
  (let [
        ;; left (max (dec slot) 0)
        ;; get the index to stop looking at in the array
        ;; this is the index occupied by the previous slot, or -1 if this is the first - this ensures the first element is considered.
        ;; this is because we exclude the number in left from the search so we need to ensure that when there *is* no number 0 is counted.
        left (or (get indices (dec slot)) -1)
        right (get indices slot) ;; right is the position in the vector of the slot
        [largest new-index] (shift-left numbers left (get indices slot))]
    (assoc indices slot new-index)))

(comment
  (shift-index [5 6 7 8 9] [9 8 7 6 5 4 1 1 1 1] 0)
  (shift-index [0 6 7 8 9] [9 8 7 6 5 4 1 1 1 1] 1)
  (shift-index [0 1 7 8 9] [9 8 7 6 5 4 1 1 1 1] 2)
  )

(defn extract-joltage-2 [numbers]
  (loop [indices (starting-indices (count numbers) 12)
         [slot & slots] (range 12)]
    (if slot
      (recur (shift-index indices numbers slot)
             slots)
      (Long/parseLong (apply str (map #(get numbers %) indices))))))

(comments
 (shift-index [2 3 4] [2 3 1 2 4] 0)
 (shift-index [2 3 4] [2 3 1 2 4] 1)
 )

(defn ans2 [input]
  (reduce +
          (map (comp extract-joltage-2 parse-line) (str/split-lines input))))
