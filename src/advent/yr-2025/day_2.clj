(ns advent.yr-2025.day-2
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (get-input 2025 2))

(defn expand-range [r]
  (let [[start end] (str/split r #"-")]
    (range (Long/parseLong start) (inc (Long/parseLong end)))))

(defn split-half [s]
  (let [l (count s)
        s1 (subs s 0 (/ l 2))
        s2 (subs s (/ l 2) l)]
    [s1 s2]))

(defn valid? [num]
  (let [numstr (str num)]
    (not (apply = (split-half numstr)))))

(defn valid-any? [num]
  (let [numstr (str num)
        divisors (range 1 (count numstr)) ;; could limit at n/2
        chunks (map (fn [n]
                      (partition n n [\x] numstr)) ;; split into all possible sets of partitions, pad with x
                    divisors)]
    (some (fn [elems] (apply = elems)) chunks))) ;; if they're all equal we are good

(defn ans1 []
  (let [rangestrs (str/split (apply str (drop-last input)) #",")
        ranges (map expand-range rangestrs)
        counts (map #(filter valid? %) ranges)]
    (apply + (map #(apply + %) counts))))

(defn run-ans2 [input]
  (let [rangestrs (str/split (apply str (drop-last input)) #",")
        ranges (map expand-range rangestrs)
        counts (map #(filter valid-any? %) ranges)]
    (apply + (map #(apply + %) counts))))

(def test-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124\n")

(defn ans2 []
  (run-ans2 input))
