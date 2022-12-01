(ns advent.yr-2022.day-1
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (get-input 2022 1))

(defn partition-nil [nums]
  (loop [groups []
         current-group []
         [n & ns] nums]
    (if n
      (recur groups
             (conj current-group n)
             ns)
      (if ns
        (recur (conj groups current-group)
               []
               ns)
        (conj groups current-group)))))

(defn partition-nil-2 [nums]
  (->> (partition-by #(= nil %) nums)
       (filter #(first %))))

(defn answer1 []
  (let [lines (str/split-lines input)
        numbers (map edn/read-string lines)
        groups (partition-nil-2 numbers)
        totals (map #(apply + %) groups)]
    (apply max totals)))


(defn answer2 []
  (let [lines (str/split-lines input)
        numbers (map edn/read-string lines)
        groups (partition-nil numbers)
        totals (map #(apply + %) groups)
        inorder (sort > totals)
        top-3 (take 3 inorder)]
    (apply + top-3)))
