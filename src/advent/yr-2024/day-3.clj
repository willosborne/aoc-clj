(ns advent.yr-2024.day-3
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [advent.core :refer [get-input]]
            ))

(defn extract-commands [line]
  (->> line
       (re-seq #"mul\((\d+),(\d+)\)")
       (map #(drop 1 %))
       (map #(mapv edn/read-string %))
       (map #(reduce * %))
       (reduce +)
       ))

(comment
  (extract-commands "mul(1,2)mul(1,2!mul(2,3)")

  )

;; (def lines (str/split-lines (get-input 2024 3)))


(defn answer1 []
  (->> (get-input 2024 3)
       (extract-commands)
     ))


(defn extract-commands-2 [line]
  (->> line
       (re-seq #"(mul\(\d+,\d+\)|don't\(\)|do\(\))")
       (mapv #(nth % 1))
       ))


(comment
  (extract-commands-2 "mul(1,2)don't()mul(1,2!do()mul(2,3)")
  )

(defn translate-command [command]
  (if-let [[[match arg1 arg2]] (re-seq #"mul\((\d+),(\d+)\)" command)]
    {:cmd :mul :val (* (edn/read-string arg1) (edn/read-string arg2))}
    {:cmd :cond :val (if (= command "don't()")
                       0
                       1)}))

(comment
  (translate-command "mul(1,2)")
  (translate-command "don't()")
  (translate-command "do()")
  )

(defn run-line [commands]
  (loop [total 0
         [c & cs] commands
         multiplier 1]
    (if (not c)
      total
      (case (:cmd c)
            :cond (recur total cs (:val c))
            :mul (recur (+ total (* multiplier (:val c)))
                        cs
                        multiplier)
           ))))


(comment
  (run-line [{:cmd :mul :val 10} {:cmd :mul :val 10}])
  (run-line [{:cmd :mul :val 10} {:cmd :cond :val 0} {:cmd :mul :val 10} {:cmd :mul :val 10}])
  (run-line [{:cmd :mul :val 10} {:cmd :cond :val 0} {:cmd :mul :val 10} {:cmd :cond :val 1} {:cmd :mul :val 10}])


  )

(defn answer2 []
  (->> (get-input 2024 3)
       (extract-commands-2)
       (map translate-command)
       (run-line)
       ))
