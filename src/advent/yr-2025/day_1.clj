(ns advent.yr-2025.day-1
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def input (get-input 2025 1))

(defn parse-direction [step]
  (* (case (first step)
     \R 1
     \L -1
     (print (first step)))
     (Integer/parseInt (apply str (rest step)))))

(comment
  (parse-direction "R21")
  (parse-direction "L21"))

(defn update [number instruction]
  (mod (+ number 100 (parse-direction instruction))
       100))

;; TODO multiple complete rotations
;; (defn zeroes-passed [number delta]
;;   (let [result (+ number delta)
;;         diff (Math/abs delta)]
;;     (cond
;;       (> diff 100) (Math/floorDiv diff 100)
;;       (< result 0) 1
;;       (> result 100) 1
;;       :else 0)))
;;
(defn zeros-passed-positive [number delta]
  ;; assume delta always positive
  (let [result (+ number delta)]
    (cond
      (= result 0) 1
      (< result 100) 0
      :else (inc (Math/floorDiv (- result 100) 100)))))

(defn zeroes-passed [number delta]
  (if (> delta 0)
    (zeros-passed-positive number delta)
    (zeros-passed-positive (- 100 number) (Math/abs delta))))


(comment
  (assert (= (zeroes-passed 1 -2) 1))
  (assert (= (zeroes-passed 99 2) 1))
  (assert (= (zeroes-passed 1 2) 0))
  (assert (= (zeroes-passed 99 1) 0))
  (assert (= (zeroes-passed 1 200) 2))
  (assert (= (zeroes-passed 55 -55) 1))
  (assert (= (zeroes-passed 50 1000) 10))
  )

(defn run-instructions [instructions]
  (loop [n 50
         [i & ins] instructions
         c 0]
    (if i
      (let [delta (parse-direction i)
            pz (zeroes-passed n delta)] ;; TODO
        (recur (update n i)
               ins
               (+ c pz)))
      c)))

(defn ans1 []
  (run-instructions (str/split-lines input)))
