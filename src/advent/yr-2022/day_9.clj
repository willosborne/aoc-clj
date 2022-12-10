(ns advent.yr-2022.day-9
  (:require [advent.core :refer [get-input guess]]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(str/split % #" "))
       (map (fn [[dir amount]]
              [dir (edn/read-string amount)]))))

(def input
  (parse-input (get-input 2022 9)))

(defn new-head-position [last-position direction]
  (let [[x y] last-position]
    (case direction
      "U" [x (dec y)]
      "D" [x (inc y)]
      "L" [(dec x) y]
      "R" [(inc x) y]
      (throw (RuntimeException. "bad input")))))

(defn distance [p q]
  (let [[px py] p
        [qx qy] q]
    (max (Math/abs (- px qx))
         (Math/abs (- py qy)))))

(defn point-behind-head [last-tail-pos new-head-pos]
  (let [[px py] last-tail-pos
        [qx qy] new-head-pos
        dx (- qx px)
        dy (- qy py)
        ax (Math/abs dx)
        ay (Math/abs dy)]
    (if (= ax ay)
      [(- qx (Integer/signum dx))
       (- qy (Integer/signum dy))]
      (if (> ax ay)
        [(- qx (Integer/signum dx))
         qy]
        [qx
         (- qy (Integer/signum dy))]))))

(defn next-body-position [current-pos last-head-pos new-head-pos]
  (if (or (= current-pos new-head-pos) (<= (distance current-pos new-head-pos) 1))
    current-pos
    (point-behind-head current-pos new-head-pos)))

(defn update-chain [chain direction]
  (let [[head & rest] chain
        new-head-pos (new-head-position head direction)]
    (loop [[tail & tails] rest
           last-head-pos head
           new-head-pos new-head-pos
           new-chain [new-head-pos]]
      (if (not tail)
        new-chain
        (recur tails
               new-head-pos
               (next-body-position tail last-head-pos new-head-pos)
               (conj new-chain (next-body-position tail last-head-pos new-head-pos)))))))

;; TODO this is crap, it sometimes produces an off by one error 
(defn run-commands [start-chain directions]
  (let [tail-locations (atom [])]
    (reduce (fn [chain direction]
              (swap! tail-locations conj (last chain))
              (update-chain chain direction))
            start-chain
            directions)
    (distinct @tail-locations)))

(defn flatten-commands [commands]
  (reduce (fn [out [dir num]]
            (into out (repeat num dir)))
          []
          commands))

(def test-input (parse-input "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"))

(def test-input-2 (parse-input "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20"))

(defn run-chain [input start-chain]
  (let [commands (flatten-commands input)]
    (run-commands start-chain commands)))

(def answer-1 (inc (count (run-chain input [[0 0] [0 0]]))))

(def answer-2 (inc (count (run-chain input (into [] (repeat 10 [0 0]))))))

