(ns advent.utils)

(defn split-by-val [separator input]
  (->> (partition-by #(= separator %) input)
       (filter #(not (= separator (first %))))))

(defn find-in-grid [grid element]
  (loop [y 0
         [r & rs] grid]
    (if-not r
      nil
      (let [x (.indexOf r element)]
        (if (= x -1)
          (recur (inc y) rs)
          [x y])))))

(defn grid-get [grid x y]
  (get (get grid y) x))

(defn filter-grid [pred grid]
  (for [x (range (count (get grid 0)))
        y (range (count grid))
        :when (pred (grid-get grid x y))]
    [x y (grid-get grid x y)]))

(defn get-neighbours [grid x y]
  (for [dx (range -1 2)
        dy (range -1 2)
        :when (not (and (= dx 0) (= dy 0)))]
    (let [nx (+ x dx)
          ny (+ y dy)]
      {:x nx
       :y ny
       :val (grid-get grid nx ny)})))
