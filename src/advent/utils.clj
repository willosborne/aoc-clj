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

(defn do-for-neighbours)
