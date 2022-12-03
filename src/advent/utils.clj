(ns advent.utils)

(defn split-by-val [separator input]
  (->> (partition-by #(= separator %) input)
       (filter #(not (= separator (first %))))))
