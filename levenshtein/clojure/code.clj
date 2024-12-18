(ns code
  (:gen-class))

(set! *unchecked-math* :warn-on-boxed)

(defn levenshtein-distance ^long [^String s1 ^String s2]
  (let [m (int (count s1))
        n (int (count s2))
        matrix (long-array (* (inc m) (inc n)))]
    (dotimes [i (inc m)]
      (aset matrix (* i (inc n)) i))
    (dotimes [j (inc n)]
      (aset matrix j j))
    (dotimes [i m]
      (dotimes [j n]
        (let [cost (if (= (.charAt s1 i) (.charAt s2 j)) 0 1)
              del (inc (aget matrix (+ (* i (inc n)) (inc j))))
              ins (inc (aget matrix (+ (* (inc i) (inc n)) j)))
              sub (+ (aget matrix (+ (* i (inc n)) j)) cost)
              idx (+ (* (inc i) (inc n)) (inc j))
              v (min del (min ins sub))]
          (aset matrix idx v))))
    (aget matrix (+ (* m (inc n)) n))))

(defn -main [& args]
  (let [strings (vec args)
        n (count strings)
        distances (for [i (range n)
                        j (range n)
                        :when (not= i j)]
                    (levenshtein-distance (nth strings i) (nth strings j)))
        min-distance (apply min distances)]
    (println "times:" (* n (dec n)))
    (println "min_distance:" min-distance)))