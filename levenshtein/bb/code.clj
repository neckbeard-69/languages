(defn levenshtein-distance [s1 s2]
  (let [m (count s1)
        n (count s2)
        ;; Create a matrix to store distances
        matrix (vec (map vec (repeat (inc m) (repeat (inc n) 0))))]
    ;; Initialize first row and column
    (loop [i 0
           matrix (assoc-in matrix [0 0] 0)]
      (if (< i (inc m))
        (recur (inc i) (assoc-in matrix [i 0] i))
        (loop [j 0
               matrix matrix]
          (if (< j (inc n))
            (recur (inc j) (assoc-in matrix [0 j] j))
            ;; Compute Levenshtein distance
            (loop [i 1
                   matrix matrix]
              (if (<= i m)
                (recur (inc i)
                       (loop [j 1
                              matrix matrix]
                         (if (<= j n)
                           (let [cost (if (= (nth s1 (dec i)) (nth s2 (dec j))) 0 1)]
                             (recur (inc j)
                                    (assoc-in matrix [i j]
                                              (min
                                               (inc (get-in matrix [(dec i) j]))              ;; Deletion
                                               (inc (get-in matrix [i (dec j)]))              ;; Insertion
                                               (+ (get-in matrix [(dec i) (dec j)]) cost))))) ;; Substitution
                           matrix)))
                (get-in matrix [m n])))))))))

(defn main [& args]
  (let [strings (vec args)
        n (count strings)
        distances (for [i (range n)
                        j (range n)
                        :when (not= i j)]
                    (levenshtein-distance (nth strings i) (nth strings j)))
        min-distance (apply min distances)]
    (println "times:" (* n (dec n)))
    (println "min_distance:" min-distance)))

(when (= *file* (System/getProperty "babashka.file"))
  (apply main *command-line-args*))

(comment
  (time
   (main "abcde" "abdef" "ghijk" "gjkl" "mno" "pqr" "stu" "vwx" "yz" "banana" "oranges"))
  ;; times: 110
  ;; min_distance: 2
  ;; "Elapsed time: 1.56575 msecs"
  :rcf)

