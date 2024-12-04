(ns code
  (:gen-class))

(defn -main [& args]
  (let [^long u (parse-long (first args)) ; Get an input number from the command line
        ^long r (rand-int 10000) ; Get a random number 0 <= r < 10k
        ^ints a (int-array 10000)] ; Array of 10k elements initialized to 0
    (loop [i 0]
      (when (< i 10000) ; 10k outer loop iterations
        (loop [j 0]
          (when (< j 100000) ; 100k inner loop iterations, per outer loop iteration
            (aset a i (+ (aget a i) (long (rem j u)))) ; Simple sum
            (recur (inc j))))
        (aset a i (+ (aget a i) r)) ; Add a random value to each element in array
        (recur (inc i))))
    (println (aget a r)))) ; Print out a single element from the array
