(ns code
  (:gen-class))

(defn- fibonacci ^long [^long n]
  (loop [a 0
         b 1
         i 0]
    (if (< i n)
      (recur b (+ a b) (inc i))
      a)))

(defn -main [& args]
  (let [^long u (parse-long (first args))
        ^long r (loop [i 1
                       sum 0]
                  (if (< i u)
                    (recur (inc i) (+ sum (fibonacci i)))
                    sum))]
    (println r)))
