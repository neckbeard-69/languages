(ns code
  (:gen-class))

(defn -main [& args]
  (let [fibonacci (fn fib ^long [^long n]
                    (if (<= n 1)
                      n
                      (+ (long (fib (- n 1)))
                         (long (fib (- n 2))))))
        ^long u (parse-long (first args))
        ^long r (loop [i 1
                       sum 0]
                  (if (< i u)
                    (recur (inc i) (long (+ sum (long (fibonacci i)))))
                    sum))]
    (println r)))
