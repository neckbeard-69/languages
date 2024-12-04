(ns code
  (:gen-class))

(definterface IFib
  (^long fib [^long n]))

(deftype Fibonacci []
  IFib
  (fib [_ n]
    (if (<= n 1)
      n
      (+ (.fib _ (- n 1))
         (.fib _ (- n 2))))))

(def ^:private ^Fibonacci fibonacci (Fibonacci.))

(defn -main [& args]
  (let [^long u (parse-long (first args))
        r (loop [i 1
                 sum 0]
            (if (< i u)
              (recur (inc i) (+ sum (.fib fibonacci i)))
              sum))]
    (println r)))
