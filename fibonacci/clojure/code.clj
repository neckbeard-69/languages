(ns code
  (:gen-class))

(set! *unchecked-math* :warn-on-boxed)

(defn- fibonacci ^long [^long n]
  ((fn fib ^long [^long n]
    (if (<= n 1)
      n
      (+ (long (fib (- n 1)))
         (long (fib (- n 2)))))) n))

(defn -main [& args]
  (let [u (long (parse-long (first args)))
        r (transduce (map fibonacci) + (range u))]
    (println r)))
