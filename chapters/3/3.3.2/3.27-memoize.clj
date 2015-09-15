;; Memoization technique.

(defn memoizer [f]
  (let [table (atom {})]
    (fn [x]
      (let [previous (get table x)]
        (if (nil? previous)
          (let [result (f x)]
            (swap! table assoc x result)
            result)
          previous)))))

;; Fibonacci sequence.

(defn fib [n]
  (condp = n
    0 0
    1 1
    (+ (fib (- n 1))
       (fib (- n 2)))))

(def memoized-fib (memoizer fib))

(println (fib 10))
(println (memoized-fib 10))

(time (fib 35))
(time (memoized-fib 35))
