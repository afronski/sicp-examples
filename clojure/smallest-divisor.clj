(defn divides? [ a b ]
  (= (rem b a) 0))

(defn find-divisor [ n test-divisor ]
  (cond (> (* test-divisor test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (find-divisor n (+ test-divisor 1))))

(defn smallest-divisor [ n ]
  (find-divisor n 2))

(defn prime? [ n ]
  (= n (smallest-divisor n)))

(println (prime? 4))
(println (prime? 3))