;; Representing infinite stream of numbers with lazy sequences in clojure.
;; Thanks to the `lazy-seq` macro, we are not evaluating whole expression
;; but we are deferring execution until it is necessary.
;;
;; Thanks to that in examples below we are calculating only 10 numbers
;; from an infinite stream.
;;
;; One more remark - it is not necessary to create `delay` and `force`
;; because Clojure already has facilities for that (`lazy-seq` is only
;; an example - I have in mind e.g. `promise` and `future`).

(defn integers-from [n]
  (cons n (lazy-seq (integers-from (inc n)))))

(def integers (integers-from 1))

(defn divisible? [x y]
  (= (rem x y) 0))

(defn no-sevens []
  (filter #(not (divisible? % 7)) integers))

(println (take 10 integers))
(println (take 10 (no-sevens)))

;; Infinite stream of Fibonacci numbers.

(defn fib [a b]
  (cons a (lazy-seq (fib b (+ a b)))))

(println (take 10 (fib 0 1)))

;; Algorithm of Eratosthenes Sieve.

(defn sieve [stream]
  (cons (first stream)
        (lazy-seq (sieve (filter #(not (divisible? % (first stream)))
                                 (rest stream))))))

;; Infinite stream of prime numbers.

(def primes (sieve (integers-from 2)))

(println (take 10 primes))
