(defn sum [ term a next b ]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(defn cube [ n ]
  (* n n n))

(defn incr [ n ]
  (+ n 1))

(defn sum-cubes [ a b ]
  (sum cube a incr b))

(println (sum-cubes 1 10))

(defn pi-sum [ a b ]
  (defn pi-term [ x ]
    (/ 1.0 (* x (+ x 2))))
  (defn pi-next [ x ]
    (+ x 4))
  (sum pi-term a pi-next b))

(println (* 8 (pi-sum 1 1000)))

(defn integral [ f a b dx ]
  (defn add-dx [ x ] (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(println (integral cube 0 1 0.001))


