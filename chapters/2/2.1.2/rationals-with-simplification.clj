(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn make-rat [n d] [n d])

(defn numer [x]
  (let [g (gcd (first x) (second x))]
    (/ (first x) g)))

(defn denom [x]
  (let [g (gcd (first x) (second x))]
    (/ (second x) g)))

(defn print-rat [x]
  (println (str (numer x)) "/" (str (denom x))))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(def one-half (make-rat 1 2))
(def one-third (make-rat 1 3))
(def two-thirds (make-rat 2 3))
(def three-fourths (make-rat 3 4))

;; Main flow.

(print-rat one-half)
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))
