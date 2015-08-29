;; Data structure.

(defn make-interval [a b] [a b])

;; Exercise 2.7
(defn lower-bound [i] (first i))
(defn upper-bound [i] (second i))

;; Operations.

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;; Exercise 2.8
(defn sub-interval [x y]
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn div-interval [x y]
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; Exercise 2.9
(defn width [i]
  (/ (Math/abs (- (upper-bound i) (lower-bound i))) 2.0))

(defn print-interval [i]
  (println (str (lower-bound i)) ".." (str (upper-bound i))))

(defn print-interval-by-center-and-margin [c m]
  (println (str c) "+/-" (str m)))

;; Exercise 2.11
(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))

;; Exercise 2.12
(defn make-center-percent [c p]
  (let [w (* c (/ p 100.0))]
    (make-center-width c w)))

(defn percent [i]
  (* (/ (width i) (center i)) 100.0))

;; Constants.

(def interval (make-interval 2.0 2.5))

;; Main flow.

(println (width interval))
(print-interval-by-center-and-margin (center interval) (width interval))

(print-interval (make-center-width 2.0 1.0))

(print-interval interval)
(print-interval (add-interval interval (make-interval 1.0 1.5)))

(print-interval (make-center-percent 100 5))
(println (percent (make-center-percent 100 5)))
