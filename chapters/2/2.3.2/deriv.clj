;; Required selectors for extracting
;; data from assumed data structures.

(defn car [x] (first x))
(defn cdr [x] (rest x))
(defn cadr [x] (car (cdr x)))
(defn caddr [x] (car (cdr (cdr x))))

;; In our application pair is a:
;;   '(+ 1 2)

(defn pair? [x] (= (count x) 3))

;; Basic predicates.

(defn variable? [x]
  (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn =number? [exp num]
  (and (number? exp) (= exp num)))

;; Custom constructors for sum and product.

(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)))

;; Predicate which detects sum.

(defn sum? [x]
  (and (pair? x) (= (car x) '+)))

;; Selectors for addition.

(defn addend [s]
  (cadr s))

(defn augend [s]
  (caddr s))

;; Custom predicate which detects product.

(defn product? [x]
  (and (pair? x) (= (car x) '*)))

;; Selectors for multiplication.

(defn multiplier [p]
  (cadr p))

(defn multiplicand [p]
  (caddr p))

;; Actual algorithm for symbolic derivation.
;; Please note how declarative this approach is,
;; how recursion actually helps to handle subsequent
;; cases and where the simplification mechanism is.

(defn deriv [exp var]
  (cond (number? exp)
         0
        (variable? exp)
         (if (same-variable? exp var) 1 0)
        (sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))
        (product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))
        :else (assert false "Unknown expression type.")))

(println (deriv '(+ x 3) 'x))
(println (deriv '(* x y) 'x))
(println (deriv '(* (* x y) (+ x 3)) 'x))
