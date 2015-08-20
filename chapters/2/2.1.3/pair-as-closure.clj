;; Pair represented as a closure.
;;
;; We are returning new function, which
;; accepts only 0 or 1 as an index value.
;;
;; This is a constructor which is first part
;; of our barrier.

(defn cons [x y]
  (fn [m]
    (cond (= m 0) x
          (= m 1) y
          :else (assert (or (= m 1) (= m 0)) "Argument should be 0 or 1."))))

;; Those functions are selectors, second
;; part of our barrier.

(defn car [z] (z 0))
(defn cdr [z] (z 1))

(println (car (cons 1 2))) ;; 1
(println (cdr (cons 1 2))) ;; 2

(println ((cons 1 2) 3)) ;; 2
