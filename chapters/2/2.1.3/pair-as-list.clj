;; Pair represented as a list.
;;
;; We are returning list, which
;; has only 2 elements
;;
;; As in the previous case,
;; this is a constructor.

(defn cons [x y]
  (list x y))

;; In that case, following
;; functions are selectors.

(defn car [z] (nth z 0))
(defn cdr [z] (nth z 1))

(println (car (cons 1 2))) ;; 1
(println (cdr (cons 1 2))) ;; 2
