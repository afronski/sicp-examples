;; Constructors.

(defn make-element [v prev next]
  (atom [v prev next]))

(defn make-deque []
  (atom [nil nil]))

;; Helpers.

(defn value [e] (nth @e 0))
(defn next-element [e] (nth @e 1))
(defn prev-element [e] (nth @e 2))

(defn front [dq] (first @dq))
(defn rear [dq] (second @dq))

(defn serialize-element [e]
  (str (value e) " <-> "))

(defn print-deque [dq]
  (letfn [(print-element [e]
            (if (nil? e)
              (print "END")
              (do (print (serialize-element e))
                  (recur (next-element e)))))]
    (print "BEGIN <-> ")
    (print-element (front dq))
    (print "\n")))

(defn set-front-ptr! [dq f]
  (reset! dq [f (rear dq)]))

(defn set-rear-ptr! [dq r]
  (reset! dq [(front dq) r]))

(defn change-next [e n]
  (reset! e [(value e) (prev-element e) n]))

(defn change-prev [e p]
  (reset! e [(value e) p (next-element e)]))

;; Selectors.

(defn empty-deque? [dq]
  (nil? (front dq)))

(defn front-deque [dq]
  (if (empty-deque? dq)
    (assert false "Trying to get front of an empty deque.")
    (value (front dq))))

(def rear-deque [dq]
  (if (empty-deque? dq)
    (assert false "Trying to get rear of an empty deque.")
    (value (rear dq))))

;; Modifiers.

(defn front-insert-deque! [dq v]
  (let [new (make-element v nil)]
    (if (empty-deque? dq)
      (do (set-front-ptr! dq new)
          (set-rear-ptr! dq new))
      (do (change-prev (front dq) new)
          (change-next new (front dq))
          (set-front-ptr! dq new))))
  dq)

(defn rear-insert-deque! [dq v]
  (let [new (make-element v nil)]
    (if (empty-deque? dq)
      (do (set-front-ptr! dq new)
          (set-rear-ptr! dq new))
      (do (change-next (rear dq) new)
          (change-prev new (rear dq))
          (set-rear-ptr! dq new))))
  dq)

(defn front-delete-deque! [dq])
(defn rear-delete-deque! [dq])

;; Main program.

(def dq1 (make-deque))

(print-deque dq1)

(print-deque (front-insert-deque! dq1 1))
(print-deque (front-insert-deque! dq1 2))
(print-deque (front-insert-deque! dq1 3))
(print-deque (rear-insert-deque! dq1 -3))
(print-deque (rear-insert-deque! dq1 -2))
(print-deque (rear-insert-deque! dq1 -1))
