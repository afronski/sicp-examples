;; Helpers.

(defn front-ptr [q] (first @q))
(defn rear-ptr [q] (second @q))

(defn make-queue-internals [f r]
  (list f r))

(defn set-front-ptr! [q e]
  (if (nil? e)
    (reset! q (make-queue-internals (list) (rear-ptr q)))
    (reset! q (make-queue-internals e (rear-ptr q)))))

(defn set-rear-ptr! [q e]
  (reset! q (make-queue-internals (front-ptr q) e)))

(defn print-queue [q]
  (println (front-ptr q)))

;; Constructor.

(defn make-queue []
  (atom (make-queue-internals (list) nil)))

;; Selectors.

(defn empty-queue? [q]
  (empty? (front-ptr q)))

(defn front-queue [q]
  (if (empty-queue? q)
    (assert false "Accessing front of the empty queue.")
    (first (front-ptr @q))))

;; Modifiers.

(defn insert-queue! [q e]
  (if (empty-queue? q)
    (do (set-front-ptr! q (list e))
        (set-rear-ptr! q e))
    (do (reset! q (make-queue-internals (conj (front-ptr q) e) e))
        (set-rear-ptr! q e)))
  q)

(defn delete-queue! [q]
  (if (empty-queue? q)
    (assert false "Deleting from the empty queue.")
    (set-front-ptr! q (butlast (front-ptr q))))
  q)

;; Main program.

(def q1 (make-queue))

(print-queue q1)

(print-queue (insert-queue! q1 1))
(print-queue (insert-queue! q1 2))
(print-queue (insert-queue! q1 3))
(print-queue (insert-queue! q1 4))
(print-queue (insert-queue! q1 5))

(print-queue (delete-queue! q1))
(print-queue (delete-queue! q1))
(print-queue (delete-queue! q1))
(print-queue (delete-queue! q1))
(print-queue (delete-queue! q1))
