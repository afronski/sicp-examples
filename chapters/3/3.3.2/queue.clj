;; Constructors.

(defn make-element [e next]
  (atom [e next]))

(defn make-queue []
  (atom [nil nil]))

;; Helpers.

(defn value [e] (first @e))
(defn next-element [e] (second @e))

(defn front [q] (first @q))
(defn rear [q] (second @q))

(defn serialize-element [e]
  (str (value e) " <- "))

(defn print-queue [q]
  (letfn [(print-element [e]
            (if (nil? e)
              (print "END")
              (do (print (serialize-element e))
                  (recur (next-element e)))))]
    (print-element (front q))
    (print "\n")))

(defn set-front-ptr! [q f]
  (reset! q [f (rear q)]))

(defn set-rear-ptr! [q r]
  (reset! q [(front q) r]))

(defn change-next [e n]
  (reset! e [(value e) n]))

;; Selectors.

(defn empty-queue? [q]
  (nil? (front q)))

(defn front-queue [q]
  (if (empty-queue? q)
    (assert false "Trying to get front of an empty queue.")
    (value (front q))))

;; Modifiers.

(defn insert-queue! [q v]
  (let [new (make-element v nil)]
    (if (empty-queue? q)
      (do (set-front-ptr! q new)
          (set-rear-ptr! q new))
      (do (change-next (rear q) new)
          (set-rear-ptr! q new))))
  q)

(defn delete-queue! [q]
  (if (empty-queue? q)
    (assert false "Deleting element from an empty queue.")
    (set-front-ptr! q (next-element (front q))))
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
