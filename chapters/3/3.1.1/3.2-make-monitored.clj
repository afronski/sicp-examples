(defn make-monitored [f]
  (let [counter (atom 0)]
      (fn [arg]
        (condp = arg
          'reset-count (reset! counter 0)
          'how-many-calls? @counter
          (do (swap! counter inc) (f arg))))))

;; Unfortunately you cannot pass directly
;; Java method here, even a static one.
;;
;; You need to wrap it in a Clojure
;; function first.

(def sqrt (make-monitored (fn [x] (Math/sqrt x))))

(println (sqrt 100))
(println (sqrt 'how-many-calls?))
(println (sqrt 25))
(println (sqrt 'how-many-calls?))
(println (sqrt 'reset-count))
(println (sqrt 'how-many-calls?))
