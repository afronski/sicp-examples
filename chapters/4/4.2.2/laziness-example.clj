(import java.util.UUID)

(defn uuid-seq []
  (lazy-seq (cons (str (UUID/randomUUID))
                  (uuid-seq))))

(println (take 3 (uuid-seq)))

(println (str (clojure.string/join (take 5 (repeat "Na "))) "Batman!"))
(println (repeatedly 5 #(rand-int 100)))

(println (take 5 (cycle [1 2 3])))
(println (take 5 (iterate (partial * 3) 1)))
