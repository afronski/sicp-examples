(defn make-accumulator [start]
  (let [acc (atom start)]
    (fn [x]
      (swap! acc + x))))

(def A (make-accumulator 5))

(println "Accumulator:" (A 10))
(println "Accumulator:" (A 10))
