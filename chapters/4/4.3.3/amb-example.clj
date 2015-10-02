(defn amb-let-helper [bindings body]
  (if (< 0 (count bindings))
    (let [[form expression] (take 2 bindings)
          more-bindings (drop 2 bindings)
          filtered-recurse (if (= :where (first more-bindings))
                             `(when ~(second more-bindings)
                                ~(amb-let-helper (drop 2 more-bindings) body))
                             (amb-let-helper more-bindings body))
          res (if  (and (seq? expression)
                        (= 'amb (first expression)))
                `(apply concat (for [~form ~(second expression)]
                                 ~filtered-recurse))
                `(let [~form ~expression]
                   ~filtered-recurse))]
      res)
    [body]))

(defmacro amb-let [bindings body]
  (amb-let-helper bindings body))

(defn permutations [set]
  (if (empty? set)
    [[]]
    (for [item set
          others (permutations (disj set item))]
      (conj others item))))

(defn triple []
  (amb-let [a (amb (range 1 100)) :where (> a 2)
            b (amb (range a 100))
            c (amb (range b 100))

            :where (= (+ (* a a)
                         (* b b))
                      (* c c))]
           [a b c]))

(println (triple))
