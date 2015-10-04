; Both `amb-let` and `amb-let-helper` implementations
; are shamelessly taken from:
;   https://github.com/abeppu/toychest

(defn amb-let-helper [bindings body]
  (if (< 0 (count bindings))
    (let [[form expression] (take 2 bindings)
          more-bindings (drop 2 bindings)

          filtered-recurse (if (= :where (first more-bindings))
                             `(when ~(second more-bindings)
                                ~(amb-let-helper (drop 2 more-bindings) body))
                             (amb-let-helper more-bindings body))

          res (if (and (seq? expression)
                       (= 'amb (first expression)))
                `(apply concat (for [~form ~(second expression)]
                                 ~filtered-recurse))
                `(let [~form ~expression]
                   ~filtered-recurse))]
      res)
    [body]))

; Macro definition.

(defmacro amb-let [bindings body]
  (amb-let-helper bindings body))

; Defining problem and its constraints.
; We would like to calculate all triples in range 100 that
; fullfilling following conditions:
;
;   2 < a < MAX
;   a <= b < MAX
;   b <= c < MAX
;
;   a^2 + b^2 = c^2

(defn triple [max]
  (amb-let [a (amb (range 1 max)) :where (> a 2)
            b (amb (range a max))
            c (amb (range b max))

            :where (= (+ (* a a) (* b b))
                      (* c c))]
           [a b c]))

(println (triple 20))
