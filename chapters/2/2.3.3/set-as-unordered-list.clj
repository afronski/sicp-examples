;; Constructors.

(defn create-set [] (list))

;; Operations.

(defn element-of-set? [x set]
  (if (empty? set) false (boolean (some #(= x %) set))))

(defn adjoin-set [x set]
  (if (element-of-set? x set) set (conj set x)))

(defn intersection-set [set1 set2]
  (letfn [(intersection-set-internal [set1 set2 result]
            (cond (or (empty? set1) (empty? set2)) result
                  (element-of-set? (first set1) set2) (recur (rest set1) set2 (conj result (first set1)))
                  :else (recur (rest set1) set2 result)))]
    (intersection-set-internal set1 set2 (create-set))))

;; Exercise 2.59
(defn union-set [set1 set2]
  (letfn [(union-set-internal [set result]
            (cond (empty? set) result
                  (element-of-set? (first set) result) (recur (rest set) result)
                  :else (recur (rest set) (conj result (first set)))))]
    (let [after-first (union-set-internal set1 (create-set))]
          (union-set-internal set2 after-first))))

;; Main flow.

(def basic-set (adjoin-set 1 (adjoin-set 2 (adjoin-set 4 (adjoin-set 5 (create-set))))))
(def another-set (adjoin-set 2 (adjoin-set 1 (create-set))))

(println "Set:" basic-set)

(println "Contains 2:" (element-of-set? 2 basic-set))
(println "Contains 3:" (element-of-set? 3 basic-set))

(println "Sets intersection:" (intersection-set basic-set another-set))
(println "Sets intersection:" (intersection-set another-set basic-set))

(println "Sets union:" (union-set another-set basic-set))
(println "Sets union:" (union-set (adjoin-set 3 (create-set)) basic-set))
