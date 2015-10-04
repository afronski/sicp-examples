(ns logic-example.core
  (:use [clojure.core.logic.pldb]))

; In the logic programming we are creating *relations* and *facts*.
; Relation describes how to interpret *facts*, with certain associations.

(db-rel father Father Child)
(db-rel mother Mother Child)

; *Facts* are the truths, nothing more than a specific data structure
; which describes our state of knowledge.

(def genealogy
  (db
   [father 'Adam 'Wiliam]
   [father 'Adam 'Thomas]
   [father 'Andrew 'Jessica]
   [father 'Andrew 'Mark]
   ; We are deliberately omitting Dorothy's father here.

   [mother 'Eve 'Wiliam]
   [mother 'Eve 'Thomas]
   [mother 'Eve 'Jessica]
   [mother 'Angie 'Mark]
   [mother 'Angie 'Dorothy]))

; Having *facts* and *relations* we can query them and thanks to them
; `unification` mechanism, based on defined relations and facts available
; in the database our logic engine will answer to that query with one,
; more or no results.

(defn jessica-mother[]
  (with-db genealogy
    (run* [q]
      (mother q 'Jessica))))

; user=> (logic-example.core/jessica-mother)
; (Eve)

(defn adam-children []
  (with-db genealogy
      (run* [q]
        (father 'Adam q))))

; user=> (logic-example.core/dorothy-father)
; (Thomas Wiliam)

(defn dorothy-father []
  (with-db genealogy
    (run* [q]
      (father q 'Dorothy))))

; user=> (logic-example.core/dorothy-father)
; ()
