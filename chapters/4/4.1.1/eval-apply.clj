(defn list-of-values [exps env]
  (if (no-operands? exps)
    (list)
    (cons (my-eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(defn my-eval-if [exp env]
  (if (true? (my-eval (if-predicate exp) env))
    (my-eval (if-consequent exp) env)
    (my-eval (if-alternative exp) env)))

(defn my-eval-sequence [exps env]
  (cond (last-exp? exps) (my-eval (first-exp exps) env)
        :else            (do (my-eval (first-exp exps) env)
                             (my-eval-sequence (rest-exps exps) env))))

(defn my-eval-assignment [exp env]
  (set-variable-value! (assignment-variable exp)
                       (my-eval (definition-value exp) env)
                       env)
  :ok)

(defn my-eval-definition [exp env]
  (define-variable! (definition-variable exp)
                    (my-eval (definition-value exp) env)
                    env)
  :ok)

(defn my-eval [exp env]
  (cond (self-evaluating? exp) exp
        (variable? exp)        (lookup-variable-value exp env)
        (quoted? exp)          (text-of-quotation exp)
        (assignment? exp)      (my-eval-assignment exp env)
        (definition? exp)      (my-eval-definition exp env)
        (if? exp)              (my-eval-if exp env)
        (lambda? exp)          (make-procedure (lambda-parameters exp)
                                               (lambda-body exp)
                                               env)
        (do? exp)              (my-eval-sequence (do-actions exp) env)
        (cond? exp)            (my-eval (cond->if exp) env)
        (application? exp)     (my-apply (my-eval (operator exp) env)
                                         (list-of-values (operands exp) env))

        :else                  (assert false "Unknown expression in `my-eval`.")))

(defn my-apply [proc args]
  (cond (primitive-procedure? proc) (my-apply-primitive-procedure proc args)
        (compound-procedure? proc)  (my-eval-sequence (procedure-body proc)
                                                      (extend-environment (procedure-parameters proc))
                                                      args
                                                      (procedure-environment proc))

        :else                       (assert false "Unknown procedure type in `my-apply`.")))
