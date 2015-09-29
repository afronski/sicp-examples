;; You can either evaluate quoted expressions
;; or strings, but keep in mind that string
;; does not have an AST-like structure by itself.
;; It needs to be parsed first (with a
;; `read-string`).

(eval '(let [a 10] (+ 3 4 a)))
(eval (read-string "(+ 1 1)"))

;; Result of executing both expressions is
;; exactly the same, but only the first one
;; is an application.
;;
;; Function application means that you have to
;; deliver all arguments upfront in a form of
;; collection.

(apply str ["str1" "str2" "str3"])
(str "str1" "str2" "str3")
