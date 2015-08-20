(defn make-account [starting-balance]
  (let [balance (atom starting-balance)]

    (letfn [(withdraw [amount] (if (>= @balance amount) (swap! balance - amount) "Not enough money!"))
            (deposit [amount] (swap! balance + amount))]

      (fn [operation amount]
        (condp = operation
          'withdraw (withdraw amount)
          'deposit (deposit amount)
          (assert false (str "Unexpected operation: " operation)))))))

(def account-1 (make-account 100))
(def account-2 (make-account 100))

(def operations-on-account-1 [['deposit 10] ['withdraw 10] ['withdraw 100] ['withdraw 10]])
(def operations-on-account-2 [['withdraw 101] ['deposit 10] ['withdraw 60]])

(defn op->symbol [op]
  (condp = op
    'withdraw "-"
    'deposit "+"
    "?"))

(doseq [[op amount] operations-on-account-1]
  (println (str "Account 1 (" (op->symbol op) amount " $):") (account-1 op amount)))

(doseq [[op amount] operations-on-account-2]
  (println (str "Account 2 (" (op->symbol op) amount " $):") (account-2 op amount)))
