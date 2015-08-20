(defn make-account [starting-balance secret]
  (let [balance (atom starting-balance)]

    (letfn [(withdraw [amount] (if (>= @balance amount) (swap! balance - amount) "Not enough money!"))
            (deposit [amount] (swap! balance + amount))]

      (fn [password operation amount]
        (if (= password secret)
          (condp = operation
            'withdraw (withdraw amount)
            'deposit (deposit amount)
            (assert false (str "Unexpected operation:" operation)))
          "Unauthorized access!"
          )))))

(def account-1 (make-account 100 "admin1"))
(def account-2 (make-account 100 "qwerty"))

(def operations-on-account-1 [["admin1" 'deposit 10] ["admin1" 'withdraw 10] ["admin1" 'withdraw 100] ["oops" 'withdraw 10]])
(def operations-on-account-2 [["qwerty" 'withdraw 101] ["yikes" 'deposit 10] ["qwerty" 'withdraw 60]])

(defn op->symbol [op]
  (condp = op
    'withdraw "-"
    'deposit "+"
    "?"))

(doseq [[pass op amount] operations-on-account-1]
  (println (str "Account 1 (" (op->symbol op) amount " $):") (account-1 pass op amount)))

(doseq [[pass op amount] operations-on-account-2]
  (println (str "Account 2 (" (op->symbol op) amount " $):") (account-2 pass op amount)))
