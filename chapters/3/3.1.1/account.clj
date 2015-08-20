(defn make-withdraw [starting-balance]
  (let [balance (atom starting-balance)]
    (fn [amount]
      (if (>= @balance amount)
        (swap! balance - amount)
        "Not enough money!"))))

(def withdraw-from-account-1 (make-withdraw 100))
(def withdraw-from-account-2 (make-withdraw 100))

(def operations-on-account-1 [10 10 10 10 60 10 10 20 10])
(def operations-on-account-2 [101 10 60])

(doseq [op operations-on-account-1]
  (println (str "Account 1 (-" op "):") (withdraw-from-account-1 op)))

(doseq [op operations-on-account-2]
  (println (str "Account 2 (-" op "):") (withdraw-from-account-2 op)))
