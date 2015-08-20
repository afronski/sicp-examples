(def max-violations 7)

(defn call-the-cops []
   (assert false "Drop the gun! You are under arrest!"))

(defn make-account [starting-balance secret]
  (let [balance (atom starting-balance)
        violation-counter (atom 0)]

    (letfn [(withdraw [amount] (if (>= @balance amount) (swap! balance - amount) "Not enough money!"))
            (deposit [amount] (swap! balance + amount))]

      (fn [password operation amount]
        (if (= password secret)
          (condp = operation
            'withdraw (withdraw amount)
            'deposit (deposit amount)
            (assert false (str "Unexpected operation:" operation)))

          (if (>= @violation-counter max-violations)
            (call-the-cops)
            (do (swap! violation-counter + 1) "Unauthorized access!")))))))

(def account (make-account 100 "admin1"))

(doseq [i (range 1 10)]
  (println (str "Account (-" i " $):") (account "nope" 'withdraw i)))
