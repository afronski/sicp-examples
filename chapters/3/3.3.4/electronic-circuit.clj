;; Helpers.

(defn invoke-all [procedures]
  (doall (map #(%) procedures)))

;; Signals.

(defn get-signal [wire] (wire :get-signal))
(defn set-signal! [wire v] (wire :set-signal! v))
(defn add-action! [wire p] (wire :add-action! p))

;; Wires.

(defn make-wire []
  (let [signal (atom false)
        effects (atom (list))]
    (letfn [(set-signal! [new]
              (if (not (= signal new))
                (do (swap! signal new)
                    (invoke-all @effects))
                :done))

            (accept-action! [procedure]
              (swap! effects conj procedure)
              (procedure))

            (dispatch [action]
              (condp = action
                :get-signal @signal
                :set-signal! set-signal!
                :add-action! add-action!
                assert false (str "Unknown operation " action " in make-wire.")))]
      dispatch)))

;; Agenda.

(defn make-time-segment [time queue]
  (atom [time queue]))

(defn segment-time [segment]
  (first @segment))

(defn segment-queue [segment]
  (second @segment))

(defn delete-queue! [segment]
  (reset! segment [(segment-time segment) nil]))

(defn make-agenda []
  (atom [0 nil])

(defn current-time [agenda]
  (first @agenda))

(defn segments [agenda]
  (second @agenda))

(defn set-current-time! [agenda t]
  (reset! agenda [t (segments agenda)]))

(defn set-segments! [agenda s]
  (reset! agenda [(current-time agenda) s]))

(defn first-segment [agenda]
  (first (segments agenda)))

(defn rest-segments [agenda]
  (rest (segments agenda)))

(defn empty-agenda? [agenda]
  (nil? (segments agenda)))

;; FIXME: Use more advanced Clojure data structures.
;; FIXME: Maybe also agent?

(defn add-to-agenda! [time action agenda]
  (letfn [(belongs-before? [segments]
            (or (nil? segments)
                (< time (segment-time (first segments)))))

          (add-to-segments! [segments]
            (if (= (segment-time (first segments)) time)
              (conj action (segment-queue (first segments)))
              (let [rest-segments (rest segments)]
                (if (belongs-before? rest-segments)
                  (reset! segments conj (make-time-segment time [action]) rest-segments)
                  (add-to-segments! rest-segments)))))]
    (let [segments (segments agenda)]
      (if (belongs-before? segments)
        (set-segments!
         agenda
         (conj (make-time-segment time [action]) segments))
        (add-to-segments! segments)))))

(defn remove-first-agenda-item! [agenda]
  (let [q (segment-queue (first-segment agenda))]
    (delete-queue! q)
    (when (empty-agenda? q)
      (set-segments! agenda (rest-segments agenda)))))

(defn first-agenda-item [agenda]
  (if (empty-agenda? agenda)
    (assert false "Agenda is empty.")
    (let [first-segment (first-segment agenda)]
      (set-current-time! agenda (segment-time first-segment))
      (first (segment-queue first-segment)))))

(def the-agenda (make-agenda))

(defn step []
  (if (empty-agenda? the-agenda)
    :done
    (let [first-item (first-agenda-item the-agenda)]
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (recur))))

;; Propagation and probes.

(defn propagation [delay action]
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(defn probe [name wire]
  (add-action! wire (fn ()
                      (println (str name " " (current-time the-agenda) " New value = " (get-signal wire))))))

;; Gates.

(def not-gate-delay 2)
(def and-gate-delay 3)
(def or-gate-delay 5)

(defn not-gate [input output]
  (letfn [(not-input []
            (let [new (not (get-signal input))]
              (propagation not-gate-delay #(set-signal! output new))))]
    (add-action! input invert-input)
    :ok))

(defn and-gate [a1 a2 output]
  (letfn [(and-input []
            (let [new (and (get-signal a1) (get-signal a2))]
              (propagation and-gate-delay #(set-signal! output new))))]
    (add-action! a1 and-input)
    (add-action! a2 and-input)
    :ok))

;; Exercise 3.28

(defn or-gate [a1 a2 output]
  (letfn [(or-input []
            (let [new (or (get-signal a1) (get-signal a2))]
              (propagation or-gate-delay #(set-signal! output new))))]
    (add-action! a1 or-input)
    (add-action! a2 or-input)
    :ok))

;; Adders.

(defn half-adder [a b s c]
  (let [d (make-wire)
        e (make-wire)]
    (or-gate a b d)
    (and-gate a b c)
    (not-gate c e)
    (and-gate d e s)
    :ok))

(defn full-adder [a b c-in sum c-out]
  (let [s (make-wire)
        c1 (make-wire)
        c2 (make-wire)]
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    :ok))

;; Simulation.

(defn input-1 (make-wire))
(defn input-2 (make-wire))
(defn sum (make-wire))
(defn carry (make-wire))

(probe :sum sum)
(probe :carry carry)

(half-adder input-1 input2 sum carry)

(set-signal! input-1 true)
(step)

(set-signal! input-2 true)
(step)
