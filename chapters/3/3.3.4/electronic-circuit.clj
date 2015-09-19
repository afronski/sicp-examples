;; Helpers.

(defn invoke-all [procedures]
  (doall (map #(%) procedures)))

;; Signals.

(defn get-signal [wire] (wire :get-signal))
(defn set-signal! [wire v] ((wire :set-signal!) v))
(defn add-action! [wire p] ((wire :add-action!) p))

;; Wires.

(defn make-wire []
  (let [signal (atom false)
        effects (atom (list))]
    (letfn [(set-signal! [new]
              (if (not (= signal new))
                (do (reset! signal new)
                    (invoke-all @effects))
                :done))

            (add-action! [procedure]
              (swap! effects conj procedure)
              (procedure))

            (dispatch [action]
              (condp = action
                :get-signal @signal
                :set-signal! set-signal!
                :add-action! add-action!
                (assert false (str "Unknown operation " action " in make-wire."))))]
      dispatch)))

;; Agenda.

(defn make-queue []
  (atom (clojure.lang.PersistentQueue/EMPTY)))

(defn insert-queue! [q v]
  (swap! q conj v))

(defn delete-queue! [q]
  (swap! q pop))

(defn empty-queue? [q]
  (or (nil? q)
      (empty? @q)))

(defn front-queue [q]
  (first @q))

(defn make-segment [time queue]
  (atom {:time time :queue queue}))

(defn segment-time [segment]
  (:time @segment))

(defn segment-queue [segment]
  (:queue @segment))

(defn make-agenda []
  (atom {:current 0 :segments nil}))

(defn current-time [agenda]
  (:current @agenda))

(defn segments [agenda]
  (:segments @agenda))

(defn set-current-time! [agenda t]
  (swap! agenda assoc :current t))

(defn set-segments! [agenda s]
  (swap! agenda assoc :segments s))

(defn first-segment [agenda]
  (first (segments agenda)))

(defn rest-segments [agenda]
  (rest (segments agenda)))

(defn empty-agenda? [agenda]
  (empty? (segments agenda)))

(defn add-to-agenda! [time action agenda]
  (letfn [(belongs-before? [slices]
            (or (empty? slices)
                (< time (segment-time (first slices)))))

          (make-new-time-segment [time action]
            (let [q (make-queue)]
              (insert-queue! q action)
              (make-segment time q)))

          (insert-to-segments! [slices index]
            (let [[before after] (split-at index slices)]
              (if (empty? after)
                (set-segments! agenda (vec (concat slices
                                                   (list (make-new-time-segment time action)))))

                (if (= (segment-time (first after)) time)
                  (insert-queue! (segment-queue (first after))
                                 action)

                  (if (belongs-before? after)
                    (set-segments! agenda (vec (concat before
                                                       (list (make-new-time-segment time action))
                                                       after)))

                    (recur slices (inc index)))))))]
    (let [slices (segments agenda)]
      (if (belongs-before? slices)
        (set-segments! agenda (into [] (conj slices (make-new-time-segment time action))))
        (insert-to-segments! slices 1)))))

(defn remove-first-agenda-item! [agenda]
  (let [q (segment-queue (first-segment agenda))]
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(defn first-agenda-item [agenda]
  (if (empty-agenda? agenda)
    (assert false "Getting first item from an empty agenda.")
    (let [nearest (first-segment agenda)]
      (set-current-time! agenda (segment-time nearest))
      (front-queue (segment-queue nearest)))))

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
  (add-action! wire
               (fn [] (println (str name " " (current-time the-agenda)
                                    " New value = " (get-signal wire))))))

;; Gates.

(def not-gate-delay 2)
(def and-gate-delay 3)
(def or-gate-delay 5)

(defn not-gate [input output]
  (letfn [(not-input []
            (let [new (not (get-signal input))]
              (propagation not-gate-delay #(set-signal! output new))))]
    (add-action! input not-input)
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

(def input-1 (make-wire))
(def input-2 (make-wire))
(def sum (make-wire))
(def carry (make-wire))

(probe :sum sum)
(probe :carry carry)

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 true)
(step)

(set-signal! input-2 true)
(step)
