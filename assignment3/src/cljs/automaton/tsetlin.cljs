;; Implementation of Tsetlin Method
(ns automaton.tsetlin)


;;
(defn new-state-after-reward
  "Return new state indicated by a number. N is the state depth of machine, R is the amount of actions, i is current state."
  [N R i]
  (if (= 0 (mod i N))
    i
    (dec i)))


;;
(defn new-state-after-penalty
  "Return new state indicated by a number. N is the state depth of machine, R is the amount of actions, i is current state."
  [N R i]
  (let [ni (inc i)]
    (if (= 0 (mod ni N))
      (mod (+ i N) (* R N))
      ni)))


(defn action
  "Determine next action"
  [N i]
  (int (/ i N)))


;;
(defn next-state
  "Return next state based on env reaction"
  [N R i penalty?]
  (if penalty?
    (new-state-after-penalty N R i)
    (new-state-after-reward N R i)))


;;
(defn pow
  "a^b"
  [a b]
  (.pow js/Math a b))


;;
(defn exact-stationary-prob
  "Using derived expression to calculate final prob distribution"
  [c1 c2 N]
  (let [d1 (- 1 c1)
        d2 (- 1 c2)
        p1 (/ 1
              (+ 1 (* (pow (/ c1 c2) N)
                      (/ (- c1 d1) (- c2 d2))
                      (/ (- (pow c2 N) (pow d2 N))
                         (- (pow c1 N) (pow d1 N))))))]
    [p1 (- 1 p1)]))


;;
(defn exact-stationary-accuracy
  "Return the prob of chosing min(c1 c2)"
  [c1 c2 N]
  (let [[acc-c1 acc-c2] (exact-stationary-prob c1 c2 N)]
    (if (> c1 c2)
      acc-c2
      acc-c1)))



;; Only assume two action.
(defn search-N-given-accuracy
  "Binary search for N with certain accuracy. If acc out of range, return nearest value (upper or lower bound). If f(n) = acc return n, else if f(n) < acc < fn(n+1) return n+1."
  [c1 c2 acc]
  (let [a0 1 b0 100
        acc-a0 (exact-stationary-accuracy c1 c2 a0)
        acc-b0 (exact-stationary-accuracy c1 c2 b0)]
    (cond
     (>= acc acc-b0) b0
     (<= acc acc-a0) a0
     :else (loop [a a0 b b0]
             (let [mid (int (/ (+ a b) 2))
                   acc-mid (exact-stationary-accuracy c1 c2 mid)]
               (cond
                (= acc-mid acc) mid
                (= mid a) (inc a)
                :else (if (> acc-mid acc)
                        (recur a mid)
                        (recur mid b))))))))
