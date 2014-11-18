;; To make life less suffer, Only implement 2-actions LRI
(ns automaton.lri)


(declare speedup-converge)

;;
(defn next-probv-var*
  "Update prob vec var using LRI rule"
  [prob-vec k a penalty?]
  (if penalty?
    prob-vec ;; ignore
    (let [b (int (* (dec a) (dec a)))
          nb (* k (get prob-vec b))]
      (-> prob-vec
          (update-in [b] #(* k %))
          (update-in [a] #(- 1 nb))))))


;;
(defn next-probv-var
  "Speeded up version"
  [probv k a penalty?]
  (speedup-converge (next-probv-var* probv k a penalty?)))


;;
(defn next-action
  "Next choosen action based on current prob var"
  [prob-vec]
  (if (< (rand) (prob-vec 0))
    0
    1))


(def valve 0.0001)

;;
(defn speedup-converge
  "If the prob var is close to absorbing state, push it a little to make it converge"
  [prob-vec]
  (if (<= (prob-vec 0) (prob-vec 1))
    (if (< (prob-vec 0) valve)
      [0 1]
      prob-vec)
    (if (< (prob-vec 1) valve)
      [1 0]
      prob-vec)))

;;
(defn is-converged?
  "Test if is converged"
  [prob-vec]
  (or (= prob-vec [0 1])
      (= prob-vec [1 0])))
