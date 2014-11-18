(ns automaton.simulation.ergodic
  (:require [automaton.tsetlin :as tsetlin]
            [automaton.krylov :as krylov]))

(defn simu-run*
  "Return a lazy seq with length N. Arg next-fn is a function of next-state of an ergodic machine. Randomly choose a start state."
  [c-vec N next-fn times]
  (let [R (count c-vec)
        i (rand-int (* N R))]
    (take times (iterate (fn [ni]
                           (let [alpha (int (/ ni N))
                                 beta (< (rand) (get c-vec alpha))]
                             (next-fn N R ni beta)))
                         i))))


;;
(defn run-tsetlin
  "Return a seq of result from Tsetlin simulation"
  [c-vec N times]
  (simu-run* c-vec N tsetlin/next-state times))



;
(defn run-krylov
  "Same as simulate-run-tsetlin but using Krylov back on"
  [c-vec N times]
  (simu-run* c-vec N krylov/next-state times))


;;
(defn time-avg
  "Average of time span. res-vec is the raw result from simulation, cut is the time-span length, to take the last cut els from res-vec to do statistics."
  [N R res-vec cut]
  (let [action-vec (mapv #(int (/ % N)) res-vec)
        cut-vec (subvec action-vec (- (count res-vec) cut))]
    (loop [probv (vec (take R (repeat 0)))
           remain cut-vec]
      (if (= 0 (count remain))
        (mapv #(/ % cut) probv)
        (let [a (first remain)]
          (recur (update-in probv [a] inc)
                 (rest remain)))))))



(defn expectation*
  "Intended for LRI machine"
  [c-vec N runner-fn times cnt cut]
  (let [R (count c-vec)
        tavgs (for [_ (range cnt)]
                (time-avg N R (simu-run* c-vec N  runner-fn times) cut))
        raw-E (apply map + tavgs)
        E (map #(/ % cnt) raw-E)]
    (vec E)))



(defn tsetlin-expectation
  "Compute the simulated stable probability of Tsetlin"
  [c-vec N times cnt cut]
  (expectation* c-vec N tsetlin/next-state times cnt cut))


(defn krylov-expectation
  "Compute the simulated stable probability of Krylov"
  [c-vec N times cnt cut]
  (expectation* c-vec N krylov/next-state times cnt cut))
