(ns automaton.krylov
  (:require [automaton.tsetlin :as tsetlin]))

;; (defmacro inherit-from
;;   "Raw inherit shortcut. Arg from is a ns and props are inherit entries."
;;   [from & props]
;;   `(doseq [v ~props]
;;      (print v)))


(defn new-state-after-penalty
  "50% chance treat as reward"
  [N R i]
  (if (> 0.5 (rand))
    (tsetlin/new-state-after-penalty N R i)
    (tsetlin/new-state-after-reward N R i)))


;; Inherit from tsetlin
(def new-state-after-reward tsetlin/new-state-after-reward)
(def action tsetlin/action)
(def exact-stationary-prob tsetlin/exact-stationary-prob)
(def exact-stationary-accuracy tsetlin/exact-stationary-accuracy)
(def search-N-given-accuracy tsetlin/search-N-given-accuracy)


;; Inherit from tsetlin
(defn next-state
  "Return next state based on env reaction"
  [N R i penalty?]
  (if penalty?
    (new-state-after-penalty N R i)
    (new-state-after-reward N R i)))
