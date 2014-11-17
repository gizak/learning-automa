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

(defn exact-stationary-prob
  "Bridge to Tsetlin"
  [c1 c2 N]
  (tsetlin/exact-stationary-prob (/ c1 2) (/ c2 2) N))

(defn exact-stationary-accuracy
  "Bridge to Tsetlin"
  [c1 c2 N]
  (tsetlin/exact-stationary-accuracy (/ c1 2) (/ c2 2) N))

(defn search-N-given-accuracy
  "Process same way as Tsetlin but cut prob in half"
  [c1 c2 acc]
  (tsetlin/search-N-given-accuracy (/ c1 2) (/ c2 2) acc))


;; Inherit from tsetlin
(defn next-state
  "Return next state based on env reaction"
  [N R i penalty?]
  (if penalty?
    (new-state-after-penalty N R i)
    (new-state-after-reward N R i)))
