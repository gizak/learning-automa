(ns automaton.core
  (:require [om.core :as om :include-macros true]
            [om-tools.dom :as dom :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [automaton.tsetlin :as tsetlin]
            [automaton.krylov :as krylov]
            [automaton.lri :as lri]))


(defn simulate-run-ergodic*
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
(defn simulate-run-tsetlin [c-vec N times]
  (simulate-run-ergodic* c-vec N tsetlin/next-state times))


;
(defn simulate-run-krylov [c-vec N times]
  (simulate-run-ergodic* c-vec N krylov/next-state times))

;;
(defn stat-time-avg
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


(defonce app-state (atom {:text ""}))


(defn main []
  (om/root
    (fn [app owner]
      (reify
        om/IRender
        (render [_]
          (dom/h1 (:text app)))))
    app-state
    {:target (. js/document (getElementById "app"))}))
