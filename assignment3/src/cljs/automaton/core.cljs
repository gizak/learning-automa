(ns automaton.core
  (:require [om.core :as om :include-macros true]
            [om-tools.dom :as dom :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [goog.string :as gs]
            [goog.string.format]
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



(defn ergodic-expectation*
  [c-vec N runner-fn times cnt cut]
  (let [R (count c-vec)
        tavgs (for [_ (range cnt)]
                (stat-time-avg N R (simulate-run-ergodic* c-vec N  runner-fn times) cut))
        raw-E (apply map + tavgs)
        E (map #(/ % cnt) raw-E)]
    (vec E)))



(defn tsetlin-expectation
  [c-vec N times cnt cut]
  (ergodic-expectation* c-vec N tsetlin/next-state times cnt cut))


(defn krylov-expectation
  [c-vec N times cnt cut]
  (ergodic-expectation* c-vec N krylov/next-state times cnt cut))


(defn simulate-run-absorbing*
  "Run until end of the day! (really I mean until converged)"
  [c-vec k next-probv-fn]
  (loop [probv (let [x (rand)] [x (- 1 x)])
         cnt 0]
    (if (lri/is-converged? probv)
      (with-meta probv {:cnt cnt})
      (let [a (lri/next-action probv)
            penalty? (< (rand) (get c-vec a))]
        (recur (next-probv-fn probv k a penalty?) (inc cnt))))))



(defn simulate-run-lri
  "Run one instance of LRI"
  [c-vec k]
  (simulate-run-absorbing* c-vec k lri/next-probv-var))



(defonce app-state (atom {:text ""
                          :tsetlin-tb-0 {:title "Tsetlin Simulation Run"
                                         :description "Run Tsetlin machine, output the expectation of p(∞)"
                                         :entries ["c1" "c2" "N" "E[p(∞)]"]
                                         :items []}

                          :tsetlin-tb-1 {:title "Tsetlin Result"
                                         :description "Run Binary Search to find min N (range: [1 100]) which makes the accuracy greater than 0.95. If cannot find N, then return the nearest value in the range."
                                         :entries ["c1" "c2" "min(N)" "accuracy"]
                                         :items []}

                          :krylov-tb {:title "Compare Krylov and Tsetlin"
                                      :description "Comparing Krylov with (c1 c2) and Tsetlin with (c1/2 c2/2)"
                                      :entries ["c1" "c2" "N" "Tsetlin E[p(∞)]" "Krylov E[p(∞)]"]
                                      :items []}}))

(defn build-table [tb]
  (dom/div {:class "res"}
           (dom/h3 (:title tb))
           (dom/small (:description tb))
           (dom/table
            (dom/tr
             (for [e (:entries tb)]
               (dom/th e)))
            (for [row (:items tb)]
               (dom/tr
                (for [item row]
                  (dom/td item)))))))


(defn tsetlin-test-0 []
  (let [N 5]
    (for [c1 (range 0.05 0.651 0.1)]
      [(gs/format "%.2f" c1) 0.7 5 (str (tsetlin-expectation [c1 0.7] N 1000 100 100))])))



(defn tsetlin-test-1 []
  (for [c1 (range 0.05 0.651 0.1)
        :let [c2 0.7
              N (tsetlin/search-N-given-accuracy c1 c2 0.95)
              actual (tsetlin/exact-stationary-accuracy c1 c2 N)]]
    [(gs/format "%.2f" c1) c2 N actual]))


(defn krylov-test []
  (let [cases [[0.1 0.3 3]
               [0.1 0.5 3]
               [0.4 0.6 4]
               [0.6 0.7 3]
               [0.6 0.6 3]
               [0.3 0.3 4]
               [0.45 0.46 4]]]
    (for [[c1 c2 N] cases
          :let [ke (str (krylov-expectation [c1 c2] N 1000 100 100))
                te (str (tsetlin-expectation [(/ c1 2) (/ c2 2)] N 1000 100 100))]]
      [c1 c2 N te ke])))


(defn lri-test-0 [])

(defn lri-test-1 [])


(defn- run-test* [app owner sel-k test-k test-fn]
  (om/set-state! owner sel-k test-k)
  (om/transact! app test-k (fn [tb]
                             (update-in tb [:items] test-fn))))



(defn main []
  (om/root
   (fn [app owner]
     (reify
       om/IInitState
       (init-state [_]
         {:sel-tb nil
          :run-test (fn [test-k test-fn]
                      (run-test* app owner :sel-tb test-k test-fn))})
       om/IRenderState
       (render-state [this {:keys [sel-tb run-test]}]
         (dom/div {:class "container"}
                  (dom/button {:on-click (fn [e]
                                           (run-test :tsetlin-tb-0 tsetlin-test-0))}
                              "Tsetlin test 1")
                  (dom/button {:on-click (fn [e]
                                           (run-test :tsetlin-tb-1 tsetlin-test-1))}
                              "Tsetlin test 2")
                  (dom/button {:on-click (fn [e]
                                           (run-test :krylov-tb krylov-test))} "Krylov test")
                  (dom/button {:on-click (fn [e])} "LRI test 1")
                  (dom/div
                   (if-not (nil? sel-tb)
                     (build-table (get app sel-tb))))))))
   app-state
   {:target (. js/document (getElementById "app"))}))
