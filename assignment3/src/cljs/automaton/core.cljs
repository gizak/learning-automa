(ns automaton.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om-tools.dom :as dom :include-macros true]
            [om-tools.core :refer-macros [defcomponent]]
            [cljs.core.async :refer [put! chan <!]]
            [goog.string :as gs]
            [goog.string.format]
            [automaton.tsetlin :as tsetlin]
            [automaton.krylov :as krylov]
            [automaton.lri :as lri]
            [automaton.simulation.ergodic :as ergodic]
            [automaton.simulation.absorbing :as absorbing]))



(defonce app-state (atom {:text ""
                          :tsetlin-tb-0 {:title "Tsetlin Simulation"
                                         :description "Run Tsetlin machine, output the simulated p(∞)"
                                         :entries ["c1" "c2" "N" "p(∞)"]
                                         :items []}

                          :tsetlin-tb-1 {:title "Tsetlin Binary Searching Accuracy"
                                         :description "Run Binary Search to find min N (range: [1 100]) which makes the accuracy greater than 0.95. If cannot find N, then return the nearest value in the range."
                                         :entries ["c1" "c2" "min(N)" "accuracy"]
                                         :items []}

                          :krylov-tb {:title "Compare Krylov and Tsetlin"
                                      :description "Comparing Krylov with (c1 c2) and Tsetlin with (c1/2 c2/2)"
                                      :entries ["c1" "c2" "N" "Tsetlin p(∞)" "Krylov p(∞)"]
                                      :items []}
                          :lri-tb-0 {:title "LRI Simulation"
                                     :description "Simulate LRI machine for given env"
                                     :entries ["c1" "c2" "mean" "p(∞)"]
                                     :items []}
                          :lri-tb-1 {:title "LRI Min λ"
                                     :description "Simulate LRI machine for the given env to find λ that makes 0.95 accuracy"
                                     :entries ["c1" "c2" "λ" "acc" "mean"]
                                     :items []}}))

(defn- build-table [tb]
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


(defn tsetlin-test-0
  "Compute the stationary probability given (assuming) inf steps"
  []
  (let [N 5]
    (for [c1 (range 0.05 0.651 0.1)]
      [(gs/format "%.2f" c1) 0.7 5 (str (ergodic/tsetlin-expectation [c1 0.7] N 1000 100 100))])))



(defn tsetlin-test-1
  "Finding the min N ranging from accuracy og 0.05..0.65 with sep size of 0.1"
  []
  (for [c1 (range 0.05 0.651 0.1)
        :let [c2 0.7
              N (tsetlin/search-N-given-accuracy c1 c2 0.95)
              actual (tsetlin/exact-stationary-accuracy c1 c2 N)]]
    [(gs/format "%.2f" c1) c2 N actual]))


(defn krylov-test
  "Comparison of Krylov and Tsetlin use different type of test cases"
  []
  (let [cases [[0.1 0.3 3]
               [0.1 0.5 3]
               [0.4 0.6 4]
               [0.6 0.7 3]
               [0.6 0.6 3]
               [0.3 0.3 4]
               [0.45 0.46 4]]]
    (for [[c1 c2 N] cases
          :let [ke (str (ergodic/krylov-expectation [c1 c2] N 1000 100 100))
                te (str (ergodic/tsetlin-expectation [(/ c1 2) (/ c2 2)] N 1000 100 100))]]
      [c1 c2 N te ke])))





(defn lri-test-0
  "Run simulation of LRI"
  []
  (for [c1 (range 0.05 0.651 0.1)
        :let [c2 0.7
              res (absorbing/stat-lri [c1 c2] 0.8 1000)]]
    [(gs/format "%.2f" c1) c2 (:mean res) (str (:probv res))]))



(defn lri-test-1 []
  (for [c1 (range 0.05 0.651 0.1)
        :let [c2 0.7
              k (absorbing/lri-bin-search-accuracy [c1 c2] 200 0.95)
              mean (:mean k)
              acc (:acc k)]]
    [(gs/format "%.2f" c1) c2 (:val k) acc mean]))


(defn- run-test*
  "For internal using only, change the hook of display table with sel-k, run test write it into coresponding table"
  [app owner sel-k test-k test-fn]
  (om/set-state! owner sel-k test-k)
  (om/transact! app test-k (fn [tb]
                             (update-in tb [:items] test-fn))))



(defn main
  "GUI entry"
  []
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
                  (dom/h2 "Learning Automaton")
                  (dom/p "Due to the heavy computing, this page may experience a few seconds freezing after a test button is clicked!")
                  (dom/button {:on-click (fn [e]
                                           (run-test :tsetlin-tb-0 tsetlin-test-0))}
                              "Tsetlin Simulation")
                  (dom/button {:on-click (fn [e]
                                           (run-test :tsetlin-tb-1 tsetlin-test-1))}
                              "Tsetlin Min N")
                  (dom/button {:on-click (fn [e]
                                           (run-test :krylov-tb krylov-test))} "Krylov vs Tsetlin")
                  (dom/button {:on-click (fn [e]
                                           (run-test :lri-tb-0 lri-test-0))} "LRI Simulation")
                  (dom/button {:on-click (fn [e]
                                           (run-test :lri-tb-1 lri-test-1))} "LRI Min λ")


                  (dom/div
                   (if-not (nil? sel-tb)
                     (build-table (get app sel-tb))))))))
   app-state
   {:target (. js/document (getElementById "app"))}))
