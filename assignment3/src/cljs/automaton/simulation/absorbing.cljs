(ns automaton.simulation.absorbing
  (:require [automaton.lri :as lri]))


(defn simu-run*
  "Run until end of the day! (really I mean until converged)"
  [c-vec k next-probv-fn]
  (loop [probv (let [x (rand)] [x (- 1 x)])
         cnt 0]
    (if (lri/is-converged? probv)
      (with-meta probv {:cnt cnt})
      (let [a (lri/next-action probv)
            penalty? (< (rand) (get c-vec a))]
        (recur (next-probv-fn probv k a penalty?) (inc cnt))))))



(defn run-lri
  "Run one instance of LRI"
  [c-vec k]
  (simu-run* c-vec k lri/next-probv-var))


(defn lri-accuracy*
  "Return LRI accuracy"
  [c-vec probv]
  (let [i (if (> (c-vec 0) (c-vec 1)) 1 0)]
    (get probv i)))


(defn stat-lri
  "Return a map containing mean converging time and prob distribution"
  [c-vec k cnt]
  (let [lri-ins (for [_ (range cnt)]
              (run-lri c-vec k))]
    (loop [probv* [0 0]
           mean* 0
           remain lri-ins]
      (if (= 0 (count remain))
        (let [mean (/ mean* cnt)
              probv (mapv #(/ % cnt) probv*)
              acc (lri-accuracy* c-vec probv)]
          {:mean mean
           :probv probv
           :acc acc})
        (recur (mapv + probv* (first remain))
               (+ mean* (:cnt (meta (first remain))))
               (rest remain))))))



;; This may take hell of time
(defn lri-bin-search-accuracy
  "Run binary search over LRI to find a accaptable k that makes accuracy reach acc, default searching range: [0.2 0.99]"
  ([c-vec cnt acc] (lri-bin-search-accuracy c-vec cnt acc 0.2 0.99))
  ([c-vec cnt acc k0 k1]
     (let [stat-k0 (stat-lri c-vec k0 cnt)
           stat-k1 (stat-lri c-vec k1 cnt)
           acc-k0 (:acc stat-k0)
           acc-k1 (:acc stat-k1)
           e 0.005]
       (cond
        (<= acc acc-k0) (with-meta k0 stat-k0)
        (>= acc acc-k1) (with-meta k1 stat-k1)
        :else (loop [a k0 b k1]
                (let [mid (/ (+ a b) 2)
                      stat-mid (stat-lri c-vec mid cnt)
                      mean (:mean stat-mid)
                      acc-mid (:acc stat-mid)]
                  (cond
                   (and (>= acc-mid acc) (< acc-mid (+ e acc))) {:val mid :mean mean :acc acc-mid}
                   :else (if (> acc-mid acc)
                           (recur a mid)
                           (recur mid b)))))))))
