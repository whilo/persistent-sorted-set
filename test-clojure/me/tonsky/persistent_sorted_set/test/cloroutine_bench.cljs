(ns me.tonsky.persistent-sorted-set.test.cloroutine-bench
  (:require
   [me.tonsky.persistent-sorted-set.cloroutine :refer [await promisify] :refer-macros [async]]))

(defn benchmark [name f iterations]
  (let [start (.now js/performance)]
    (dotimes [_ iterations]
      (f))
    (let [end (.now js/performance)
          total-time (- end start)
          avg-time (/ total-time iterations)]
      (println (str name ": " (.toFixed total-time 3) "ms total, "
                   (.toFixed avg-time 6) "ms per operation")))))

(defn async-benchmark [name f iterations]
  (async
    (let [start (.now js/performance)]
      (dotimes [_ iterations]
        (await (f)))
      (let [end (.now js/performance)
            total-time (- end start)
            avg-time (/ total-time iterations)]
        (println (str name ": " (.toFixed total-time 3) "ms total, "
                     (.toFixed avg-time 6) "ms per operation"))))))

(defn ^:export -main []
  (println "=== Cloroutine Overhead Benchmark ===")

  (let [iterations 10000]
    (println (str "Running " iterations " iterations each..."))

    ;; Sync operations
    (benchmark "Sync function call" #(+ 1 2 3) iterations)
    (benchmark "Sync Promise.resolve" #(.resolve js/Promise 42) iterations)

    ;; Test our async overhead - run them sequentially
    (-> (async-benchmark "Async simple operation"
                        #(async (+ 1 2 3))
                        iterations)
        (.then (fn [_]
                 (async-benchmark "Async with Promise.resolve"
                                 #(async (await (js/Promise.resolve 42)))
                                 iterations)))
        (.then (fn [_]
                 (async-benchmark "Async with promisified callback"
                                 #(async (await (promisify (fn [resolve _]
                                                            (resolve 42)))))
                                 iterations)))
        (.then (fn [_] (println "=== Benchmark Complete ==="))))))