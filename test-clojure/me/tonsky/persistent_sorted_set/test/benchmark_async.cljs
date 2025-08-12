(ns me.tonsky.persistent-sorted-set.test.benchmark-async
  "Benchmarks comparing synchronous vs asynchronous performance"
  (:require
   [cljs.test :refer-macros [deftest testing is] :as test]
   [cloroutine.impl :as impl]
   [me.tonsky.persistent-sorted-set.async-await :as async-await]
   [me.tonsky.persistent-sorted-set :as set]
   [await-cps.await-cps :refer [await]]
   [me.tonsky.persistent-sorted-set.async-utils :as utils])
  (:require-macros
   #_[me.tonsky.persistent-sorted-set.async-await :refer [async await]]
   [await-cps.await-cps :refer [async]]))

;; Benchmark utilities
(defn- now []
  (js/performance.now))

(defn- mean [nums]
  (/ (reduce + nums) (count nums)))

(defn- median [nums]
  (let [sorted (sort nums)
        n (count sorted)
        mid (quot n 2)]
    (if (even? n)
      (mean [(nth sorted (dec mid)) (nth sorted mid)])
      (nth sorted mid))))

(defn- std-dev [nums]
  (let [m (mean nums)
        squared-diffs (map #(js/Math.pow (- % m) 2) nums)]
    (js/Math.sqrt (mean squared-diffs))))

(defn- percentile [nums p]
  (let [sorted (sort nums)
        idx (js/Math.floor (* (/ p 100) (dec (count sorted))))]
    (nth sorted idx)))

(defn- run-benchmark
  "Run a benchmark function multiple times and collect timing statistics"
  [name f warmup-runs test-runs]
  ;; Warmup
  (dotimes [_ warmup-runs]
    (f))
  
  ;; Actual measurements
  (let [timings (atom [])]
    (dotimes [_ test-runs]
      (let [start (now)]
        (f)
        (let [end (now)]
          (swap! timings conj (- end start)))))
    
    (let [times @timings]
      {:name name
       :mean (mean times)
       :median (median times)
       :std-dev (std-dev times)
       :min (apply min times)
       :max (apply max times)
       :p95 (percentile times 95)
       :p99 (percentile times 99)
       :samples test-runs})))

(defn- run-async-benchmark
  "Run an async benchmark function multiple times"
  [name f warmup-runs test-runs]
  (async
    ;; Warmup
    (dotimes [_ warmup-runs]
      (await (f)))
    
    ;; Actual measurements
    (let [timings (atom [])]
      (dotimes [_ test-runs]
        (let [start (now)]
          (await (f))
          (let [end (now)]
            (swap! timings conj (- end start)))))
      
      (let [times @timings]
        {:name name
         :mean (mean times)
         :median (median times)
         :std-dev (std-dev times)
         :min (apply min times)
         :max (apply max times)
         :p95 (percentile times 95)
         :p99 (percentile times 99)
         :samples test-runs}))))

(defn- format-comparison [sync-stats async-stats]
  (let [overhead-factor (/ (:mean async-stats) (:mean sync-stats))
        overhead-pct (* (- overhead-factor 1) 100)]
    {:sync sync-stats
     :async async-stats
     :overhead-factor overhead-factor
     :overhead-percent overhead-pct}))

(defn- print-comparison [comparison]
  (println "\n===" (:name (:sync comparison)) "===")
  (println (str "Sync:  mean=" (.toFixed (:mean (:sync comparison)) 3) "ms"
                " median=" (.toFixed (:median (:sync comparison)) 3) "ms"
                " p95=" (.toFixed (:p95 (:sync comparison)) 3) "ms"))
  (println (str "Async: mean=" (.toFixed (:mean (:async comparison)) 3) "ms"
                " median=" (.toFixed (:median (:async comparison)) 3) "ms"  
                " p95=" (.toFixed (:p95 (:async comparison)) 3) "ms"))
  (println (str "Overhead: " (.toFixed (:overhead-factor comparison) 2) "x"
                " (" (if (pos? (:overhead-percent comparison)) "+" "")
                (.toFixed (:overhead-percent comparison) 1) "%)")))

;; Benchmark: Single element operations
(deftest bench-single-operations
  (test/async done
    (-> (async
      (println "\n### Single Element Operations ###")
      
      ;; Setup
      (let [sync-storage (utils/make-sync-storage)
            async-storage (utils/make-async-storage 0) ; Zero delay for pure overhead measurement

            ;; Create test sets with 1000 elements
            sync-set (reduce set/conj
                             (set/sorted-set* {:storage sync-storage})
                             (range 1000))
            async-set (await (reduce (fn [s-ch n]
                                       (async (let [s (await s-ch)]
                                                (await (set/conj s n compare {:sync? false})))))
                                     (async (set/sorted-set* {:storage async-storage}))
                                     (range 1000)))
            warmup-runs 100000
            test-runs 50000]

        ;; Benchmark: conj single element
        (let [sync-conj (run-benchmark
                         "conj"
                         #(set/conj sync-set 1001)
                         warmup-runs test-runs)
              async-conj (await (run-async-benchmark
                                 "conj"
                                 #(set/conj sync-set 1001 compare {:sync? false})
                                 warmup-runs test-runs))]
          (print-comparison (format-comparison sync-conj async-conj)))

        ;; Benchmark: lookup existing element
        (let [sync-lookup (run-benchmark
                           "Sync lookup"
                           #(get sync-set 500)
                           warmup-runs test-runs)
              async-lookup (await (run-async-benchmark
                                   "lookup"
                                   #(async (await (set/lookup-async async-set 500)))
                                   warmup-runs test-runs))]
          (print-comparison (format-comparison sync-lookup async-lookup)))

        ;; Benchmark: contains? check
        (let [sync-contains (run-benchmark
                             "contains?"
                             #(contains? sync-set 500)
                             warmup-runs test-runs)
              async-contains (await (run-async-benchmark
                                     "contains?"
                                     #(async (await (set/contains-async? async-set 500)))
                                     warmup-runs test-runs))]
          (print-comparison (format-comparison sync-contains async-contains)))

        ;; Benchmark: disj single element
        (let [sync-disj (run-benchmark
                         "disj"
                         #(set/disj sync-set 500)
                         warmup-runs test-runs)
              async-disj (await (run-async-benchmark
                                 "disj"
                                 #(set/disj async-set 500 compare {:sync? false})
                                 warmup-runs test-runs))]
          (print-comparison (format-comparison sync-disj async-disj)))))
        (.then (fn [_] (done)))
        (.catch (fn [e] (println "Error in bench-single-operations:" e) (done))))))

;; Benchmark: Bulk operations
#_(deftest bench-bulk-operations
  (test/async done
    (-> (async
      (println "\n### Bulk Operations ###")
      
      (let [sync-storage (utils/make-sync-storage)
            async-storage (utils/make-async-storage 0)]
        
        ;; Benchmark: Building sets of different sizes
        (doseq [n [100 1000 test-runs]]
          (let [nums (shuffle (range n))
                
                sync-build (run-benchmark
                           (str "Sync build " n " elements")
                           #(reduce set/conj
                                   (set/sorted-set* {:storage sync-storage})
                                   nums)
                           2 10)
                
                async-build (await (run-async-benchmark
                                (str "Async build " n " elements")
                                #(reduce (fn [s-ch num]
                                          (async (let [s (await s-ch)]
                                                (await (set/conj s num compare {:sync? false})))))
                                        (async (set/sorted-set* {:storage async-storage}))
                                        nums)
                                2 10))]
            (print-comparison (format-comparison sync-build async-build)))))
        (.then (fn [_] (done)))
        (.catch (fn [e] (println "Error in bench-bulk-operations:" e) (done))))))

;; Benchmark: Iteration
(deftest bench-iteration
  (test/async done
         (-> (async
           (println "\n### Iteration Performance ###")

           (let [sync-storage (utils/make-sync-storage)
                 async-storage (utils/make-async-storage 0)

                 ;; Create test sets
                 sync-set (reduce set/conj
                                  (set/sorted-set* {:storage sync-storage})
                                  (range 1000))
                 async-set (await (reduce (fn [s-ch n]
                                        (async (let [s (await s-ch)]
                                              (await (set/conj s n compare {:sync? false})))))
                                      (async (set/sorted-set* {:storage async-storage}))
                                      (range 1000)))]

             ;; Benchmark: Full iteration
             (let [sync-iter (run-benchmark
                              "Sync full iteration"
                              #(doall (seq sync-set))
                              5 20)

                   async-iter (await (run-async-benchmark
                                  "Async full iteration"
                                  #(async
                                    (let [ch (await (set/async-slice async-set nil nil))
                                          results (atom [])]
                                      (loop []
                                        (when-let [v (await ch)]
                                          (swap! results conj v)
                                          (recur)))
                                      @results))
                                  5 20))]
               (print-comparison (format-comparison sync-iter async-iter)))

             ;; Benchmark: Slice iteration (100 elements)
             (let [sync-slice (run-benchmark
                               "Sync slice (100 elements)"
                               #(doall (set/slice sync-set 400 499))
                               5 20)

                   async-slice (await (run-async-benchmark
                                   "Async slice (100 elements)"
                                   #(async
                                     (let [ch (await (set/async-slice async-set 400 499))
                                           results (atom [])]
                                       (loop []
                                         (when-let [v (await ch)]
                                           (swap! results conj v)
                                           (recur)))
                                       @results))
                                   5 20))]
               (print-comparison (format-comparison sync-slice async-slice)))))
          (.then (fn [_] (done)))
          (.catch (fn [e] (println "Error:" e) (done)))))))

;; Benchmark: Storage operations with different delays
#_(deftest bench-storage-delays
  (test/async done
         (-> (async
           (println "\n### Storage Operations with Delays ###")

           (doseq [delay-ms [0 1 5]]
             (println (str "\n--- Storage delay: " delay-ms "ms ---"))

             (let [sync-storage (utils/make-sync-storage)
                   async-storage (utils/make-async-storage delay-ms)

                   ;; Build initial sets for each storage type
                   sync-set (reduce set/conj
                                    (set/sorted-set* {:storage sync-storage})
                                    (range 500))
                   async-set (await (reduce (fn [acc-ch v]
                                          (async (let [acc (await acc-ch)]
                                                (await (set/conj acc v compare {:sync? false})))))
                                        (async (set/sorted-set* {:storage async-storage}))
                                        (range 500)))]

               ;; Store and restore benchmark
               (let [sync-store-restore (run-benchmark
                                         "Sync store+restore"
                                         #(let [store-info (set/store-set sync-set)]
                                            (set/restore store-info sync-storage))
                                         2 10)

                     async-store-restore (await (run-async-benchmark
                                             "Async store+restore"
                                             #(async
                                               (let [store-info (await (set/store-set async-set {:sync? false}))]
                                                 (await (set/restore store-info async-storage {:sync? false}))))
                                             2 10))]
                 (print-comparison (format-comparison sync-store-restore async-store-restore))))))
          (.then (fn [_] (done)))
          (.catch (fn [e] (println "Error:" e) (done))))))

;; Benchmark: Lazy loading overhead
#_(deftest bench-lazy-loading
  (test/async done
         (-> (async
           (println "\n### Lazy Loading Overhead ###")

           (let [sync-storage (utils/make-sync-storage)
                 async-storage (utils/make-async-storage 1) ; Small delay to simulate I/O

                 ;; Create and store a large set
                 large-set (reduce set/conj
                                   (set/sorted-set* {:storage sync-storage})
                                   (range warmup-runs))
                 sync-addr (set/store-set large-set)
                 async-addr (await (set/store-set large-set {:sync? false}))

                 ;; Restore lazy sets
                 sync-lazy (set/restore sync-addr sync-storage
                                        {:count warmup-runs :shift (.-shift large-set)})
                 async-lazy (await (set/restore async-addr async-storage
                                            {:count warmup-runs :shift (.-shift large-set) :sync? false}))]

             ;; Benchmark: First access (cold)
             (let [sync-first (run-benchmark
                               "Sync first access (cold)"
                               #(get sync-lazy test-runs)
                               2 10)

                   async-first (await (run-async-benchmark
                                   "Async first access (cold)"
                                   #(set/lookup-async async-lazy test-runs)
                                   2 10))]
               (print-comparison (format-comparison sync-first async-first)))

             ;; Benchmark: Subsequent access (warm)
             ;; Prime the cache first
             (get sync-lazy test-runs)
             (await (set/lookup-async async-lazy test-runs))

             (let [sync-warm (run-benchmark
                              "Sync access (warm cache)"
                              #(get sync-lazy test-runs)
                              5 20)

                   async-warm (await (run-async-benchmark
                                  "Async access (warm cache)"
                                  #(async (await (set/lookup-async async-lazy test-runs)))
                                  5 20))]
               (print-comparison (format-comparison sync-warm async-warm)))))
          (.then (fn [_] (done)))
          (.catch (fn [e] (println "Error:" e) (done))))))

;; Run benchmarks
(cljs.test/run-tests)