(ns me.tonsky.persistent-sorted-set.test.benchmark-async
  "Benchmarks comparing synchronous vs asynchronous performance"
  (:require
   [cljs.test :refer-macros [deftest testing is async]]
   [cljs.core.async :as async :refer [go <!]]
   [me.tonsky.persistent-sorted-set :as set]
   [me.tonsky.persistent-sorted-set.async-utils :as utils]))

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
  (go
    ;; Warmup
    (dotimes [_ warmup-runs]
      (<! (f)))
    
    ;; Actual measurements
    (let [timings (atom [])]
      (dotimes [_ test-runs]
        (let [start (now)]
          (<! (f))
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
  (async done
    (go
      (println "\n### Single Element Operations ###")
      
      ;; Setup
      (let [sync-storage (utils/make-sync-storage)
            async-storage (utils/make-async-storage 0) ; Zero delay for pure overhead measurement
            
            ;; Create test sets with 1000 elements
            sync-set (reduce set/conj 
                            (set/sorted-set* {:storage sync-storage})
                            (range 1000))
            async-set (<! (reduce (fn [s-ch n]
                                   (go (let [s (<! s-ch)]
                                         (<! (set/conj s n compare {:sync? false})))))
                                 (go (set/sorted-set* {:storage async-storage}))
                                 (range 1000)))]
        
        ;; Benchmark: conj single element
        (let [sync-conj (run-benchmark
                        "Sync conj"
                        #(set/conj sync-set 1001)
                        10 50)
              async-conj (<! (run-async-benchmark
                            "Async conj"
                            #(set/conj sync-set 1001 compare {:sync? false})
                            10 50))]
          (print-comparison (format-comparison sync-conj async-conj)))
        
        ;; Benchmark: lookup existing element
        (let [sync-lookup (run-benchmark
                          "Sync lookup"
                          #(get sync-set 500)
                          10 50)
              async-lookup (<! (run-async-benchmark
                              "Async lookup"
                              #(set/lookup-async async-set 500)
                              10 50))]
          (print-comparison (format-comparison sync-lookup async-lookup)))
        
        ;; Benchmark: contains? check
        (let [sync-contains (run-benchmark
                            "Sync contains?"
                            #(contains? sync-set 500)
                            10 50)
              async-contains (<! (run-async-benchmark
                                "Async contains?"
                                #(set/contains-async? async-set 500)
                                10 50))]
          (print-comparison (format-comparison sync-contains async-contains)))
        
        ;; Benchmark: disj single element
        (let [sync-disj (run-benchmark
                        "Sync disj"
                        #(set/disj sync-set 500)
                        10 50)
              async-disj (<! (run-async-benchmark
                            "Async disj"
                            #(set/disj async-set 500 compare {:sync? false})
                            10 50))]
          (print-comparison (format-comparison sync-disj async-disj))))
      
      (done))))

;; Benchmark: Bulk operations
(deftest bench-bulk-operations
  (async done
    (go
      (println "\n### Bulk Operations ###")
      
      (let [sync-storage (utils/make-sync-storage)
            async-storage (utils/make-async-storage 0)]
        
        ;; Benchmark: Building sets of different sizes
        (doseq [n [100 1000 5000]]
          (let [nums (shuffle (range n))
                
                sync-build (run-benchmark
                           (str "Sync build " n " elements")
                           #(reduce set/conj
                                   (set/sorted-set* {:storage sync-storage})
                                   nums)
                           2 10)
                
                async-build (<! (run-async-benchmark
                                (str "Async build " n " elements")
                                #(reduce (fn [s-ch num]
                                          (go (let [s (<! s-ch)]
                                                (<! (set/conj s num compare {:sync? false})))))
                                        (go (set/sorted-set* {:storage async-storage}))
                                        nums)
                                2 10))]
            (print-comparison (format-comparison sync-build async-build)))))
      
      (done))))

;; Benchmark: Iteration
(deftest bench-iteration
  (async done
    (go
      (println "\n### Iteration Performance ###")
      
      (let [sync-storage (utils/make-sync-storage)
            async-storage (utils/make-async-storage 0)
            
            ;; Create test sets
            sync-set (reduce set/conj
                           (set/sorted-set* {:storage sync-storage})
                           (range 1000))
            async-set (<! (reduce (fn [s-ch n]
                                  (go (let [s (<! s-ch)]
                                        (<! (set/conj s n compare {:sync? false})))))
                                (go (set/sorted-set* {:storage async-storage}))
                                (range 1000)))]
        
        ;; Benchmark: Full iteration
        (let [sync-iter (run-benchmark
                        "Sync full iteration"
                        #(doall (seq sync-set))
                        5 20)
              
              async-iter (<! (run-async-benchmark
                            "Async full iteration"
                            #(go
                               (let [ch (<! (set/async-slice async-set nil nil))
                                     results (atom [])]
                                 (loop []
                                   (when-let [v (<! ch)]
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
              
              async-slice (<! (run-async-benchmark
                             "Async slice (100 elements)"
                             #(go
                                (let [ch (<! (set/async-slice async-set 400 499))
                                      results (atom [])]
                                  (loop []
                                    (when-let [v (<! ch)]
                                      (swap! results conj v)
                                      (recur)))
                                  @results))
                             5 20))]
          (print-comparison (format-comparison sync-slice async-slice))))
      
      (done))))

;; Benchmark: Storage operations with different delays
(deftest bench-storage-delays
  (async done
    (go
      (println "\n### Storage Operations with Delays ###")
      
      (doseq [delay-ms [0 1 5]]
        (println (str "\n--- Storage delay: " delay-ms "ms ---"))
        
        (let [sync-storage (utils/make-sync-storage)
              async-storage (utils/make-async-storage delay-ms)
              
              ;; Build initial sets for each storage type
              sync-set (reduce set/conj
                              (set/sorted-set* {:storage sync-storage})
                              (range 500))
              async-set (<! (reduce (fn [acc-ch v]
                                     (go (let [acc (<! acc-ch)]
                                           (<! (set/conj acc v compare {:sync? false})))))
                                   (go (set/sorted-set* {:storage async-storage}))
                                   (range 500)))]
          
          ;; Store and restore benchmark
          (let [sync-store-restore (run-benchmark
                                   "Sync store+restore"
                                   #(let [store-info (set/store-set sync-set)]
                                      (set/restore store-info sync-storage))
                                   2 10)
                
                async-store-restore (<! (run-async-benchmark
                                        "Async store+restore"
                                        #(go
                                           (let [store-info (<! (set/store-set async-set {:sync? false}))]
                                             (<! (set/restore store-info async-storage {:sync? false}))))
                                        2 10))]
            (print-comparison (format-comparison sync-store-restore async-store-restore)))))
      
      (done))))

;; Benchmark: Lazy loading overhead
(deftest bench-lazy-loading
  (async done
    (go
      (println "\n### Lazy Loading Overhead ###")
      
      (let [sync-storage (utils/make-sync-storage)
            async-storage (utils/make-async-storage 1) ; Small delay to simulate I/O
            
            ;; Create and store a large set
            large-set (reduce set/conj
                            (set/sorted-set* {:storage sync-storage})
                            (range 10000))
            sync-addr (set/store-set large-set)
            async-addr (<! (set/store-set large-set {:sync? false}))
            
            ;; Restore lazy sets
            sync-lazy (set/restore sync-addr sync-storage
                                  {:count 10000 :shift (.-shift large-set)})
            async-lazy (<! (set/restore async-addr async-storage
                                      {:count 10000 :shift (.-shift large-set) :sync? false}))]
        
        ;; Benchmark: First access (cold)
        (let [sync-first (run-benchmark
                         "Sync first access (cold)"
                         #(get sync-lazy 5000)
                         2 10)
              
              async-first (<! (run-async-benchmark
                             "Async first access (cold)"
                             #(set/lookup-async async-lazy 5000)
                             2 10))]
          (print-comparison (format-comparison sync-first async-first)))
        
        ;; Benchmark: Subsequent access (warm)
        ;; Prime the cache first
        (get sync-lazy 5000)
        (<! (set/lookup-async async-lazy 5000))
        
        (let [sync-warm (run-benchmark
                        "Sync access (warm cache)"
                        #(set/lookup sync-lazy 5000)
                        5 20)
              
              async-warm (<! (run-async-benchmark
                            "Async access (warm cache)"
                            #(set/lookup-async async-lazy 5000)
                            5 20))]
          (print-comparison (format-comparison sync-warm async-warm))))
      
      (done))))

(deftest bench-summary
  (async done
    (go
      (println "\n### Benchmark Summary ###")
      (println "Run all benchmarks above to see detailed performance comparison")
      (println "Key metrics:")
      (println "- Overhead factor: async_time / sync_time")  
      (println "- Lower is better for async overhead")
      (println "- Expect 1.1-2x overhead for zero-delay async operations")
      (println "- Storage delays amplify the difference")
      (println "- Callback approach should be 3-8x faster than core.async")
      (done))))

;; Run benchmarks
(cljs.test/run-tests)