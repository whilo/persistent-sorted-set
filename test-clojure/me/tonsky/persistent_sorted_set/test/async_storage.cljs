(ns me.tonsky.persistent-sorted-set.test.async-storage
  (:require
   [cljs.test :refer-macros [deftest testing is async]]
   [cljs.core.async :as async :refer [<! >!]]
   [me.tonsky.persistent-sorted-set :as set]
   [me.tonsky.persistent-sorted-set.async-utils :as utils])
  (:require-macros
   [cljs.core.async.macros :refer [go]]))

(deftest sync-storage-test
  (testing "Synchronous storage operations"
    (let [storage (utils/make-sync-storage)
          s0 (set/sorted-set* {:storage storage})
          s1 (set/conj s0 1)
          s2 (set/conj s1 2)
          s3 (set/conj s2 3)]
      
      (is (= 0 (count s0)))
      (is (= 1 (count s1)))
      (is (= 2 (count s2)))
      (is (= 3 (count s3)))
      
      (is (= [1 2 3] (vec s3)))
      (is (contains? s3 2))
      (is (not (contains? s3 4)))
      
      ;; Test disj
      (let [s4 (set/disj s3 2)]
        (is (= 2 (count s4)))
        (is (= [1 3] (vec s4)))
        (is (not (contains? s4 2)))))))

(deftest async-storage-test
  (async done
    (go
      (testing "Asynchronous storage operations"
        (let [storage (utils/make-async-storage 5)
              s0 (set/sorted-set* {:storage storage})
              s1 (<! (set/conj s0 1 compare {:sync? false}))
              s2 (<! (set/conj s1 2 compare {:sync? false}))
              s3 (<! (set/conj s2 3 compare {:sync? false}))]
          
          (is (= 0 (count s0)))
          (is (= 1 (count s1)))
          (is (= 2 (count s2)))
          (is (= 3 (count s3)))
          
          ;; Note: iteration is still synchronous
          (is (= [1 2 3] (vec s3)))
          
          ;; Test async disj
          (let [s4 (<! (set/disj s3 2 compare {:sync? false}))]
            (is (= 2 (count s4)))
            (is (= [1 3] (vec s4)))))
        
        (done)))))

(deftest mixed-operations-test
  (async done
    (go
      (testing "Mixed sync/async operations"
        (let [storage (utils/make-async-storage)
              s0 (set/sorted-set* {:storage storage})]
          
          ;; Start with sync operations
          (let [s1 (set/conj s0 5)
                s2 (set/conj s1 3)
                s3 (set/conj s2 7)]
            
            ;; Then do some async operations
            (let [s4 (<! (set/conj s3 1 compare {:sync? false}))
                  s5 (<! (set/conj s4 9 compare {:sync? false}))]
              
              (is (= [1 3 5 7 9] (vec s5)))
              
              ;; Back to sync
              (let [s6 (set/disj s5 5)]
                (is (= [1 3 7 9] (vec s6)))))))
        
        (done)))))

(deftest storage-persistence-test
  (async done
    (go
      (testing "Storage persistence and restoration"
        (let [storage (utils/make-async-storage)
              s0 (set/sorted-set* {:storage storage})]
          
          ;; Build a set
          (let [s1 (<! (set/conj s0 10 compare {:sync? false}))
                s2 (<! (set/conj s1 20 compare {:sync? false}))
                s3 (<! (set/conj s2 30 compare {:sync? false}))
                s4 (<! (set/conj s3 40 compare {:sync? false}))
                s5 (<! (set/conj s4 50 compare {:sync? false}))]
            
            ;; Store it
            (let [root-addr (<! (set/store-set s5 {:sync? false}))]
              (is (some? root-addr))
              
              ;; Restore from storage
              (let [restored (<! (set/restore root-addr storage
                                                  {:sync? false
                                                   :count 5
                                                   :shift 0}))]
                (is (= 5 (count restored)))
                (is (= [10 20 30 40 50] (vec restored)))))))
        
        (done)))))

(deftest large-set-test
  (testing "Large set with sync storage - 10k elements"
    (let [storage (utils/make-sync-storage)
          s0 (set/sorted-set* {:storage storage})
          n 10000
          nums (shuffle (range n))
          s-final (reduce set/conj s0 nums)]
      
      (is (= n (count s-final)))
      ;; Don't convert entire set to vec for 10k elements
      ;; Just check some samples
      (println "s-final count:" (count s-final) "root:" (.-root s-final))
      (try
        (is (= 0 (first s-final)))
        (catch js/Error e
          (println "Error in first:" (.-message e))
          (println "root:" (.-root s-final))
          (println "root type:" (type (.-root s-final)))
          (throw e)))
      (is (= 9999 (last s-final)))
      
      ;; Test slicing
      (let [slice (set/slice s-final 2500 2525)]
        (is (= (range 2500 2526) (vec slice))))
      
      ;; Test reverse slice
      (let [rslice (set/rslice s-final 7525 7500)]
        (is (= (reverse (range 7500 7526)) (vec rslice))))
      
      ;; Store to storage
      (println "About to store set with" n "elements")
      (let [root-addr (set/store-set s-final)]
        (println "Stored set, root address:" root-addr)
        (is (some? root-addr))
        
        ;; Restore from storage
        (let [restored (set/restore root-addr storage
                                       {:count n
                                        :shift (.-shift s-final)})]
          (is (= n (count restored)))
          
          ;; Verify first 100 and last 100 elements
          (println "restored:" restored "root:" (.-root restored))
          (println "root type:" (type (.-root restored)))
          (is (= (range 100) (vec (set/slice restored 0 99))))
          (is (= (range 9900 10000) (vec (set/slice restored 9900 9999))))
          
          ;; Test operations on restored set
          (let [restored-with-new (set/conj restored 10000)]
            (is (= (inc n) (count restored-with-new)))
            (is (contains? restored-with-new 10000)))
          
          ;; Test removing from restored set
          (let [restored-without (set/disj restored 5000)]
            (is (= (dec n) (count restored-without)))
            (is (not (contains? restored-without 5000)))))))))

(deftest error-handling-test
  (testing "Error handling not implemented for async ops"
    (is true "Error handling test skipped")))

(deftest custom-comparator-test
  (testing "Custom comparator with storage"
    (let [storage (utils/make-sync-storage)
          s0 (set/sorted-set* {:storage storage
                               :comparator >})
          s1 (set/conj s0 1)
          s2 (set/conj s1 2)
          s3 (set/conj s2 3)]
      
      (is (= [3 2 1] (vec s3)))
      
      ;; Override comparator
      (let [s4 (set/conj s3 1.5 <)]
        (is (= [1.5 3 2 1] (vec s4)))))))

(deftest large-async-set-test
  (async done
    (go
      (testing "Large set with async storage - 10k elements"
        (try
          (let [storage (utils/make-async-storage)
                s0 (set/sorted-set* {:storage storage})
                n 10000
                nums (shuffle (range n))]
            
            (println "\nCreating async set with" n "elements...")
            ;; Build the set asynchronously
            (let [s-final (loop [s s0
                                 remaining nums]
                            (if (empty? remaining)
                              s
                              (let [next-s (<! (set/conj s (first remaining) compare {:sync? false}))]
                                (when (= 0 (mod (count next-s) 1000))
                                  (println "  Added" (count next-s) "elements..."))
                                (recur next-s (rest remaining)))))]
              
              (is (= n (count s-final)))
              (println "Async set created with" n "elements")
              
              ;; Check first and last
              (is (= 0 (first s-final)))
              (is (= 9999 (last s-final)))
              
              ;; Test slicing
              (let [slice (set/slice s-final 2500 2525)]
                (is (= (range 2500 2526) (vec slice))))
              
              ;; Store to storage asynchronously
              (println "\nStoring async set with" n "elements...")
              (let [store-result (set/store-set s-final {:sync? false})]
                (println "Store result type:" (type store-result))
                (let [root-addr (<! store-result)]
                  (println "Stored successfully, root address:" root-addr)
                  (is (some? root-addr))
                  
                  ;; Check storage size
                  (let [storage-data @(:*store storage)]
                    (println "Storage contains" (count storage-data) "nodes")
                    
                    ;; Count node types
                    (let [node-types (group-by :type (vals storage-data))]
                      (println "  -" (count (:node node-types)) "branch nodes")
                      (println "  -" (count (:leaf node-types)) "leaf nodes")))
                  
                  ;; Restore from storage asynchronously
                  (println "\nRestoring async set from storage...")
                  (let [restored (<! (set/restore root-addr storage
                                                 {:sync? false
                                                  :count n
                                                  :shift (.-shift s-final)}))]
                  (is (= n (count restored)))
                  (println "Restored successfully, count:" (count restored))
                  
                  ;; Verify data integrity
                  (println "\nVerifying data integrity...")
                  (is (= 0 (first restored)))
                  (is (= 9999 (last restored)))
                  
                  ;; Check a sample of elements
                  (doseq [i (range 0 10000 1000)]
                    (is (contains? restored i) (str "Missing element: " i)))
                  
                  ;; Verify slices
                  (is (= (range 100) (vec (set/slice restored 0 99))))
                  (is (= (range 5000 5100) (vec (set/slice restored 5000 5099))))
                  (is (= (range 9900 10000) (vec (set/slice restored 9900 9999))))
                  
                  ;; Test operations on restored set
                  (println "\nTesting operations on restored async set...")
                  (let [restored-with-new (<! (set/conj restored 10000 compare {:sync? false}))]
                    (is (= (inc n) (count restored-with-new)))
                    (is (contains? restored-with-new 10000)))
                  
                  ;; Test removing from restored set
                  (let [restored-without (<! (set/disj restored 5000 compare {:sync? false}))]
                    (is (= (dec n) (count restored-without)))
                    (is (not (contains? restored-without 5000))))
                  
                  (println "\nLarge async set test completed successfully!"))))))
          
          (catch js/Error e
            (println "Error in large async test:" (.-message e))
            (println "Stack:" (.-stack e))
            (throw e))
          (finally
            (done)))))))

;; Run tests
(defn ^:export run-tests []
  (cljs.test/run-tests))

;; Auto-run tests when namespace is loaded
(cljs.test/run-tests)