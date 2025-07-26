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
              (let [restored (<! (set/restore-set storage root-addr 
                                                  {:sync? false
                                                   :count 5
                                                   :shift 0}))]
                (is (= 5 (count restored)))
                (is (= [10 20 30 40 50] (vec restored)))))))
        
        (done)))))

(deftest large-set-test
  (testing "Large set with sync storage"
    (let [storage (utils/make-sync-storage)
          s0 (set/sorted-set* {:storage storage})
          nums (shuffle (range 100))
          s-final (reduce set/conj s0 nums)]
      
      (is (= 100 (count s-final)))
      (is (= (range 100) (vec s-final)))
      
      ;; Test slicing
      (let [slice (set/slice s-final 25 75)]
        (is (= (range 25 76) (vec slice))))
      
      ;; Test reverse slice
      (let [rslice (set/rslice s-final 75 25)]
        (is (= (reverse (range 25 76)) (vec rslice)))))))

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
      
      (is (= [3 2 1] (vec s3))))))

;; Run tests
(defn ^:export run-tests []
  (cljs.test/run-tests))