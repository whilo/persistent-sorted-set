(ns me.tonsky.persistent-sorted-set.test.cloroutine-storage-tests
  "Comprehensive integration tests for storage with both sync and async implementations"
  (:require
   [cljs.test :refer-macros [deftest testing is async]]
   #_[cljs.core.async :as async :refer [sp ?]]
   [missionary.core :refer [sp] :as m]
   [me.tonsky.persistent-sorted-set :as set]
   [me.tonsky.persistent-sorted-set.test.async-utils :as utils])
  (:require-macros
   [missionary.core :refer [?]]))

(deftest sync-storage-integration-test
  #_(testing "Full integration test with sync storage - 10k elements"
    (println "Running sync-storage-integration-test")
    (let [storage (utils/make-sync-storage)
          s0 (set/sorted-set* {:storage storage})
          n 10000
          nums (shuffle (range n))
          s-final (reduce set/conj s0 nums)]

      (is (= n (count s-final)))

      ;; Check first and last
      (is (= 0 (first s-final)))
      (is (= 9999 (last s-final)))

      ;; Test slicing on original set
      (let [slice (set/slice s-final 2500 2525)]
        (is (= (range 2500 2526) (vec slice))))

      ;; Test reverse slicing
      ;; rslice creates a reverse iterator from key-from to key-to
      (let [rslice (set/rslice s-final 7500 7525)]
        (when (some? rslice)
          (is (= (reverse (range 7500 7526)) (vec rslice)))))

      ;; Store to storage
      (let [store-info (set/store-set s-final)]
        (is (some? store-info))
        (is (map? store-info))
        (is (:root-address store-info))

        ;; Check storage size
        (let [storage-data @(:*store storage)]
          (is (> (count storage-data) 100) "Should have many nodes stored"))

        ;; Restore from storage
        (let [restored (set/restore store-info storage)]
          (is (= n (count restored)))

          ;; Verify data integrity
          (is (= 0 (first restored)))
          (is (= 9999 (last restored)))

          ;; Check a sample of elements using contains?
          (doseq [i (range 0 10000 1000)]
            (is (contains? restored i) (str "Missing element: " i)))

          ;; Verify slices on restored set
          (is (= (range 100) (vec (set/slice restored 0 99))))
          (is (= (range 5000 5100) (vec (set/slice restored 5000 5099))))
          (is (= (range 9900 10000) (vec (set/slice restored 9900 9999))))

          ;; Test operations on restored set
          (testing "conj on restored set"
            (let [restored-with-new (set/conj restored 10000)]
              (is (= (inc n) (count restored-with-new)))
              (is (contains? restored-with-new 10000))))

          ;; Test removing from restored set
          (testing "disj on restored set"
            (let [restored-without (set/disj restored 5000)]
              (is (= (dec n) (count restored-without)))
              (is (not (contains? restored-without 5000)))))

          ;; Test iteration over restored set
          (testing "iteration over restored set"
            (let [first-100 (take 100 restored)]
              (is (= (range 100) first-100)))
            (let [last-100 (take 100 (drop 9900 restored))]
              (is (= (range 9900 10000) last-100)))))))))

(deftest async-storage-integration-test
  #_
  (async done
    ;; Execute the missionary process
    ((sp
       (testing "Full integration test with async storage - 10k elements"
        (let [storage (utils/make-async-storage 1) ; 1ms delay to simulate async
              s0 (set/sorted-set* {:storage storage})
              n 10000
              nums (shuffle (range n))
              ;; Build set with async conj
              s-final (loop [s s0
                            remaining nums]
                       (if (empty? remaining)
                         s
                         (recur (? (set/conj s (first remaining) compare {:sync? false}))
                                (rest remaining))))]

          (is (= n (count s-final)))

          ;; Check first and last
          (is (= 0 (first s-final)))
          (is (= 9999 (last s-final)))

          ;; Store to storage (async)
          (let [store-info (? (set/store-set s-final {:sync? false}))]
            (is (some? store-info))
            (is (map? store-info))
            (is (:root-address store-info))

            ;; Check storage size
            (let [storage-data @(:*store storage)]
              (is (> (count storage-data) 100) "Should have many nodes stored"))

            ;; Restore from storage (async)
            (let [restored (? (set/restore store-info storage {:sync? false}))]
              (is (= n (count restored)))

              ;; Verify data integrity
              (is (= 0 (first restored)))
              (is (= 9999 (last restored)))

              ;; Test async operations on restored set
              (testing "async conj on restored set"
                (let [restored-with-new (? (set/conj restored 10000 compare {:sync? false}))]
                  (is (= (inc n) (count restored-with-new)))
                  (is (contains? restored-with-new 10000))))

              ;; Test async disj from restored set
              (testing "async disj on restored set"
                (let [restored-without (? (set/disj restored 5000 compare {:sync? false}))]
                  (is (= (dec n) (count restored-without)))
                  (is (not (contains? restored-without 5000)))))

              ;; Test async lookup
              (testing "async lookup on restored set"
                (is (= 5000 (? (set/lookup-async restored 5000))))
                (is (nil? (? (set/lookup-async restored 10001))))
                (is (true? (? (set/contains-async? restored 5000))))
                (is (false? (? (set/contains-async? restored 10001)))))

              ;; Test async slicing
              (testing "async slicing on restored set"
                (let [slice-flow (? (set/-async-slice restored 100 199 compare))
                      slice-items (? (m/reduce conj [] slice-flow))]
                  (is (= (range 100 200) slice-items) "Should have all items in range"))))))
       (done)))
     (fn [result] (println "Async test completed with result:" result))
     (fn [error]
       (println "Async test failed with error:" (str error))
       (done)))))

(deftest mixed-storage-operations-test
  #_
  (testing "Mixed sync/async operations with storage"
    (println "Running mixed-storage-operations-test")
    (let [storage (utils/make-sync-storage)
          s0 (set/sorted-set* {:storage storage})
          ;; Build a smaller set for quicker testing
          n 1000
          s1 (reduce set/conj s0 (shuffle (range n)))]

      ;; Store synchronously
      (let [store-info (set/store-set s1)]
        ;; Restore synchronously
        (let [restored (set/restore store-info storage)]

          ;; Mix of operations
          (let [s2 (-> restored
                      (set/conj 1000)    ; sync conj
                      (set/disj 500)     ; sync disj
                      (set/conj 1001)    ; sync conj
                      (set/disj 100))]   ; sync disj

            (is (= n (count s2))) ; n - 2 removed + 2 added = n
            (is (contains? s2 1000))
            (is (contains? s2 1001))
            (is (not (contains? s2 500)))
            (is (not (contains? s2 100)))

            ;; Verify iteration still works
            (is (= 0 (first s2)))
            (is (= 1001 (last s2)))

            ;; Store the modified set
            (let [store-info2 (set/store-set s2)]
              ;; Restore and verify
              (let [restored2 (set/restore store-info2 storage)]
                (is (= n (count restored2)))
                (is (= (vec s2) (vec restored2)))))))))))

(deftest custom-comparator-storage-test
  (testing "Custom comparator with storage"
    (let [storage (utils/make-sync-storage)
          ;; Create set with reverse comparator
          s0 (set/sorted-set* {:storage storage
                               :comparator >})
          s1 (set/conj s0 1)
          s2 (set/conj s1 2)
          s3 (set/conj s2 3)]

      (is (= [3 2 1] (vec s3)))

      ;; Test overriding comparator in conj
      (let [s4 (set/conj s3 1.5 <)]
        (is (= [1.5 3 2 1] (vec s4))))

      ;; Store and restore with custom comparator
      (let [root-addr (set/store-set s3)]
        (let [restored (set/restore root-addr storage
                                   {:count 3
                                    :shift (.-shift s3)
                                    :comparator >})]
          (is (= [3 2 1] (vec restored)))

          ;; Operations on restored set maintain comparator
          (let [s5 (set/conj restored 2.5)]
            (is (= [3 2.5 2 1] (vec s5)))))))))

;; Run tests when namespace is loaded
; (cljs.test/run-tests)