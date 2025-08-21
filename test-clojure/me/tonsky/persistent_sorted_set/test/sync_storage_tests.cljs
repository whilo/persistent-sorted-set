(ns me.tonsky.persistent-sorted-set.test.sync-storage-tests
  (:require-macros [me.tonsky.persistent-sorted-set.test.macros :refer [testing-group]])
  (:require [cljs.test :refer-macros [deftest testing is]]
            [me.tonsky.persistent-sorted-set :as set]
            [me.tonsky.persistent-sorted-set.test.async-utils :as utils]))


(deftest sync-storage-test
  (testing "Full integration test with sync storage - 10k elements"
    (let [storage (utils/make-sync-storage)
          s0 (set/sorted-set* {:storage storage})
          n 10000
          nums (shuffle (range n))
          s-final (reduce set/conj s0 nums)]
      (and
       (is (= n (count s-final)))
       (is (= 0 (first s-final)))
       (is (= 9999 (last s-final)))
       (testing-group "slicing on original set"
         (let [slice (set/slice s-final 2500 2525)]
           (is (= (range 2500 2526) (vec slice)))))
       (testing-group "reverse slicing: from key-from to key-to"
         (let [rslice (set/rslice s-final 7500 7525)]
           (and
             (is (some? rslice))
             (is (= (reverse (range 7500 7526)) (vec rslice))))))
       (testing-group "Store to storage"
         (let [store-info (set/store-set s-final)]
           (and
            (is (some? store-info))
            (is (map? store-info))
            (is (:root-address store-info))
            (testing  "Check storage size"
              (let [storage-data @(:*store storage)]
                (is (> (count storage-data) 100) "Should have many nodes stored"))))

           ;; Restore from storage
           ; (let [restored (set/restore store-info storage)]
           ;   (is (= n (count restored)))
           ;
           ;   ;; Verify data integrity
           ;   (is (= 0 (first restored)))
           ;   (is (= 9999 (last restored)))
           ;
           ;   ;; Check a sample of elements using contains?
           ;   (doseq [i (range 0 10000 1000)]
           ;     (is (contains? restored i) (str "Missing element: " i)))
           ;
           ;   ;; Verify slices on restored set
           ;   (is (= (range 100) (vec (set/slice restored 0 99))))
           ;   (is (= (range 5000 5100) (vec (set/slice restored 5000 5099))))
           ;   (is (= (range 9900 10000) (vec (set/slice restored 9900 9999))))
           ;
           ;   ;; Test operations on restored set
           ;   (testing "conj on restored set"
           ;     (let [restored-with-new (set/conj restored 10000)]
           ;       (is (= (inc n) (count restored-with-new)))
           ;       (is (contains? restored-with-new 10000))))
           ;
           ;   ;; Test removing from restored set
           ;   (testing "disj on restored set"
           ;     (let [restored-without (set/disj restored 5000)]
           ;       (is (= (dec n) (count restored-without)))
           ;       (is (not (contains? restored-without 5000)))))
           ;
           ;   ;; Test iteration over restored set
           ;   (testing "iteration over restored set"
           ;     (let [first-100 (take 100 restored)]
           ;       (is (= (range 100) first-100)))
           ;     (let [last-100 (take 100 (drop 9900 restored))]
           ;       (is (= (range 9900 10000) last-100)))))
           ))))))