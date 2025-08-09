(ns me.tonsky.persistent-sorted-set.test.sync-only
  (:require
   [cljs.test :refer-macros [deftest testing is]]
   [me.tonsky.persistent-sorted-set :as set]
   [me.tonsky.persistent-sorted-set.async-utils :as utils]))

(deftest sync-storage-test
  (testing "Synchronous storage operations"
    (let [storage (utils/make-sync-storage)
          _ (println "Created storage:" storage)
          s0 (set/sorted-set* {:storage storage})
          _ (println "Created s0:" s0 "root:" (.-root s0))
          s1 (set/conj s0 1)
          _ (println "Created s1:" s1 "root:" (.-root s1))
          s2 (set/conj s1 2)
          s3 (set/conj s2 3)]
      
      (is (= 0 (count s0)))
      (is (= 1 (count s1)))
      (is (= 2 (count s2)))
      (is (= 3 (count s3)))
      
      (println "About to vec s3, type:" (type s3))
      (println "s3 root:" (.-root s3))
      (try
        (is (= [1 2 3] (vec s3)))
        (catch js/Error e
          (println "Error in vec:" (.-message e))
          (println "Stack:" (.-stack e))
          (throw e)))
      (is (contains? s3 2))
      (is (not (contains? s3 4)))
      
      ;; Test disj
      (let [s4 (set/disj s3 2)]
        (is (= 2 (count s4)))
        (is (= [1 3] (vec s4)))
        (is (not (contains? s4 2)))))))

(cljs.test/run-tests)