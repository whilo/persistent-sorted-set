(ns me.tonsky.persistent-sorted-set.test.await-cps-sequence-tests
  (:require [cljs.test :as test :refer [deftest]]
            [await-cps :refer [await run-async] :refer-macros [async]]
            [me.tonsky.persistent-sorted-set :as set]
            [me.tonsky.persistent-sorted-set.async-transducers :as at]
            [me.tonsky.persistent-sorted-set.test.async-utils :as utils]))

(defn consume-async-seq
  "Helper to consume an entire AsyncSeq into a vector"
  [async-seq]
  (async
   (loop [s async-seq
          result []
          count 0]
     (if (and s (< count 20))  ; Safety limit
       (let [v (await (set/-afirst s))]
         (if (some? v)  ; Use some? to handle false/nil correctly
           (let [next-s (await (set/-arest s))]
             (recur next-s (conj result v) (inc count)))
           result))
       result))))

(defn do-sequence-test []
  (async
   (println "\n=== Testing async transducer functions ===")
   ;; Create test set
   (let [storage (utils/make-async-storage 0)
         _(println "storage" storage )
         s0 (set/sorted-set* {:storage storage})
         _(println "s0" s0)
         async-set (await (reduce (fn [s-promise n]
                                    (async
                                     (let [s (if (fn? s-promise)
                                               (await s-promise)
                                               s-promise)]
                                       (await (set/conj s n compare {:sync? false})))))
                                  s0
                                  (range 10)))]

     (println "Test set created:" (seq async-set))

     ;; Test 1: async-transduce with identity
     (println "\n1. Testing async-transduce with identity...")
     (let [async-seq (await (set/async-slice async-set nil nil))
           result (await (at/async-transduce identity conj [] async-seq))]
       (println "   Result:" result)
       (assert (= result (vec (range 10)))))

     ;; Test 2: async-sequence with map
     (println "\n2. Testing async-sequence with map...")
     (let [async-seq (await (set/async-slice async-set nil nil))
           mapped-seq (at/async-sequence (map #(* % 2)) async-seq)
           result (await (consume-async-seq mapped-seq))]
       (println "   Result:" result)
       (assert (= result [0 2 4 6 8 10 12 14 16 18])))

     ;; Test 3: async-sequence with filter
     (println "\n3. Testing async-sequence with filter...")
     (let [async-seq (await (set/async-slice async-set nil nil))
           filtered-seq (at/async-sequence (filter even?) async-seq)
           result (await (consume-async-seq filtered-seq))]
       (println "   Result:" result)
       (assert (= result [0 2 4 6 8])))

     ;; Test 4: async-sequence with take
     (println "\n4. Testing async-sequence with take...")
     (let [async-seq (await (set/async-slice async-set nil nil))
           taken-seq (at/async-sequence (take 5) async-seq)
           result (await (consume-async-seq taken-seq))]
       (println "   Result:" result)
       (assert (= result [0 1 2 3 4])))

     ;; Test 5: async-sequence with drop
     (println "\n5. Testing async-sequence with drop...")
     (let [async-seq (await (set/async-slice async-set nil nil))
           dropped-seq (at/async-sequence (drop 7) async-seq)
           result (await (consume-async-seq dropped-seq))]
       (println "   Result:" result)
       (assert (= result [7 8 9])))

     ;; Test 6: async-sequence with partition
     (println "\n6. Testing async-sequence with partition...")
     (let [async-seq (await (set/async-slice async-set nil nil))
           partitioned-seq (at/async-sequence (partition-all 3) async-seq)
           result (await (consume-async-seq partitioned-seq))]
       (println "   Result:" result)
       (assert (= result [[0 1 2] [3 4 5] [6 7 8] [9]])))

     ;; Test 7: async-sequence with composed transducers
     (println "\n7. Testing async-sequence with composed transducers...")
     (let [async-seq (await (set/async-slice async-set nil nil))
           xform (comp (filter odd?) (map #(* % 2)) (take 3))
           transformed-seq (at/async-sequence xform async-seq)
           result (await (consume-async-seq transformed-seq))]
       (println "   Result:" result)
       (assert (= result [2 6 10])))

     ;; Test 8: async-sequence with mapcat (inflating transducer)
     (println "\n8. Testing async-sequence with mapcat...")
     (let [async-seq (await (set/async-slice async-set 0 2))  ; Get 0, 1, 2
           mapcatted-seq (at/async-sequence (mapcat #(vector % (* % 10))) async-seq)
           result (await (consume-async-seq mapcatted-seq))]
       (println "   Result:" result)
       (assert (= result [0 0 1 10 2 20]))
       ;; Also test with larger expansion
       (let [async-seq2 (await (set/async-slice async-set 0 1))  ; Get 0, 1
             expanded-seq (at/async-sequence (mapcat #(range % (+ % 3))) async-seq2)
             result2 (await (consume-async-seq expanded-seq))]
         (println "   Expanded result:" result2)
         (assert (= result2 [0 1 2 1 2 3]))))

     ;; Test 9: async-into helper
     (println "\n9. Testing async-into...")
     (let [async-seq (await (set/async-slice async-set nil nil))
           result (await (at/async-into []
                                        (comp (filter odd?)
                                              (map #(* % 3)))
                                        async-seq))]
       (println "   Result:" result)
       (assert (= result [3 9 15 21 27])))

     ;; Test 10: async-transduce with early termination
     (println "\n10. Testing async-transduce with early termination...")
     (let [async-seq (await (set/async-slice async-set nil nil))
           ; Find first element > 5
           xf (comp (filter #(> % 5)) (take 1))
           rf (fn
                ([result] result)  ; Completion arity
                ([_ v] (reduced v)))  ; Step arity
           result (await (at/async-transduce xf rf nil async-seq))]
       (println "   Result:" result)
       (assert (= result 6)))

     (println "\n=== All async transducer tests passed! ==="))))

(deftest sequence-test
  (test/async done
    (run-async (do-sequence-test)
      (fn [ok]
        (js/console.info "sequence-test success")
        (done))
      (fn [err]
        (js/console.error err)
        (done)))))
