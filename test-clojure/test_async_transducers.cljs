(ns test-async-transducers
  (:require [me.tonsky.persistent-sorted-set :as set]
            [me.tonsky.persistent-sorted-set.async-utils-simple :as utils]
            [await-cps :refer [await run-async]])
  (:require-macros [await-cps :refer [async]]))

;; Simple transducer support for AsyncSeq

(defn async-transduce
  "Reduce an AsyncSeq with a transducer.
   Returns a CPS function that yields the reduced value."
  ([xform f init async-seq]
   (async
     (let [rf (xform f)]
       (loop [result init
              s async-seq]
         (if s
           (let [v (await (set/-afirst s))]
             (if v
               (let [result' (rf result v)]
                 (if (reduced? result')
                   @result'
                   (let [rest-s (await (set/-arest s))]
                     (recur result' rest-s))))
               (rf result)))  ; completion
           (rf result)))))))  ; completion

(defn async-into
  "Pour an AsyncSeq into a collection via a transducer."
  ([to xform async-seq]
   (async-transduce xform conj to async-seq)))

(defn run-test []
  (async
    (println "\n=== Testing Async Transducers ===")

    ;; Create test set
    (let [storage (utils/make-async-storage 0)
          s0 (set/sorted-set* {:storage storage})
          ;; Build set with values 0-9
          async-set (await
                      (reduce (fn [s-promise n]
                               (async
                                 (let [s (if (fn? s-promise)
                                          (await s-promise)
                                          s-promise)]
                                   (await (set/conj s n compare {:sync? false})))))
                             s0
                             (range 10)))]

      (println "Set built with values:" (seq async-set))

      ;; Test 1: Identity transducer
      (println "\n1. Testing identity transducer...")
      (let [async-seq (await (set/async-slice async-set nil nil))
            result (await (async-transduce identity conj [] async-seq))]
        (println "   Result:" result)
        (assert (= result (vec (range 10)))))

      ;; Test 2: Filter transducer
      (println "\n2. Testing filter transducer...")
      (let [async-seq (await (set/async-slice async-set nil nil))
            result (await (async-transduce (filter even?) conj [] async-seq))]
        (println "   Result:" result)
        (assert (= result [0 2 4 6 8])))

      ;; Test 3: Map transducer
      (println "\n3. Testing map transducer...")
      (let [async-seq (await (set/async-slice async-set nil nil))
            result (await (async-transduce (map #(* % 2)) conj [] async-seq))]
        (println "   Result:" result)
        (assert (= result (vec (map #(* % 2) (range 10))))))

      ;; Test 4: Composed transducers
      (println "\n4. Testing composed transducers...")
      (let [async-seq (await (set/async-slice async-set nil nil))
            xform (comp (filter odd?)
                       (map #(* % 3))
                       (take 3))
            result (await (async-transduce xform conj [] async-seq))]
        (println "   Result:" result)
        (assert (= result [3 9 15])))

      ;; Test 5: Take transducer with early termination
      (println "\n5. Testing take transducer (early termination)...")
      (let [async-seq (await (set/async-slice async-set nil nil))
            result (await (async-transduce (take 5) conj [] async-seq))]
        (println "   Result:" result)
        (assert (= result [0 1 2 3 4])))

      ;; Test 6: Using async-into helper
      (println "\n6. Testing async-into helper...")
      (let [async-seq (await (set/async-slice async-set nil nil))
            result (await (async-into [] (comp (drop 2) (take 3)) async-seq))]
        (println "   Result:" result)
        (assert (= result [2 3 4])))

      ;; Test 7: Partition transducer
      (println "\n7. Testing partition transducer...")
      (let [async-seq (await (set/async-slice async-set nil nil))
            result (await (async-transduce (partition-all 3) conj [] async-seq))]
        (println "   Result:" result)
        (assert (= result [[0 1 2] [3 4 5] [6 7 8] [9]]))))

    (println "\n=== All transducer tests passed! ===")))

(defn ^:export -main []
  (println "Starting async transducer tests...")

  ;; Run async tests
  (run-async
    (run-test)
    (fn [_]
      (println "Tests completed successfully")
      (js/process.exit 0))
    (fn [e]
      (println "Error:" e)
      (when e
        (println "Stack:" (.-stack e)))
      (js/process.exit 1))))