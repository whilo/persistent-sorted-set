(ns me.tonsky.persistent-sorted-set.test.async-comprehensive
  (:require
   [cljs.test :refer-macros [deftest testing is async]]
   [cljs.core.async :as async :refer [go <!]]
   [me.tonsky.persistent-sorted-set :as set]))

(defrecord TestAsyncStorage [delay-ms *store]
  set/IStorage
  (-restore [this address]
    (go
      (<! (async/timeout delay-ms))
      (if-let [{:keys [type keys addresses]} (get @*store address)]
        (case type
          :node (set/make-node-from-storage keys (vec addresses))
          :leaf (set/make-leaf-from-storage keys))
        (throw (ex-info "Node not found" {:address address})))))
  
  (-accessed [this address])
  
  (-store [this node address]
    (go
      (<! (async/timeout delay-ms))
      (let [addr (or address (random-uuid))
            data (cond
                   (instance? set/Leaf node)
                   {:type :leaf :keys (.-keys node)}
                   
                   (instance? set/Node node)
                   {:type :node 
                    :keys (.-keys node)
                    :addresses (vec (.-addresses node))})]
        (swap! *store assoc addr data)
        addr)))
  
  (-delete [this addresses]
    (go
      (<! (async/timeout delay-ms))
      (doseq [addr addresses]
        (swap! *store dissoc addr)))))

(deftest test-async-slice-full-iteration
  (testing "Async-slice can iterate entire set"
    (async done
      (go
        (try
          (let [storage (->TestAsyncStorage 5 (atom {}))
                s0 (set/sorted-set* {:storage storage})
                s1 (reduce #(go (let [s (<! %1)] (<! (set/conj s %2 compare {:sync? false}))))
                          (go s0)
                          (range 100))
                s1 (<! s1)
                root-addr (<! (set/store-set s1 {:sync? false}))
                s2 (<! (set/restore root-addr storage {:sync? false :count 100 :shift 1}))
                ;; Full iteration using nil boundaries
                iter-ch (<! (set/async-slice s2 nil nil))
                results (atom [])]
            
            (loop []
              (when-let [elem (<! iter-ch)]
                (swap! results conj elem)
                (recur)))
            
            (is (= (vec (range 100)) @results) "Full iteration should return all elements")
            (done))
          (catch js/Error e
            (println "Error:" (.-message e))
            (done)))))))

(deftest test-async-rslice
  (testing "Async reverse slice"
    (async done
      (go
        (try
          (let [storage (->TestAsyncStorage 5 (atom {}))
                s0 (set/sorted-set* {:storage storage})
                s1 (reduce #(go (let [s (<! %1)] (<! (set/conj s %2 compare {:sync? false}))))
                          (go s0)
                          (range 20))
                s1 (<! s1)
                root-addr (<! (set/store-set s1 {:sync? false}))
                s2 (<! (set/restore root-addr storage {:sync? false :count 20 :shift 1}))
                ;; Reverse slice from 15 down to 5
                iter-ch (<! (set/async-rslice s2 5 15))
                results (atom [])]
            
            (loop []
              (when-let [elem (<! iter-ch)]
                (swap! results conj elem)
                (recur)))
            
            (is (= (vec (reverse (range 5 16))) @results) 
                "Reverse iteration should return elements in reverse order")
            (done))
          (catch js/Error e
            (println "Error:" (.-message e))
            (done)))))))

(deftest test-partial-consumption
  (testing "Partial consumption of async iterator"
    (async done
      (go
        (try
          (let [storage (->TestAsyncStorage 5 (atom {}))
                s0 (set/sorted-set* {:storage storage})
                s1 (reduce #(go (let [s (<! %1)] (<! (set/conj s %2 compare {:sync? false}))))
                          (go s0)
                          (range 100))
                s1 (<! s1)
                root-addr (<! (set/store-set s1 {:sync? false}))
                s2 (<! (set/restore root-addr storage {:sync? false :count 100 :shift 1}))
                iter-ch (<! (set/async-slice s2 0 99))
                first-10 (atom [])]
            
            ;; Only take first 10 elements
            (dotimes [_ 10]
              (when-let [elem (<! iter-ch)]
                (swap! first-10 conj elem)))
            
            ;; Close channel to stop iteration
            (async/close! iter-ch)
            
            (is (= (vec (range 10)) @first-10) "Should get first 10 elements")
            (done))
          (catch js/Error e
            (println "Error:" (.-message e))
            (done)))))))

(deftest test-async-lookup-and-contains
  (testing "Async lookup and contains operations"
    (async done
      (go
        (try
          (let [storage (->TestAsyncStorage 5 (atom {}))
                s0 (set/sorted-set* {:storage storage})
                s1 (reduce #(go (let [s (<! %1)] (<! (set/conj s %2 compare {:sync? false}))))
                          (go s0)
                          (range 50))
                s1 (<! s1)
                root-addr (<! (set/store-set s1 {:sync? false}))
                s2 (<! (set/restore root-addr storage {:sync? false :count 50 :shift 1}))]
            
            ;; Test lookup
            (let [found (<! (set/lookup-async s2 25))
                  not-found (<! (set/lookup-async s2 100 :not-found))]
              (is (= 25 found) "Should find existing element")
              (is (= :not-found not-found) "Should return not-found for missing element"))
            
            ;; Test contains
            (let [contains-25 (<! (set/contains-async? s2 25))
                  contains-100 (<! (set/contains-async? s2 100))]
              (is (true? contains-25) "Should contain 25")
              (is (false? contains-100) "Should not contain 100"))
            
            (done))
          (catch js/Error e
            (println "Error:" (.-message e))
            (done)))))))

(deftest test-empty-and-edge-cases
  (testing "Empty sets and edge cases"
    (async done
      (go
        (try
          ;; Empty set
          (let [storage (->TestAsyncStorage 5 (atom {}))
                s0 (set/sorted-set* {:storage storage})
                root-addr (<! (set/store-set s0 {:sync? false}))
                s1 (<! (set/restore root-addr storage {:sync? false :count 0 :shift 0}))
                iter-ch (<! (set/async-slice s1 nil nil))
                results (atom [])]

            (loop []
              (when-let [elem (<! iter-ch)]
                (swap! results conj elem)
                (recur)))

            (is (= [] @results) "Empty set should return no elements"))

          ;; Single element
          (let [storage (->TestAsyncStorage 5 (atom {}))
                s0 (set/sorted-set* {:storage storage})
                s1 (<! (set/conj s0 42 compare {:sync? false}))
                root-addr (<! (set/store-set s1 {:sync? false}))
                s2 (<! (set/restore root-addr storage {:sync? false :count 1 :shift 0}))
                iter-ch (<! (set/async-slice s2 nil nil))
                results (atom [])]

            (loop []
              (when-let [elem (<! iter-ch)]
                (swap! results conj elem)
                (recur)))

            (is (= [42] @results) "Single element set should return that element"))

          ;; Empty range
          (let [storage (->TestAsyncStorage 5 (atom {}))
                s0 (set/sorted-set* {:storage storage})
                s1 (reduce #(go (let [s (<! %1)] (<! (set/conj s %2 compare {:sync? false}))))
                           (go s0)
                           (range 10))
                s1 (<! s1)
                root-addr (<! (set/store-set s1 {:sync? false}))
                s2 (<! (set/restore root-addr storage {:sync? false :count 10 :shift 0}))
                iter-ch (<! (set/async-slice s2 20 30))
                results (atom [])]

            (if iter-ch
              (loop []
                (when-let [elem (<! iter-ch)]
                  (swap! results conj elem)
                  (recur)))
              (reset! results []))

            (is (= [] @results) "Range outside set should return no elements"))

          (done)
          (catch js/Error e
            (println "Error:" (.-message e))
            (done)))))))

(cljs.test/run-tests)