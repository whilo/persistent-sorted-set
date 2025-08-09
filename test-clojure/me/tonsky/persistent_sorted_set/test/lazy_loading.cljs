(ns me.tonsky.persistent-sorted-set.test.lazy-loading
  (:require
   [cljs.test :refer-macros [deftest testing is async]]
   [cljs.core.async :as async :refer [go <!]]
   [me.tonsky.persistent-sorted-set :as set]))

(defrecord TrackedAsyncStorage [delay-ms *store *access-log]
  set/IStorage
  (-restore [this address]
    (go
      (swap! *access-log conj {:op :restore :address address :time (js/Date.now)})
      (<! (async/timeout delay-ms))
      (if-let [{:keys [type keys addresses]} (get @*store address)]
        (case type
          :node (set/make-node-from-storage keys (vec addresses))
          :leaf (set/make-leaf-from-storage keys))
        (throw (ex-info "Node not found" {:address address})))))
  
  (-accessed [this address]
    (swap! *access-log conj {:op :accessed :address address :time (js/Date.now)}))
  
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
        (swap! *access-log conj {:op :store :address addr :time (js/Date.now)})
        addr)))
  
  (-delete [this addresses]
    (go
      (<! (async/timeout delay-ms))
      (doseq [addr addresses]
        (swap! *store dissoc addr)
        (swap! *access-log conj {:op :delete :address addr :time (js/Date.now)})))))

(defn make-tracked-storage 
  ([] (make-tracked-storage 5))
  ([delay-ms] (->TrackedAsyncStorage delay-ms (atom {}) (atom []))))

(deftest verify-lazy-loading
  (testing "Nodes are only loaded when accessed during iteration"
    (async done
      (go
        (try
          (let [access-log (atom [])
                storage (->TrackedAsyncStorage 5 (atom {}) access-log)
                ;; Create a large set with multiple levels
                s0 (set/sorted-set* {:storage storage})
                s1 (reduce #(go (let [s (<! %1)] (<! (set/conj s %2 compare {:sync? false}))))
                          (go s0)
                          (range 1000))
                s1 (<! s1)
                _ (println "Created set with 1000 elements")
                
                ;; Store the set
                store-info (<! (set/store-set s1 {:sync? false}))
                _ (println "Stored set, store info:" store-info)
                
                ;; Clear access log after storing
                _ (reset! access-log [])
                
                ;; Restore the set (only root should be loaded)
                s2 (<! (set/restore store-info storage {:sync? false}))
                restore-accesses (count (filter #(= (:op %) :restore) @access-log))
                _ (println "After restore, loaded" restore-accesses "nodes")
                _ (is (= 1 restore-accesses) "Should only load root node on restore")
                
                ;; Clear log again
                _ (reset! access-log [])
                
                ;; Iterate over a small slice (should only load necessary nodes)
                iter-ch (<! (set/async-slice s2 100 110))
                results (atom [])]
            
            ;; Collect elements
            (loop []
              (when-let [elem (<! iter-ch)]
                (swap! results conj elem)
                (recur)))
            
            (let [slice-accesses (count (filter #(= (:op %) :restore) @access-log))]
              (println "For slice 100-110, loaded" slice-accesses "nodes")
              (println "Results:" @results)
              (is (= (vec (range 100 111)) @results) "Should get correct elements")
              (is (< slice-accesses 10) 
                  (str "Should only load a few nodes for small slice, but loaded " slice-accesses)))
            
            ;; Clear log and try a different slice
            (reset! access-log [])
            
            ;; Iterate over a different range
            (let [iter-ch2 (<! (set/async-slice s2 500 510))
                  results2 (atom [])]
              (loop []
                (when-let [elem (<! iter-ch2)]
                  (swap! results2 conj elem)
                  (recur)))
              
              (let [slice-accesses (count (filter #(= (:op %) :restore) @access-log))]
                (println "For slice 500-510, loaded" slice-accesses "nodes")
                (is (= (vec (range 500 511)) @results2) "Should get correct elements")
                (is (< slice-accesses 10) 
                    (str "Should only load a few nodes for second slice, but loaded " slice-accesses))))
            
            (done))
          (catch js/Error e
            (println "Error in verify-lazy-loading:" (.-message e))
            (println "Stack trace:")
            (println (.-stack e))
            (done)))))))

(deftest partial-consumption-cleanup
  (testing "Partial iteration doesn't load unnecessary nodes"
    (async done
      (go
        (try
          (let [access-log (atom [])
                storage (->TrackedAsyncStorage 5 (atom {}) access-log)
                s0 (set/sorted-set* {:storage storage})
                s1 (reduce #(go (let [s (<! %1)] (<! (set/conj s %2 compare {:sync? false}))))
                          (go s0)
                          (range 1000))
                s1 (<! s1)
                store-info (<! (set/store-set s1 {:sync? false}))
                _ (reset! access-log [])
                s2 (<! (set/restore store-info storage {:sync? false}))
                _ (reset! access-log [])
                
                ;; Start iteration but only consume first 5 elements
                iter-ch (<! (set/async-slice s2 0 999))
                first-5 (atom [])]
            
            ;; Only take 5 elements
            (dotimes [_ 5]
              (when-let [elem (<! iter-ch)]
                (swap! first-5 conj elem)))
            
            ;; Close the channel to stop iteration
            (async/close! iter-ch)
            
            (let [partial-accesses (count (filter #(= (:op %) :restore) @access-log))]
              (println "For partial iteration (5 elements), loaded" partial-accesses "nodes")
              (is (= [0 1 2 3 4] @first-5) "Should get first 5 elements")
              (is (<= partial-accesses 3) 
                  (str "Should only load minimal nodes for 5 elements, but loaded " partial-accesses)))
            
            (done))
          (catch js/Error e
            (println "Error in partial-consumption-cleanup:" (.-message e))
            (println "Stack:" (.-stack e))
            (done)))))))

(deftest async-lookup-lazy-loading
  (testing "Async lookup only loads necessary path to element"
    (async done
      (go
        (try
          (let [access-log (atom [])
                storage (->TrackedAsyncStorage 5 (atom {}) access-log)
                s0 (set/sorted-set* {:storage storage})
                s1 (reduce #(go (let [s (<! %1)] (<! (set/conj s %2 compare {:sync? false}))))
                          (go s0)
                          (range 1000))
                s1 (<! s1)
                store-info (<! (set/store-set s1 {:sync? false}))
                _ (reset! access-log [])
                s2 (<! (set/restore store-info storage {:sync? false}))]
            
            ;; Clear log after restore
            (reset! access-log [])
            
            ;; Lookup a single element
            (let [found (<! (set/lookup-async s2 500))
                  lookup-accesses (count (filter #(= (:op %) :restore) @access-log))]
              (println "For lookup of 500, loaded" lookup-accesses "nodes")
              (is (= 500 found) "Should find the element")
              (is (<= lookup-accesses 3) 
                  (str "Should only load path to element, but loaded " lookup-accesses " nodes")))
            
            ;; Clear log and check contains
            (reset! access-log [])
            
            (let [contains (<! (set/contains-async? s2 750))
                  contains-accesses (count (filter #(= (:op %) :restore) @access-log))]
              (println "For contains? 750, loaded" contains-accesses "nodes")
              (is (true? contains) "Should contain the element")
              (is (<= contains-accesses 3) 
                  (str "Should only load path for contains, but loaded " contains-accesses " nodes")))
            
            (done))
          (catch js/Error e
            (println "Error in async-lookup-lazy-loading:" (.-message e))
            (println "Stack:" (.-stack e))
            (done)))))))

(cljs.test/run-tests)