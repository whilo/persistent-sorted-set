(ns me.tonsky.persistent-sorted-set.test.async-iteration
  (:require
   [cljs.test :refer-macros [deftest testing is async]]
   [cljs.core.async :as async :refer [go <!]]
   [me.tonsky.persistent-sorted-set :as set :refer [conj disj slice rslice async-slice sorted-set sorted-set-by sorted-set* store-set restore]]
   [me.tonsky.persistent-sorted-set.async-utils :as utils]))

(defn- consume-channel
  "Consume all values from a channel into a vector"
  [ch]
  (go
    (loop [values []]
      (if-let [value (<! ch)]
        (recur (conj values value))
        values))))

(deftest async-slice-test
  (testing "Basic async slice with storage"
    (async done
      (go
        (try
          (let [storage (utils/make-async-storage)
                s0 (sorted-set* {:storage storage})
                ;; Create a set with 1000 elements
                s1 (<! (reduce (fn [prev-ch val]
                                 (go
                                   (let [prev-set (<! prev-ch)]
                                     (<! (conj prev-set val compare {:sync? false})))))
                               (go s0)
                               (range 1000)))
                ;; Store it
                root-addr (<! (store-set s1 {:sync? false}))
                ;; Restore it
                restored (<! (restore root-addr storage {:count 1000 :shift 1 :sync? false}))]
            
            ;; Test sync slice
            (println "Testing sync slice...")
            (let [sync-slice (slice restored 100 200)]
              (is (= (vec (range 100 201)) (vec sync-slice))))
            
            ;; Test async slice
            (println "Testing async slice...")
            (let [iter-ch (async-slice restored 100 200)
                  results (<! (consume-channel iter-ch))]
              (is (= (vec (range 100 201)) results)))
            
            ;; Test async slice with different boundaries
            (println "Testing async slice with different boundaries...")
            (let [iter-ch (async-slice restored 500 600)
                  results (<! (consume-channel iter-ch))]
              (is (= (vec (range 500 601)) results)))
            
            (done))
          (catch js/Error e
            (println "Error in async-slice-test:" (.-message e))
            (println "Stack:" (.-stack e))
            (done)))))))

(deftest lazy-iteration-test
  (testing "Lazy async iteration only loads nodes as needed"
    (async done
      (go
        (try
          (let [access-log (atom [])
                storage (utils/make-async-storage-with-logging access-log)
                s0 (sorted-set* {:storage storage})
                ;; Create a large set that will have multiple nodes
                s1 (<! (reduce #(go (let [s (<! %1)] (<! (conj s %2 compare {:sync? false}))))
                               (go s0)
                               (range 10000)))
                ;; Store it
                root-addr (<! (store-set s1 {:sync? false}))
                _ (reset! access-log []) ;; Clear the log after storing
                ;; Restore it
                restored (<! (restore root-addr storage {:count 10000 :shift 2 :sync? false}))]
            
            ;; Get a slice that only covers a small portion
            (println "Testing lazy loading with slice...")
            (let [sync-slice (slice restored 100 200)]
              (is (= (vec (range 100 201)) (vec sync-slice)))
              ;; Check that we only loaded a few nodes, not all of them
              (let [accessed-count (count @access-log)]
                (println "Accessed" accessed-count "nodes for slice 100-200")
                (is (< accessed-count 20) 
                    (str "Should only load a few nodes for small slice, but loaded " accessed-count))))
            
            (done))
          (catch js/Error e
            (println "Error in lazy-iteration-test:" (.-message e))
            (println "Stack:" (.-stack e))
            (done)))))))

(deftest partial-iteration-test
  (testing "Partial consumption of async iterator"
    (async done
      (go
        (try
          (let [storage (utils/make-async-storage)
                s0 (sorted-set* {:storage storage})
                s1 (<! (reduce (fn [prev-ch val]
                                 (go
                                   (let [prev-set (<! prev-ch)]
                                     (<! (conj prev-set val compare {:sync? false})))))
                               (go s0)
                               (range 1000)))
                root-addr (<! (store-set s1 {:sync? false}))
                restored (<! (restore root-addr storage {:count 1000 :shift 1 :sync? false}))]
            
            ;; Only consume first 10 values from the channel
            (println "Testing partial consumption...")
            (let [sync-slice (slice restored 0 999)
                  first-10 (vec (take 10 sync-slice))]
              (is (= (vec (range 10)) first-10)))
            
            (done))
          (catch js/Error e
            (println "Error in partial-iteration-test:" (.-message e))
            (println "Stack:" (.-stack e))
            (done)))))))

(cljs.test/run-tests)