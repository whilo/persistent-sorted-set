(ns me.tonsky.persistent-sorted-set.async-utils
  "Utilities for async+sync operations with persistent sorted set"
  (:require
   #?(:clj  [clojure.core.async :as async :refer [go <! >!]]
      :cljs [cljs.core.async :as async :refer [<! >!]])
   #?(:clj [me.tonsky.persistent-sorted-set.macros :refer [async+sync]]))
  #?(:cljs
     (:require-macros
      [cljs.core.async.macros :refer [go]]
      [me.tonsky.persistent-sorted-set.macros :refer [async+sync]])))

;; Re-export the async+sync macro and translation
(def storage-translation
  '{go do
    <! do})

;; Helper to serialize nodes for storage
(defn serialize-node [node]
  #?(:clj
     (cond
       (instance? me.tonsky.persistent_sorted_set.Branch node)
       {:type :branch
        :keys (.keys node)
        :addresses (.addresses node)}
       
       (instance? me.tonsky.persistent_sorted_set.Leaf node)
       {:type :leaf
        :keys (.keys node)}
       
       :else
       (throw (ex-info "Unknown node type" {:node node})))
     
     :cljs
     (cond
       (instance? me.tonsky.persistent-sorted-set/Node node)
       {:type :node
        :keys (.-keys node)
        :pointers (count (.-pointers node))}
       
       (instance? me.tonsky.persistent-sorted-set/Leaf node)
       {:type :leaf
        :keys (.-keys node)}
       
       :else
       (throw (ex-info "Unknown node type" {:node node})))))

;; Helper to deserialize nodes from storage
(defn deserialize-node [data]
  #?(:clj
     (case (:type data)
       :branch (me.tonsky.persistent_sorted_set.Branch. 
                (int (:level data))
                (:keys data)
                (:addresses data)
                (me.tonsky.persistent_sorted_set.Settings.))
       :leaf (me.tonsky.persistent_sorted_set.Leaf.
              (:keys data)
              (me.tonsky.persistent_sorted_set.Settings.)))
     
     :cljs
     (case (:type data)
       :node (throw (ex-info "Cannot deserialize Node without children" {:data data}))
       :leaf (me.tonsky.persistent-sorted-set/Leaf. (:keys data) nil))))

;; Test storage implementations

(defrecord TestSyncStorage [*store]
  #?(:clj  me.tonsky.persistent_sorted_set.IStorage
     :cljs me.tonsky.persistent-sorted-set/IStorage)
  
  (restore [_ address]
    (if-let [data (get @*store address)]
      (deserialize-node data)
      (throw (ex-info "Node not found" {:address address}))))
  
  (store [_ node existing-address]
    (let [addr (or existing-address (random-uuid))
          data (serialize-node node)]
      (swap! *store assoc addr data)
      addr))
  
  (accessed [_ address] nil)
  
  (delete [_ addresses]
    (swap! *store #(apply dissoc % addresses))))

(defrecord TestAsyncStorage [*store delay-ms]
  #?(:clj  me.tonsky.persistent_sorted_set.IStorage
     :cljs me.tonsky.persistent-sorted-set/IStorage)
  
  (restore [_ address]
    (go
      (<! (async/timeout delay-ms))
      (if-let [data (get @*store address)]
        (deserialize-node data)
        (throw (ex-info "Node not found" {:address address})))))
  
  (store [_ node existing-address]
    (go
      (<! (async/timeout delay-ms))
      (let [addr (or existing-address (random-uuid))
            data (serialize-node node)]
        (swap! *store assoc addr data)
        addr)))
  
  (accessed [_ address]
    (go nil))
  
  (delete [_ addresses]
    (go
      (<! (async/timeout delay-ms))
      (swap! *store #(apply dissoc % addresses)))))

;; Helpers for testing
(defn make-sync-storage []
  (->TestSyncStorage (atom {})))

(defn make-async-storage 
  ([] (make-async-storage 10))
  ([delay-ms] (->TestAsyncStorage (atom {}) delay-ms)))