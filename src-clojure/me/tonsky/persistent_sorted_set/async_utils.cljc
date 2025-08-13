(ns me.tonsky.persistent-sorted-set.async-utils
  "Utilities for async+sync operations with persistent sorted set"
  (:require
   [await-cps :refer [await]]
   [me.tonsky.persistent-sorted-set :as set]
   [me.tonsky.persistent-sorted-set.arrays :as arrays]
   #?(:clj [me.tonsky.persistent-sorted-set.macros :refer [async+sync]]))
  #?(:cljs
     (:require-macros
      [await-cps :refer [async]]
      [me.tonsky.persistent-sorted-set.macros :refer [async+sync]])))

;; Re-export the async+sync macro and translation
(def storage-translation
  '{sp do
    ? do})

;; Storage serialization is handled by the storage implementations directly
;; No need for generic serialize/deserialize helpers since nodes contain all needed data)

;; Test storage implementations

(defrecord TestSyncStorage [*store]
  #?(:clj  me.tonsky.persistent_sorted_set.IStorage
     :cljs me.tonsky.persistent-sorted-set/IStorage)
  
  (-restore [this address]
    (if-let [{:keys [type keys addresses]} (get @*store address)]
      (case type
        :node
        ;; Create Node with addresses for lazy restoration (following Java pattern)
        #?(:cljs (set/make-node-from-storage keys (vec addresses)))
        
        :leaf
        #?(:cljs (set/make-leaf-from-storage keys)))
      (throw (ex-info "Node not found" {:address address}))))
  
  (-store [_ node existing-address]
    (let [addr (or existing-address (random-uuid))
          data (cond
                 ;; Node - store addresses if available, otherwise store child addresses
                 #?@(:cljs [(= (type node) set/Node)
                            (if (.-addresses node)
                              {:type :node
                               :keys (.-keys node)
                               :addresses (vec (.-addresses node))}
                              ;; Node without addresses - need to store children first
                              {:type :node
                               :keys (.-keys node)
                               :addresses []})])  ;; Empty addresses for now
                 
                 ;; Leaf node
                 #?@(:cljs [(= (type node) set/Leaf)
                            {:type :leaf
                             :keys (.-keys node)}])
                 
                 :else
                 (throw (ex-info "Unknown node type for storage" {:node node})))]
      (swap! *store assoc addr data)
      addr))
  
  (-accessed [_ address] nil)
  
  (-delete [_ addresses]
    (swap! *store #(apply dissoc % addresses))))

(defrecord TestAsyncStorage [*store delay-ms]
  #?(:clj  me.tonsky.persistent_sorted_set.IStorage
     :cljs me.tonsky.persistent-sorted-set/IStorage)
  
  (-restore [this address]
    (async
      (when (pos? delay-ms)
        (await (js/Promise. (fn [resolve _] (js/setTimeout resolve delay-ms)))))
      (if-let [{:keys [type keys addresses]} (get @*store address)]
        (case type
          :node
          ;; Create Node with addresses for lazy restoration (following Java pattern)
          #?(:cljs (set/make-node-from-storage keys (vec addresses)))
          
          :leaf
          #?(:cljs (set/make-leaf-from-storage keys)))
        (throw (ex-info "Node not found" {:address address})))))
  
  (-store [_ node existing-address]
    (async
      (when (pos? delay-ms)
        (await (js/Promise. (fn [resolve _] (js/setTimeout resolve delay-ms)))))
      (let [addr (or existing-address (random-uuid))
            data (cond
                   ;; Node - store addresses if available, otherwise store child addresses
                   #?@(:cljs [(= (type node) set/Node)
                              (if (.-addresses node)
                                {:type :node
                                 :keys (.-keys node)
                                 :addresses (vec (.-addresses node))}
                                ;; Node without addresses - need to store children first
                                {:type :node
                                 :keys (.-keys node)
                                 :addresses []})])  ;; Empty addresses for now
                   
                   ;; Leaf node
                   #?@(:cljs [(= (type node) set/Leaf)
                              {:type :leaf
                               :keys (.-keys node)}])
                   
                   :else
                   (throw (ex-info "Unknown node type for storage" {:node node})))]
        (swap! *store assoc addr data)
        addr))))
  
  (-accessed [_ address]
    (async nil))
  
  (-delete [_ addresses]
    (async
      (when (pos? delay-ms)
        (await (js/Promise. (fn [resolve _] (js/setTimeout resolve delay-ms)))))
      (swap! *store #(apply dissoc % addresses))))

;; Helpers for testing
(defn make-sync-storage []
  (->TestSyncStorage (atom {})))

(defn make-async-storage 
  ([] (make-async-storage 10))
  ([delay-ms] (->TestAsyncStorage (atom {}) delay-ms)))

(defn make-async-storage-with-logging
  "Creates an async storage that logs all accesses"
  [access-log]
  (let [store (atom {})
        id-counter (atom 0)]
    (reify 
      #?(:clj  me.tonsky.persistent_sorted_set.IStorage
         :cljs me.tonsky.persistent-sorted-set/IStorage)
      
      (-store [this node address]
        (async
          (let [address (or address (str "node-" (swap! id-counter inc)))
                data (cond
                       #?@(:cljs [(= (type node) set/Node)
                                  {:type :node
                                   :keys (vec (.-keys node))
                                   :addresses (when (.-addresses node)
                                                (vec (.-addresses node)))}])
                       
                       #?@(:cljs [(= (type node) set/Leaf)
                                  {:type :leaf
                                   :keys (vec (.-keys node))}])
                       
                       :else
                       (throw (ex-info "Unknown node type" {:node node})))]
            (swap! store assoc address data)
            address)))
      
      (-restore [this address]
        (async
          (swap! access-log conj address)
          (if-let [{:keys [type keys addresses]} (get @store address)]
            (case type
              :node
              ;; Create Node with addresses for lazy restoration (following Java pattern)
              #?(:cljs (set/make-node-from-storage keys addresses))
              
              :leaf
              #?(:cljs (set/make-leaf-from-storage keys)))
            (throw (ex-info "Node not found" {:address address})))))
      
      (-accessed [this address]
        ;; Record access
        nil)
      
      (-delete [this addresses]
        (async
          (swap! store #(apply dissoc % addresses)))))))

(defn make-sync-storage-with-logging
  "Creates a sync storage that logs all accesses"
  [access-log]
  (let [store (atom {})
        id-counter (atom 0)]
    (reify 
      #?(:clj  me.tonsky.persistent_sorted_set.IStorage
         :cljs me.tonsky.persistent-sorted-set/IStorage)
      
      (-store [this node address]
        (let [address (or address (str "node-" (swap! id-counter inc)))
              data (cond
                     #?@(:cljs [(= (type node) set/Node)
                                {:type :node
                                 :keys (vec (.-keys node))
                                 :addresses (when (.-addresses node)
                                              (vec (.-addresses node)))}])
                     
                     #?@(:cljs [(= (type node) set/Leaf)
                                {:type :leaf
                                 :keys (vec (.-keys node))}])
                     
                     :else
                     (throw (ex-info "Unknown node type" {:node node})))]
          (swap! store assoc address data)
          address))
      
      (-restore [this address]
        (swap! access-log conj address)
        (if-let [{:keys [type keys addresses]} (get @store address)]
          (case type
            :node
            ;; Create Node with addresses for lazy restoration (following Java pattern)
            #?(:cljs (set/make-node-from-storage keys addresses))
            
            :leaf
            #?(:cljs (set/make-leaf-from-storage keys)))
          (throw (ex-info "Node not found" {:address address}))))
      
      (-accessed [this address]
        ;; Record access
        nil)
      
      (-delete [this addresses]
        (swap! store #(apply dissoc % addresses))))))