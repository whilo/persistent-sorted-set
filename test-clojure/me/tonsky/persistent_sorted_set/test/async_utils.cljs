(ns me.tonsky.persistent-sorted-set.test.async-utils
  "Simple utilities for async testing"
  (:require [await-cps :refer [await smart-trampoline]
                       :refer-macros [async]]
            [me.tonsky.persistent-sorted-set :as set]))

(defrecord TestSyncStorage [*store]
  me.tonsky.persistent-sorted-set/IStorage
  (-restore [this address]
    (if-let [{:keys [type keys addresses]} (get @*store address)]
      (case type
        :node (set/make-node-from-storage keys (vec addresses))
        :leaf (set/make-leaf-from-storage keys))
      (throw (ex-info "Node not found" {:address address}))))
  (-store [_ node existing-address]
    (let [addr (or existing-address (random-uuid))
          data (cond
                 (= (type node) set/Node)
                 {:type :node
                  :keys (.-keys node)
                  :addresses (when (.-addresses node) (vec (.-addresses node)))}
                 (= (type node) set/Leaf)
                 {:type :leaf
                  :keys (.-keys node)}
                 :else
                 (throw (ex-info "Unknown node type" {:node node})))]
      (swap! *store assoc addr data)
      addr))
  (-accessed [_ address] nil)
  (-delete [_ addresses] (swap! *store #(apply dissoc % addresses))))

(defrecord TestAsyncStorage [*store delay-ms]
  me.tonsky.persistent-sorted-set/IStorage
  (-restore [this address]
    ;; Always use callback style, even with zero delay
    (fn [resolve raise]
      (if (zero? delay-ms)
        ;; Fast path: no delay = immediate callback
        (try
          (if-let [{:keys [type keys addresses]} (get @*store address)]
            (let [node (case type
                         :node (set/make-node-from-storage keys (vec addresses))
                         :leaf (set/make-leaf-from-storage keys))]
              (resolve node))
            (raise (ex-info "Node not found" {:address address})))
          (catch :default e
            (raise e)))
        (js/setTimeout
         (fn []
           (smart-trampoline
            (fn []
              (try
                (if-let [{:keys [type keys addresses]} (get @*store address)]
                  (let [node (case type
                               :node
                               (set/make-node-from-storage keys (vec addresses))
                               :leaf
                               (set/make-leaf-from-storage keys))]
                    (resolve node))
                  (raise (ex-info "Node not found" {:address address})))
                (catch :default e
                  (raise e))))))
         delay-ms))))

  (-store [_ node existing-address]
    (fn [resolve raise]
      (if (zero? delay-ms)
        ;; Fast path: no delay = immediate callback
        (try
          (let [addr (or existing-address (random-uuid))
                data (cond
                       (= (type node) set/Node)
                       {:type :node
                        :keys (.-keys node)
                        :addresses (when (.-addresses node) (vec (.-addresses node)))}
                       (= (type node) set/Leaf)
                       {:type :leaf
                        :keys (.-keys node)}
                       :else
                       (throw (ex-info "Unknown node type" {:node node})))]
            (swap! *store assoc addr data)
            (resolve addr))
          (catch :default e
            (raise e)))
        ;; Slow path: delay = async callback
        (js/setTimeout
         (fn []
           ;; Wrap callback execution in smart-trampoline to restart await-cps context
           (smart-trampoline
            (fn []
              (try
                (let [addr (or existing-address (random-uuid))
                      data (cond
                             (= (type node) set/Node)
                             {:type :node
                              :keys (.-keys node)
                              :addresses (when (.-addresses node) (vec (.-addresses node)))}
                             (= (type node) set/Leaf)
                             {:type :leaf
                              :keys (.-keys node)}
                             :else
                             (throw (ex-info "Unknown node type" {:node node})))]
                  (swap! *store assoc addr data)
                  (resolve addr))
                (catch :default e
                  (raise e))))))
         delay-ms))))
  (-accessed [_ address] nil)
  (-delete [_ addresses] (swap! *store #(apply dissoc % addresses))))

(defn make-sync-storage [] (->TestSyncStorage (atom {})))

(defn make-async-storage
  ([] (make-async-storage 10))
  ([delay-ms] (->TestAsyncStorage (atom {}) delay-ms)))

(defn async-build-set [n]
  (async
   (let [storage (make-async-storage 0)
         s0      (set/sorted-set* {:storage storage})]
     (await
      (reduce (fn [s-promise n]
                (async
                 (let [s (if (fn? s-promise) (await s-promise) s-promise)]
                   (await (set/conj s n compare {:sync? false})))))
              s0
              (range n))))))
