# Async Storage Design Document for ClojureScript Persistent Sorted Set

## 1. Overview

This document describes the design for adding asynchronous storage support to the ClojureScript implementation of persistent-sorted-set. The goal is to enable integration with async storage backends (like Konserve, IndexedDB) while maintaining full backwards compatibility with existing synchronous storage implementations.

## 2. Design Principles

- **Single Protocol**: Use one `IStorage` protocol for both sync and async implementations
- **Backwards Compatible**: Default to synchronous behavior (`{:sync? true}`)
- **Unified API**: All public functions accept an optional `opts` map
- **Lazy Loading**: Preserve the lazy node loading behavior
- **Error Propagation**: Use `go-try-` for proper exception handling across async boundaries

## 3. Storage Protocol

Keep the existing protocol - implementations decide if they're sync or async by return type:

```clojure
(defprotocol IStorage
  (restore [this address])        ; Returns value or channel
  (store [this node existing-address])  ; Returns address or channel
  (accessed [this address])       ; Returns nil or channel
  (delete [this addresses]))      ; Returns nil or channel
```

## 4. Core Implementation Strategy

### 4.1 Translation Map

```clojure
(def ^:dynamic *storage-translation*
  '{go-try- do
    <?- do})
```

### 4.2 Modified Core Methods

All storage-interacting methods accept options and use `async+sync`:

```clojure
(defn node-child [node idx storage opts]
  (let [{:keys [sync?] :or {sync? true}} opts]
    (async+sync sync? *storage-translation*
      (go-try-
        (when-not (= -1 idx)
          (let [child (arrays/aget (.-children node) idx)
                address (when (.-_addresses node) 
                         (arrays/aget (.-_addresses node) idx))]
            (if-not child
              (let [restored (<?- (restore storage address))]
                (set-child! (.-children node) idx restored)
                restored)
              (do
                (when (and storage address)
                  (<?- (accessed storage address)))
                child))))))))
```

### 4.3 Methods Requiring Modification

**INode Protocol Methods**:
- `node-child` - loads children from storage
- `node-lookup` - recursively searches nodes
- `node-conj` - adds elements, may split nodes
- `node-disj` - removes elements, may merge nodes
- `node-merge` - merges nodes, calls delete

**IStore Protocol Methods**:
- `store-aux` - recursively stores nodes

**BTSet Methods**:
- `-ensure-root-node` - loads root from storage
- All public API methods that touch storage

## 5. Public API Changes

All public functions accept an optional `opts` map as the last parameter:

```clojure
;; Before
(conj set key)
(conj set key comparator)

;; After
(conj set key)                    ; sync by default
(conj set key comparator)          ; sync by default
(conj set key comparator opts)     ; can specify {:sync? false}
```

Example implementation:
```clojure
(defn conj
  ([set key] (conj set key (.-comparator set) {}))
  ([set key cmp] (conj set key cmp {}))
  ([set key cmp opts]
   (let [{:keys [sync?] :or {sync? true}} opts]
     (async+sync sync? *storage-translation*
       (go-try-
         (let [roots (<?- (node-conj (<?- (-ensure-root-node set opts))
                                     cmp key (.-storage set) opts))]
           (cond
             (nil? roots) set
             (== (arrays/alength roots) 1)
             (alter-btset set (arrays/aget roots 0) (.-shift set) (inc (.-cnt set)))
             :else
             (alter-btset set
                         (new-node (arrays/amap node-lim-key roots) roots
                                  (node-addresses->array roots))
                         (inc (.-shift set))
                         (inc (.-cnt set))))))))))
```

## 6. Iterator Design

### 6.1 Synchronous Iterator (Default)

The existing iterator implementation remains unchanged for sync mode. It will:
- Work with sync storage or pre-loaded nodes
- Throw an error if it encounters a channel in sync mode
- Implement standard Clojure seq protocols

### 6.2 Asynchronous Iterator

For async iteration, we need a new protocol:

```clojure
(defprotocol IAsyncSeq
  (-first-async [this opts])
  (-rest-async [this opts])
  (-next-async [this opts]))

;; Helper function for async iteration
(defn async-seq
  "Returns a channel that yields [value next-seq-chan] pairs"
  [iter opts]
  (go-try-
    (when-let [v (<?- (-first-async iter opts))]
      [v (async-seq (<?- (-next-async iter opts)) opts)])))
```

## 7. Storage Implementation Examples

### 7.1 Synchronous Storage (existing)

```clojure
(defrecord MemoryStorage [*data]
  IStorage
  (restore [_ address]
    (get @*data address))  ; Returns value directly
  
  (store [_ node existing-address]
    (let [addr (or existing-address (random-uuid))]
      (swap! *data assoc addr (serialize-node node))
      addr))  ; Returns address directly
  
  (accessed [_ address] nil)
  (delete [_ addresses]
    (swap! *data #(apply dissoc % addresses))))
```

### 7.2 Asynchronous Storage (new)

```clojure
(defrecord KonserveStorage [store]
  IStorage
  (restore [_ address]
    (go-try-
      (<?- (k/get store address))))  ; Returns channel
  
  (store [_ node existing-address]
    (go-try-
      (let [addr (or existing-address (random-uuid))]
        (<?- (k/assoc store addr (serialize-node node)))
        addr)))  ; Returns channel
  
  (accessed [_ address]
    (go-try- nil))  ; Returns channel
  
  (delete [_ addresses]
    (go-try-
      (doseq [addr addresses]
        (<?- (k/dissoc store addr))))))  ; Returns channel
```

## 8. Usage Examples

### 8.1 Synchronous Usage (default)

```clojure
;; Create set with sync storage
(def sync-set (sorted-set* {:storage (->MemoryStorage (atom {}))}))

;; All operations are synchronous by default
(-> sync-set
    (conj 1)
    (conj 2)
    (conj 3))
;; Returns immediately
```

### 8.2 Asynchronous Usage

```clojure
;; Create set with async storage
(go
  (let [store (<! (konserve/connect-fs-store "/tmp/storage"))
        async-set (sorted-set* {:storage (->KonserveStorage store)})]
    
    ;; Async operations
    (let [s1 (<! (conj async-set 1 compare {:sync? false}))
          s2 (<! (conj s1 2 compare {:sync? false}))
          s3 (<! (conj s2 3 compare {:sync? false}))]
      (println "Set contains:" (<! (vec s3 {:sync? false}))))))
```

## 9. Error Handling

With `go-try-`, exceptions propagate correctly:

```clojure
(go
  (try
    (let [result (<! (conj my-set 42 compare {:sync? false}))]
      (process result))
    (catch js/Error e
      (handle-storage-error e))))
```

## 10. Implementation Phases

1. **Phase 1**: Core protocol modifications
   - Modify `node-child`, `node-lookup`, `node-conj`, `node-disj`
   - Add opts parameter threading

2. **Phase 2**: Public API updates
   - Update `conj`, `disj`, `contains?`, `-lookup`
   - Add opts parameter with defaults

3. **Phase 3**: Storage operations
   - Update `store`, `restore`, `-ensure-root-node`
   - Implement test storage backends

4. **Phase 4**: Iterator support
   - Keep sync iterators as-is
   - Add async iterator protocol for future use

5. **Phase 5**: Testing
   - Test with sync storage (backwards compatibility)
   - Test with async storage (new functionality)
   - Performance benchmarks

## 11. Testing Strategy

Create both sync and async test storage implementations:

```clojure
;; Sync test storage
(defrecord TestSyncStorage [*store]
  IStorage
  (restore [_ address] (get @*store address))
  (store [_ node address]
    (let [addr (or address (random-uuid))]
      (swap! *store assoc addr node)
      addr)))

;; Async test storage with simulated delay
(defrecord TestAsyncStorage [*store delay-ms]
  IStorage
  (restore [_ address]
    (go
      (<! (timeout delay-ms))
      (get @*store address)))
  (store [_ node address]
    (go
      (<! (timeout delay-ms))
      (let [addr (or address (random-uuid))]
        (swap! *store assoc addr node)
        addr))))
```

## 12. Backwards Compatibility

- All existing code continues to work without modification
- Sync storage implementations work as-is
- Default `{:sync? true}` ensures sync behavior
- No breaking changes to public API

## 13. Future Considerations

- Batch operations for performance
- Caching strategies at the storage layer
- Streaming iterators for large datasets
- Transaction support (if storage backend supports it)