(ns ^{:doc
      "A B-tree based persistent sorted set. Supports transients, custom comparators, fast iteration, efficient slices (iterator over a part of the set) and reverse slices. Almost a drop-in replacement for [[clojure.core/sorted-set]], the only difference being this one can't store nil."
      :author "Nikita Prokopov"}
  me.tonsky.persistent-sorted-set
  (:refer-clojure :exclude [conj disj sorted-set sorted-set-by iter])
  (:require-macros [me.tonsky.persistent-sorted-set.macros :refer [async+sync]])
  (:require [me.tonsky.persistent-sorted-set.arrays :as arrays]
            [await-cps :refer [await] :refer-macros [async]]))

; B+ tree
; -------

; Leaf:     keys[]     :: array of values

; Node:     children[] :: links to children nodes
;           keys[]     :: max value for whole subtree
;                         node.keys[i] == max(node.children[i].keys)
; All arrays are 16..32 elements, inclusive

; BTSet:    root       :: Node or Leaf
;           shift      :: depth - 1
;           cnt        :: size of a set, integer, rolling
;           comparator :: comparator used for ordering
;           meta       :: clojure meta map
;           _hash      :: hash code, same as for clojure collections, on-demand, cached

; Path: conceptually a vector of indexes from root to leaf value, but encoded in a single number.
;       E.g. we have path [7 30 11] representing root.children[7].children[30].keys[11].
;       In our case level-shift is 5, meaning each index will take 5 bits:
;       (7 << 10) | (30 << 5) | (11 << 0) = 8139
;         00111       11110       01011

; Iter:     set       :: Set this iterator belongs to
;           left      :: Current path
;           right     :: Right bound path (exclusive)
;           keys      :: Cached ref for keys array for a leaf
;           idx       :: Cached idx in keys array
; Keys and idx are cached for fast iteration inside a leaf"






(defprotocol IStorage
  (-store [this node opts])
  (-restore [this address opts])
  (-accessed [this address])
  (-delete [this addresses]))

(defn- path-str [^number path]
  (loop [res ()
         path path]
    (if (not= path 0)
      (recur (cljs.core/conj res (mod path max-len)) (Math/floor (/ path max-len)))
      (vec res))))

(defn- ensure-root
  ([^BTSet set]
   (ensure-root set {}))
  ([^BTSet set {:keys [sync?] :or {sync? true} :as opts}]
   (assert (or (some? (.-address set)) (some? (.-root set))))
   (assert (some? (.-storage set)))
   (async+sync sync?
     (async
       (do
         (when (and (nil? (.-root set)) (some? (.-address set)))
           (set! (.-root set) (await (-restore (.-storage set) (.-address set) opts)))))
       (.-root set)))))

(defn alter-btset
  ([^BTSet set root shift cnt]
   (BTSet. root shift cnt (.-comparator set) (.-meta set) uninitialized-hash (.-storage set) (.-address set)))
  ([^BTSet set root shift cnt cmp]
   (BTSet. root shift cnt cmp (.-meta set) uninitialized-hash (.-storage set) (.-address set))))

;; iteration





(defn- next-path-async
  "Async version of next-path that returns channel with next path"
  [set ^number path]
  (async+sync false
              (async
               (if (neg? path)
                 empty-path
                 (or
                  (await (-next-path set (.-root set) path (.-shift set) {:sync? false}))
                  (path-inc (if (.-storage set)
                              (await (-rpath (.-root set) empty-path (.-shift set) (.-storage set) {:sync? false}))
                              (-rpath (.-root set) empty-path (.-shift set)))))))))




(defn- prev-path-async
  "Async version of prev-path that returns channel with previous path"
  [set ^number path]
  (async
    (if (> (path-get path (inc (.-shift set))) 0) ;; overflow
      (if (.-storage set)
        (await (-rpath (.-root set) path (.-shift set) (.-storage set) {:sync? false}))
        (-rpath (.-root set) path (.-shift set)))
      (or
       (await (-prev-path set (.-root set) path (.-shift set) {:sync? false}))
       (path-dec empty-path)))))


(defn- node-requires-storage?
  "Check if a node itself needs to be loaded from storage"
  [node]
  (and (instance? Node node)
       (.-addresses node)           ; Has storage addresses
       (nil? (.-children node))))   ; But children not loaded yet

(defn- slice-path-requires-storage?
  "Check if any nodes in the slice path need storage access.
   This is the critical optimization - only check nodes that
   the slice operation will actually traverse."
  [^BTSet set node key-from key-to level]
  (let [cmp (.-comparator set)
        keys (.-keys node)]
    (when (instance? Node node)
      (let [keys-l (arrays/alength keys)
            ;; Find which children the slice bounds span
            from-idx (if key-from
                       (binary-search-l cmp keys (- keys-l 2) key-from)
                       0)
            to-idx   (if key-to
                       (binary-search-r cmp keys (- keys-l 2) key-to)
                       (dec (arrays/alength (.-addresses node))))]

        ;; Only check nodes that are actually in the slice path
        (loop [idx from-idx]
          (when (<= idx to-idx)
            (let [;; Check if this child needs loading
                  child-addr (when (.-addresses node)
                               (arrays/aget (.-addresses node) idx))
                  child-loaded? (when (.-children node)
                                 (arrays/aget (.-children node) idx))]
              (if (and child-addr (not child-loaded?))
                ;; Found unloaded node in slice path
                true
                ;; This child is loaded, recurse if needed
                (if (and child-loaded? (> level 1))
                  ;; Recursively check this child's path requirements
                  (or (slice-path-requires-storage? set child-loaded? key-from key-to (dec level))
                      ;; Move to next child in slice range
                      (recur (inc idx)))
                  ;; Move to next child in slice range
                  (recur (inc idx)))))))))))

(defn requires-storage-access?
  "Fast check if a slice operation will need to access storage.
   Returns true if any nodes in the slice path are not loaded yet."
  [^BTSet set key-from key-to]
  (when (.-storage set)
    (let [root (.-root set)
          shift (.-shift set)]
      (if (== 0 shift)
        ;; Root is a leaf - check if it needs loading
        (node-requires-storage? root)
        ;; Tree traversal needed - check path nodes
        (or (node-requires-storage? root)
            (slice-path-requires-storage? set root key-from key-to shift))))))

(defn -slice [^BTSet set key-from key-to comparator]
  (when-some [path (-seek* set key-from comparator)]
    (let [till-path (-rseek* set key-to comparator)]
      (when (path-lt path till-path)
        (Iter. set path till-path (keys-for set path) (path-get path 0))))))

(defprotocol IAsyncSeq
  (-afirst [this] "Returns async expression yielding first element")
  (-arest [this] "Returns async expression yielding rest of sequence"))

(deftype AsyncSeq [^BTSet set path till-path ^:mutable keys ^:mutable idx]
  IAsyncSeq
  (-afirst [this]
    (async
      (when (and path (path-lt path till-path))
        ;; Load keys only if not cached
        (when (nil? keys)
          (set! keys (await (keys-for set path {:sync? false})))
          (set! idx (path-get path 0)))
        (arrays/aget keys idx))))

  (-arest [this]
    (async
      (when (and path (path-lt path till-path))
        ;; Load keys only if not cached
        (when (nil? keys)
          (set! keys (await (keys-for set path {:sync? false})))
          (set! idx (path-get path 0)))
        (if (< (inc idx) (arrays/alength keys))
          ;; Next element is in same leaf - reuse keys array!
          (AsyncSeq. set (path-inc path) till-path keys (inc idx))
          ;; Need to move to next leaf
          (let [next-path (await (next-path-async set path))]
            (when (and next-path (path-lt next-path till-path))
              ;; Don't pass keys - will be loaded lazily for new leaf
              (AsyncSeq. set next-path till-path nil nil)))))))
  Object
  (toString [this]
    (str "AsyncSeq[" (path-str path) " -> " (path-str till-path) "]"))
  IPrintWithWriter
  (-pr-writer [this writer opts]
    (-write writer (str this))))

(defn async-seq
  "Create an async sequence from a BTSet and path range"
  [set path till-path]
  (when (and path (path-lt path till-path))
    (AsyncSeq. set path till-path nil nil)))

;; Updated async-slice to return AsyncSeq
(defn -async-slice
  "Async version of slice that returns an AsyncSeq."
  [^BTSet set key-from key-to comparator]
  (async
    (when-some [path (await (-seek* set key-from comparator {:sync? false}))]
      (let [till-path (await (-rseek* set key-to comparator {:sync? false}))]
        (async-seq set path till-path)))))

(defn arr-map-inplace [f arr]
  (let [len (arrays/alength arr)]
    (loop [i 0]
      (when (< i len)
        (arrays/aset arr i (f (arrays/aget arr i)))
        (recur (inc i))))
    arr))

(defn arr-partition-approx
  "Splits `arr` into arrays of size between min-len and max-len,
   trying to stick to (min+max)/2"
  [min-len max-len arr]
  (let [chunk-len avg-len
        len       (arrays/alength arr)
        acc       (transient [])]
    (when (pos? len)
      (loop [pos 0]
        (let [rest (- len pos)]
          (cond
            (<= rest max-len)
            (conj! acc (.slice arr pos))
            (>= rest (+ chunk-len min-len))
            (do
              (conj! acc (.slice arr pos (+ pos chunk-len)))
              (recur (+ pos chunk-len)))
            :else
            (let [piece-len (arrays/half rest)]
              (conj! acc (.slice arr pos (+ pos piece-len)))
              (recur (+ pos piece-len)))))))
    (to-array (persistent! acc))))

(defn- sorted-arr-distinct? [arr cmp]
  (let [al (arrays/alength arr)]
    (if (<= al 1)
      true
      (loop [i 1
             p (arrays/aget arr 0)]
        (if (>= i al)
          true
          (let [e (arrays/aget arr i)]
            (if (== 0 (cmp e p))
              false
              (recur (inc i) e))))))))

(defn sorted-arr-distinct
  "Filter out repetitive values in a sorted array.
   Optimized for no-duplicates case"
  [arr cmp]
  (if (sorted-arr-distinct? arr cmp)
    arr
    (let [al (arrays/alength arr)]
      (loop [acc (transient [(arrays/aget arr 0)])
             i   1
             p   (arrays/aget arr 0)]
        (if (>= i al)
          (into-array (persistent! acc))
          (let [e (arrays/aget arr i)]
            (if (== 0 (cmp e p))
              (recur acc (inc i) e)
              (recur (conj! acc e) (inc i) e))))))))

(declare store-node)

(defn make-node-from-storage
  "Create a Node with addresses for lazy restoration"
  [keys addresses]
  (Node. keys nil (into-array addresses) nil))

(defn make-leaf-from-storage
  "Create a Leaf from stored data"
  [keys]
  (Leaf. keys nil))

(defn store-node
  "Store a node recursively. Returns address or channel depending on sync mode."
  [node storage  {:keys [sync?] :or {sync? true} :as opts}]
  (async+sync sync?
    (cond
      (instance? Leaf node)
      (-store storage node opts)

      (instance? Node node)
      (async
       (let [children (.-children node)
             addresses (arrays/make-array (arrays/alength children))]
         ;; store children first
         (dotimes [i (arrays/alength children)]
           (let [child (arrays/aget children i)
                 addr (await (store-node child storage opts))]
             (arrays/aset addresses i addr)))
         ;; Then store this node with addresses
         (let [node-with-addresses (Node. (.-keys node) nil addresses nil)
               final-addr (await (-store storage node-with-addresses opts))]
           final-addr)))

      :else
      (throw (ex-info "Unknown node type" {:node node :type (type node)})))))

;; Public interface

(defn conj
  "Analogue to [[clojure.core/conj]] but with comparator that overrides the one stored in set.
   Accepts optional opts map with {:sync? true/false} (defaults to true)."
  ([^BTSet set key] (conj set key (.-comparator set) {}))
  ([^BTSet set key cmp] (conj set key cmp {}))
  ([^BTSet set key cmp {:keys [sync?] :or {sync? true} :as opts}]
   (async+sync sync?
    (async
      (let [roots (await (node-conj (.-root set) cmp key (.-storage set) opts))]
        (cond
          ;; tree not changed
          (nil? roots)
          set

          ;; keeping single root
          (== (arrays/alength roots) 1)
          (alter-btset set
                       (arrays/aget roots 0)
                       (.-shift set)
                       (inc (.-cnt set)))

          ;; introducing new root
          :else
          (alter-btset set
                       (Node. (arrays/amap node-lim-key roots) roots nil nil)
                       (inc (.-shift set))
                       (inc (.-cnt set)))))))))

(defn disj
  "Analogue to [[clojure.core/disj]] with comparator that overrides the one stored in set.
   Accepts optional opts map with {:sync? true/false} (defaults to true)."
  ([^BTSet set key] (disj set key (.-comparator set) {}))
  ([^BTSet set key cmp] (disj set key cmp {}))
  ([^BTSet set key cmp {:keys [sync?] :or {sync? true} :as opts}]
   (async+sync sync?
    (async
      (let [new-roots (await (node-disj (.-root set) cmp key true nil nil (.-storage set) opts))]
        (if (nil? new-roots) ;; nothing changed, key wasn't in the set
          set
          (let [new-root (arrays/aget new-roots 0)]
            (if (and (instance? Node new-root)
                     (== 1 (arrays/alength (.-children new-root))))

              ;; root has one child, make him new root
              (alter-btset set
                           (arrays/aget (.-children new-root) 0)
                           (dec (.-shift set))
                           (dec (.-cnt set)))

              ;; keeping root level
              (alter-btset set
                           new-root
                           (.-shift set)
                           (dec (.-cnt set)))))))))))

(defn slice
  "An iterator for part of the set with provided boundaries.
   `(slice set from to)` returns iterator for all Xs where from <= X <= to.
   Optionally pass in comparator that will override the one that set uses. Supports efficient [[clojure.core/rseq]]."
  ([^BTSet set key-from key-to]
   (-slice set key-from key-to (.-comparator set)))
  ([^BTSet set key-from key-to comparator]
   (-slice set key-from key-to comparator)))

(defn async-slice
  "Async version of slice that returns a Promise resolving to a vector of elements.
   Returns a Promise that resolves to a vector containing all elements in the range [key-from, key-to)."
  ([^BTSet set key-from key-to]
   (-async-slice set key-from key-to (.-comparator set)))
  ([^BTSet set key-from key-to comparator]
   (-async-slice set key-from key-to comparator)))

(defn rslice
  "A reverse iterator for part of the set with provided boundaries.
   `(rslice set from to)` returns backwards iterator for all Xs where from <= X <= to.
   Optionally pass in comparator that will override the one that set uses. Supports efficient [[clojure.core/rseq]]."
  ([^BTSet set key]
   (some-> (-slice set key key (.-comparator set)) rseq))
  ([^BTSet set key-from key-to]
   (some-> (-slice set key-to key-from (.-comparator set)) rseq))
  ([^BTSet set key-from key-to comparator]
   (some-> (-slice set key-to key-from comparator) rseq)))

(defn seek
  "An efficient way to seek to a specific key in a seq (either returned by [[clojure.core.seq]] or a slice.)
  `(seek (seq set) to)` returns iterator for all Xs where to <= X.
  Optionally pass in comparator that will override the one that set uses."
  ([seq to]
   (-seek seq to))
  ([seq to cmp]
   (-seek seq to cmp)))

(defn lookup-async
  "Async version of lookup that works with async storage.
   Returns a channel that will contain the value if found, or nil/not-found."
  ([^BTSet set key]
   (lookup-async set key nil))
  ([^BTSet set key not-found]
   (async
     (or (await (node-lookup (.-root set) (.-comparator set) key (.-storage set) {:sync? false}))
         not-found))))

(defn contains-async?
  "Async version of contains? that works with async storage.
   Returns a channel that will contain true if key exists, false otherwise."
  [^BTSet set key]
  (async
    (some? (await (node-lookup (.-root set) (.-comparator set) key (.-storage set) {:sync? false})))))

(defn from-sorted-array
  "Fast path to create a set if you already have a sorted array of elements on your hands."
  ([cmp arr]
   (from-sorted-array cmp arr (arrays/alength arr)))
  ([cmp arr _len]
   (from-sorted-array cmp arr _len {}))
  ([cmp arr _len opts]
   (let [leaves (->> arr
                     (arr-partition-approx min-len max-len)
                     (arr-map-inplace #(Leaf. % nil)))
         storage (:storage opts)]
     (loop [current-level leaves
            shift 0]
       (case (count current-level)
         0 (BTSet. (Leaf. (arrays/array) nil) 0 0 cmp nil uninitialized-hash storage nil)
         1 (BTSet. (first current-level) shift (arrays/alength arr) cmp nil uninitialized-hash storage nil)
         (recur
          (->> current-level
               (arr-partition-approx min-len max-len)
               (arr-map-inplace #(Node. (arrays/amap node-lim-key %) % nil nil)))
          (inc shift)))))))

(defn from-sequential
  "Create a set with custom comparator and a collection of keys. Useful when you don't want to call [[clojure.core/apply]] on [[sorted-set-by]]."
  [cmp seq]
  (let [arr (-> (into-array seq) (arrays/asort cmp) (sorted-arr-distinct cmp))]
    (from-sorted-array cmp arr)))

(defn sorted-set-by
  ([cmp] (BTSet. (Leaf. (arrays/array) nil) 0 0 cmp nil uninitialized-hash nil nil))
  ([cmp & keys] (from-sequential cmp keys)))

(defn sorted-set
  ([] (sorted-set-by compare))
  ([& keys] (from-sequential compare keys)))

(defn sorted-set*
  "Create a set with options map containing:
   - :storage  Storage implementation
   - :comparator  Custom comparator (defaults to compare)
   - :meta     Metadata"
  [opts]
  (BTSet. (Leaf. (arrays/array) nil) 0 0 (or (:comparator opts) compare)
          (:meta opts) uninitialized-hash (:storage opts) nil))

(defn store
  "Accepts optional opts map with {:sync? true/false} (defaults to true).
   returns address specified by storage"
  ([^BTSet set]
   (store set (.-storage set) {}))
  ([^BTSet set arg]
   (if (implements? IStorage arg)
     (do
       (set! (.-storage set) arg)
       (store set arg {}))
     (store set (.-storage set) arg)))
  ([^BTSet set storage {:keys [sync?] :or {sync? true} :as opts}]
   (assert (instance? BTSet set))
   (assert (implements? IStorage storage))
   (async+sync sync?
     (async
      (do
        (when (nil? (.-address set))
          (set! (.-address set) (await (store-node (.-root set) storage opts))))
        (.-address set))))))

(defn restore
  "Restore a set from storage given root-address-or-info and storage.
   + First arg can be either:
     - A root address (UUID) - requires opts with :shift and :count
     - A map from store-set with :root-address, :shift, :count, :comparator
   + Storage operations will use the provided opts for sync/async mode.
   + This operation is always synchronous and does not initiate io."
  ([root-address-or-info storage]
   (restore root-address-or-info storage {}))
  ([root-address-or-info storage opts]
   (let [;; Handle both old format (bare UUID) and new format (map with metadata)
         address      (if (map? root-address-or-info)
                        (:root-address root-address-or-info)
                        root-address-or-info)
         _ (assert (some? address))
         meta (or (and (map? root-address-or-info) (:meta root-address-or-info))
                  (:meta opts))
         shift        (if (map? root-address-or-info)
                        (:shift root-address-or-info)
                        (:shift opts 0))
         cnt          (if (map? root-address-or-info)
                        (:count root-address-or-info)
                        (:count opts 0))
         cmp          (if (map? root-address-or-info)
                        (or (:comparator root-address-or-info) compare)
                        (or (:comparator opts) compare))]
   ;(IPersistentMap meta, Comparator<Key> cmp, Address address, IStorage<Key, Address> storage, Object root, int count, Settings settings, int version)
   ;(PersistentSortedSet. nil cmp address storage nil -1 (map->settings opts) 0)
   #_(BTSet root shift cnt cmp meta ^:mutable _hash storage address)
     (BTSet. nil shift cnt cmp meta uninitialized-hash storage address))))