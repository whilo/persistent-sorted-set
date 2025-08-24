(ns ^{:doc
      "A B-tree based persistent sorted set. Supports transients, custom comparators, fast iteration, efficient slices (iterator over a part of the set) and reverse slices. Almost a drop-in replacement for [[clojure.core/sorted-set]], the only difference being this one can't store nil."
      :author "Nikita Prokopov"}
  me.tonsky.persistent-sorted-set
  (:refer-clojure :exclude [conj disj sorted-set sorted-set-by iter])
  (:require-macros [me.tonsky.persistent-sorted-set.macros :refer [async+sync]])
  (:require [me.tonsky.persistent-sorted-set.arrays :as arrays]
            [me.tonsky.persistent-sorted-set.btset :as btset]
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



(defn- path-str [^number path]
  #_(loop [res ()
         path path]
    (if (not= path 0)
      (recur (cljs.core/conj res (mod path max-len)) (Math/floor (/ path max-len)))
      (vec res))))

(defn- next-path-async
  "Async version of next-path that returns channel with next path"
  [set ^number path]
  #_
  (async+sync false
              (async
               (if (neg? path)
                 empty-path
                 (or
                  (await (-next-path set (.-root set) path (.-shift set) {:sync? false}))
                  (path-inc (if (.-storage set)
                              (await (-rpath (.-root set) empty-path (.-shift set) (.-storage set) {:sync? false}))
                              (-rpath (.-root set) empty-path (.-shift set)))))))))

(defn requires-storage-access?
  "Fast check if a slice operation will need to access storage.
   Returns true if any nodes in the slice path are not loaded yet."
  [^BTSet set key-from key-to]
  #_(when (.-storage set)
    (let [root (.-root set)
          shift (.-shift set)]
      (if (== 0 shift)
        ;; Root is a leaf - check if it needs loading
        (node-requires-storage? root)
        ;; Tree traversal needed - check path nodes
        (or (node-requires-storage? root)
            (slice-path-requires-storage? set root key-from key-to shift))))))



;; Public interface

(defn conj
  "Analogue to [[clojure.core/conj]] but with comparator that overrides the one stored in set.
   Accepts optional opts map with {:sync? true/false} (defaults to true)."
  ([^BTSet set key] (conj set key (.-comparator set) {}))
  ([^BTSet set key cmp] (conj set key cmp {}))
  ([^BTSet set key cmp opts] (btset/conjoin set key cmp opts)))

(defn disj
  "Analogue to [[clojure.core/disj]] with comparator that overrides the one stored in set.
   Accepts optional opts map with {:sync? true/false} (defaults to true)."
  ([^BTSet set key] (disj set key (.-comparator set) {}))
  ([^BTSet set key cmp] (disj set key cmp {}))
  ([^BTSet set key cmp opts] (btset/disjoin set key cmp opts)))

(defn slice
  "An iterator for part of the set with provided boundaries.
   `(slice set from to)` returns iterator for all Xs where from <= X <= to.
   Optionally pass in comparator that will override the one that set uses. Supports efficient [[clojure.core/rseq]]."
  ([^BTSet set key-from key-to]
   (btset/slice set key-from key-to (.-comparator set)))
  ([^BTSet set key-from key-to comparator]
   (btset/slice set key-from key-to comparator)))

(defn async-slice
  "Async version of slice that returns a Promise resolving to a vector of elements.
   Returns a Promise that resolves to a vector containing all elements in the range [key-from, key-to)."
  ([^BTSet set key-from key-to]
   (btset/async-slice set key-from key-to (.-comparator set)))
  ([^BTSet set key-from key-to comparator]
   (btset/async-slice set key-from key-to comparator)))

(defn rslice
  "A reverse iterator for part of the set with provided boundaries.
   `(rslice set from to)` returns backwards iterator for all Xs where from <= X <= to.
   Optionally pass in comparator that will override the one that set uses. Supports efficient [[clojure.core/rseq]]."
  ([^BTSet set key]
   (some-> (btset/slice set key key (.-comparator set)) rseq))
  ([^BTSet set key-from key-to]
   (some-> (btset/slice set key-to key-from (.-comparator set)) rseq))
  ([^BTSet set key-from key-to comparator]
   (some-> (btset/slice set key-to key-from comparator) rseq)))

(defn seek
  "An efficient way to seek to a specific key in a seq (either returned by [[clojure.core.seq]] or a slice.)
  `(seek (seq set) to)` returns iterator for all Xs where to <= X.
  Optionally pass in comparator that will override the one that set uses."
  ([seq to]
   (btset/-seek seq to))
  ([seq to cmp]
   (btset/-seek seq to cmp)))

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