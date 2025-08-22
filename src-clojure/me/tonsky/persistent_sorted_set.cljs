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

; Node:     pointers[] :: links to children nodes
;           keys[]     :: max value for whole subtree
;                         node.keys[i] == max(node.pointers[i].keys)
; All arrays are 16..32 elements, inclusive

; BTSet:    root       :: Node or Leaf
;           shift      :: depth - 1
;           cnt        :: size of a set, integer, rolling
;           comparator :: comparator used for ordering
;           meta       :: clojure meta map
;           _hash      :: hash code, same as for clojure collections, on-demand, cached

; Path: conceptually a vector of indexes from root to leaf value, but encoded in a single number.
;       E.g. we have path [7 30 11] representing root.pointers[7].pointers[30].keys[11].
;       In our case level-shift is 5, meaning each index will take 5 bits:
;       (7 << 10) | (30 << 5) | (11 << 0) = 8139
;         00111       11110       01011

; Iter:     set       :: Set this iterator belongs to
;           left      :: Current path
;           right     :: Right bound path (exclusive)
;           keys      :: Cached ref for keys array for a leaf
;           idx       :: Cached idx in keys array
; Keys and idx are cached for fast iteration inside a leaf"

(def ^:const max-safe-path
  "js limitation for bit ops"
  (js/Math.pow 2 31))

(def ^:const bits-per-level
  "tunable param"
  5)

(def ^:const max-len
  (js/Math.pow 2 bits-per-level)) ;; 32

(def ^:const min-len
  (/ max-len 2)) ;; 16

(def ^:private ^:const avg-len
  (arrays/half (+ max-len min-len))) ;; 24

(def ^:const max-safe-level
  (js/Math.floor (/ 31 bits-per-level))) ;; 6

(def ^:const bit-mask
  (- max-len 1)) ;; 0b011111 = 5 bit

(def factors
  (arrays/into-array (map #(js/Math.pow 2 %) (range 0 52 bits-per-level))))

(def ^:const empty-path 0)

(defprotocol IStorage
  (-restore [this address])
  (-accessed [this address])
  (-store [this node address])
  (-delete [this addresses]))

(defn- path-get ^number [^number path ^number level]
  (if (< level max-safe-level)
    (-> path
      (unsigned-bit-shift-right (* level bits-per-level))
      (bit-and bit-mask))
    (-> path
      (/ (arrays/aget factors level))
      (js/Math.floor)
      (bit-and bit-mask))))

(defn- path-set ^number [^number path ^number level ^number idx]
  (let [smol? (and (< path max-safe-path) (< level max-safe-level))
        old   (path-get path level)
        minus (if smol?
                (bit-shift-left old (* level bits-per-level))
                (* old (arrays/aget factors level)))
        plus  (if smol?
                (bit-shift-left idx (* level bits-per-level))
                (* idx (arrays/aget factors level)))]
    (-> path
      (- minus)
      (+ plus))))

(defn- path-inc ^number [^number path]
  (inc path))

(defn- path-dec ^number [^number path]
  (dec path))

(defn- path-cmp ^number [^number path1 ^number path2]
  (- path1 path2))

(defn- path-lt ^boolean [^number path1 ^number path2]
  (< path1 path2))

(defn- path-lte ^boolean [^number path1 ^number path2]
  (<= path1 path2))

(defn- path-eq ^boolean [^number path1 ^number path2]
  (== path1 path2))

(defn- path-same-leaf ^boolean [^number path1 ^number path2]
  (if (and
        (< path1 max-safe-path)
        (< path2 max-safe-path))
    (==
      (unsigned-bit-shift-right path1 bits-per-level)
      (unsigned-bit-shift-right path2 bits-per-level))
    (==
      (Math/floor (/ path1 max-len))
      (Math/floor (/ path2 max-len)))))

(defn- path-str [^number path]
  (loop [res ()
         path path]
    (if (not= path 0)
      (recur (cljs.core/conj res (mod path max-len)) (Math/floor (/ path max-len)))
      (vec res))))

(defn- binary-search-l [cmp arr r k]
  (loop [l 0
         r (long r)]
    (if (<= l r)
      (let [m  (arrays/half (+ l r))
            mk (arrays/aget arr m)]
        (if (neg? (cmp mk k))
          (recur (inc m) r)
          (recur l (dec m))))
      l)))

(defn- binary-search-r [cmp arr r k]
  (loop [l 0
         r (long r)]
    (if (<= l r)
      (let [m  (arrays/half (+ l r))
            mk (arrays/aget arr m)]
        (if (pos? (cmp mk k))
          (recur l (dec m))
          (recur (inc m) r)))
      l)))

(defn- lookup-exact [cmp arr key]
  (let [arr-l (arrays/alength arr)
        idx   (binary-search-l cmp arr (dec arr-l) key)]
    (if (and (< idx arr-l)
             (== 0 (cmp (arrays/aget arr idx) key)))
      idx
      -1)))

(defn- lookup-range [cmp arr key]
  (let [arr-l (arrays/alength arr)
        idx   (binary-search-l cmp arr (dec arr-l) key)]
    (if (== idx arr-l)
      -1
      idx)))

;; Array operations

(defn- cut-n-splice [arr cut-from cut-to splice-from splice-to xs]
  (let [xs-l (arrays/alength xs)
        l1   (- splice-from cut-from)
        l2   (- cut-to splice-to)
        l1xs (+ l1 xs-l)
        new-arr (arrays/make-array (+ l1 xs-l l2))]
    (arrays/acopy arr cut-from splice-from new-arr 0)
    (arrays/acopy xs 0 xs-l new-arr l1)
    (arrays/acopy arr splice-to cut-to new-arr l1xs)
    new-arr))

(defn- splice [arr splice-from splice-to xs]
  (cut-n-splice arr 0 (arrays/alength arr) splice-from splice-to xs))

(defn- insert [arr idx xs]
  (cut-n-splice arr 0 (arrays/alength arr) idx idx xs))

(defn- merge-n-split [a1 a2]
  (let [a1-l    (arrays/alength a1)
        a2-l    (arrays/alength a2)
        total-l (+ a1-l a2-l)
        r1-l    (arrays/half total-l)
        r2-l    (- total-l r1-l)
        r1      (arrays/make-array r1-l)
        r2      (arrays/make-array r2-l)]
    (if (<= a1-l r1-l)
      (do
        (arrays/acopy a1 0             a1-l          r1 0)
        (arrays/acopy a2 0             (- r1-l a1-l) r1 a1-l)
        (arrays/acopy a2 (- r1-l a1-l) a2-l          r2 0))
      (do
        (arrays/acopy a1 0    r1-l r1 0)
        (arrays/acopy a1 r1-l a1-l r2 0)
        (arrays/acopy a2 0    a2-l r2 (- a1-l r1-l))))
    (arrays/array r1 r2)))

(defn- ^boolean eq-arr [cmp a1 a1-from a1-to a2 a2-from a2-to]
  (let [len (- a1-to a1-from)]
    (and
     (== len (- a2-to a2-from))
     (loop [i 0]
       (cond
         (== i len)
         true

         (not (== 0 (cmp
                     (arrays/aget a1 (+ i a1-from))
                     (arrays/aget a2 (+ i a2-from)))))
         false

         :else
         (recur (inc i)))))))

(defn- check-n-splice [cmp arr from to new-arr]
  (if (eq-arr cmp arr from to new-arr 0 (arrays/alength new-arr))
    arr
    (splice arr from to new-arr)))

(defn- return-array
  "Drop non-nil references and return array of arguments"
  ([a1]
   (arrays/array a1))
  ([a1 a2]
   (if a1
     (if a2
       (arrays/array a1 a2)
       (arrays/array a1))
     (arrays/array a2)))
  ([a1 a2 a3]
   (if a1
     (if a2
       (if a3
         (arrays/array a1 a2 a3)
         (arrays/array a1 a2))
       (if a3
         (arrays/array a1 a3)
         (arrays/array a1)))
     (if a2
       (if a3
         (arrays/array a2 a3)
         (arrays/array a2))
       (arrays/array a3)))))

;;

(defprotocol INode
  (node-lim-key       [_])
  (node-len           [_])
  (node-merge         [_ next])
  (node-merge-n-split [_ next])
  (node-lookup        [_ cmp key storage opts])
  (node-conj          [_ cmp key storage opts])
  (node-disj          [_ cmp key root? left right storage opts]))

(declare Node Leaf ensure-child)

(defn- rotate [node root? left right]
  (cond
    ;; root never merges
    root?
    (return-array node)

    ;; enough keys, nothing to merge
    (> (node-len node) min-len)
    (return-array left node right)

    ;; left and this can be merged to one
    (and left (<= (node-len left) min-len))
    (return-array (node-merge left node) right)

    ;; right and this can be merged to one
    (and right (<= (node-len right) min-len))
    (return-array left (node-merge node right))

    ;; left has fewer nodes, redestribute with it
    (and left (or (nil? right)
                  (< (node-len left) (node-len right))))
    (let [nodes (node-merge-n-split left node)]
      (return-array (arrays/aget nodes 0) (arrays/aget nodes 1) right))

    ;; right has fewer nodes, redestribute with it
    :else
    (let [nodes (node-merge-n-split node right)]
      (return-array left (arrays/aget nodes 0) (arrays/aget nodes 1)))))

(deftype Node [keys ^:mutable pointers ^:mutable addresses ^:mutable _hash]
  Object
  (toString [_] (pr-str* (vec keys)))

  INode
  (node-lim-key [_]
    (arrays/alast keys))

  (node-len [_]
    (arrays/alength keys))

  (node-merge [_ next]
    (Node. (arrays/aconcat keys (.-keys next))
           (arrays/aconcat pointers (.-pointers next))
           nil nil))

  (node-merge-n-split [_ next]
    (let [ks (merge-n-split keys     (.-keys next))
          ps (merge-n-split pointers (.-pointers next))]
      (return-array
       (Node. (arrays/aget ks 0) (arrays/aget ps 0) nil nil)
       (Node. (arrays/aget ks 1) (arrays/aget ps 1) nil nil))))

  (node-lookup [this cmp key storage opts]
    (let [{:keys [sync?] :or {sync? true}} opts
          idx (lookup-range cmp keys key)]
      (async+sync sync? {async do, await do}
                  (async
                    (when-not (== -1 idx)
                      (let [child-node (await (ensure-child this idx storage opts))]
                        (await (node-lookup child-node cmp key storage opts))))))))

  (node-conj [this cmp key storage opts]
    (let [{:keys [sync?] :or {sync? true}} opts
          idx   (binary-search-l cmp keys (- (arrays/alength keys) 2) key)]
      (async+sync sync? {async do, await do}
                  (async
                    (let [child-node (await (ensure-child this idx storage opts))]
                      (when-let [nodes (await (node-conj child-node cmp key storage opts))]
                        (let [new-keys     (check-n-splice cmp keys     idx (inc idx) (arrays/amap node-lim-key nodes))
                              new-pointers (splice             pointers idx (inc idx) nodes)]
                          (if (<= (arrays/alength new-pointers) max-len)
                            ;; ok as is
                            (arrays/array (Node. new-keys new-pointers nil nil))
                            ;; sptta split it up
                            (let [middle (arrays/half (arrays/alength new-pointers))]
                              (arrays/array
                               (Node. (.slice new-keys     0 middle) (.slice new-pointers 0 middle) nil nil)
                               (Node. (.slice new-keys     middle)   (.slice new-pointers middle)   nil nil)))))))))))

  (node-disj [this cmp key root? left right storage opts]
    (let [{:keys [sync?] :or {sync? true}} opts
          idx (lookup-range cmp keys key)]
      (async+sync sync? {async do, await do}
                  (async
                    (when-not (== -1 idx) ;; short-circuit, key not here
                      (let [child       (await (ensure-child this idx storage opts))
                            left-child  (when (>= (dec idx) 0)          (await (ensure-child this (dec idx) storage opts)))
                            right-child (when (< (inc idx) (arrays/alength pointers)) (await (ensure-child this (inc idx) storage opts)))
                            disjned     (await (node-disj child cmp key false left-child right-child storage opts))]
                        (when disjned     ;; short-circuit, key not here
                          (let [left-idx     (if left-child  (dec idx) idx)
                                right-idx    (if right-child (+ 2 idx) (+ 1 idx))
                                new-keys     (check-n-splice cmp keys     left-idx right-idx (arrays/amap node-lim-key disjned))
                                new-pointers (splice             pointers left-idx right-idx disjned)]
                            (rotate (Node. new-keys new-pointers nil nil) root? left right))))))))))

(deftype Leaf [keys ^:mutable _hash]
  Object
  (toString [_] (pr-str* (vec keys)))

  INode
  (node-lim-key [_]
    (arrays/alast keys))

  (node-len [_]
    (arrays/alength keys))

  (node-merge [_ next]
    (Leaf. (arrays/aconcat keys (.-keys next)) nil))

  (node-merge-n-split [_ next]
    (let [ks (merge-n-split keys (.-keys next))]
      (return-array (Leaf. (arrays/aget ks 0) nil)
                    (Leaf. (arrays/aget ks 1) nil))))

  (node-lookup [_ cmp key storage opts]
    (let [{:keys [sync?] :or {sync? true}} opts
          idx (lookup-exact cmp keys key)
          result (when-not (== -1 idx)
                   (arrays/aget keys idx))]
      (if sync?
        result
        (async result))))

  (node-conj [_ cmp key storage opts]
    (let [{:keys [sync?] :or {sync? true}} opts
          idx    (binary-search-l cmp keys (dec (arrays/alength keys)) key)
          keys-l (arrays/alength keys)
          result (cond
                   ;; element already here
                   (and (< idx keys-l)
                        (== 0 (cmp key (arrays/aget keys idx))))
                   nil

                   ;; splitting
                   (== keys-l max-len)
                   (let [middle (arrays/half (inc keys-l))]
                     (if (> idx middle)
                       ;; new key spes to the second half
                       (arrays/array
                        (Leaf. (.slice keys 0 middle) nil)
                        (Leaf. (cut-n-splice keys middle keys-l idx idx (arrays/array key)) nil))
                       ;; new key spes to the first half
                       (arrays/array
                        (Leaf. (cut-n-splice keys 0 middle idx idx (arrays/array key)) nil)
                        (Leaf. (.slice keys middle keys-l) nil))))

                   ;; ok as is
                   :else
                   (arrays/array (Leaf. (splice keys idx idx (arrays/array key)) nil)))]
      (if sync?
        result
        (async result))))

  (node-disj [_ cmp key root? left right storage opts]
    (let [{:keys [sync?] :or {sync? true}} opts
          idx (lookup-exact cmp keys key)]
      (async+sync sync? {async do, await do}
                  (async
                    (when-not (== -1 idx) ;; key is here
                      (let [new-keys (splice keys idx (inc idx) (arrays/array))]
                        (rotate (Leaf. new-keys nil) root? left right))))))))

;; BTSet

(declare conj disj btset-iter)

(def ^:const uninitialized-hash nil)

(deftype BTSet [root shift cnt comparator meta ^:mutable _hash storage]
  Object
  (toString [this] (pr-str* this))

  ICloneable
  (-clone [_] (BTSet. root shift cnt comparator meta _hash storage))

  IWithMeta
  (-with-meta [_ new-meta] (BTSet. root shift cnt comparator new-meta _hash storage))

  IMeta
  (-meta [_] meta)

  IEmptyableCollection
  (-empty [_] (BTSet. (Leaf. (arrays/array) nil) 0 0 comparator meta uninitialized-hash storage))

  IEquiv
  (-equiv [this other]
    (and
     (set? other)
     (== cnt (count other))
     (every? #(contains? this %) other)))

  IHash
  (-hash [this] (caching-hash this hash-unordered-coll _hash))

  ICollection
  (-conj [this key] (conj this key comparator {}))

  ISet
  (-disjoin [this key] (disj this key comparator {}))

  ILookup
  (-lookup [this k]
    (node-lookup root comparator k storage {:sync? true}))
  (-lookup [this k not-found]
    (or (node-lookup root comparator k storage {:sync? true}) not-found))

  ISeqable
  (-seq [this] (btset-iter this))

  IReduce
  (-reduce [this f]
    (if-let [i (btset-iter this)]
      (-reduce i f)
      (f)))
  (-reduce [this f start]
    (if-let [i (btset-iter this)]
      (-reduce i f start)
      start))

  IReversible
  (-rseq [this]
    (rseq (btset-iter this)))

  ; ISorted
  ; (-sorted-seq [this ascending?])
  ; (-sorted-seq-from [this k ascending?])
  ; (-entry-key [this entry] entry)
  ; (-comparator [this] comparator)

  ICounted
  (-count [_] cnt)

  IEditableCollection
  (-as-transient [this] this)

  ITransientCollection
  (-conj! [this key] (conj this key comparator {}))
  (-persistent! [this] this)

  ITransientSet
  (-disjoin! [this key] (disj this key comparator {}))

  IFn
  (-invoke [this k] (-lookup this k))
  (-invoke [this k not-found] (-lookup this k not-found))

  IPrintWithWriter
  (-pr-writer [this writer opts]
    (pr-sequential-writer writer pr-writer "#{" " " "}" opts (seq this))))

(defn- ensure-child
  "Get child at index, with lazy restoration if needed.
   When storage is provided, supports lazy restoration.
   In sync mode returns child directly, in async mode returns channel."
  ([node idx]
   ;; Simple case - no storage, just return the child
   (when (instance? Node node)
     (arrays/aget (.-pointers node) idx)))
  ([node idx storage opts]
   (let [{:keys [sync?] :or {sync? true}} opts]
     (async+sync sync? {async do, await do}
                 (async
                   (when (instance? Node node)
                     ;; Initialize pointers array if needed
                     (when (nil? (.-pointers node))
                       (set! (.-pointers node) (arrays/make-array (arrays/alength (.-addresses node)))))

                     (if-let [child (arrays/aget (.-pointers node) idx)]
                       child
                       ;; Lazy restoration from storage
                       (when-let [addresses (.-addresses node)]
                         (when-let [addr (arrays/aget addresses idx)]
                           (let [child (await (-restore storage addr))]
                             (arrays/aset (.-pointers node) idx child)
                             child))))))))))


(defn- keys-for
  "Returns keys array for the leaf node at the given path.
   In sync mode returns keys directly, in async mode returns channel."
  ([set path]
   (keys-for set path {:sync? true}))
  ([set path opts]
   (let [{:keys [sync?] :or {sync? true}} opts]
     (async+sync sync? {async do, await do}
       (async
         (loop [level (.-shift set)
                node  (.-root set)]
           (if (pos? level)
             (recur
              (dec level)
              (if (.-storage set)
                (await (ensure-child node (path-get path level) (.-storage set) opts))
                (ensure-child node (path-get path level))))
             (.-keys node))))))))


(defn alter-btset
  ([^BTSet set root shift cnt]
   (BTSet. root shift cnt (.-comparator set) (.-meta set) uninitialized-hash (.-storage set)))
  ([^BTSet set root shift cnt cmp]
   (BTSet. root shift cnt cmp (.-meta set) uninitialized-hash (.-storage set))))

;; iteration

(defn- -next-path
  "Returns next path or nil if at end. In sync mode returns path directly, in async mode returns channel."
  ([set node ^number path ^number level]
   (-next-path set node path level {:sync? true}))
  ([set node ^number path ^number level opts]
   (let [{:keys [sync?] :or {sync? true}} opts
         idx (path-get path level)]
     (async+sync sync? {async do, await do}
       (async
         (if (pos? level)
           ;; inner node
           (let [child-node (if (.-storage set)
                             (await (ensure-child node idx (.-storage set) opts))
                             (ensure-child node idx))
                 sub-path (await (-next-path set child-node path (dec level) opts))]
             (if (nil? sub-path)
               ;; nested node overflow
               (if (< (inc idx) (arrays/alength (.-pointers node)))
                 ;; advance current node idx, reset subsequent indexes
                 (path-set empty-path level (inc idx))
                 ;; current node overflow
                 nil)
               ;; keep current idx
               (path-set sub-path level idx)))
           ;; leaf
           (if (< (inc idx) (arrays/alength (.-keys node)))
             ;; advance leaf idx
             (path-set empty-path 0 (inc idx))
             ;; leaf overflow
             nil)))))))

(defn- -rpath
  "Returns rightmost path possible starting from node and sping deeper.
   In sync mode returns path directly, in async mode returns channel."
  ([node ^number path ^number level]
   ;; For compatibility - no storage
   (if (pos? level)
     ;; inner node
     (let [last-idx (dec (arrays/alength (.-pointers node)))]
       (recur
         (arrays/aget (.-pointers node) last-idx)
         (path-set path level last-idx)
         (dec level)))
     ;; leaf
     (path-set path 0 (dec (arrays/alength (.-keys node))))))
  ([node ^number path ^number level storage]
   ;; With storage, default to sync
   (-rpath node path level storage {:sync? true}))
  ([node ^number path ^number level storage opts]
   ;; With storage and opts
   (let [{:keys [sync?] :or {sync? true}} opts]
     (async+sync sync? {async do, await do}
       (async
         (if (pos? level)
           ;; inner node
           (let [last-idx (dec (node-len node))
                 child-node (await (ensure-child node last-idx storage opts))]
             (await (-rpath child-node
                         (path-set path level last-idx)
                         (dec level)
                         storage
                         opts)))
           ;; leaf
           (path-set path 0 (dec (arrays/alength (.-keys node))))))))))

(defn- next-path-async
  "Async version of next-path that returns channel with next path"
  [set ^number path]
  (async
    (if (neg? path)
      empty-path
      (or
       (await (-next-path set (.-root set) path (.-shift set) {:sync? false}))
       (path-inc (if (.-storage set)
                      (await (-rpath (.-root set) empty-path (.-shift set) (.-storage set) {:sync? false}))
                      (-rpath (.-root set) empty-path (.-shift set))))))))

(defn- next-path
  "Returns path representing next item after `path` in natural traversal order.
   Will overflow at leaf if at the end of the tree"
  [set ^number path]
  (if (neg? path)
    empty-path
    (or
     (-next-path set (.-root set) path (.-shift set))
     (path-inc (if (.-storage set)
                    (-rpath (.-root set) empty-path (.-shift set) (.-storage set))
                    (-rpath (.-root set) empty-path (.-shift set)))))))

(defn- -prev-path
  "Returns previous path or nil if at beginning. In sync mode returns path directly, in async mode returns channel."
  ([set node ^number path ^number level]
   (-prev-path set node path level {:sync? true}))
  ([set node ^number path ^number level opts]
   (let [{:keys [sync?] :or {sync? true}} opts
         idx (path-get path level)]
     (async+sync sync? {async do, await do}
       (async
         (cond
           ;; leaf overflow
           (and (== 0 level) (== 0 idx))
           nil

           ;; leaf
           (== 0 level)
           (path-set empty-path 0 (dec idx))

           ;; branch that was overflow before
           (>= idx (node-len node))
           (if (.-storage set)
             (await (-rpath node path level (.-storage set) opts))
             (-rpath node path level))

           :else
           (let [child-node (if (.-storage set)
                             (await (ensure-child node idx (.-storage set) opts))
                             (ensure-child node idx))
                 path' (await (-prev-path set child-node path (dec level) opts))]
             (cond
               ;; no sub-overflow, keep current idx
               (some? path')
               (path-set path' level idx)

               ;; nested overflow + this node overflow
               (== 0 idx)
               nil

               ;; nested overflow, advance current idx, reset subsequent indexes
               :else
               (let [child-node (if (.-storage set)
                                 (await (ensure-child node (dec idx) (.-storage set) opts))
                                 (ensure-child node (dec idx)))
                     path' (if (.-storage set)
                             (await (-rpath child-node path (dec level) (.-storage set) opts))
                             (-rpath child-node path (dec level)))]
                 (path-set path' level (dec idx)))))))))))

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

(defn- prev-path
  "Returns path representing previous item before `path` in natural traversal order.
   Will overflow at leaf if at beginning of tree"
  [set ^number path]
  (if (> (path-get path (inc (.-shift set))) 0) ;; overflow
    (if (.-storage set)
      (-rpath (.-root set) path (.-shift set) (.-storage set))
      (-rpath (.-root set) path (.-shift set)))
    (or
     (-prev-path set (.-root set) path (.-shift set))
     (path-dec empty-path))))

(declare iter riter)

(defn- btset-iter
  "Iterator that represents the whole set"
  [set]
  (when (pos? (node-len (.-root set)))
    (let [left  empty-path
          rpath (if (.-storage set)
                  (-rpath (.-root set) empty-path (.-shift set) (.-storage set))
                  (-rpath (.-root set) empty-path (.-shift set)))
          right (next-path set rpath)]
      (iter set left right))))

;; replace with cljs.core/ArrayChunk after https://dev.clojure.org/jira/browse/CLJS-2470
(deftype Chunk [arr off end]
  ICounted
  (-count [_] (- end off))

  IIndexed
  (-nth [this i]
    (aget arr (+ off i)))

  (-nth [this i not-found]
    (if (and (>= i 0) (< i (- end off)))
      (aget arr (+ off i))
      not-found))

  IChunk
  (-drop-first [this]
    (if (== off end)
      (throw (js/Error. "-drop-first of empty chunk"))
      (Chunk. arr (inc off) end)))

  IReduce
  (-reduce [this f]
    (if (== off end)
      (f)
      (-reduce (-drop-first this) f (aget arr off))))

  (-reduce [this f start]
    (loop [val start, n off]
      (if (< n end)
        (let [val' (f val (aget arr n))]
          (if (reduced? val')
            @val'
            (recur val' (inc n))))
        val))))

(defprotocol IIter
  (-copy [this left right]))

(defprotocol ISeek
  (-seek
    [this key]
    [this key comparator]))

(declare -seek* -rseek*)

(deftype Iter [^BTSet set left right keys idx]
  IIter
  (-copy [_ l r]
    (Iter. set l r (keys-for set l) (path-get l 0)))

  IEquiv
  (-equiv [this other] (equiv-sequential this other))

  ISequential
  ISeqable
  (-seq [this] (when keys this))

  ISeq
  (-first [this]
    (when keys
      (arrays/aget keys idx)))

  (-rest [this]
    (or (-next this) ()))

  INext
  (-next [this]
    (when keys
      (if (< (inc idx) (arrays/alength keys))
        ;; can use cached array to move forward
        (let [left' (path-inc left)]
          (when (path-lt left' right)
            (Iter. set left' right keys (inc idx))))
        (let [left' (next-path set left)]
          (when (path-lt left' right)
            (-copy this left' right))))))

  IChunkedSeq
  (-chunked-first [this]
    (let [end-idx (if (path-same-leaf left right)
                    ;; right is in the same node
                    (path-get right 0)
                    ;; right is in a different node
                    (arrays/alength keys))]
      (Chunk. keys idx end-idx)))

  (-chunked-rest [this]
    (or (-chunked-next this) ()))

  IChunkedNext
  (-chunked-next [this]
    (let [last  (path-set left 0 (dec (arrays/alength keys)))
          left' (next-path set last)]
      (when (path-lt left' right)
        (-copy this left' right))))

  IReduce
  (-reduce [this f]
    (if (nil? keys)
      (f)
      (let [first (-first this)]
        (if-some [next (-next this)]
          (-reduce next f first)
          first))))

  (-reduce [this f start]
    (loop [left left
           keys keys
           idx  idx
           acc  start]
      (if (nil? keys)
        acc
        (let [new-acc (f acc (arrays/aget keys idx))]
          (cond
            (reduced? new-acc)
            @new-acc

            (< (inc idx) (arrays/alength keys)) ;; can use cached array to move forward
            (let [left' (path-inc left)]
              (if (path-lt left' right)
                (recur left' keys (inc idx) new-acc)
                new-acc))

            :else
            (let [left' (next-path set left)]
              (if (path-lt left' right)
                (recur left' (keys-for set left') (path-get left' 0) new-acc)
                new-acc)))))))

  IReversible
  (-rseq [this]
    (when keys
      (riter set (prev-path set left) (prev-path set right))))

  ISeek
  (-seek [this key]
    (-seek this key (.-comparator set)))

  (-seek [this key cmp]
    (cond
      (nil? key)
      (throw (js/Error. "seek can't be called with a nil key!"))

      (nat-int? (cmp (arrays/aget keys idx) key))
      this

      :else
      (when-some [left' (-seek* set key cmp)]
        (Iter. set left' right (keys-for set left') (path-get left' 0)))))

  Object
  (toString [this] (pr-str* this))

  IPrintWithWriter
  (-pr-writer [this writer opts]
    (pr-sequential-writer writer pr-writer "(" " " ")" opts (seq this))))

(defn iter [set left right]
  (Iter. set left right (keys-for set left) (path-get left 0)))

;; reverse iteration

(deftype ReverseIter [^BTSet set left right keys idx]
  IIter
  (-copy [_ l r]
    (ReverseIter. set l r (keys-for set r) (path-get r 0)))

  IEquiv
  (-equiv [this other] (equiv-sequential this other))

  ISequential
  ISeqable
  (-seq [this] (when keys this))

  ISeq
  (-first [this]
    (when keys
      (arrays/aget keys idx)))

  (-rest [this]
    (or (-next this) ()))

  INext
  (-next [this]
    (when keys
      (if (> idx 0)
        ;; can use cached array to advance
        (let [right' (path-dec right)]
          (when (path-lt left right')
            (ReverseIter. set left right' keys (dec idx))))
        (let [right' (prev-path set right)]
          (when (path-lt left right')
            (-copy this left right'))))))

  IReversible
  (-rseq [this]
    (when keys
      (iter set (next-path set left) (next-path set right))))

  ISeek
  (-seek [this key]
    (-seek this key (.-comparator set)))

  (-seek [this key cmp]
    (cond
      (nil? key)
      (throw (js/Error. "seek can't be called with a nil key!"))

      (nat-int? (cmp key (arrays/aget keys idx)))
      this

      :else
      (let [right' (prev-path set (-rseek* set key cmp))]
        (when (and
               (nat-int? right')
               (path-lte left right')
               (path-lt  right' right))
          (ReverseIter. set left right' (keys-for set right') (path-get right' 0))))))

  Object
  (toString [this] (pr-str* this))

  IPrintWithWriter
  (-pr-writer [this writer opts]
    (pr-sequential-writer writer pr-writer "(" " " ")" opts (seq this))))

(defn riter [^BTSet set left right]
  (ReverseIter. set left right (keys-for set right) (path-get right 0)))

;; distance

(defn- -distance [^BTSet set ^Node node left right level]
  (let [idx-l (path-get left level)
        idx-r (path-get right level)]
    (if (pos? level)
      ;; inner node
      (if (== idx-l idx-r)
        (-distance set (if (.-storage set)
                          (ensure-child node idx-l (.-storage set) {:sync? true})
                          (ensure-child node idx-l)) left right (dec level))
        (loop [level level
               res   (- idx-r idx-l)]
          (if (== 0 level)
            res
            (recur (dec level) (* res avg-len)))))
      (- idx-r idx-l))))

(defn- distance [^BTSet set path-l path-r]
  (cond
    (path-eq path-l path-r)
    0

    (path-eq (path-inc path-l) path-r)
    1

    (path-eq (next-path set path-l) path-r)
    1

    :else
    (-distance set (.-root set) path-l path-r (.-shift set))))

(defn est-count [iter]
  (distance (.-set iter) (.-left iter) (.-right iter)))

;; Slicing

(defn- -seek*
  "Returns path to first element >= key,
   or nil if all elements in a set < key.
   In sync mode returns path directly, in async mode returns channel."
  ([^BTSet set key comparator]
   (-seek* set key comparator {:sync? true}))
  ([^BTSet set key comparator opts]
   (let [{:keys [sync?] :or {sync? true}} opts]
     (async+sync sync? {async do, await do}
       (async
         (if (nil? key)
           empty-path
           (loop [node  (.-root set)
                  path  empty-path
                  level (.-shift set)]
             (let [keys-l (node-len node)]
               (if (== 0 level)
                 (let [keys (.-keys node)
                       idx  (binary-search-l comparator keys (dec keys-l) key)]
                   (if (== keys-l idx)
                     nil
                     (path-set path 0 idx)))
                 (let [keys (.-keys node)
                       idx  (binary-search-l comparator keys (- keys-l 2) key)
                       child-node (if (.-storage set)
                                    (await (ensure-child node idx (.-storage set) opts))
                                    (ensure-child node idx))]
                   (recur
                    child-node
                    (path-set path level idx)
                    (dec level))))))))))))

(defn- -rseek*
  "Returns path to the first element that is > key.
   If all elements in a set are <= key, returns `(-rpath set) + 1`.
   It's a virtual path that is bigger than any path in a tree.
   In sync mode returns path directly, in async mode returns channel."
  ([^BTSet set key comparator]
   (-rseek* set key comparator {:sync? true}))
  ([^BTSet set key comparator opts]
   (let [{:keys [sync?] :or {sync? true}} opts]
     (async+sync sync? {async do, await do}
       (async
         (if (nil? key)
           (path-inc (if (.-storage set)
                       (await (-rpath (.-root set) empty-path (.-shift set) (.-storage set) opts))
                       (-rpath (.-root set) empty-path (.-shift set))))
           (loop [node  (.-root set)
                  path  empty-path
                  level (.-shift set)]
             (let [keys-l (node-len node)]
               (if (== 0 level)
                 (let [keys (.-keys node)
                       idx  (binary-search-r comparator keys (dec keys-l) key)
                       res  (path-set path 0 idx)]
                   res)
                 (let [keys (.-keys node)
                       idx  (binary-search-r comparator keys (- keys-l 2) key)
                       res  (path-set path level idx)
                       child-node (if (.-storage set)
                                    (await (ensure-child node idx (.-storage set) opts))
                                    (ensure-child node idx))]
                   (recur
                    child-node
                    res
                    (dec level))))))))))))

(defn- node-requires-storage?
  "Check if a node itself needs to be loaded from storage"
  [node]
  (and (instance? Node node)
       (.-addresses node)           ; Has storage addresses
       (nil? (.-pointers node))))   ; But pointers not loaded yet

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
                  child-loaded? (when (.-pointers node)
                                 (arrays/aget (.-pointers node) idx))]
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

;; Storage operations

(declare store-node)

;; Helper functions for creating nodes from storage
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
  [node storage opts]
  (let [{:keys [sync?] :or {sync? true}} opts]
  (async+sync sync? {async do, await do}
              (cond
                ;; Leaf node - just store it
                (instance? Leaf node)
                (-store storage node opts)

                ;; Branch node - store children first
                (instance? Node node)
                (async
                  (let [children (.-pointers node)
                        addresses (arrays/make-array (arrays/alength children))]
                    (dotimes [i (arrays/alength children)]
                      (let [child (arrays/aget children i)
                            addr (await (store-node child storage opts))]
                        (arrays/aset addresses i addr)))
                    ;; Then store this node with addresses
                    (let [node-with-addresses (Node. (.-keys node) nil addresses nil)
                          final-addr (await (-store storage node-with-addresses nil))]
                      final-addr)))

                :else
                (throw (ex-info "Unknown node type" {:node node :type (type node)}))))))

;; Public interface

(defn conj
  "Analogue to [[clojure.core/conj]] but with comparator that overrides the one stored in set.
   Accepts optional opts map with {:sync? true/false} (defaults to true)."
  ([^BTSet set key] (conj set key (.-comparator set) {}))
  ([^BTSet set key cmp] (conj set key cmp {}))
  ([^BTSet set key cmp opts]
   (let [{:keys [sync?] :or {sync? true}} opts]
     (async+sync sync? {async do, await do}
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
                                    (inc (.-cnt set))
                                    )

                       ;; introducing new root
                       :else
                       (alter-btset set
                                    (Node. (arrays/amap node-lim-key roots) roots nil nil)
                                    (inc (.-shift set))
                                    (inc (.-cnt set))
                                    ))))))))

(defn disj
  "Analogue to [[clojure.core/disj]] with comparator that overrides the one stored in set.
   Accepts optional opts map with {:sync? true/false} (defaults to true)."
  ([^BTSet set key] (disj set key (.-comparator set) {}))
  ([^BTSet set key cmp] (disj set key cmp {}))
  ([^BTSet set key cmp opts]
   (let [{:keys [sync?] :or {sync? true}} opts]
     (async+sync sync? {async do, await do}
       (async
         (let [new-roots (await (node-disj (.-root set) cmp key true nil nil (.-storage set) opts))]
           (if (nil? new-roots) ;; nothing changed, key wasn't in the set
             set
             (let [new-root (arrays/aget new-roots 0)]
               (if (and (instance? Node new-root)
                        (== 1 (arrays/alength (.-pointers new-root))))

                 ;; root has one child, make him new root
                 (alter-btset set
                              (arrays/aget (.-pointers new-root) 0)
                              (dec (.-shift set))
                              (dec (.-cnt set))
                              )

                 ;; keeping root level
                 (alter-btset set
                              new-root
                              (.-shift set)
                              (dec (.-cnt set))
                              ))))))))))

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

;; TODO: Implement async-rslice similar to async-slice
;; Would return a Promise resolving to a reverse-ordered vector of elements

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
         0 (BTSet. (Leaf. (arrays/array) nil) 0 0 cmp nil uninitialized-hash storage)
         1 (BTSet. (first current-level) shift (arrays/alength arr) cmp nil uninitialized-hash storage)
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
  ([cmp] (BTSet. (Leaf. (arrays/array) nil) 0 0 cmp nil uninitialized-hash nil))
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
          (:meta opts) uninitialized-hash (:storage opts)))

(defn store
  "Store the set to storage. Returns map with :root-address, :shift, :count.
   Accepts optional opts map with {:sync? true/false} (defaults to true)."
  ([^BTSet set opts]
   (store set (.-storage set) opts))
  ([^BTSet set storage {:keys [sync?] :or {sync? true} :as opts}]
   (assert (instance? BTSet set))
   (assert (implements? me.tonsky.persistent-sorted-set/IStorage storage))
   (async+sync sync? {async do, await do}
     (async
       (let [root-addr (await (store-node (.-root set) storage opts))]
         {:root-address root-addr
          :shift (.-shift set)
          :count (.-cnt set)
          :comparator (.-comparator set)})))))

(defn restore
  "Restore a set from storage given root-address-or-info and storage.
   First arg can be either:
   - A root address (UUID) - requires opts with :shift and :count
   - A map from store-set with :root-address, :shift, :count, :comparator
   Storage operations will use the provided opts for sync/async mode."
  ([root-address-or-info storage] (restore root-address-or-info storage {}))
  ([root-address-or-info storage opts]
   (let [{:keys [sync?] :or {sync? true}} opts
         ;; Handle both old format (bare UUID) and new format (map with metadata)
         root-address (if (map? root-address-or-info)
                        (:root-address root-address-or-info)
                        root-address-or-info)
         ;; Use metadata from map if available, otherwise fall back to opts
         shift (if (map? root-address-or-info)
                 (:shift root-address-or-info)
                 (:shift opts 0))
         cnt (if (map? root-address-or-info)
               (:count root-address-or-info)
               (:count opts 0))
         cmp (if (map? root-address-or-info)
               (or (:comparator root-address-or-info) compare)
               (or (:comparator opts) compare))]
     (async+sync sync? {async do, await do}
                 (async
                   (let [root (await (-restore storage root-address))]
                     (BTSet. root shift cnt cmp nil uninitialized-hash storage)))))))