(ns me.tonsky.persistent-sorted-set.btset
  (:refer-clojure :exclude [iter])
  (:require-macros [me.tonsky.persistent-sorted-set.macros :refer [async+sync]])
  (:require [me.tonsky.persistent-sorted-set.arrays :as arrays]
            [me.tonsky.persistent-sorted-set.constants
             :refer [max-len uninitialized-hash empty-path avg-len
                     bits-per-level max-safe-path max-safe-level bit-mask]]
            [me.tonsky.persistent-sorted-set.leaf :as leaf]
            [me.tonsky.persistent-sorted-set.node :as node]
            [me.tonsky.persistent-sorted-set.protocols :refer [INode] :as impl]
            [me.tonsky.persistent-sorted-set.util
             :refer [rotate lookup-exact splice cut-n-splice
                     binary-search-l binary-search-r
                     return-array merge-n-split check-n-splice]]
            [await-cps :refer [await] :refer-macros [async]]))

(declare iter riter -seek* -rseek* -rpath)

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

(def factors
  (arrays/into-array (map #(js/Math.pow 2 %) (range 0 52 bits-per-level))))

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

(defn- -next-path
  "Returns next path or nil if at end. In sync mode returns path directly, in async mode returns channel."
  ([set node ^number path ^number level]
   (-next-path set node path level {:sync? true}))
  ([set node ^number path ^number level {:keys [sync?] :or {sync? true} :as opts}]
   (let [idx (path-get path level)]
     (async+sync sync?
                 (async
                  (if (pos? level)
                    ;; inner node
                    (let [child-node (if (.-storage set)
                                       (await (node/ensure-child node idx (.-storage set) opts))
                                       (node/ensure-child node idx))
                          sub-path (await (-next-path set child-node path (dec level) opts))]
                      (if (nil? sub-path)
                        ;; nested node overflow
                        (if (< (inc idx) (arrays/alength (.-children node)))
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
     (async+sync sync?
                 (async
                  (cond
                    ;; leaf overflow
                    (and (== 0 level) (== 0 idx))
                    nil

                    ;; leaf
                    (== 0 level)
                    (path-set empty-path 0 (dec idx))

                    ;; branch that was overflow before
                    (>= idx (impl/node-len node))
                    (if (.-storage set)
                      (await (-rpath node path level (.-storage set) opts))
                      (-rpath node path level))

                    :else
                    (let [child-node (if (.-storage set)
                                       (await (node/ensure-child node idx (.-storage set) opts))
                                       (node/ensure-child node idx))
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
                                           (await (node/ensure-child node (dec idx) (.-storage set) opts))
                                           (node/ensure-child node (dec idx)))
                              path' (if (.-storage set)
                                      (await (-rpath child-node path (dec level) (.-storage set) opts))
                                      (-rpath child-node path (dec level)))]
                          (path-set path' level (dec idx)))))))))))

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

(defn- keys-for
  "Returns keys array for the leaf node at the given path.
   In sync mode returns keys directly, in async mode returns channel."
  ([set path]
   (keys-for set path {:sync? true}))
  ([set path {:keys [sync?] :or {sync? true} :as opts}]
   (async+sync sync?
    (async
      (loop [level (.-shift set)
             node  (.-root set)]
        (if (pos? level)
          (recur
            (dec level)
            (if (.-storage set)
              (await (node/ensure-child node (path-get path level) (.-storage set) opts))
              (node/ensure-child node (path-get path level))))
          (.-keys node)))))))

;; replace with cljs.core/ArrayChunk after https://dev.clojure.org/jira/browse/CLJS-2470
(deftype Chunk [arr off end]
  ICounted
  (-count [_] (- end off))

  IIndexed
  (-nth [this i] (aget arr (+ off i)))

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
  (-seek [this key] [this key comparator]))

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
  (-first [_] (when keys (arrays/aget keys idx)))

  (-rest [this] (or (-next this) ()))

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

(defn- -rpath
  "Returns rightmost path possible starting from node and sping deeper.
   In sync mode returns path directly, in async mode returns channel."
  ([node ^number path ^number level]
   ;; For compatibility - no storage
   (if (pos? level)
     ;; inner node
     (let [last-idx (dec (arrays/alength (.-children node)))]
       (recur
         (arrays/aget (.-children node) last-idx)
         (path-set path level last-idx)
         (dec level)))
     ;; leaf
     (path-set path 0 (dec (arrays/alength (.-keys node))))))
  ([node ^number path ^number level storage]
   ;; With storage, default to sync
   (-rpath node path level storage nil))
  ([node ^number path ^number level storage {:keys [sync?] :or {sync? true} :as opts}]
   (async+sync sync?
               (async
                (if (pos? level)
                  ;; inner node
                  (let [last-idx (dec (impl/node-len node))
                        child-node (await (node/ensure-child node last-idx storage opts))]
                    (await (-rpath child-node
                                   (path-set path level last-idx)
                                   (dec level)
                                   storage
                                   opts)))
                  ;; leaf
                  (path-set path 0 (dec (arrays/alength (.-keys node)))))))))

(defn- -distance [^BTSet set ^Node node left right level]
  (let [idx-l (path-get left level)
        idx-r (path-get right level)]
    (if (pos? level)
      ;; inner node
      (if (== idx-l idx-r)
        (-distance set (if (.-storage set)
                          (node/ensure-child node idx-l (.-storage set) {:sync? true})
                          (node/ensure-child node idx-l)) left right (dec level))
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


;; Slicing

(defn- -seek*
  "Returns path to first element >= key,
   or nil if all elements in a set < key.
   In sync mode returns path directly, in async mode returns channel."
  ([^BTSet set key comparator]
   (-seek* set key comparator {:sync? true}))
  ([^BTSet set key comparator opts]
   (let [{:keys [sync?] :or {sync? true}} opts]
     (async+sync sync?
       (async
         (if (nil? key)
           empty-path
           (loop [node  (.-root set)
                  path  empty-path
                  level (.-shift set)]
             (let [keys-l (impl/node-len node)]
               (if (== 0 level)
                 (let [keys (.-keys node)
                       idx  (binary-search-l comparator keys (dec keys-l) key)]
                   (if (== keys-l idx)
                     nil
                     (path-set path 0 idx)))
                 (let [keys (.-keys node)
                       idx  (binary-search-l comparator keys (- keys-l 2) key)
                       child-node (if (.-storage set)
                                    (await (node/ensure-child node idx (.-storage set) opts))
                                    (node/ensure-child node idx))]
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
  ([^BTSet set key comparator {:keys [sync?] :or {sync? true} :as opts}]
   (async+sync sync?
     (async
       (if (nil? key)
         (path-inc (if (.-storage set)
                     (await (-rpath (.-root set) empty-path (.-shift set) (.-storage set) opts))
                     (-rpath (.-root set) empty-path (.-shift set))))
         (loop [node  (.-root set)
                path  empty-path
                level (.-shift set)]
           (let [keys-l (impl/node-len node)]
             (if (== 0 level)
               (let [keys (.-keys node)
                     idx  (binary-search-r comparator keys (dec keys-l) key)
                     res  (path-set path 0 idx)]
                 res)
               (let [keys       (.-keys node)
                     idx        (binary-search-r comparator keys (- keys-l 2) key)
                     res        (path-set path level idx)
                     child-node (if (.-storage set)
                                  (await (node/ensure-child node idx (.-storage set) opts))
                                  (node/ensure-child node idx))]
                 (recur
                   child-node
                   res
                   (dec level)))))))))))

(defn- btset-iter
  "Iterator that represents the whole set"
  [set]
  (when (pos? (impl/node-len (.-root set)))
    (let [left  empty-path
          rpath (if (.-storage set)
                  (-rpath (.-root set) empty-path (.-shift set) (.-storage set))
                  (-rpath (.-root set) empty-path (.-shift set)))
          right (next-path set rpath)]
      (iter set left right))))

(deftype BTSet [root shift cnt comparator meta ^:mutable _hash storage address]
  Object
  (toString [this] (pr-str* this))

  ICloneable
  (-clone [_] (BTSet. root shift cnt comparator meta _hash storage address))

  IWithMeta
  (-with-meta [_ new-meta] (BTSet. root shift cnt comparator new-meta _hash storage address))

  IMeta
  (-meta [_] meta)

  IEmptyableCollection
  (-empty [_] (BTSet. (leaf/Leaf. (arrays/array) nil) 0 0 comparator meta uninitialized-hash storage address))

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
           (impl/node-lookup root comparator k storage {:sync? true}))
  (-lookup [this k not-found]
           (or (impl/node-lookup root comparator k storage {:sync? true}) not-found))

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