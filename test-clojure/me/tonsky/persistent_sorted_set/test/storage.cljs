(ns me.tonsky.persistent-sorted-set.test.storage
  (:require-macros [me.tonsky.persistent-sorted-set.test.storage :refer [with-stats dobatches]])
  (:require
   [cljs.test :as t :refer [is are deftest testing]]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [me.tonsky.persistent-sorted-set :as set]
   [me.tonsky.persistent-sorted-set.protocols :refer [IStorage] :as impl]
   [me.tonsky.persistent-sorted-set.leaf :refer [Leaf] :as leaf]
   [me.tonsky.persistent-sorted-set.node :refer [Node] :as node]))

(def ^:dynamic *debug* false)

(defn dbg [& args]
  (when *debug*
    (apply println args)))

(defn gen-addr [] (random-uuid))

(def *stats
  (atom
    {:reads 0
     :writes 0
     :accessed 0}))

(defrecord Storage [*memory *disk]
  IStorage
  (store [_ node opts]
    (dbg "store<" (type node) ">")
    (swap! *stats update :writes inc)
    (let [address (gen-addr)]
      (swap! *disk assoc address
             (pr-str
              {:level     (.-shift node) ;;<------------------------------------ FIX ME
               :keys      (.-keys node)
               :addresses (when (instance? Node node) (.-addresses node))}))
      address))
  (restore [_ address _opts]
    (or
     (@*memory address)
     (let [{:keys [keys addresses level]} (edn/read-string (@*disk address))
           node (if addresses
                  (let [n (Node. keys nil addresses nil)]
                    (set! (.-level n) level) ;;<-------------------------------- FIX ME
                    n)
                  (Leaf. keys nil))]
       (dbg "restored<" (type node) ">")
       (swap! *stats update :reads inc)
       (swap! *memory assoc address node)
       node)))
  (accessed [_ address]
    (swap! *stats update :accessed inc)
    nil)
  (delete [this addresses] (throw (js/Error. "unimplemented"))))

(defn storage
  ([] (->Storage (atom {}) (atom {})))
  ([*disk] (->Storage (atom {}) *disk))
  ([*memory *disk] (->Storage *memory *disk)))

(defn roundtrip [set]
  (let [storage (storage)
        address (set/store set storage)]
    (set/restore address storage)))

(defn children [node] (some->> (.-children node) (filter some?)))

(defn ks [node] (some->> (.-keys node) (filterv some?)))

(defn node? [o] (instance? Node o))


;; TODO ILookup needs ensure-root

(deftest ascending-insert-test
  (and
   (testing "one item"
     (let [_(reset! *stats {:reads 0 :writes 0 :accessed 0})
           original (conj (set/sorted-set* {}) 0)]
       (and
        (is (= 0 (:writes @*stats)))
        (is (= 0 (:reads @*stats)))
        (is (instance? Leaf (.-root original)))
        (is (nil? (.-address original)))
        (let [storage (storage)
              address (set/store original storage)]
          (and
           (is (uuid? address))
           (is (= address (.-address original)))
           (is (= 1 (:writes @*stats)))
           (is (= 0 (:reads @*stats)))
           (testing "restoring one item"
             (let [restored (set/restore address storage {:count 1})]             ;<-- XXX
               (and
                (is (set? original))
                (is (set? restored))
                (is (= 1 (count original)) "original has 1 item")
                (is (= 1 (count restored)) "restored has 1 item")
                (is (= restored original)  "restored is equiv")
                (is (instance? Leaf (.-root restored)))
                (is (= 1 (:reads @*stats)))))))))))
   (testing "one full leaf"
     (let [_(reset! *stats {:reads 0 :writes 0 :accessed 0})
           original (into (set/sorted-set* {}) (range 0 32))]
       (and
        (is (= 0 (:writes @*stats)))
        (is (= 0 (:reads @*stats)))
        (is (instance? Leaf (.-root original)))
        (is (nil? (.-address original)))
        (let [storage  (storage)
              address (set/store original storage)]
          (and
           (is (uuid? address))
           (is (= address (.-address original)))
           (is (= 1 (:writes @*stats)))
           (is (= 0 (:reads @*stats)))
           (let [restored (set/restore address storage {:count 32})]            ;<-- XXX
             (and
              (is (= restored original))
              (is (instance? Leaf (.-root restored)))
              (is (= 1 (:reads @*stats))))))))))
   (testing "full-leaf + 1"
     (let [_(reset! *stats {:reads 0 :writes 0 :accessed 0})
           original (into (set/sorted-set* {}) (range 0 33))]
       (and
        (is (= 0 (:writes @*stats)))
        (is (= 0 (:reads @*stats)))
        (is (instance? Node (.-root original)))
        (let [children (children (.-root original))]
          (and
           (is (= 2 (count children)))
           (is (instance? Leaf (nth children 0)))
           (is (= 16 (count (.-keys (nth children 0)))))
           (is (instance? Leaf (nth children 1)))
           (is (= 17 (count (.-keys (nth children 1)))))))
        (is (nil? (.-address original)))
        (let [storage (storage)
              address (set/store original storage)]
          (and
           (is (uuid? address))
           (is (= address (.-address original)))
           (is (= 3 (:writes @*stats)))
           (is (= 0 (:reads @*stats)))
           (let [restored (set/restore address storage {:count 33})]            ;<--- XXX
             (and
              (is (= restored original))
              (let [children (children (.-root restored))]
                (and
                 (is (= 2 (count children)))
                 (is (instance? Leaf (nth children 0)))
                 (is (= 16 (count (.-keys (nth children 0)))))
                 (is (instance? Leaf (nth children 1)))
                 (is (= 17 (count (.-keys (nth children 1)))))))
              (is (= 3 (:reads @*stats))))))))))
   (testing "32^2"
     (reset! *stats {:reads 0 :writes 0 :accessed 0})
     (let [original  (into (set/sorted-set* {}) (range 0 1024))]
       (and
        (is (= 0 (:writes @*stats)))
        (is (= 0 (:reads @*stats)))
        (is (instance? Node (.-root original)))
        (let [cs (children (.-root original))
              root-keys (ks (.-root original))]
          (and
           (is (= 3 (count cs)))
           (is (every? node? cs))
           (is (= [255 511 1023] root-keys))
           (is (nil? (.-address original)))
           (let [storage (storage)
                 address (set/store original storage)]
             (and
              (is (uuid? address))
              (is (= address (.-address original)))
              (is (= 67 (:writes @*stats)))
              (is (= 0 (:reads @*stats)))
              (is (empty? (deref (:*memory storage))))
              (let [restored (set/restore address storage {:count 1024})]     ;<--- XXX
                (and
                 (is (empty? (deref (:*memory storage))))
                 (is (= 0 (:reads @*stats)))
                 (is (= restored original))
                 (is (= 67 (:reads @*stats)))
                 (let [cs (children (.-root restored))
                       root-keys (ks (.-root original))]
                   (and
                    (is (= 3 (count cs)))
                    (is (= 3 (count root-keys)))
                    (is (every? node? cs))
                    (is (= [255 511 1023] root-keys)))))))))))))))

;;; uses ensure-root
;; slice
;; rslice
;; contains
;; conj
;; disj
;; count
;; walk-addresses


; (defn loaded-ratio
;   ([set]
;    (let [storage (.-storage set)
;          address (.-address set)
;          root    (.-root set)]
;      (loaded-ratio (some-> storage :*memory deref) address root)))
;   ([memory address node]
;    (when *debug*
;      (println address (contains? memory address) node (memory address)))
;    (if (and address (not (contains? memory address)))
;      0.0
;      (let [node (or node (memory address))]
;        (if (instance? Leaf node)
;          1.0
;          (let [len (count (.-keys node))]
;            (double
;              (/ (->>
;                   (mapv
;                     (fn [_ child-addr child]
;                       (loaded-ratio memory child-addr child))
;                     (range len)
;                     (or (.-addresses node) (repeat len nil))
;                     (or (.-children node)  (repeat len nil)))
;                   (reduce + 0))
;                len))))))))

; (defn durable-ratio
;   ([set]
;    (durable-ratio (.-address set) (.-root set)))
;   ([address node]
;    (cond
;      (some? address)       1.0
;      (instance? Leaf node) 0.0
;      :else
;      (let [len (count (.-keys node))]
;        (/ (->>
;             (map
;               (fn [_ child-addr child]
;                 (durable-ratio child-addr child))
;               (range len)
;               (.-addresses node)
;               (.-children node))
;             (reduce + 0))
;          len)))))

