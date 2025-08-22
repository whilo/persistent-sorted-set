(ns me.tonsky.persistent-sorted-set.test.storage
  (:require-macros [me.tonsky.persistent-sorted-set.test.storage :refer [with-stats dobatches]])
  (:require
   [cljs.test :as t :refer [is are deftest testing]]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [me.tonsky.persistent-sorted-set :as set :refer [Leaf Node]]))

(def ^:dynamic *debug* false)

(defn gen-addr [] (random-uuid))

(def *stats
  (atom
    {:reads 0
     :writes 0
     :accessed 0}))

(defrecord Storage [*memory *disk]
  set/IStorage
  (-store [_ node opts]
    (swap! *stats update :writes inc)
    (let [address (gen-addr)]
      (swap! *disk assoc address
             (pr-str
              {:level     (.level node)
               :keys      (.keys node)
               :addresses (when (instance? Node node) (.addresses node))}))
      address))
  (-restore [_ address opts]
    (or
     (@*memory address)
     (let [{:keys [keys addresses]} (edn/read-string (@*disk address))
           node (if addresses
                  (Node. keys nil addresses nil)
                  (Leaf. keys nil))]
       (swap! *stats update :reads inc)
       (swap! *memory assoc address node)
       node)))
  (-accessed [_ address]
    (swap! *stats update :accessed inc)
    nil)
  (-delete [this addresses] (throw (js/Error. "unimplemented"))))

(defn storage
  ([] (->Storage (atom {}) (atom {})))
  ([*disk] (->Storage (atom {}) *disk))
  ([*memory *disk] (->Storage *memory *disk)))

(defn roundtrip [set]
  (let [storage (storage)
        address (set/store set storage)]
    (set/restore address storage)))

(deftest basics
  (and
   (testing "one leaf."
     (let [_(reset! *stats {:reads 0 :writes 0 :accessed 0})
           original (conj (set/sorted-set* {:branching-factor 32}) 0)
           storage  (storage)]
       (and
        (is (= 0 (:writes @*stats)))
        (is (= 0 (:reads @*stats)))
        (let [address (set/store original storage)]
          (and
           (is (uuid? address))
           (is (= 1 (:writes @*stats)))
           (is (= 0 (:reads @*stats)))
           (let [restored (set/restore address storage {:branching-factor 32})]
             (and
              (is (= 0 (:reads @*stats)))
              (is (= restored original))
              )))))))))

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

