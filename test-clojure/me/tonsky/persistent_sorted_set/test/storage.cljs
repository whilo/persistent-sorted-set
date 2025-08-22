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
  (-store [_ node address]
    (swap! *stats update :writes inc)
    (swap! *disk assoc address
           (pr-str
            {:keys      (.-keys node)
             :addresses (when (instance? Node node) (.-addresses node))}))
    address)
  (-accessed [_ address]
    (swap! *stats update :accessed inc)
    nil)
  (-restore [_ address]
    (or
     (@*memory address)
     (let [{:keys [keys addresses]} (edn/read-string (@*disk address))
           node (if addresses
                  (Node. keys nil addresses nil)
                  (Leaf. keys nil))]
       (swap! *stats update :reads inc)
       (swap! *memory assoc address node)
       node)))
  (-delete [this addresses] (throw (js/Error. "unimplemented"))))

(defn storage
  ([] (->Storage (atom {}) (atom {})))
  ([*disk] (->Storage (atom {}) *disk))
  ([*memory *disk] (->Storage *memory *disk)))

(defn roundtrip [set]
  (let [storage (storage)
        address (set/store set storage)]
    (set/restore address storage)))

(defn loaded-ratio
  ([set]
   (let [storage (.-storage set)
         address (.-address set)
         root    (.-root set)]
     (loaded-ratio (some-> storage :*memory deref) address root)))
  ([memory address node]
   (when *debug*
     (println address (contains? memory address) node (memory address)))
   (if (and address (not (contains? memory address)))
     0.0
     (let [node (or node (memory address))]
       (if (instance? Leaf node)
         1.0
         (let [len (count (.-keys node))]
           (double
             (/ (->>
                  (mapv
                    (fn [_ child-addr child]
                      (loaded-ratio memory child-addr child))
                    (range len)
                    (or (.-addresses node) (repeat len nil))
                    (or (.-children node)  (repeat len nil)))
                  (reduce + 0))
               len))))))))

(defn durable-ratio
  ([set]
   (durable-ratio (.-address set) (.-root set)))
  ([address node]
   (cond
     (some? address)       1.0
     (instance? Leaf node) 0.0
     :else
     (let [len (count (.-keys node))]
       (/ (->>
            (map
              (fn [_ child-addr child]
                (durable-ratio child-addr child))
              (range len)
              (.-addresses node)
              (.-children node))
            (reduce + 0))
         len)))))

(deftest test-lazy-remove
  "Check that invalidating middle branch does not invalidates siblings"
  #_(let [size 7000 ;; 3-4 branches
        xs   (shuffle (range size))
        set  (into (set/sorted-set* {}) xs)]
    (set/store set (storage) {:sync? true})
    (and
     (is (= 1.0 (durable-ratio set)))
     (let [set' (disj set 3500)] ;; one of the middle branches
       (is (< 0.98 (durable-ratio set'))))
     (let [set' (disj set (quot size 2))] ;; middle-ish key
       (is (< 0.98 (durable-ratio set')))))))

(deftest stresstest-stable-addresses
  #_
  (let [size      10000
        adds      (shuffle (range size))
        removes   (shuffle adds)
        *set      (atom (set/sorted-set))
        *disk     (atom {})
        storage   (storage *disk)
        invariant (fn invariant
                    ([o]
                     (invariant (.-root o) (some? (.-address o))))
                    ([o stored?]
                     (condp instance? o
                       Node
                       (let [node ^Node o
                             len (count (.-keys node))]
                         (doseq [i (range len)
                                 :let [addr   (nth (.-addresses node) i)
                                       child  (.child node storage (int i))
                                       {:keys [keys addresses]} (edn/read-string (@*disk addr))]]
                           ;; nodes inside stored? has to ALL be stored
                           (when stored?
                             (is (some? addr)))
                           (when (some? addr)
                             (is (= keys
                                   (take (.len ^ANode child) (.-_keys ^ANode child))))
                             (is (= addresses
                                   (when (instance? Branch child)
                                     (take (.len ^Branch child) (.-_addresses ^Branch child))))))
                           (invariant child (some? addr))))
                       Leaf
                       true)))]
    (testing "Persist after each"
      (dobatches [xs adds]
        (let [set' (swap! *set into xs)]
          (invariant set')
          (set/store set' storage)))
      (invariant @*set)
      (dobatches [xs removes]
        (let [set' (swap! *set #(reduce disj % xs))]
          (invariant set')
          (set/store set' storage))))

    (testing "Persist once"
      (reset! *set (into (set/sorted-set) adds))
      (set/store @*set storage)
      (dobatches [xs removes]
        (let [set' (swap! *set #(reduce disj % xs))]
          (invariant set'))))))


; (deftest test-walk
;   (let [size    1000000
;         xs      (shuffle (range size))
;         set     (into (set/sorted-set* {}) xs)
;         *stored (atom 0)]
;     (set/walk-addresses set
;       (fn [addr]
;         (is (nil? addr))))
;     (set/store set (storage))
;     (set/walk-addresses set
;       (fn [addr]
;         (is (some? addr))
;         (swap! *stored inc)))
;     (let [set'     (conj set (* 2 size))
;           *stored' (atom 0)]
;       (set/walk-addresses set'
;         (fn [addr]
;           (if (some? addr)
;             (swap! *stored' inc))))
;       (is (= (- @*stored 4) @*stored')))))


; (deftest test-lazyness
;   (let [
;         ; size       1000000
;         size       1000
;         xs         (shuffle (range size))
;         rm         (vec (repeatedly (quot size 5) #(rand-nth xs)))
;         original   (-> (reduce disj (into (set/sorted-set* {}) xs) rm)
;                      (disj (quot size 4) (quot size 2)))
;         storage    (storage)
;         _          (is (= 0 (:writes @*stats)))
;         address    (with-stats (set/store original storage))
;         _          (is (= 0 (:reads @*stats)))
;         _          (is (> (:writes @*stats) (/ size set/max-len)))
;         _(js/console.log :*memory (deref (:*memory storage)))
;         _(js/console.log address)
;         loaded     (set/restore address storage {:sync? true})
;         _          (is (= 0 (:reads @*stats)))
;         ; _          (is (= 0.0 (loaded-ratio loaded)))
;         ; _          (is (= 1.0 (durable-ratio loaded)))
;
;         ; ; touch first 100
;         ; _       (is (= (take 100 loaded) (take 100 original)))
;         ; _       (is (<= 5 (:reads @*stats) 7))
;         ; l100    (loaded-ratio loaded)
;         ; _       (is (< 0 l100 1.0))
;         ;
;         ; ; touch first 5000
;         ; _       (is (= (take 5000 loaded) (take 5000 original)))
;         ; l5000   (loaded-ratio loaded)
;         ; _       (is (< l100 l5000 1.0))
;         ;
;         ; ; touch middle
;         ; from    (- (quot size 2) (quot size 200))
;         ; to      (+ (quot size 2) (quot size 200))
;         ; _       (is (= (vec (set/slice loaded from to))
;         ;                (vec (set/slice loaded from to))))
;         ; lmiddle (loaded-ratio loaded)
;         ; _       (is (< l5000 lmiddle 1.0))
;         ;
;         ; ; touch 100 last
;         ; _       (is (= (take 100 (rseq loaded)) (take 100 (rseq original))))
;         ; lrseq   (loaded-ratio loaded)
;         ; _       (is (< lmiddle lrseq 1.0))
;         ;
;         ; ; touch 10000 last
;         ; from    (- size (quot size 100))
;         ; to      size
;         ; _       (is (= (vec (set/slice loaded from to))
;         ;                (vec (set/slice loaded from 1000000))))
;         ; ltail   (loaded-ratio loaded)
;         ; _       (is (< lrseq ltail 1.0))
;         ;
;         ; ; conj to beginning
;         ; loaded' (conj loaded -1)
;         ; _       (is (= ltail (loaded-ratio loaded')))
;         ; _       (is (< (durable-ratio loaded') 1.0))
;         ;
;         ; ; conj to middle
;         ; loaded' (conj loaded (quot size 2))
;         ; _       (is (= ltail (loaded-ratio loaded')))
;         ; _       (is (< (durable-ratio loaded') 1.0))
;         ;
;         ; ; conj to end
;         ; loaded' (conj loaded Long/MAX_VALUE)
;         ; _       (is (= ltail (loaded-ratio loaded')))
;         ; _       (is (< (durable-ratio loaded') 1.0))
;         ;
;         ; ; conj to untouched area
;         ; loaded' (conj loaded (quot size 4))
;         ; _       (is (< ltail (loaded-ratio loaded') 1.0))
;         ; _       (is (< ltail (loaded-ratio loaded) 1.0))
;         ; _       (is (< (durable-ratio loaded') 1.0))
;         ;
;         ; ; transients conj
;         ; xs      (range -10000 0)
;         ; loaded' (into loaded xs)
;         ; _       (is (every? loaded' xs))
;         ; _       (is (< ltail (loaded-ratio loaded')))
;         ; _       (is (< (durable-ratio loaded') 1.0))
;         ;
;         ; ; incremental persist
;         ; _       (with-stats
;         ;           (set/store loaded' storage))
;         ; _       (is (< (:writes @*stats) 350)) ;; ~ 10000 / 32 + 10000 / 32 / 32 + 1
;         ; _       (is (= 1.0 (durable-ratio loaded')))
;         ;
;         ; ; transient disj
;         ; xs      (take 100 loaded)
;         ; loaded' (reduce disj loaded xs)
;         ; _       (is (every? #(not (loaded' %)) xs))
;         ; _       (is (< (durable-ratio loaded') 1.0))
;         ;
;         ; ; count fetches everything
;         ; _       (is (= (count loaded) (count original)))
;         ; l0      (loaded-ratio loaded)
;         ; _       (is (= 1.0 l0))
;         ]))