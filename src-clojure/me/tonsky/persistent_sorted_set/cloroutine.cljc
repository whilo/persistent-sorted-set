(ns me.tonsky.persistent-sorted-set.cloroutine
  "Drop-in replacements for missionary's sp and ? using cloroutine"
  (:refer-clojure :exclude [await])
  #?(:clj (:require [cloroutine.core :refer [cr]]
                    [cloroutine.impl :as impl]
                    [clojure.walk]))
  #?(:cljs (:require [cloroutine.impl :as impl])
     :cljs (:require-macros [cloroutine.core :refer [cr]])))

;; Following the Java async/await example from cloroutine docs

;; Dynamic bindings for fiber context
(def ^:dynamic *fiber*)
(def ^:dynamic *value*)
(def ^:dynamic *error*)

;; Internal await that always suspends - only used when we know we have a Promise
(defn await-internal
  "Internal await that always suspends via cloroutine. Only call with actual Promises."
  [promise]
  #?(:cljs
     (let [current-fiber *fiber*]
       ;; Register fiber with Promise
       (.then promise
              (fn [value] (current-fiber current-fiber value nil))
              (fn [error] (current-fiber current-fiber nil error)))
       ;; Always return nil to suspend the coroutine
       nil)
     :default promise))

;; Smart await macro with conditional fast path
(defmacro await
  "Smart await that checks if value is a Promise at runtime.
   If it's a Promise, uses cloroutine suspension.
   If it's a synchronous value, returns directly with zero overhead."
  [value-expr]
  (if (:js-globals &env) ; ClojureScript
    `(let [val# ~value-expr]
       (if (instance? js/Promise val#)
         (await-internal val#)    ; Slow path: cloroutine suspension
         val#))                   ; Fast path: direct return, no cloroutine
    ;; Clojure - just return the value
    value-expr))

;; Helper function for converting callback-based operations to Promises
(defn promisify
  "Converts a callback-based async function to a Promise"
  [callback-fn]
  #?(:cljs
     (js/Promise.
      (fn [resolve reject]
        (callback-fn resolve reject)))
     :default (throw (ex-info "promisify only supported in ClojureScript" {}))))

(defn thunk
  "Resume function - returns the value or throws the error"
  []
  (if-some [e *error*]
    (throw e)
    *value*))

;; Direct translation of the Java CompletableFuture example to JavaScript Promise
(defmacro async [& body]
  (if (:ns &env) ;; ClojureScript compilation
    (let [expanded-body (clojure.walk/macroexpand-all `(do ~@body))]
      `(let [resolve-fn# (atom nil)
             reject-fn# (atom nil)
             promise# (js/Promise.
                        (fn [resolve# reject#]
                          (reset! resolve-fn# resolve#)
                          (reset! reject-fn# reject#)))
             cr# (cr {await-internal thunk}
                   (try
                     (@resolve-fn# ~expanded-body)
                     (catch :default e#
                       (@reject-fn# e#))))]
         ;; Create fiber exactly like Java BiConsumer
         (binding [*fiber* (fn [f# v# e#]
                            (binding [*fiber* f#
                                      *value* v#
                                      *error* e#]
                              (cr#)))]
           (cr#))
         promise#))
    ;; Clojure compilation
    `(do ~@body)))

;; For easier migration from missionary patterns
(defmacro sp
  "Alias for async (missionary compatibility)"
  [& body]
  `(async ~@body))

;; Note: We focus on `await` and `async` as the primary API
;; These provide the core async/await functionality with proper cloroutine integration

;; Make Promise callable like missionary's process for compatibility
#?(:cljs
   (extend-type js/Promise
     IFn
     (-invoke
       ;; Called with no args - cancel (returns nil like missionary)
       ([this] nil)
       ;; Called with success callback only
       ([this success]
        (.then this success))
       ;; Called with success and failure callbacks
       ([this success failure]
        (-> this
            (.then success)
            (.catch failure))))))

;; Helper for creating Promise-returning operations that work with await
(defn to-promise
  "Converts a value or async operation to a Promise.
   - If already a Promise, returns it as-is
   - If a synchronous value, wraps in Promise.resolve for fast path
   This ensures await always gets a Promise but sync values settle immediately."
  [value-or-promise]
  #?(:cljs
     (if (instance? js/Promise value-or-promise)
       value-or-promise
       (js/Promise.resolve value-or-promise))
     :default value-or-promise))

;; Helper function for storage operations
(defn make-async-storage-op
  "Creates a Promise-based async operation for storage functions"
  [storage operation & args]
  #?(:cljs
     (if (and storage (.-async storage))
       ;; If storage supports async operations, use them
       (let [async-op (get (.-async storage) (str (.-name operation)))]
         (if async-op
           (promisify (fn [resolve reject]
                        (try
                          (async-op storage (fn [result error]
                                              (if error
                                                (reject error)
                                                (resolve result)))
                                  (first args) (second args) (nth args 2 nil))
                          (catch :default e
                            (reject e)))))
           ;; Fallback to sync operation wrapped in Promise
           (js/Promise.resolve (apply operation storage args))))
       ;; No async storage, use sync operation wrapped in Promise
       (js/Promise.resolve (apply operation storage args)))
     :default
     ;; Clojure - just call synchronously
     (apply operation storage args)))
