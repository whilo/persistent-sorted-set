(ns me.tonsky.persistent-sorted-set.async-await
  "Drop-in replacements for missionary's sp and ? using cloroutine"
  (:refer-clojure :exclude [await])
  #?(:clj (:require [cloroutine.core :refer [cr]]))
  #?(:cljs (:require-macros [cloroutine.core :refer [cr]])))

;; Following the Java async/await example from cloroutine docs

;; Dynamic bindings for fiber context
(def ^:dynamic *fiber*)
(def ^:dynamic *value*)
(def ^:dynamic *error*)

;; Following the Java async/await example exactly: await only accepts Promises (like Java's CompletableFuture)
(defn await 
  "Awaits a JavaScript Promise (like Java's CompletableFuture.whenComplete)"
  [promise]
  #?(:cljs 
     (cond
       (instance? js/Promise promise)
       ;; For Promises, register fiber (like .whenComplete in Java)
       (let [current-fiber *fiber*]
         (.then promise 
                (fn [value] (current-fiber current-fiber value nil))
                (fn [error] (current-fiber current-fiber nil error)))
         ;; Don't return the Promise - let cloroutine suspend here
         nil)
       
       :else
       ;; Direct value - just return it (no async operation needed)
       promise)
     :default promise))

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
    `(let [resolve-fn# (atom nil)
           reject-fn# (atom nil)
           promise# (js/Promise. 
                      (fn [resolve# reject#]
                        (reset! resolve-fn# resolve#)
                        (reset! reject-fn# reject#)))
           cr# (cr {await thunk}
                 (try 
                   (@resolve-fn# (do ~@body))
                   (catch :default e#
                     (@reject-fn# e#))))]
       ;; Create fiber exactly like Java BiConsumer
       (binding [*fiber* (fn [f# v# e#]
                          (binding [*fiber* f#
                                    *value* v#
                                    *error* e#]
                            (cr#)))]
         (cr#))
       promise#)
    ;; Clojure compilation
    `(do ~@body)))

;; For easier migration from missionary patterns
(defmacro sp
  "Alias for async (missionary compatibility)"
  [& body]
  `(async ~@body))

;; Note: We focus on `await` and `async` as the primary API
;; These provide the core async/await functionality with proper cloroutine integration

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

;; ============================================================================
;; Compatibility Layer
;; ============================================================================

(defn task?
  "Check if a value is a Promise or task"
  [x]
  #?(:cljs (or (instance? js/Promise x) (fn? x))
     :default (fn? x)))

(defn run-sync
  "Run a Promise or task synchronously (for testing) - returns Promise in ClojureScript"
  [promise-or-task]
  #?(:cljs
     (if (instance? js/Promise promise-or-task)
       ;; Just return the Promise as-is - caller can await it
       promise-or-task
       (if (fn? promise-or-task)
         (promise-or-task)
         promise-or-task))
     :default
     (if (fn? promise-or-task)
       (promise-or-task)
       promise-or-task)))

(defn run-async
  "Run a Promise or task with callbacks"
  [promise-or-task on-success on-error]
  #?(:cljs
     (if (instance? js/Promise promise-or-task)
       (.then promise-or-task on-success on-error)
       (if (fn? promise-or-task)
         (promise-or-task on-success on-error)
         (on-success promise-or-task)))
     :default
     (if (fn? promise-or-task)
       (promise-or-task on-success on-error)
       (on-success promise-or-task))))