(ns me.tonsky.persistent-sorted-set.macros
  (:require [clojure.walk]))

(defmacro async+sync
  [sync? async->sync async-code]
  (let [async->sync (if (symbol? async->sync)
                      (or (resolve async->sync)
                          (when-let [_ns (or (get-in &env [:ns :use-macros async->sync])
                                             (get-in &env [:ns :uses async->sync]))]
                            (resolve (symbol (str _ns) (str async->sync)))))
                      async->sync)]
    (assert (some? async->sync))
    `(if ~sync?
       ~(clojure.walk/postwalk (fn [n]
                                 (if-not (meta n)
                                   (async->sync n n) ;; primitives have no metadata
                                   (with-meta (async->sync n n)
                                     (update (meta n) :tag (fn [t] (async->sync t t))))))
                               async-code)
       ~async-code)))

(def ^:dynamic *default-sync-translation*
  '{go-try try
    <? do
    go-try- try
    <!- do
    <?- do
    go-locked locked})

(defmacro show-expansion
  [sync? async->sync async-code]
  (let [expanded (if sync?
                   (clojure.walk/postwalk (fn [n]
                                           (if-not (meta n)
                                             (async->sync n n)
                                             (with-meta (async->sync n n)
                                               (update (meta n) :tag (fn [t] (async->sync t t))))))
                                         async-code)
                   async-code)]
    (println "MACRO EXPANSION:")
    (println "Original:" async-code)
    (println "Expanded:" expanded)
    (println "Expanded type:" (type expanded))
    ;; Return the expansion
    expanded))