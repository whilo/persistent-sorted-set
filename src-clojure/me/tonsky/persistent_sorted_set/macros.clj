(ns me.tonsky.persistent-sorted-set.macros
  (:refer-clojure :exclude [async await])
  (:require [clojure.walk]
            [await-cps :refer [await async]]))

(def async->sync '{async do, await do})

(defmacro async+sync [sync? async-code]
  (let [sync-code (clojure.walk/postwalk
                    (fn [n]
                      (if-not (meta n)
                        (async->sync n n)
                        (with-meta (async->sync n n)
                                   (update (meta n) :tag (fn [t] (async->sync t t))))))
                    async-code)]
    `(if ~sync?
       ~sync-code
       ~async-code)))
