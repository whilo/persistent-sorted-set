# Test Environment Setup for Async Storage

## Current Environment

The project uses:
- **shadow-cljs** for ClojureScript builds
- **deps.edn** for Clojure dependencies
- Test target configured as `:node-test` in shadow-cljs

## Dependencies Needed

For async storage testing, we need to add:

1. **core.async** - For async operations
2. **konserve** (optional) - For testing with a real async storage backend
3. **superv.async** - For the async+sync macro and go-try-

## Setup Steps

### 1. Update deps.edn

Add a new alias for async testing:

```clojure
:async-test
{:extra-paths ["test-clojure"]
 :extra-deps
 {org.clojure/core.async {:mvn/version "1.6.681"}
  io.replikativ/konserve {:mvn/version "0.6.0-alpha3"}
  io.replikativ/superv.async {:mvn/version "0.2.11"}}
 :main-opts ["-m" "cognitect.test-runner"]}
```

### 2. Create Test File Structure

```
test-clojure/
└── me/
    └── tonsky/
        └── persistent_sorted_set/
            └── test/
                ├── async_storage.cljs   # New async storage tests
                └── async_utils.cljc     # Shared async test utilities
```

### 3. Basic Test Storage Implementation

Create a simple async test storage that simulates network delays:

```clojure
(ns me.tonsky.persistent-sorted-set.test.async-utils
  (:require
   [clojure.core.async :as async :refer [go <!]]
   [superv.async :refer [go-try- <?-]]))

(defrecord TestAsyncStorage [*store delay-ms]
  IStorage
  (restore [_ address]
    (go
      (<! (async/timeout delay-ms))
      (get @*store address)))
  
  (store [_ node existing-address]
    (go
      (<! (async/timeout delay-ms))
      (let [addr (or existing-address (random-uuid))
            data {:keys (.-keys node)
                  :addresses (when (instance? Node node)
                              (.-_addresses node))}]
        (swap! *store assoc addr data)
        addr)))
  
  (accessed [_ address]
    (go nil))
  
  (delete [_ addresses]
    (go
      (<! (async/timeout delay-ms))
      (swap! *store #(apply dissoc % addresses)))))
```

### 4. Running Tests

For ClojureScript tests:
```bash
# Install dependencies
yarn install

# Run async tests
yarn shadow-cljs compile test

# Or watch mode
yarn shadow-cljs watch test
```

For Clojure tests (if needed):
```bash
clj -M:async-test
```

## Test Strategy

1. **Unit Tests** - Test each modified function with both sync and async storage
2. **Integration Tests** - Test full operations (conj, disj, etc.) with async storage
3. **Compatibility Tests** - Ensure sync storage still works exactly as before
4. **Performance Tests** - Compare sync vs async performance

## Example Test Case

```clojure
(deftest async-conj-test
  (async done
    (go-try-
      (let [storage (->TestAsyncStorage (atom {}) 10)
            set0 (sorted-set* {:storage storage})
            set1 (<?- (conj set0 1 compare {:sync? false}))
            set2 (<?- (conj set1 2 compare {:sync? false}))
            set3 (<?- (conj set2 3 compare {:sync? false}))]
        (is (= 3 (count set3)))
        (is (= [1 2 3] (vec set3)))
        (done)))))
```

## Next Steps

1. Set up the test environment with these dependencies
2. Create basic async storage implementation
3. Write tests for core functionality
4. Implement the async storage support based on the design document