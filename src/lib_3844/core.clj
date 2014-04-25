(ns lib-3844.core
  (:require [lib-3844.api :as api])
  (:import foo.bar.Baz))

(defn -main
  [& args]
  ;; Demonstrate Clojure call with nil `this`
  (api/-doFoo nil 1 2)
  ;; Demonstrate Java call with instance
  (.doFoo (foo.bar.Baz.) 1 (to-array []) 3))
