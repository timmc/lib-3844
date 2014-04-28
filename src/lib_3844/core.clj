(ns lib-3844.core
  (:require [lib-3844.api :as api])
  (:import foo.bar.Baz))

(defn -main
  [& args]

  ;; Demonstrate Clojure call with nil `this` and two-arg call.
  (api/-doFoo nil 1 2)

  ;; Demonstrate Java call with instance and three-arg call.
  (let [api (foo.bar.Baz.)]
    ;; or api.doFoo(...) from Java
    (.doFoo api 1 (to-array []) 3)

    ;; Just to show that manualMethod still works
    (.manualMethod api 1 2)))
