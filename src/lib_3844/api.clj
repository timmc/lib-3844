(ns lib-3844.api)

;; The macro, which would be off in some utility library

(defmacro gen-class-alias
  "Provide a map of gen-class options followed by alternating method
and function symbols (without method prefixes) for aliasing and
adding to the gen-class."
  [gen-class-options & mf-paired]
  (when-not (every? symbol mf-paired)
    (throw (IllegalArgumentException. "Function and method names must be symbols.")))
  (when-not (even? (count mf-paired))
    (throw (IllegalArgumentException. "Must have even number of method and function names.")))
  (let [get-tag #(or (:tag (meta %)) 'java.lang.Object)
        prefix (:prefix gen-class-options)
        mappings (for [[m f] (partition 2 mf-paired)]
                   (let [mp (symbol (str prefix (name m)))
                         f-meta (meta (resolve f))
                         doc-add (format "(Method alias of `%s` function accepting nil for `this`.)"
                                         (name f))
                         doc (if-let [f-doc (:doc f-meta)]
                               (str f-doc "\n" doc-add)
                               doc-add)]
                     (when (:macro f-meta)
                       (throw (IllegalArgumentException. (str "Cannot alias a macro: " f))))
                     {:defn `(defn ~mp
                               ~doc
                               ~@(for [arglist (:arglists f-meta)]
                                   `([this# ~@arglist] (~f ~@arglist))))
                      :method-specs (for [arglist (:arglists f-meta)]
                                      (let [ret-type (get-tag arglist)
                                            params (vec (map get-tag arglist))]
                                        [m params ret-type]))}))]
    `(~'do
       ;; Emit prefixed methods
       ~@(map :defn mappings)
       ;; Emit a gen-class with the methods added
       ~(let [more-methods (mapcat :method-specs mappings)
              options (apply update-in gen-class-options [:methods] (fnil conj []) more-methods)]
          `(gen-class ~@(mapcat identity options))))))

;; Sample fns and methods

(defn do-foo ;; we'd like to alias this as a method
  ([a b]
     (println "arity 2 called"))
  (^long [a ^"[Ljava.lang.Object;" b c] ;; demonstrate type annotations
         (println "arity 3 called")
         12345 ;; gotta return something for that ^long
         ))

(defn -manualMethod ;; something we'll export manually
  [_ a b]
  (println "Called manualMethod arity 2 fn with" a b))

;; Exporting the methods

(gen-class-alias
 {:prefix "-"
  :name foo.bar.Baz
  :methods [[manualMethod [Object Object] Object]]}
 doFoo do-foo)

