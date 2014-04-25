(ns lib-3844.api)

;; Sample fns and methods

(defn do-foo
  ([a b]
     (println "binary called"))
  ([a b c]
     (println "ternary called")))

(defn -manualFoo
  [a b]
  (do-foo a b))


;; The macro


;; First attempt, doesn't declare methods as required. Probably would
;; need an ns block containing (:gen-class :name foo.bar.Baz
;; :implements foo.bar.IBaz) where IBaz specifies the methods.
#_
(defmacro class-alias
  [prefix & fm-paired]
  (when-not (symbol? prefix)
    (throw (IllegalArgumentException. "Prefix must be a symbol.")))
  (when-not (every? symbol fm-paired)
    (throw (IllegalArgumentException. "Function and method names must be symbols.")))
  (when-not (even? (count fm-paired))
    (throw (IllegalArgumentException. "Must have even number of function and method names.")))
  (cons 'do
        (for [[f m] (partition 2 fm-paired)]
          (let [m (symbol (str (name prefix) (name m)))
                f-meta (meta (resolve f))
                doc-add (format "(Method alias of `%s` function accepting nil for `this`.)" (name f))
                doc (if-let [f-doc (:doc f-meta)]
                      (str f-doc "\n" doc-add)
                      doc-add)]
            (when (:macro f-meta)
              (throw (IllegalArgumentException. (str "Cannot alias a macro: " f))))
            `(defn ~m
               ~doc
               ~@(for [arglist (:arglists f-meta)]
                   `([this# ~@arglist] (~f ~@arglist))))))))

;; Second attempt, carrying its own gen-class.
(defmacro gen-class-alias
  [gen-class-options & fm-paired]
  (when-not (every? symbol fm-paired)
    (throw (IllegalArgumentException. "Function and method names must be symbols.")))
  (when-not (even? (count fm-paired))
    (throw (IllegalArgumentException. "Must have even number of function and method names.")))
  (let [prefix (:prefix gen-class-options)
        mappings (for [[f m] (partition 2 fm-paired)]
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
                      :method-specs (let [ret-type (or (:tag f-meta) 'java.lang.Object)]
                                      (for [arglist (:arglists f-meta)]
                                        (let [params (vec (map #(or (:tag (meta %)) 'java.lang.Object)
                                                               arglist))]
                                          [m params ret-type])))}))]
    `(~'do
       ;; Emit prefixed methods
       ~@(map :defn mappings)
       ;; Emit a gen-class with the methods added
       ~(let [more-methods (mapcat :method-specs mappings)
              options (apply update-in gen-class-options [:methods] (fnil conj []) more-methods)]
          `(gen-class ~@(mapcat identity options))))))

(gen-class-alias
 {:prefix "-"
  :name foo.bar.Baz
  :methods [[manualFoo [Object Object] Object]]}
 do-foo doFoo)

