(ns money.sql
  (:refer-clojure :exclude [alias format interpose])
  (:require
   [clojure.string :as str]
   [clojure.spec.alpha :as s]
   [methodical.core :as m]
   [money.sql.clause-order :as clause-order]
   [money.sql.util :as u]))

;; define the default clause orders.
(doseq [[x y] [[:alter-table :add-column]
               [:add-column :drop-column]
               [:drop-column :modify-column]
               [:modify-column :rename-column]
               [:rename-column :add-index]
               [:add-index :drop-index]
               [:drop-index :rename-table]
               [:rename-table :create-table]
               [:create-table :create-table-as]
               [:create-table-as :with-columns]
               [:with-columns :create-view]
               [:create-view :create-materialized-view]
               [:create-materialized-view :create-extension]
               [:create-extension :drop-table]
               [:drop-table :drop-view]
               [:drop-view :drop-materialized-view]
               [:drop-materialized-view :drop-extension]
               [:drop-extension :refresh-materialized-view]
               [:refresh-materialized-view :raw]
               [:raw :nest]
               [:nest :with]
               [:with :with-recursive]
               [:with-recursive :intersect]
               [:intersect :union]
               [:union :union-all]
               [:union-all :except]
               [:except :except-all]
               [:except-all :table]
               [:table :select]
               [:select :select-distinct]
               [:select-distinct :select-distinct-on]
               [:select-distinct-on :select-top]
               [:select-top :select-distinct-top]
               [:select-distinct-top :into]
               [:into :bulk-collect-into]
               [:bulk-collect-into :insert-into]
               [:insert-into :update]
               [:update :delete]
               [:delete :delete-from]
               [:delete-from :truncate]
               [:truncate :columns]
               [:columns :set]
               [:set :from]
               [:from :using]
               [:using :join-by]
               [:join-by :join]
               [:join :left-join]
               [:left-join :right-join]
               [:right-join :inner-join]
               [:inner-join :outer-join]
               [:outer-join :full-join]
               [:full-join :cross-join]
               [:cross-join :where]
               [:where :group-by]
               [:group-by :having]
               [:having :window]
               [:window :partition-by]
               [:partition-by :order-by]
               [:order-by :limit]
               [:limit :offset]
               [:offset :fetch]
               [:fetch :for]
               [:for :lock]
               [:lock :values]
               [:values :on-conflict]
               [:on-conflict :on-constraint]
               [:on-constraint :do-nothing]
               [:do-nothing :do-update-set]
               [:do-update-set :on-duplicate-key-update]
               [:on-duplicate-key-update :returning]
               [:returning :with-data]]]
  (clause-order/define-before! x y))

;; TODO -- let's make this an atom when https://github.com/camsaul/methodical/issues/85 is fixed
(defonce hierarchy (make-hierarchy))

(defn derive! [tag parent]
  (alter-var-root #'hierarchy derive tag parent))

;; default dialects
(derive! :oracle :ansi)

(m/defmulti to-sql
  "Return a vector of `[sql & args]`."
  {:arglists '([form options])}
  (fn [form {:keys [dialect], :as _options}]
    [(if (and (sequential? form)
              (keyword? (first form)))
       (first form)
       (type form))
     (keyword dialect)])
  :hierarchy #'hierarchy)

(m/defmethod to-sql :around :default
  [x options]
  (try
    (u/debug (pr-str (list 'to-sql x options))
      (let [result (next-method x options)]
        (assert (or (nil? result)
                    (and (sequential? result)
                         (string? (first result))))
                (str "Expected sql-args, got " (pr-str result)))
        result))
    (catch Throwable e
      (throw (ex-info (clojure.core/format "Error compiling %s to SQL: %s"
                                           (pr-str x)
                                           (or (ex-message e) e))
                      {:form x, :options options}
                      #_e)))))

;;;; Type definitions

(m/defmethod to-sql [:default :default]
  [x {:keys [inline], :as options}]
  (cond
    (= x [:default]) ["DEFAULT"]
    (sequential? x)  (to-sql (into [::fn-call] x) options)
    inline           [(str x)]
    :else            ["?" x]))

(m/defmethod to-sql [String :default]
  [s {:keys [inline], :as options}]
  (if inline
    ;; TODO -- need to escape the string.
    [(clojure.core/format "'%s'" (str/replace s #"'" "''"))]
    (next-method s options)))

(m/defmethod to-sql [Boolean :default]
  [bool _options]
  [(if bool
     "TRUE"
     "FALSE")])

(m/defmethod to-sql [nil :default]
  [_ _options]
  ["NULL"])

(m/defmethod to-sql [:inline :default]
  [[_ value] options]
  (to-sql value (assoc options :inline true)))

(defn inline [x]
  [:inline x])

;; TODO -- or should this be `:raw`?
(m/defmethod to-sql [::raw :default]
  [[_ & sql-args] _options]
  (vec sql-args))

;; (m/defmethod to-sql [::pretty-newline :default]
;;   [_ {:keys [pretty pretty-indent]}]
;;   (if-not pretty
;;     " "
;;     (str
;;      "\n"
;;      (when pretty-indent
;;        (str/join (repeat pretty-indent "  "))))))

;; (defn pretty-newline []
;;   [::pretty-newline])

;; (defn pretty-indented [x]
;;   [::pretty-indent x])

;; (m/defmethod to-sql [::pretty-indent :default]
;;   [[_ x] options]
;;   (to-sql x (update options :pretty-indent (fnil inc 0))))

;;;; Misc utils

(defn raw [sql & args]
  (into [::raw sql] args))

(defn ->snake-case [s {:keys [quoted-snake], :as _options}]
  (if quoted-snake
    s
    (str/replace s #"-" "_")))

(m/defmethod to-sql [::identifier-part :default]
  [[_ part, :as x] {:keys [quoted dialect], :as options}]
  ;; if `quoted` is specified but a dialect was not, call the ansi quote method (defined in [[money.sql.dialects]])
  (if (and quoted (not dialect))
    (to-sql x (merge {:dialect :ansi} options))
    [(->snake-case (name part) options)]))

(defn quote-identifier-part [part {:keys [quoted], :or {quoted true}, :as options} quote-fn]
  [(cond-> (->snake-case part options)
     (and quoted (not= part "*")) quote-fn)])

(defn mysql-quote [s]
  (str "`" (str/replace s #"`" "``") "`"))

(m/defmethod to-sql [::identifier-part :mysql]
  [[_ part] options]
  (quote-identifier-part part options mysql-quote))

(defn ansi-quote [s]
  (str "\"" (str/replace s #"\"" "\"\"") "\""))

(m/defmethod to-sql [::identifier-part :ansi]
  [[_ part] options]
  (quote-identifier-part part options ansi-quote))

(defn sqlserver-quote [s]
  (str "[" (str/replace s #"\]" "]]") "]"))

(m/defmethod to-sql [::identifier-part :sqlserver]
  [[_ part] options]
  (quote-identifier-part part options sqlserver-quote))

(defn combine-sql-args
  ([xs]
   (combine-sql-args xs identity))
  ([xs xform]
   {:pre [(fn? xform)]}
   (transduce
    ((remove empty?) xform)
    (completing
     (fn [[sql & args] [more-sql & more-args]]
       (into [(str sql more-sql)]
             cat
             [args more-args])))
    [""]
    xs)))

(defn interpose-sql-args [separator xs]
  (combine-sql-args xs
                    (comp
                     (remove empty?)
                     (clojure.core/interpose [separator]))))

(m/defmethod to-sql [::interpose :default]
  [[_ separator & xs] options]
  (interpose-sql-args separator (for [x xs]
                                  (to-sql x options))))

(defn interpose [separator xs]
  (into [::interpose separator] xs))

(m/defmethod to-sql [::identifier :default]
  [[_ & parts] options]
  (to-sql (interpose "." (for [part parts]
                           [::identifier-part part]))
          options))

(defn preserve-kebabs [x]
  [::preserve-kebabs x])

(m/defmethod to-sql [::preserve-kebabs :default]
  [[_ x] options]
  (to-sql x (assoc options :quoted-snake true)))

(m/defmulti parse-keyword
  {:arglists '([k options])}
  (fn [k {:keys [dialect]}]
    [(second (str k))
     (keyword dialect)]))

(m/defmethod parse-keyword :default
  [k _options]
  (into [::identifier] (-> (name k)
                           (str/split  #"\."))))

(m/defmethod parse-keyword [\% :default]
  [k _options]
  (let [k                (keyword (.substring (str k) 2))
        [fn-name & args] (str/split (name k) #"\.")]
    (into [(keyword (namespace k) fn-name)] (map keyword) args)))

(m/defmethod parse-keyword [\' :default]
  [k options]
  (preserve-kebabs (parse-keyword (keyword (.substring (str k) 2)) options)))

(m/defmethod parse-keyword [:default \?]
  [k _options]
  (let [k (keyword (.substring (str k) 2))]
    [::parameter k]))

(m/defmethod to-sql [::parameter :default]
  [[_ k] {:keys [params], :as options}]
  (to-sql (get params k) options))

(m/defmethod to-sql [clojure.lang.Keyword :default]
  [k options]
  (to-sql (parse-keyword k options) options))

(m/defmethod to-sql [::commas :default]
  [[_ & xs] options]
  (to-sql (interpose ", " xs)
          options))

(defn commas [xs]
  (into [::commas] xs))

(m/defmethod to-sql [::parens :default]
  [[_ x] options]
  (combine-sql-args [["("]
                     (to-sql x options)
                     [")"]]))

(defn parens [x]
  [::parens x])

;; (m/defmethod to-sql [::parens-body :default]
;;   [[_ x] {:keys [pretty], :as options}]
;;   (combine-sql-args [["(" (if pretty "\n" "")]
;;                      (to-sql (pretty-indented x) options)
;;                      [")"]]))

;; (defn parens-body [x]
;;   [::parens-body x])

(m/defmethod to-sql [::fn-call :default]
  [[_ fn-name & args] options]
  (assert ((some-fn keyword? string?) fn-name) (str "Not a valid function name: " (pr-str fn-name)))
  (combine-sql-args [[(name fn-name)]
                     (to-sql (parens (commas args)) options)]))

(defn call [& args]
  (into [::fn-call] args))

(m/defmethod to-sql [::as :default]
  [[_ identifier alias] options]
  (to-sql (interpose " AS "
                     [identifier
                      ;; TODO -- this needs to be configurable. Or use [[keyword-identifier]] with a no split option
                      [::identifier (str/replace (name alias) #"-" "_")]])
          options))

(defn identifier [x]
  (cond
    (and (sequential? x)
         (> (count x) 1))
    (into [::as] x)

    (sequential? x)
    (first x)

    :else x))

(m/defmethod to-sql [::identifier-list :default]
  [[_ & xs] options]
  (to-sql (commas (map identifier xs)) options))

(defn identifier-list [xs]
  (into [::identifier-list] xs))

;;;; Misc built-in functions

(m/defmethod to-sql [:interval :default]
  [[_ n unit] options]
  (interpose-sql-args " " [["interval"]
                           (to-sql n options)
                           [(name unit)]]))

;;;; where clauses

(m/defmethod to-sql [:= :default]
  [[_ x y] options]
  (to-sql (if (some nil? [x y])
            [:is x y]
            (interpose " = " [x y]))
          options))

(m/defmethod to-sql [:is :default]
  [[_ x y] options]
  (to-sql (interpose " IS " [x y]) options))

(m/defmethod to-sql [:is-not :default]
  [[_ x y] options]
  (to-sql (interpose " IS NOT " [x y]) options))

(m/defmethod to-sql [:<> :default]
  [[_ x y] options]
  (to-sql (if (some nil? [x y])
            [:is-not x y]
            (interpose " <> " [x y]))
          options))

(m/defmethod to-sql [:!= :default]
  [[_ x y] options]
  (to-sql [:<> x y] options))

(m/defmethod to-sql [:not= :default]
  [[_ x y] options]
  (to-sql [:<> x y] options))

(m/defmethod to-sql [:in :default]
  [[_ x vs] options]
  #_(assert (sequential? vs))
  (to-sql (interpose " IN " [x (parens (commas vs))]) options))

(m/defmethod to-sql [:|| :default]
  [[_ & args] options]
  (to-sql (interpose " || " args) options))

;;;; math clauses

(defn math-operator [separator xs]
  (parens (interpose separator xs)))

(m/defmethod to-sql [:+ :default]
  [[_ & xs] options]
  (to-sql (math-operator " + " xs) options))

(m/defmethod to-sql [:- :default]
  [[_ & xs] options]
  (to-sql (math-operator " - " xs) options))

(m/defmethod to-sql [:< :default]
  [[_ & xs] options]
  (to-sql (math-operator " < " xs) options))

(m/defmethod to-sql [:<= :default]
  [[_ & xs] options]
  (to-sql (math-operator " <= " xs) options))

(m/defmethod to-sql [:> :default]
  [[_ & xs] options]
  (to-sql (math-operator " > " xs) options))

(m/defmethod to-sql [:>= :default]
  [[_ & xs] options]
  (to-sql (math-operator " >= " xs) options))

;;;; compound where clauses

;; TODO -- we should simplify compound filter clauses, perhaps using the MBQL utils
(m/defmethod to-sql [:and :default]
  [[_ & subclauses] options]
  (to-sql (if (= (count subclauses) 1)
            (first subclauses)
            (interpose " AND " (for [subclause subclauses]
                                 (parens subclause))))
          options))

;; convenience for using a map inside a WHERE clause or similar.
(m/defmethod to-sql [::map= :default]
  [[_ m] options]
  (to-sql (into [:and]
                (map (fn [[k v]]
                       [:= k v]))
                m)
          options))

(defn map= [m]
  [::map= m])

;;;; top-level clauses/query compilation

(m/defmethod to-sql [clojure.lang.IPersistentMap :default]
  [query {:keys [subquery? pretty #_pretty-indent], :as options}]
  (let [clause-keys (clause-order/sort-clause-keys (keys query))]
    (to-sql (cond-> (interpose (if pretty "\n" " ")
                                (for [clause-key clause-keys]
                                  [clause-key (get query clause-key)]))
              subquery? parens)
            (assoc options :query query))))

;;;; SELECT

(defn identifier-or-identifier-list
  "Like [[identifier-list]] but handles a single identifier e.g. for a `FROM` clause."
  [args]
  (identifier-list
   (if (sequential? args)
     args
     [args])))

(m/defmethod to-sql [:select :default]
  [[_ args] options]
  (combine-sql-args [["SELECT "]
                     (to-sql (identifier-or-identifier-list args) options)]))

;;;; from

(m/defmethod to-sql [:from :default]
  [[_ args] options]
  (combine-sql-args [["FROM "]
                     (to-sql (identifier-or-identifier-list args) (assoc options :subquery? true))]))

;;;; JOINS

(defn join [join-type [table condition :as _join-spec]]
  [::join join-type table condition])

(m/defmethod to-sql [::join :default]
  [[_ join-type table condition] options]
  (combine-sql-args
   [[(str join-type \space)]
    (to-sql (identifier table) options)
    (when condition
      (combine-sql-args
       [[" ON "]
        (to-sql condition options)]))]))

(derive! :join :inner-join)

(m/defmethod to-sql [:left-join :default]
  [[_ join-spec] options]
  (to-sql (join "LEFT JOIN" join-spec) options))

(m/defmethod to-sql [:right-join :default]
  [[_ join-spec] options]
  (to-sql (join "RIGHT JOIN" join-spec) options))

(m/defmethod to-sql [:inner-join :default]
  [[_ join-spec] options]
  (to-sql (join "INNER JOIN" join-spec) options))

(m/defmethod to-sql [:outer-join :default]
  [[_ join-spec] options]
  (to-sql (join "OUTER JOIN" join-spec) options))

(m/defmethod to-sql [:full-join :default]
  [[_ join-spec] options]
  (to-sql (join "FULL JOIN" join-spec) options))

(m/defmethod to-sql [:cross-join :default]
  [[_ join-spec] options]
  (to-sql (join "CROSS JOIN" join-spec) options))

;;;; where

(m/defmethod to-sql [:where :default]
  [[_ args] options]
  (let [args (if (and (map? args)
                              (not (record? args)))
                       [::map= args]
                       args)]
    (combine-sql-args [["WHERE "]
                       (to-sql args options)])))

;;;; group by

(m/defmethod to-sql [:group-by :default]
  [[_ args] options]
  (combine-sql-args [["GROUP BY "]
                     (to-sql (commas args) options)]))

;;;; order by

(m/defmethod to-sql [:order-by-subclause :default]
  [[_ x direction] options]
  (combine-sql-args [(to-sql x options)
                     [(case direction
                        :asc  " ASC"
                        :desc " DESC")]]))

(defn order-by-subclause [subclause]
  (if (and (sequential? subclause)
           (#{:asc :desc} (second subclause)))
    [:order-by-subclause (first subclause) (second subclause)]
    [:order-by-subclause subclause :asc]))

(m/defmethod to-sql [:order-by :default]
  [[_ subclauses] options]
  (combine-sql-args [["ORDER BY "]
                     (to-sql (commas (map order-by-subclause subclauses)) options)]))

;;;; LIMIT

(defn assert-integer [k n]
  (assert (integer? n) (clojure.core/format "k should be an integer, got ^%s ^s" k (pr-str (type n)) (pr-str n))))

(m/defmethod to-sql [:limit :default]
  [[_ n] options]
  (when n
    (assert-integer :limit n)
    (combine-sql-args [["LIMIT "]
                       (to-sql n options)])))

;;;; OFFSET

(m/defmethod to-sql [:offset :default]
  [[_ n] {{:keys [fetch]} :query, :as options}]
  ;; TODO -- should we ignore offset of zero?
  (when n
    (assert-integer :offset n)
    (combine-sql-args [["OFFSET "]
                       (to-sql n options)
                       (when (integer? fetch)
                         (if (= n 1)
                           [" ROW"]
                           [" ROWS"]))])))

(m/defmethod to-sql [:offset :sqlserver]
  [[_ n] options]
  (when n
    (assert-integer :offset n)
    (combine-sql-args [["OFFSET "]
                       (to-sql n options)
                       [" ROWS"]])))

;;;; fetch

(m/defmethod to-sql [:fetch :default]
  [[_ n] {{:keys [offset]} :query, :as options}]
  (when n
    (assert-integer :fetch n)
    (combine-sql-args [(if (integer? offset)
                         ["FETCH NEXT "]
                         ["FETCH FIRST "])
                       (to-sql n options)
                       (if (= n 1)
                         [" ROW ONLY"]
                         [" ROWS ONLY"])])))

;;;; UNION

(m/defmethod to-sql [:union :default]
  [[_ subqueries] {:keys [pretty], :as options}]
  (to-sql (interpose (str (if pretty "\n" " ")
                          "UNION"
                          (if pretty "\n" " "))
                     subqueries)
          options))

;;;; TRUNCATE

(m/defmethod to-sql [:truncate :default]
  [[_ table] options]
  (combine-sql-args [["TRUNCATE "] (to-sql (identifier table) options)]))

;;;; INSERT INTO

(m/defmethod to-sql [:insert-into :default]
  [[_ table] options]
  (combine-sql-args [["INSERT INTO "] (to-sql (identifier table) options)]))

(m/defmethod to-sql [:columns :default]
  [[_ columns] options]
  (to-sql (parens (identifier-list columns)) options))

;;;; values

(m/defmethod to-sql [:values :default]
  [[_ rows] options]
  (combine-sql-args [["VALUES "]
                     (to-sql (commas (for [row rows]
                                       (parens (commas row))))
                             options)]))

;;; ON CONFLICT, ON CONSTRAINT, DO NOTHING

(m/defmethod to-sql [:on-conflict :default]
  [[_ x] options]
  (if (= x [])
    ["ON CONFLICT"]
    (combine-sql-args [["ON CONFLICT "]
                       (to-sql (if (map? x)
                                 x
                                 (parens (identifier-or-identifier-list x)))
                               options)])))

(m/defmethod to-sql [:on-constraint :default]
  [[_ x] options]
  (combine-sql-args [["ON CONSTRAINT "]
                     (to-sql x options)]))


(m/defmethod to-sql [:do-nothing :default]
  [[_ do-nothing?] _]
  (when do-nothing?
    ["DO NOTHING"]))

;;; DO UPDATE SET

(m/defmethod to-sql [:do-update-set :default]
  [[_ m] options]
  (combine-sql-args [["DO UPDATE SET "]
                     (to-sql (if (map? m)
                               (map= m)
                               m)
                             options)]))

;;;; Locking SELECT -- FOR UPDATE, FOR SHARE, etc.

(m/defmethod to-sql [:for :default]
  [[_ what] options]
  (combine-sql-args [["FOR "]
                     (to-sql (let [what (if (sequential? what)
                                          what
                                          [what])]
                               (if (and (keyword? (first what))
                                        (namespace (first what)))
                                 what
                                 (into [(keyword "money.sql.for" (name (first what)))] (rest what))))
                             options)]))

(def update-options
  (atom {:wait          "WAIT"
         :nowait        "NOWAIT"
         :skip-locked   "SKIP LOCKED"
         :no-key-update "NO KEY UPDATE"}))

(s/def ::for-update-args
  (letfn [(identifier? [x]
            (if (keyword? x)
              (not (@update-options x))
              true))]
    (s/cat :identifiers     (s/? (s/nonconforming
                                  (s/or :list (s/* identifier?)
                                        :single identifier?)))
           :update-options (s/* #(@update-options %)))))

(m/defmethod to-sql [:money.sql.for/update :default]
  [[_ & args] options]
  (when-not (s/valid? ::for-update-args args)
    (throw (ex-info (str "Invalid :for :update args: " (s/explain-str ::for-update-args args))
                    (s/explain-data ::for-update-args args))))
  (let [{:keys [identifiers update-options]} (s/conform ::for-update-args args)]
    (combine-sql-args
     (concat
      [["UPDATE"]]
      (when identifiers
        [[" OF "]
         (to-sql (identifier-or-identifier-list identifiers) options)])
      (when (seq update-options)
        (cons
         [" "]
         (for [option update-options]
           (to-sql (if (and (keyword? option)
                            (not (namespace option)))
                     [(keyword "money.sql.for.update" (name option))]
                     option)
                   options))))))))

;; TODO -- not sure we need this
(defn- for-clause [s args options]
  (if (empty? args)
    [s]
    (combine-sql-args [[(str s \space)]
                       (to-sql args options)])))

(m/defmethod to-sql [:money.sql.for/share :default]
  [[_ & args] options]
  (for-clause "SHARE" args options))

(m/defmethod to-sql [:money.sql.for/key-share :default]
  [[_ & args] options]
  (for-clause "KEY SHARE" args options))

(m/defmethod to-sql [:lock :default]
  [[_ arg] options]
  (combine-sql-args [["LOCK "] (to-sql arg options)]))

(m/defmethod to-sql [:in-share-mode :default]
  [_ _]
  ["IN SHARE MODE"])

;;;; Main user-facing entry point

(defn format
  ([query]
   (format query nil))
  ([query options]
   (if (:debug options)
     (binding [u/*debug* true]
       (to-sql query options))
     (to-sql query options)))
  ([query k v & {:as options}]
   (format query (merge {k v} options))))
