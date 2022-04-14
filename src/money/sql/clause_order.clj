(ns money.sql.clause-order)

(defonce befores (atom {:before-fn (constantly false)}))

;; TODO -- Not sure this is really the best way to do this stuff -- why not just create a bunch of big sets the way
;; [[derive]] does
(defn- before-fn [befores]
  (fn before? [x y]
    (let [x-befores (get-in befores [:before x])]
      (or (contains? x-befores y)
          (some (fn [x-before]
                  (before? x-before y))
                x-befores)))))

(defn define-before [befores before after]
  ;; step one: add immediate before and after entries
  (let [befores   (update-in befores [:before before] (fn [before-set]
                                                        (conj (set before-set) after)))
        before-fn (memoize (before-fn befores))]
    (assoc befores :before-fn before-fn)))

(defn clause-before?
  "Should top-level clause `x` come before top-level clause `y`?"
  [x y]
  ((:before-fn @befores) x y))

(defn define-befores [befores before afters]
  (reduce
   (fn [befores after]
     (define-before befores before after))
   befores
   afters))

(defn define-before! [before & afters]
  (swap! befores define-befores before afters))

(defn compare-clause-keys [x y]
  (cond
    (clause-before? x y) -1
    (clause-before? y x) 1
    :else                0))

(defn sort-clause-keys [ks]
  (sort-by identity (memoize compare-clause-keys) ks))

;;; define the default clause orders.
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
  (define-before! x y))
