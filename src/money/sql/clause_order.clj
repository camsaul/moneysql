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
