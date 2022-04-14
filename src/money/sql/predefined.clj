(ns money.sql.predefined
  "Misc out-of-the box function/expression definitions."
  (:require [methodical.core :as m]
            [money.sql.interface :as i]
            [money.sql.primitives :as p]))

;; TODO -- not happy with this namespace name

(m/defmethod i/to-sql [:interval :default]
  [[_ n unit] options]
  (i/to-sql (p/interpose [" "] [["interval"]
                                n
                                [(name unit)]])
            options))

;;;; where clauses

(m/defmethod i/to-sql [:= :default]
  [[_ x y] options]
  (i/to-sql (if (some nil? [x y])
              [:is x y]
              (p/interpose " = " [x y]))
            options))

(m/defmethod i/to-sql [:is :default]
  [[_ x y] options]
  (i/to-sql (p/interpose " IS " [x y]) options))

(m/defmethod i/to-sql [:is-not :default]
  [[_ x y] options]
  (i/to-sql (p/interpose " IS NOT " [x y]) options))

(m/defmethod i/to-sql [:<> :default]
  [[_ x y] options]
  (i/to-sql (if (some nil? [x y])
              [:is-not x y]
              (p/interpose " <> " [x y]))
            options))

(m/defmethod i/to-sql [:!= :default]
  [[_ x y] options]
  (i/to-sql [:<> x y] options))

(m/defmethod i/to-sql [:not= :default]
  [[_ x y] options]
  (i/to-sql [:<> x y] options))

(m/defmethod i/to-sql [:in :default]
  [[_ x vs] options]
  #_(assert (sequential? vs))
  (i/to-sql (p/interpose " IN " [x (p/parens (p/commas vs))]) options))

(m/defmethod i/to-sql [:|| :default]
  [[_ & args] options]
  (i/to-sql (p/interpose " || " args) options))

;;;; math clauses

(defn math-operator [separator xs]
  (p/parens (p/interpose separator xs)))

(m/defmethod i/to-sql [:+ :default]
  [[_ & xs] options]
  (i/to-sql (math-operator " + " xs) options))

(m/defmethod i/to-sql [:- :default]
  [[_ & xs] options]
  (i/to-sql (math-operator " - " xs) options))

(m/defmethod i/to-sql [:< :default]
  [[_ & xs] options]
  (i/to-sql (math-operator " < " xs) options))

(m/defmethod i/to-sql [:<= :default]
  [[_ & xs] options]
  (i/to-sql (math-operator " <= " xs) options))

(m/defmethod i/to-sql [:> :default]
  [[_ & xs] options]
  (i/to-sql (math-operator " > " xs) options))

(m/defmethod i/to-sql [:>= :default]
  [[_ & xs] options]
  (i/to-sql (math-operator " >= " xs) options))

;;;; compound where clauses

;; TODO -- we should simplify compound filter clauses, perhaps using the MBQL utils
(m/defmethod i/to-sql [:and :default]
  [[_ & subclauses] options]
  (i/to-sql (if (= (count subclauses) 1)
              (first subclauses)
              (p/interpose " AND " (for [subclause subclauses]
                                     (p/parens subclause))))
            options))
