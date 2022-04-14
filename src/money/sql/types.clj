(ns money.sql.types
  "Default impls for various classes"
  (:require [money.sql.interface :as i]
            [clojure.string :as str]
            [methodical.core :as m]))

;; TODO -- better name. Objects?

(m/defmethod i/to-sql [String :default]
  [s {:keys [inline], :as options}]
  (if inline
    ;; TODO -- need to escape the string.
    [(format "'%s'" (str/replace s #"'" "''"))]
    (next-method s options)))

(m/defmethod i/to-sql [Boolean :default]
  [bool _options]
  [(if bool
     "TRUE"
     "FALSE")])

(m/defmethod i/to-sql [nil :default]
  [_ _options]
  ["NULL"])
