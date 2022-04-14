(ns money.sql.interface
  (:require [methodical.core :as m]))

(m/defmulti to-sql
  {:arglists '([x options])}
  (fn [x {:keys [dialect]}]
    ;; TODO -- dialects namespacing or whatever
    [(type x) dialect]))

(m/defmethod to-sql [Object :default]
  [x _options]
  ["?" x])
