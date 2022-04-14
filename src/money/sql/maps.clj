(ns money.sql.maps
  (:require [money.sql.interface :as i]
            [money.sql.clause-order :as clause-order]
            [methodical.core :as m]
            [money.sql.primitives :as p]))

(m/defmethod i/to-sql [[::map-entry] :default]
  [[_ k query] {:keys [dialect], :as options}]
  (if-let [method (m/effective-primary-method i/to-sql [{k (type (get query k))} dialect])]
    (method {k query} options)
    (throw (ex-info (str "Don't know how to handle top-level key " (pr-str k))
                    {:k k, :v (get query k)}))))

(m/defmethod i/to-sql [clojure.lang.IPersistentMap :default]
  [query {:keys [subquery? pretty #_pretty-indent], :as options}]
  (let [clause-keys (clause-order/sort-clause-keys (keys query))]
    (i/to-sql (cond-> (p/interpose (if pretty "\n" " ")
                                   (for [k clause-keys]
                                     [::map-entry k query]))
                subquery? p/parens)
              options)))
