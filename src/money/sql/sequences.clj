(ns money.sql.sequences
  (:require [methodical.core :as m]
            [money.sql.interface :as i]
            [money.sql.primitives :as p]))

(m/defmethod i/to-sql [clojure.lang.Sequential :default]
  [[first-x :as xs] {:keys [dialect], :as options}]
  (letfn [(this-method? [a-method]
            (identical? (m/effective-method i/to-sql [clojure.lang.Sequential :default]) a-method))
          (matching-method [dispatch-value]
            (let [method (m/effective-method i/to-sql dispatch-value)]
              (when-not (this-method? method)
                method)))]
    (let [method (or (matching-method [[first-x] dialect])
                     (matching-method [[(type first-x)] dialect]))]
      (if method
        (method xs options)
        (i/to-sql (p/combine xs) options)))))

(m/defmethod i/to-sql [[String] :default]
  [sql-args _options]
  sql-args)

(m/defmethod i/to-sql [[clojure.lang.Keyword] :default]
  [fn-args options]
  ;; TODO -- snake_case
  (i/to-sql (apply p/call fn-args) options))
