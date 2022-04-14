(ns money.sql.primitives
  (:refer-clojure :exclude [interpose])
  (:require [money.sql.interface :as i]
            [methodical.core :as m]))

(m/defmethod i/to-sql [[::combine] :default]
  [[_ & xs] options]
  (loop [acc-sql "", acc-args [], [x & more] xs]
    (if-not x
      (into [acc-sql] acc-args)
      (let [[sql & args] (i/to-sql x options)]
        (recur (str acc-sql sql) (concat acc-args args) more)))))

(defn combine [xs]
  (into [::combine] xs))

(m/defmethod i/to-sql [[::interpose] :default]
  [[_ separator & xs] options]
  (i/to-sql (combine (clojure.core/interpose separator xs)) options))

(defn interpose [separator args]
  (into [::interpose separator] args))

(m/defmethod i/to-sql [[::commas] :default]
  [[_ & xs] options]
  (i/to-sql (interpose [", "] xs) options))

(defn commas [args]
  (into [::commas] args))

(m/defmethod i/to-sql [[::parens] :default]
  [[_ x] options]
  (i/to-sql [["("] x [")"]] options))

(defn parens [x]
  [::parens x])

(m/defmethod i/to-sql [:inline :default]
  [[_ value] options]
  (i/to-sql value (assoc options :inline true)))

(defn inline [x]
  [:inline x])

;;;; Function Calls

(m/defmethod i/to-sql [[::call] :default]
  [[_ fn-name & args] options]
  (i/to-sql [[(name fn-name)] (parens (commas args))] options))

(defn call [fn-name & args]
  (into [::call fn-name] args))
