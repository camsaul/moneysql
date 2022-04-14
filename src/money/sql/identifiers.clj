(ns money.sql.identifiers
  (:require
   [clojure.string :as str]
   [methodical.core :as m]
   [money.sql.interface :as i]
   [money.sql.primitives :as p]))

(defn ->snake-case [s {:keys [quoted-snake], :as _options}]
  (if quoted-snake
    s
    (str/replace s #"-" "_")))

(m/defmethod i/to-sql [::identifier-part :default]
  [[_ part, :as x] {:keys [quoted dialect], :as options}]
  ;; if `quoted` is specified but a dialect was not, call the ansi quote method (defined in [[money.sql.dialects]])
  (if (and quoted (not dialect))
    (i/to-sql x (merge {:dialect :ansi} options))
    [(->snake-case (name part) options)]))

(defn quote-identifier-part [part {:keys [quoted], :or {quoted true}, :as options} quote-fn]
  [(cond-> (->snake-case part options)
     (and quoted (not= part "*")) quote-fn)])

(defn mysql-quote [s]
  (str "`" (str/replace s #"`" "``") "`"))

(m/defmethod i/to-sql [::identifier-part :mysql]
  [[_ part] options]
  (quote-identifier-part part options mysql-quote))

(defn ansi-quote [s]
  (str "\"" (str/replace s #"\"" "\"\"") "\""))

(m/defmethod i/to-sql [::identifier-part :ansi]
  [[_ part] options]
  (quote-identifier-part part options ansi-quote))

(defn sqlserver-quote [s]
  (str "[" (str/replace s #"\]" "]]") "]"))

(m/defmethod i/to-sql [::identifier-part :sqlserver]
  [[_ part] options]
  (quote-identifier-part part options sqlserver-quote))

(m/defmethod i/to-sql [::identifier :default]
  [[_ & parts] options]
  (i/to-sql (p/interpose "." (for [part parts]
                               [::identifier-part part]))
            options))

(defn preserve-kebabs [x]
  [::preserve-kebabs x])

(m/defmethod i/to-sql [::preserve-kebabs :default]
  [[_ x] options]
  (i/to-sql x (assoc options :quoted-snake true)))

(m/defmethod i/to-sql [::as :default]
  [[_ identifier alias] options]
  (i/to-sql (p/interpose " AS "
                         [identifier
                          ;; TODO -- this needs to be configurable. Or use [[keyword-identifier]] with a no split option
                          [::identifier (str/replace (name alias) #"-" "_")]])
            options))

(defn as [x y]
  [::as x y])

(defn identifier [x]
  (cond
    (and (sequential? x)
         (> (count x) 1))
    (apply as x)

    (sequential? x)
    (first x)

    :else x))

(m/defmethod i/to-sql [::identifier-list :default]
  [[_ & xs] options]
  (i/to-sql (p/commas (map identifier xs)) options))

(defn identifier-list [xs]
  (into [::identifier-list] xs))

(defn identifier-or-identifier-list
  "Like [[identifier-list]] but handles a single identifier e.g. for a `FROM` clause."
  [args]
  (identifier-list
   (if (sequential? args)
     args
     [args])))
