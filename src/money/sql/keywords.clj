(ns money.sql.keywords
  (:require [methodical.core :as m]
            [money.sql.interface :as i]
            [money.sql.identifiers :as id]
            [money.sql.primitives :as p]
            [clojure.string :as str]))

(m/defmulti keyword-to-sql
  {:arglists '([k options])}
  (fn [k {:keys [dialect]}]
    [(second (str k)) dialect]))

(m/defmethod i/to-sql [clojure.lang.Keyword :default]
  [k options]
  (keyword-to-sql k options))

(m/defmethod keyword-to-sql :default
  [k options]
  (i/to-sql (id/identifier k) options))

(m/defmethod keyword-to-sql [\% :default]
  [k options]
  (let [s                (.substring (str k) 2)
        [fn-name & args] (str/split s #"\.")]
    (i/to-sql (apply p/call fn-name (map keyword args)) options)))

;; TODO -- quote sigil

;; TODO -- question mark sigil (parameters)

;; (m/defmethod parse-keyword [\% :default]
;;   [k _options]
;;   (let [k                (keyword (.substring (str k) 2))
;;         [fn-name & args] (str/split (name k) #"\.")]
;;     (into [(keyword (namespace k) fn-name)] (map keyword) args)))

;; (m/defmethod parse-keyword [\' :default]
;;   [k options]
;;   (preserve-kebabs (parse-keyword (keyword (.substring (str k) 2)) options)))

;; (m/defmethod parse-keyword [:default \?]
;;   [k _options]
;;   (let [k (keyword (.substring (str k) 2))]
;;     [::parameter k]))

;; This might belong in primitives

;; (m/defmethod to-sql [::parameter :default]
;;   [[_ k] {:keys [params], :as options}]
;;   (to-sql (get params k) options))
