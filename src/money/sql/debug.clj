(ns money.sql.debug)

(m/defmethod to-sql :around :default
  [x options]
  (try
    (u/debug (pr-str (list 'to-sql x options))
      (let [result (next-method x options)]
        (assert (or (nil? result)
                    (and (sequential? result)
                         (string? (first result))))
                (str "Expected sql-args, got " (pr-str result)))
        result))
    (catch Throwable e
      (throw (ex-info (clojure.core/format "Error compiling %s to SQL: %s"
                                           (pr-str x)
                                           (or (ex-message e) e))
                      {:form x, :options options}
                      #_e)))))
