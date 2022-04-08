(ns money.sql.util)

;; TODO -- just check options instead?
(def ^:dynamic *debug* false)

(def ^:dynamic *debug-indent* 0)

(defn debug-println [& args]
  (dotimes [_ *debug-indent*]
    (print "  "))
  (apply println args))

(defn do-debug [message-thunk thunk]
  (if-not *debug*
    (thunk)
    (do
      (debug-println (message-thunk))
      (binding [*debug-indent* (inc *debug-indent*)]
        (let [result (thunk)]
          (debug-println '=> (pr-str result))
          result)))))

(defmacro debug {:style/indent 1} [message-form & body]
  `(do-debug (fn [] ~message-form) (fn [] ~@body)))

(defn upper-case-en
  "Locale-agnostic version of [[clojure.string/upper-case]]. [[clojure.string/upper-case]] uses the default locale in
  conversions, turning `id` into `İD`, in the Turkish locale. This function always uses the `Locale/US` locale."
  [^CharSequence s]
  (.. s toString (toUpperCase (java.util.Locale/US))))
