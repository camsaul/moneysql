# MoneySQL

1. Step one: Clojure
2. Step two: SQL
3. Step three: Profit

MoneySQL is an experimental drop-in replacement for [HoneySQL](https://github.com/seancorfield/honeysql).

# Cool, but WHY

- More flexible and extensible. One single point of extensibility -- the multimethod `to-sql` -- that is used to
  implement the entire library -- anything offered out of the box could be added by library users.
- Built-in tools to make extension and customization easier
- Simplicity: codebase is less than half the size. Zero dynamic variables. Code is much simpler
- Dialect-awareness from top to bottom
- Better HoneySQL 1 backwards-compatibility
- Easier debugging
- More flexible pretty-printing

# Examples

## Usage

```clj
(sql/format {:select [[:field :f]]
             :from   [[:table :t]]
             :join   [:t2
                      [:= :t1.whatever :%function.t2.wow]]}
            {:dialect :mysql})
;; => ["SELECT `field` AS `f` FROM `table` AS `t` INNER JOIN `t2` ON `t1`.`whatever` = function(`t2`, `wow`)"]
```

## Extending

### Arbitrary SQL snippets

```clj
;; to-sql just needs to return a [sql & args] vector
(m/defmethod sql/to-sql [:now-6 :mysql]
  [_clause _options]
  ["now(6)"])

(sql/format {:select [[:%now-6 :now]], :from :my_table} {:dialect :mysql})
;; => ["SELECT now(6) AS `now` FROM `my_table`"]
```

```clj
(m/defmethod sql/to-sql [:date-trunc :postgres]
  [[_ unit expr] options]
  ;; Most to-sql implementations are implemented with more calls to to-sql
  (sql/to-sql (sql/call :date_trunc (sql/inline (name unit)) expr) options))

(sql/format {:select [[[:date-trunc :day :field]]], :from :table} {:dialect :postgres})
;; => ["SELECT date_trunc('day', field) FROM table"]
```

### Top-level clauses

```clj
;; tell MoneySQL that ::please-select should come before from
(clause-order/define-before! [::please-select :from])

(m/defmethod sql/to-sql [::please-select :default]
  [[_ arg] options]
  ;; helpers like combine-sql-args and identifier-or-identifier-list are used to implement out-of-the-box stuff and
  ;; available for extensions
  (sql/combine-sql-args [["PLEASE SELECT "]
                         (sql/to-sql (sql/identifier-or-identifier-list arg) options)]))

(sql/format {::please-select [:a [:b :c] [:%now :now]], :from :my_table})
;; => ["PLEASE SELECT a, b AS c, now() AS now FROM my_table"]
```

## Work in Progress!

Stay tuned for more exciting updates.
