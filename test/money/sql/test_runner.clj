(ns money.sql.test-runner
  (:require [cognitect.test-runner.api :as test-runner]
            [pjstadig.humane-test-output :as humane-test-output]))

(humane-test-output/activate!)

(defn run-tests [args]
  (test-runner/test args))
