;;;; day-1-tests.lisp
;;;; TODO: docs

(defpackage day-1-tests
  (:use #:cl #:fiveam)
  (:export #:run!
           #:all-tests))

(in-package :day-1-tests)

(def-suite all-tests
  :description "Test suite for Day 1")

(in-suite all-tests)

(test first-test
      :description "TODO"
      (is (day-1:christmas-shopping 6 6 10 t)))

(test second-test
      :description "TODO"
      (is (not (day-1:christmas-shopping 13 7 11 nil))))

(test third-test
      :description "TODO"
      (is (not (day-1:christmas-shopping 18 30 12 t))))

(test sanity-check
      :description "Sanity check"
      (is (= 4 (+ 2 2))
          "Sanity check failed: apparently 2 + 2 != 4"))
