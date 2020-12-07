;;;; day-1.asd

(asdf:defsystem #:day-1
  :description "Day 1 of CoL advent of code 2020"
  :author "Erumachi"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :components ((:module "src"
                :components
                ((:file "day-1"))))
  :in-order-to ((test-op (test-op "day-1-tests")))
  :build-operation "program-op"
  :build-pathname "day-1"
  :entry-point "day-1:-main")
