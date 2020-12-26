;;;; day-3.asd

(asdf:defsystem #:day-3
  :description "Day 3 of CoL advent of code 2020"
  :author "Erumachi"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :components ((:module "src"
                :components
                ((:file "day-3"))))
  :build-operation "program-op"
  :build-pathname "day-3"
  :entry-point "day-3:-main")
