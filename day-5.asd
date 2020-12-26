;;;; day-5.asd

(asdf:defsystem #:day-5
  :description "Day 5 of CoL advent of code 2020"
  :author "Erumachi"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :components ((:module "src"
                :components
                ((:file "day-5"))))
  :build-operation "program-op"
  :build-pathname "day-5"
  :entry-point "day-5:-main")
