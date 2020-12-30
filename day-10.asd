;;;; day-10.asd

(asdf:defsystem #:day-10
  :description "Day 10 of CoL advent of code 2020"
  :author "Erumachi"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :components ((:module "src"
                :components
                ((:file "day-10"))))
  :build-operation "program-op"
  :build-pathname "day-10"
  :entry-point "day-10:-main")
