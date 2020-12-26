;;;; day-4.asd

(asdf:defsystem #:day-4
  :description "Day 4 of CoL advent of code 2020"
  :author "Erumachi"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :components ((:module "src"
                :components
                ((:file "day-4"))))
  :build-operation "program-op"
  :build-pathname "day-4"
  :entry-point "day-4:-main")
