;;;; day-1.asd

(asdf:defsystem #:day-2
  :description "Day 2 of CoL advent of code 2020"
  :author "Erumachi"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :components ((:module "src"
                :components
                ((:file "day-2"))))
  :build-operation "program-op"
  :build-pathname "day-2"
  :entry-point "day-2:-main")
