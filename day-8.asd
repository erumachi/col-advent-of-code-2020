;;;; day-8.asd

(asdf:defsystem #:day-8
  :description "Day 8 of CoL advent of code 2020"
  :author "Erumachi"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :components ((:module "src"
                :components
                ((:file "day-8"))))
  :build-operation "program-op"
  :build-pathname "day-8"
  :entry-point "day-8:-main")
