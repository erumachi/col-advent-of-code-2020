;;;; day-7.asd

(asdf:defsystem #:day-7
  :description "Day 7 of CoL advent of code 2020"
  :author "Erumachi"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :components ((:module "src"
                :components
                ((:file "day-7"))))
  :build-operation "program-op"
  :build-pathname "day-7"
  :entry-point "day-7:-main")
