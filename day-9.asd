;;;; day-9.asd

(asdf:defsystem #:day-9
  :description "Day 9 of CoL advent of code 2020"
  :author "Erumachi"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :components ((:module "src"
                :components
                ((:file "day-9"))))
  :build-operation "program-op"
  :build-pathname "day-9"
  :entry-point "day-9:-main")
