;;;; day-6.asd

(asdf:defsystem #:day-6
  :description "Day 6 of CoL advent of code 2020"
  :author "Erumachi"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :components ((:module "src"
                :components
                ((:file "day-6"))))
  :build-operation "program-op"
  :build-pathname "day-6"
  :entry-point "day-6:-main")
