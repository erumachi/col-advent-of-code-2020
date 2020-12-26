;;;; day-6.lisp

;; Challenge 6: Closing a loop

;; An elf over at the R&D Department was experimenting with using wormholes as a mode of delivering presents. Unfortunately, a ‘small’ accident occured, and someone bumped your only chocolate chip cookie into the wormhole. This wouldn’t be such a problem, were it not for the fact that the wormhole keeps on… duplicating items that are in it. In order to get your cookie back, you need to retrieve all duplicates as well.

;; Your task is to calculate the amount of cookies that are in the wormhole, given the amount of times that the wormhole has duplicated items. You’ve noticed that for each time the wormhole duplicates, it doubles the amount of cookies present and adds 2.
;; Requirements

;;     If your language supports it, use recursion.
;;     Design a function that takes the amount of loops L as an input, and returns the total number of cookies in the wormhole.


(defpackage :day-6
  (:use #:cl)
  (:export
   #:-main
   #:closing-a-loop)
  )

(in-package :day-6)

;; Actual challenge
(defun closing-a-loop (loop-count)
  "Return the number of cookies in the wormhole"
  (cond
    ;; Termination condition: no loops -> 1 cookie
    ((= loop-count 0) 1)
    ;; General case
    (t (+ 2 (* 2 (closing-a-loop (- loop-count 1)))))
    ))

;; Helper to print a test's input and result
(defun execute-test (loop-count)
  (progn
    (format t "L = ~a~%" loop-count)
    (format t "Return: ~a~%" (closing-a-loop loop-count))
    0))

;; Main entry point. It just runs the tests
(defun -main ()
  "Entry point of the challenge"
  (progn
    (execute-test 0)
    (format t "~%")
    (execute-test 1)
    (format t "~%")
    (execute-test 2)
    (format t "~%")
    (execute-test 5)
    (format t "~%")
    0))
