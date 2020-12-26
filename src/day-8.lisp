;;;; day-8.lisp

;; Challenge 8: Staircase

;;     Author’s note: I spent a lot of time bugfixing an assignment for uni last night so the creative neurons haven’t woken up yet.

;;     Seeing as I’m in a grumpy mood, here’s a challenge that me and a friend found particularly annoying to do the first time we got it.

;; You are tasked with writing a program that prints out stairs. The base and height of each staircase must always be equal. Take this example staircase of size 4 going left:

;;    #
;;   ##
;;  ###
;; ####

;; Note that it is exactly 4 lines long, and that the staircase is left-aligned.
;; Requirements

;; Design a function that takes in a single input N, and prints a left-aligned staircase of that size.

(defpackage :day-8
  (:use #:cl)
  (:export
   #:-main
   #:staircase)
  )

(in-package :day-8)

;; Actual challenge
(defun staircase (n)
  (let ((my-str "~%"))
    (progn
      (loop for lvl from 0 to (- n 1)
            do (progn
                 (loop for i from 0 to (- n lvl 1)
                       do (setq my-str (concatenate 'string "#" my-str)))
                 (loop for j from 1 to lvl
                       do (setq my-str (concatenate 'string " " my-str)))
                 (setq my-str (concatenate 'string "~%" my-str))
                 ))
      (format t my-str))))


;; Helper to print a test's input and result
(defun execute-test (n)
  (progn
    (format t "N: ~a~%" n)
    (format t "return:~%---~%")
    (staircase n)
    (format t "---~%")
    0))

;; Main entry point. It just runs the tests
(defun -main ()
  "Entry point of the challenge"
  (progn
    (execute-test 1)
    (format t "~%")
    (execute-test 2)
    (format t "~%")
    (execute-test 4)
    0))
