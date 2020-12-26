;;;; day-3.lisp

;; Challenge 3: A lot at stake

;; Santa’s computer broke and he lost a lot of important data, among which is the number
;; of children in each house he’s visiting. You, being the IT elf, tried to recover the
;; data but couldn’t. While surfing online, you find 2 arrays with population statistics.
;; Those will be enough to save the day. Make a program that calculates the amount of
;; gifts that Santa needs to bring.

;; Task

;; You are given 2 arrays, the first contains the number of houses in each city/country
;; (up for interpretation) that Santa is visiting, the second contains the average number of
;; children in each house of each city/country respectively. You need to calculate
;; the total number of gifts.

(defpackage :day-3
  (:use #:cl)
  (:export
   #:-main
   #:hot-off-the-press)
  )

(in-package :day-3)

;; Actual challenge
(defun a-lot-at-stake (homes density)
  "Given an array of number of houses per city and the average number of
   children per house of each city, calculate the total number of gifts"
  (multiple-value-bind (quot rem) (ceiling          ;; ceiling returns two values. We only
                                                    ;; care about the first one.
                   (apply #'+ (loop                 ;; Sum the elements returned by the loop
                                for x in homes      ;; In the loop, call x each element in homes
                                for y in density    ;; and y each element in density
                                collect (* x y))))  ;; and return the list of the products of the two
    quot)) ;; This is the actual return value

;; Helper to print a test's input and result
(defun execute-test (homes density)
  (progn
    (format t "Homes: [~{~a~^, ~}]~%" homes)
    (format t "Density: [~{~a~^, ~}]~%" density)
    (format t "Return: ~a~%" (a-lot-at-stake homes density))
    0))

;; Main entry point. It just runs the tests
(defun -main ()
  "Entry point of the challenge"
  (progn
    (execute-test '(1 2) '(1 3))
    (format t "~%")
    (execute-test '(5 4 8) '(2 1.5 2.5))
    0))
