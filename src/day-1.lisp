;;;; day-1.lisp

;; Challenge 1: Christmas Shopping

;; A small christmas shop in your area is working on the automation of selling christmas items. The incoming orders have already been processed by one machine, but the shop thinks the program is acting weirdly.
;; Task

;; Your task is to design a program that checks if a given order is correct. To do this, they hand you the list of requirements to check and see if an order is correct.
;; Requirements

;; You will be given the following input:

;;     B - the amount of christmas balls ordered. (type: integer)
;;     C - the amount of chocolate cookies ordered. (type: integer)
;;     M - the month in which the order was placed. (type: integer)
;;     T - whether or not a tree was ordered. (type: boolean)

;; Your return should be a boolean (true/false).

;; The following requirements should be met:

;;     The amount of christmas balls should be a multiple of 6.
;;     The amount of cookies ordered must be a minimum of 6, and can be no more than 24.
;;     The order must have been placed before december.
;;     If the order was placed in november, it cannot contain a tree.
;;     If the order contains a tree, the maximum amount of cookies that can be ordered is 48.

;; If all of the requirements are met, the program should return true. If not, the program should return false.

(defpackage :day-1
  (:use #:cl)
  (:export
   #:-main
   #:christmas-shopping)
  )

(in-package :day-1)

;; Actual challenge
(defun christmas-shopping (balls cookies month tree-ordered)
  "Given the order, return true if it's correct,
  false otherwise"
  (and                                    ;; All these conditions should be true
   (= 0 (mod balls 6))                    ;; - If balls modulo 6 is equal to 0, balls is multiple of 6
   (>= cookies 6)                         ;; - cookies should be at least 6
   (< month 12)                           ;; - Order placed before december
   (if (= 11 month) (not tree-ordered) t) ;; - If order placed in november, it can't have a tree, otherwise return true
   (if tree-ordered (<= cookies 48) (<= cookies 24))))  ;; - If the tree is in the order, cookies are 48 max, otherwise they're 24 max

;; Helper for printing booleans
(defun bool-to-str (b)
  (if b "true" "false"))

;; Helper to print a test's input and result
(defun execute-test (balls cookies month tree-ordered)
  (progn
    (format t "B = ~d~%" balls)
    (format t "C = ~d~%" cookies)
    (format t "M = ~d~%" month)
    (format t "T = ~A~%" (bool-to-str tree-ordered))
    (format t "return: ~A~%" (bool-to-str (christmas-shopping balls cookies month tree-ordered)))
    0))

;; Main entry point. It just runs the tests
(defun -main ()
  "Entry point of the challenge"
  (progn
    (execute-test 6 6 10 t)
    (format t "~%")
    (execute-test 13 7 11 nil)
    (format t "~%")
    (execute-test 18 30 12 t)
    0))
