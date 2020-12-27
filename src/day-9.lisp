;;;; day-9.lisp

;; Challenge 9: Candy Cane Madness

;; You’re in charge of Santa’s candy cane factory. Your job is to figure out which
;; candy canes are ready for delivery, and which ones are faulty and can be reused.

;; You have decided that the tallest candy canes are the ones that can be shipped,
;; and that any candy cane below that height can be reused.

;; Requirements

;; Your task is to create a function that takes the following input:

;;     N, the amount of candy canes
;;     C, A list of heights of all the different candy canes.

;; You need to return the following two integers in a list:

;;     The number of candy canes that are the tallest,
;;     The amount of candy cane material that can be reused.

;; The amount of material that can be reused is calculated by summing up their heights.

(defpackage :day-9
  (:use #:cl)
  (:export
   #:-main
   #:candy-cane-madness)
  )

(in-package :day-9)

;; Actual challenge
(defun candy-cane-madness (n c)
  "Return the occurences of the maximum element and the sum
   of the elements that are not the maximum"
  (cond
    ;; If the list of candy canes is empty, return (0 0)
    ((= n 0) '(0 0))
    ;; General case
    (t (let*
           ;; max-elem: highest candy cane's height
           ((max-elem (reduce #'max c))
            ;; max-count: count the number of candy canes that have max height
            (max-count (reduce (lambda (accum elem)
                                 (cond
                                   ((= max-elem elem) (+ accum 1))
                                   (t accum)))
                               c :initial-value 0))
            ;; reusable: sum the candy canes that are not the tallest
            (reusable (reduce (lambda (accum elem)
                                (cond
                                  ((not (= max-elem elem)) (+ elem accum))
                                  (t accum)))
                              c :initial-value 0)))
         ;; return the number of talles candy canes and the sum of the reusable material
         (list max-count reusable)))))


;; Helper to print a test's input and result
(defun execute-test (n c)
  (progn
    (format t "N = ~a~%" n)
    (format t "C = ~a~%" c)
    (format t "return: ~a~%" (candy-cane-madness n c))
    0))

;; Main entry point. It just runs the tests
(defun -main ()
  "Entry point of the challenge"
  (progn
    (execute-test 4 '(3 2 1 3))
    (format t "~%")
    (execute-test 3 '(1 2 5))
    (format t "~%")
    (execute-test 7 '(1 2 3 4 5 7 7))
    0))
