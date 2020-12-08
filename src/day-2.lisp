;;;; day-2.lisp

;; Challenge 2: Hot off the press

;; The christmas shop is grateful for your previous work, and would like for you to repair more of their broken machines. This time, they would like you to automate their product tallying. However, seeing as their other machines are still broken, the input format is less than ideal: You will be given a text file containing a bunch of characters…
;; Task

;; Your task is to design a program/function that returns a list of numbers, given the name/location of a text file as input.

;; The file itself will consist of 4 lines, that look something like this:

;; BBBBBBBBBBBB
;; MMMMMMM
;; CCCCCCCCCC
;; T

;; Where each line represents the amount of items in an order. In this instance, the order contains 12 balls, it was placed in July, contains 10 cookies and a tree.

;; Your output will be a list of numbers, where each number in the list represents the amount of a particular item.

(defpackage :day-2
  (:use #:cl)
  (:export
   #:-main
   #:hot-off-the-press)
  )

(in-package :day-2)

;; Actual challenge
(defun hot-off-the-press (filename)
  "Given the file, return a list of numbers where each number is
   the number of characters in each line. There should be 4 lines in the file"
  (with-open-file (stream filename)
    (let ((ret '()))
      ;; Loop reading the file until it's empty, line by line
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        ;; Body of the loop: add to ret the length of the line
        (push (length line) ret))
      ;; After the loop, do another to make sure that
      ;; the list is at least 4 elements long, filling with zeros
      ;; if necessary
      (loop
        while (< (length ret) 4)
        do (push 0 ret))
      ;; Reverse the list (push adds at the start)
      ;; and return the first 4 elements (in case the file is longer)
      (subseq (nreverse ret) 0 4))))

;; Helper to print a test's input and result
(defun execute-test (filename)
  (progn
    (format t "filename = ~A~%" filename)
    (format t "File contents:~%---~%")
    (with-open-file (stream filename)
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (format t "~A~%" line)))
    (format t "---~%")
    (format t "return: [~{~a~^, ~}]~%" (hot-off-the-press filename))
    0))

;; Main entry point. It just runs the tests
(defun -main ()
  "Entry point of the challenge"
  (progn
    (execute-test "2-input-1.txt")
    (format t "~%")
    (execute-test "2-input-2.txt")
    0))
