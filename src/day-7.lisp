;;;; day-7.lisp

;; Challenge 7: Randomness

;; You work at a shop that sells candy canes. Your boss, the shop owner, wants to know how much he should expect to earn the next day. He knows you’re a nerd so he asks you that question.

;; “Boss,” you said, “I can’t possibly determine that without knowing how many customers we’ll have, and how many of them will buy candy canes.”

;; He replies, “Well, you know my candy canes cost a dollar each, which is a great deal, 9 times out of 10 you’ll buy one without hesitation. I also advertised the shop everywhere, so I expect maybe a thousand visitors tomorrow.”

;; You immediately answer, “Well, that’s 900 dollars.”

;; Your boss isn’t convinced one can be so sure of that so quickly, even after you explain to him how simple the formula is. He tells you that you need to programmatically simulate each person walking into the shop and deciding whether or not to buy a candy cane. You need to keep this job, so…
;; Task

;; You are given 3 variables C, P, and N, which are, respectively, the price of each cane, the probability that someone buys a cane (between 0 and 1), and the number of visitors. You need to make a function that will loop through each visitor and randomly decide whether or not they’ll buy a cane, and then return the total profit.

(defpackage :day-7
  (:use #:cl)
  (:export
   #:-main
   #:randomness)
  )

(in-package :day-7)

(defun will-buy-cane (buy-prob)
  "Return a random boolean value with the given probability
   to decide if a customer will buy or not"
  (<= (random 1.0) buy-prob))

;; Actual challenge
(defun randomness (price buy-prob visitors)
  "Simulate a day at the shop and return the total profit"
  (let ((profit 0))
         (loop for i from 1 to visitors
               do (when (will-buy-cane buy-prob)
                    (setq profit (+ profit price))))
         profit))


;; Helper to print a test's input and result
(defun execute-test (price buy-prob visitors)
  (progn
    (format t "C: ~a~%" price)
    (format t "P: ~a~%" buy-prob)
    (format t "N: ~a~%" visitors)
    (format t "My output was: ~a~%" (randomness price buy-prob visitors))
    0))

;; Main entry point. It just runs the tests
(defun -main ()
  "Entry point of the challenge"
  (progn
    (execute-test 2 0.5 10)
    (format t "~%")
    (execute-test 1 0.9 1000)
    0))
