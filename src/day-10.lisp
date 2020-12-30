;;;; day-10.lisp

;; Challenge 10: Precise time

;; When Santa’s computer broke like a few challenges ago, he lost a program that
;; calculated the amount of time needed for his elves to finish wrapping all the gifts.
;; Santa wants you to make a new program, but this time instead of outputting an
;; amount of time, he really wants it to output the time at which the elves will be done
;; wrapping (Santa hates mental arithmetic).

;; Task

;; You are given the following variables:

;;     G - The number of gifts
;;     E - The number of elves
;;     T - The time it takes for one elf to wrap one present (in minutes).
;;     S - The starting time

;; The S variable is a number which equals the number of hours after 00:00 Monday, so S=1
;; means the start time is 01:00 and S=2.5 is 02:30 etc etc

;; The output will be the time and day at which the elves will be done, rounded down to the
;; nearest minute. If the end time is later than 23:59 Sunday then you can have a
;; special message like “get more elves mate” or something lol

(defpackage :day-10
  (:use #:cl)
  (:export
   #:-main
   #:precise-time)
  )

(in-package :day-10)

;; Convert hours (as a float) to minutes (as an integer)
(defun float-hours-to-int-minutes (hours)
  (nth-value 0 (floor (* hours 60))))

(defun num-to-day (n)
  "Associate a day of the week (as a string) to an int"
  (cond
    ((= 0 n) "Monday")
    ((= 1 n) "Tuesday")
    ((= 2 n) "Wednesday")
    ((= 3 n) "Thursday")
    ((= 4 n) "Friday")
    ((= 5 n) "Saturday")
    ((= 6 n) "Sunday")
    (t "Unknown day of the week")))

(defun minutes-to-time-str (total-time)
  "Given the time offset in minutes, return the date
   from Monday 00:00 as a string if the offset is smaller
   than a week, otherwise return an error string"
  (let*
       ;; Minutes (0-59) to be printed
      ((minutes (mod total-time 60))
       ;; Hours (0-23) to be printed
       (hours (mod (/ (- total-time minutes) 60) 24))
       ;; Day of the week (0-6) to be printed
       (day (/ (/ (- total-time minutes (* hours 60)) 60) 24))
       ;; Test to see if the time is over a week
       (time-greater-than-max (> total-time (* 60 24 7))))
    (cond
      ;; If the time is too big, not enough elves
      (time-greater-than-max "That's not enough elves")
      ;; Otherwise, return a string formatted like "hh:mm day-of-the-week"
      (t (format nil "~2,'0d:~2,'0d ~a" hours minutes (num-to-day day))))))

;; Actual challenge
(defun precise-time (gifts elves wrap-time start-time)
  "Return the end date as a string"
  (let*
      ;; Time in minutes to wrap every present
      ((total-wrap-time-minutes (/ (* gifts wrap-time)
                                   elves))
       ;; Estimated end time in minutes
       (end-time-minutes (+ (float-hours-to-int-minutes start-time)
                            total-wrap-time-minutes)))
    ;; Convert the end time to a string
    (minutes-to-time-str end-time-minutes)))

(defun execute-test (gifts elves wrap-time start-time)
  "Helper to print a test's input and result"
  (progn
    (format t "G: ~a~%" gifts)
    (format t "E: ~a~%" elves)
    (format t "T: ~a~%" wrap-time)
    (format t "S: ~a~%" start-time)
    (format t "Return: ~a~%" (precise-time gifts elves wrap-time start-time))
    0))

;; Main entry point. It just runs the tests
(defun -main ()
  "Entry point of the challenge"
  (progn
    (execute-test 10 1 60 0)
    (format t "~%")
    (execute-test 100 5 60 30)
    (format t "~%")
    (execute-test 1000 1 60 0)
    0))
