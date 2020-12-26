;;;; day-5.lisp

;; Challenge 5: Speed is key

;; Santa is getting old. It seems that he forgot how fast his reindeer are supposed to go. He asks you to recalculate how fast he needs to be going in order to get to all the houses. He adds that he must be as slow as he can so that he doesn’t tire his reindeer.
;; Task

;; You are given 4 inputs:

;;     H - The total number of homes Santa needs to visit
;;     G - The time it takes to deliver a house’s gifts (in seconds)
;;     D - The total distance between all cities (in kilometres)
;;     T - The total time that Santa has (in minutes)

;; You need to calculate in km/h the minimum speed Santa needs to be travelling in order to deliver all the gifts in the time required; going faster than necessary strains the reindeer.

(defpackage :day-5
  (:use #:cl)
  (:export
   #:-main
   #:speed-is-key)
  )

(in-package :day-5)

;; Actual challenge
(defun speed-is-key (homes gift-time distance total-minutes)
  "Return the minimun speed in kilometers per second to travel and
   deliver all the gifts in the specified time"
  (let*
      ((secs-for-gifts (* homes gift-time))
       (distance-meters (* distance 1000))
       (total-seconds (* total-minutes 60))
       (travel-seconds (- total-seconds secs-for-gifts))
       (min-ms-speed (/ distance-meters travel-seconds)))
    (* min-ms-speed 3.6)))
  
;; Helper to print a test's input and result
(defun execute-test (homes gift-time distance total-minutes)
  (progn
    (format t "H: ~a~%" homes)
    (format t "G: ~a~%" gift-time)
    (format t "D: ~a~%" distance)
    (format t "T: ~a~%~%" total-minutes)
    (format t "Return: ~a~%" (speed-is-key homes gift-time distance total-minutes))
    0))

;; Main entry point. It just runs the tests
(defun -main ()
  "Entry point of the challenge"
  (progn
    (execute-test 5 3 2 4)
    (format t "~%")
    (execute-test 24 30 10 60)
    0))
