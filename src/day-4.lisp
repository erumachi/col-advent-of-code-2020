;;;; day-4.lisp

;; Challenge 4: A perfect fit

;; Being happy with your work from yesterday, Santa asks you to do even more research: This time,
;; he wants to have an idea of how much magic he needs to use in order to teleport his presents
;; down each persons chimney. When asking how to calculate the amount of magic required, Santa says:

;; “Well, if the present fits through the chimney, it does not require any. If it does not,
;; the present needs to be teleported. In that case, the amount of magic required is equal
;; to the volume of the present.

;; Task

;; You are to design a function, that takes the following arguments:

;;     W - the width of the chimney.
;;     H - the height of the chimney.
;;     X - the length of the present.
;;     Y - the height of the present.
;;     Z - the depth of the present.

;; The function should return the amount of magic required to get the present through the chimney.

;; Important thing to remember: A present can be rotated in order to fit through!

(defpackage :day-4
  (:use #:cl)
  (:export
   #:-main
   #:a-perfect-fit)
  )

(in-package :day-4)

;; helper function
(defun permutations-of (lst &optional (remain lst))
  "Given a list lst of elements, generate all the possible
   permutations of these elements"
  (cond
    ;; Termination condition for the recursion
    ((null remain) nil)
    ;; If the list has only one element (its tail is empty),
    ;; return a list containing the list itself (its head, as a list)
    ((null (rest lst)) (list lst))
    ;; Otherwise compute the first element of the permutations
    ;; of the tail of the list
    ;; and append them to each of the permutations of the rest.
    (t (append
        (mapcar (lambda (l) (cons (first lst) l))
                (permutations-of (rest lst)))
        (permutations-of (append (rest lst) (list (first lst))) (rest remain))))))

;; The present fits in this position if its sides are smaller than the chimney
(defun present-fits-constraints (sides constraints)
  "Given the sides, test if those fit the constraints in that order"
  (progn
    (and
     (<= (nth 0 sides) (nth 0 constraints))
     (<= (nth 1 sides) (nth 1 constraints))
     (<= (nth 2 sides) (nth 2 constraints)))))

;; Actual challenge
(defun a-perfect-fit (w h x y z)
  "Given the sizes of the present and the chimney, return the amount of magic
   required to get the present through the chimney"
  (let
      ;; List of the sides of the chimney
      ((constraints (list (* w w)
                          (* w w)
                          (* w h)))
       ;; Volume of the present
       (present-volume (* x y z))
       ;; List of the sides of the present
       (present-sides (list (* x y)
                            (* x z)
                            (* y z))))
    ;; If there is at least one permutation of present sides that
    ;; fits, the present fits. Return 0 in this case
    (cond
      ((some #'identity ;; Return true if at least one of the elements returned is true
             (mapcar    ;; apply the following lambda to each permutation
              (lambda (permut)
                (present-fits-constraints permut constraints)) ;; Check if this permutation fits the constraints
              (permutations-of present-sides))) ;; Generate list of permutations to apply the lambda to
       0)
      ;; Otherwise, the present doesn't fit. Return its volume
      (t
       present-volume))))

;; Helper to print a test's input and result
(defun execute-test (w h x y z)
  (progn
    (format t "W=~a, H=~a, X=~a, Y=~a, Z=~a~%" w h x y z)
    (format t "Return: ~a~%" (a-perfect-fit w h x y z))
    0))

;; Main entry point. It just runs the tests
(defun -main ()
  "Entry point of the challenge"
  (progn
    (execute-test 2 2 1 1 3)
    (format t "~%")
    (execute-test 4 1 5 3 2)
    (format t "~%")
    (execute-test 10 3 7 8 3)
    0))
