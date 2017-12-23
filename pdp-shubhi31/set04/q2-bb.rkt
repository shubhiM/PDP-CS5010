#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "q2.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Black-box tests.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We don't know how the Flapjack and Skillet data types are reqresented,
;;; so we can't use check-equal? on values of those data types.
;;; To simplify black-box testing, we convert values of those data
;;; types to lists of their observable properties, which can then
;;; be compared using check-equal?
;;;
;;; We've got a special problem with lists, because they can be
;;; in any order.  We'll sort them into some canonical order.

;;; flapjack-as-list : Flapjack -> List
;;; Given a flapjack, returns a partial representation as a list.
;;; Examples: see tests below.

(define (flapjack-as-list fj)
  (list 'flapjack
        (flapjack-x fj)
        (flapjack-y fj)
        (flapjack-radius fj)))

;;; list-of-flapjacks-as-list : ListOfFlapjack -> ListOfList
;;; Given a list of flapjacks, returns a list representing each
;;; flapjack as above.

(define (list-of-flapjacks-as-list lof)
  (map flapjack-as-list lof))

;;; sorted-flapjacks : ListOfFlapjacks -> ListOfFlapjacks
;;; GIVEN: a list of flapjacks
;;; RETURNS: that list sorted into some canonical order

(define (sorted-flapjacks fjs)
  (insertion-sort flapjack< fjs))

;;; flapjack< : Flapjack Flapjack -> Boolean
;;; Given two flapjacks,
;;; returns true iff the first is less than the second
;;; in some arbitrary but fixed canonical ordering

(define (flapjack< fj1 fj2)
  (cond ((< (flapjack-x fj1) (flapjack-x fj2))
         true)
        ((> (flapjack-x fj1) (flapjack-x fj2))
         false)
        ((< (flapjack-y fj1) (flapjack-y fj2))
         true)
        ((> (flapjack-y fj1) (flapjack-y fj2))
         false)
        ((< (flapjack-radius fj1) (flapjack-radius fj2))
         true)
        ((> (flapjack-radius fj1) (flapjack-radius fj2))
         false)
        (else
         false)))

;;; insertion-sort : (X -> Boolean) ListOfX -> ListOfX
;;; Given a less-than relation on X and a ListOfX,
;;; returns the list sorted according to the given ordering.

(define (insertion-sort < xs)
  (cond ((empty? xs)
         xs)
        (else
         (insert <
                 (first xs)
                 (insertion-sort < (rest xs))))))

;;; insert : (X -> Boolean) X ListOfX -> ListOfX
;;; Given a less-than relation on X, an X, and a sorted ListOfX,
;;; returns the sorted list obtained by inserting X into the given list.

(define (insert < x xs)
  (cond ((empty? xs)
         (list x))
        ((< x (first xs))
         (cons x xs))
        (else
         (cons (first xs)
               (insert < x (rest xs))))))


(define tests
  (test-suite
   "q2"
   
   (test-case
    "Test #1"
    (check-equal? (flapjack-as-list
                   (make-flapjack 3 4 5))
                  '(flapjack 3 4 5)
                  "something's wrong with make-flapjack"))
   
   (test-case
    "Test #2"
    (check-equal? (let ((sk (make-skillet 8 9 10)))
                    (list (skillet-x sk)
                          (skillet-y sk)
                          (skillet-radius sk)))
                  '(8 9 10)
                  "something's wrong with make-skillet"))
   
   (test-case
    "Test #3"
    (check-equal? (overlapping-flapjacks empty)
                  empty
                  "overlapping-flapjacks fails on empty list"))
   
   (test-case
    "Test #4"
    (check-equal? (map list-of-flapjacks-as-list
                       (map
                        sorted-flapjacks
                        (overlapping-flapjacks
                         (list (make-flapjack -10  2 5)
                               (make-flapjack  -3  0 4)
                               (make-flapjack   4 -2 4.6)
                               (make-flapjack 7.2  6 5)
                               (make-flapjack  20  4 4.2)))))
                  (list (list '(flapjack -10  2 5)
                              '(flapjack  -3  0 4))
                        (list '(flapjack -10  2 5)
                              '(flapjack  -3  0 4)
                              '(flapjack   4 -2 4.6))
                        (list '(flapjack  -3  0 4)
                              '(flapjack   4 -2 4.6)
                              '(flapjack 7.2  6 5))
                        (list '(flapjack   4 -2 4.6)
                              '(flapjack 7.2  6 5))
                        (list '(flapjack  20  4 4.2)))
                  "overlapping-flapjacks fails on example in problem set"))
   
   (test-case
    "Test #5"
    (check-equal? (map list-of-flapjacks-as-list
                       (map
                        sorted-flapjacks
                        (overlapping-flapjacks
                         (list (make-flapjack -30 0 5)
                               (make-flapjack -20 0 5)
                               (make-flapjack 0 0 15)
                               (make-flapjack 25 0 10)
                               (make-flapjack 35 0 2)
                               (make-flapjack 0 40 25)))))
                  '(((flapjack -30 0 5)
                     (flapjack -20 0 5))
                    ((flapjack -30 0 5)
                     (flapjack -20 0 5)
                     (flapjack 0 0 15))
                    ((flapjack -20 0 5)
                     (flapjack 0 0 15)
                     (flapjack 0 40 25)
                     (flapjack 25 0 10))
                    ((flapjack 0 0 15)
                     (flapjack 25 0 10)
                     (flapjack 35 0 2))
                    ((flapjack 25 0 10)
                     (flapjack 35 0 2))
                    ((flapjack 0 0 15)
                     (flapjack 0 40 25)))
                  "overlapping-flapjacks fails on touching flapjacks"))
   
   (test-case
    "Test #6"
    (check-equal? (sorted-flapjacks
                   (non-overlapping-flapjacks empty))
                  empty
                  "non-overlapping-flapjacks fails on empty list"))
   
   (test-case
    "Test #7"
    (check-equal? (map flapjack-as-list
                       (sorted-flapjacks
                        (non-overlapping-flapjacks
                         (list (make-flapjack -10  2 5)
                               (make-flapjack  -3  0 4)
                               (make-flapjack   4 -2 4.6)
                               (make-flapjack 7.2  6 5)
                               (make-flapjack  20  4 4.2)))))
                  '((flapjack 20 4 4.2))
                  "non-overlapping-flapjacks fails on example in problem set"))
   
   (test-case
    "Test #8"
    (check-equal? (map flapjack-as-list
                       (sorted-flapjacks
                        (non-overlapping-flapjacks
                         (list (make-flapjack -10  2 5)
                               (make-flapjack -10  9 1)
                               (make-flapjack  -3  0 4)
                               (make-flapjack  -3  0 4)
                               (make-flapjack   4 -2 4.6)
                               (make-flapjack 7.2  6 5)
                               (make-flapjack  20  4 4.2)))))
                  '((flapjack -10 9 1)
                    (flapjack 20 4 4.2))
                  "non-overlapping-flapjacks fails on a similar example"))
   
   (test-case
    "Test #9"
    (check-equal? (map flapjack-as-list
                       (sorted-flapjacks
                        (flapjacks-in-skillet empty (make-skillet 12 30 10))))
                  empty
                  "flapjacks-in-skillet fails on empty list"))
   
   (test-case
    "Test #10"
    (check-equal? (map flapjack-as-list
                       (sorted-flapjacks
                        (flapjacks-in-skillet
                         (list (make-flapjack -10  2 5)
                               (make-flapjack  -3  0 4)
                               (make-flapjack   4 -2 4.6)
                               (make-flapjack 7.2  6 5)
                               (make-flapjack  20  4 4.2))
                         (make-skillet 2 3 12))))
                  '((flapjack  -3  0 4)
                    (flapjack   4 -2 4.6)
                    (flapjack 7.2  6 5))
                  "flapjacks-in-skillet fails on example in problem set"))
   
   (test-case
    "Test #11"
    (check-equal? (map flapjack-as-list
                       (sorted-flapjacks
                        (flapjacks-in-skillet
                         (list (make-flapjack 10 24 6)
                               (make-flapjack 10 24 7)
                               (make-flapjack 12 30 10)
                               (make-flapjack 12 34 6)
                               (make-flapjack 17 30 5)
                               (make-flapjack 12 30 11)
                               (make-flapjack 12 35 6)
                               (make-flapjack 16.5 30 6))
                         (make-skillet 12 30 10))))
                  '((flapjack 12 30 10)
                    (flapjack 12 34 6)
                    (flapjack 17 30 5))
                  "flapjacks-in-skillet fails on a similar example"))
   
   ))

(run-tests tests 'verbose)
