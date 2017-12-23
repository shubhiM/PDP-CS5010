;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; q2.rkt: Implements the Flapjack and Skillet data types and functions related
; to them

(require "extras.rkt")
(require rackunit)

(provide make-flapjack)
(provide make-skillet)
(provide flapjack-x)
(provide flapjack-y)
(provide flapjack-radius)
(provide skillet-x)
(provide skillet-y)
(provide skillet-radius)
(provide flapjacks-in-skillet)
(provide overlapping-flapjacks)
(provide non-overlapping-flapjacks)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA DEFINITIONS:

(define-struct flapjack (x y radius))
  
; A Flapjack is a
; -- (make-flapjack Real Real PosReal)
; INTERPRETATION:
; -- x is the x-coordinate of the center of the flapjack
; -- y is the y-coordinate of the center of the flapjack
; -- radius is the radius of the flapjack
;    x and y both represents graphic coordinates

; TEMPLATE
; flapjack-fn : Flapjack -> ??
#|
(define (flapjack-fn f)
  (...
   (flapjack-x f)
   (flapjack-y f)
   (flapjack-radius f)))
|#


(define-struct skillet (x y radius))

; A Skillet is a
; -- (make-skillet Real Real PosReal)
; INTERPRETATION
; -- x is the x-coordinate of the center of the skillet
; -- y is the y-oordinate of the center of the skillet
; -- radius is the radius of the skillet

; TEMPLATE
; skillet-fn : Skillet -> ??
#|
(define (skillet-fn s)
  (...
   (skillet-x s)
   (skillet-y s)
   (skillet-radius s)))
|#


; A ListOfFlapjack (LOF) is either
; -- empty
; -- (cons Flapjack LOF)

; TEMPLATE
; lof-fn : LOF -> ??


#|
(define (lof-fn lst)
  (cond
    [(empty? lst) ...]
    [else
     (...
      (first lst)
      (lof-fn (rest lst)))]))
|#


; A ListOfListOfFlapjack (LOLOF) is either
; -- empty
; -- (cons LOF LOLOF)

; TEMPLATE:
; lolof-fn : LOLOF -> ??

#|
(define (lolof-fn lst)
  (cond [(empty? lst) ...]
        [else
         (...
          (lof-fn (first lst))
          (lolof-fn (rest lst)))]))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-location "04" "q2.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common testing examples

(define list-of-flapjacks
  (list (make-flapjack -10  2 5)
        (make-flapjack  -3  0 4)
        (make-flapjack   4 -2 4.6)
        (make-flapjack 7.2  6 5)
        (make-flapjack  20  4 4.2)))

(define skillet-1 (make-skillet 2 3 12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; HELPER FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; from-index: List PosInt -> ??
;; GIVEN : A list and an index of the element to be returned
;; WHERE: index is greater than or equal to 0 and less than lenght of given list
;; RETURNS: The element at given index from the given list
;; EXAMPLES:
;; -- (from-index list-of-flapjacks 0) = (make-flapjack -10  2 5)
;; -- (from-index list-of-flapjacks 1) = (make-flapjack 20  4 4.2)
;; DESIGN STRATEGY: combine simpler functions

(define (from-index lst index)
  (list-ref lst index))


; distance-between-centers : Real Real Real Real -> Real
; GIVEN x and y coordinates of two points
; RETURNS: the distance between the two points
; EXAMPLES:
;  -- (distance-between-centers -10 -3 2 0) = #i7.280109889280518
;  -- (distance-between-centers 1 2 3 4) = #i1.4142135623730951
; DESIGN STRATEGY: combine simpler functions

(define (distance-between-centers x1 x2 y1 y2)
  (sqrt (+ (sqr (- x2 x1))
           (sqr (- y2 y1)))))

; distance-between-centers-of-flapjack-and-skillet : Flapjack Skillet -> Real
; GIVEN: A flapjack and a skillet
; RETURNS: the distance between the centers of the flapjack and the skillet
; EXAMPLES:
;(distance-between-centers-of-flapjack-and-skillet
; (first list-of-flapjacks) skillet-1) = #i12.041594578792296
; DESIGN STRATEGY: combine simpler functions

(define (distance-between-centers-of-flapjack-and-skillet f s)
  (distance-between-centers
   (flapjack-x f) (skillet-x s) (flapjack-y f) (skillet-y s)))


; distance-between-centers-of-flapjacks : Flapjack Flapjack -> Real
; GIVEN: Two flapjacks
; RETURNS: the distance between the centers of two flapjacks
; EXAMPLES:
#|
(distance-between-centers-of-flapjacks
 (first list-of-flapjacks)
 (first (rest list-of-flapjacks))) = #i7.280109889280518
|#
; DESIGN STRATEGY: combine simpler functions

(define (distance-between-centers-of-flapjacks f1 f2)
  (distance-between-centers
   (flapjack-x f1) (flapjack-x f2) (flapjack-y f1) (flapjack-y f2)))

; sum-of-radius: Flapjack Flapjack -> Real
; GIVEN: Two flapjacks
; RETURNS: The sum of the radius of the two given flapjacks
; EXAMPLES:
#|
(sum-of-radius (first list-of-flapjacks)
               (first (rest list-of-flapjacks))) = 9
|#
;DESIGN STRATEGY: combine simpler functions

(define (sum-of-radius f1 f2)
  (+ (flapjack-radius f1) (flapjack-radius f2)))


; overlaps? : Flapjack Flapjack -> Boolean
; RETURNS: true if two flapjacks overlap
; EXAMPLES:
#|
(overlaps? (first list-of-flapjacks)
           (first (rest list-of-flapjacks))) = true
|#
; DESIGN STRATEGY: combine simpler functions

(define (overlaps? f1 f2)
  (< (distance-between-centers-of-flapjacks f1 f2) (sum-of-radius f1 f2)))


; inside-of? Flapjack Skillet -> Boolean
; RETURNS: true if flapjack is entirely inside the given skillet
; EXAMPLES:
; -- (inside-of? (first list-of-flapjacks) skillet-1) = false
; -- (inside-of? (first (rest list-of-flapjacks)) skillet-1) = true
; DESIGN STRATEGY: combine simpler functions
(define (inside-of? f s)
  (<= (+ (distance-between-centers-of-flapjack-and-skillet f s)
         (flapjack-radius f))
      (skillet-radius s)))


; overlaps-with : Flapjack ListOfFlapjack -> ListOfFlapjack
; RETURNS: A list of flapjacks from the given list of flapjacks
;  that overlap with the given flapjack
; EXAMPLES:
#|
(overlaps-with
 (first list-of-flapjacks)
 list-of-flapjacks) = (list (make-flapjack -10 2 5) (make-flapjack -3 0 4))
|#
; DESIGN STRATEGY: use template for ListOfFlapjack (LOF)

(define (overlaps-with f lof)
  (cond [(empty? lof) empty]
        [else (if (overlaps? f (first lof))
                  (cons (first lof) (overlaps-with  f (rest lof)))
                  (overlaps-with f (rest lof)))]))


(begin-for-test
  (check-equal?
   (overlaps-with (first list-of-flapjacks) empty)
   empty
   "overlaps-with should return empty if an empty list is supplied"))

; overlapping? : Flapjack ListOfFlapjack : Boolean
; RETURNS: true if the given flapjack overlaps with any of the flapjacks in
; the given list
; EXAMPLES:
; -- (overlapping? (first list-of-flapjacks) list-of-flapjacks) = true
; DESIGN STRATEGY: combine simpler functions

(define (overlapping? f lof)
  (> (length (overlaps-with f lof)) 1))

; list-of-overlapping-flapjacks : PosInteger LOF -> LOLOF
; GIVEN: A list of flapjacks and a counter with value ranging from 0 to
; length of the list of flapjacks
; RETURNS: a list of the same length whose i-th element is a list of flapjacks
; in the given list that overlap with the i-th flapjack in the given list
; EXAMPLES:
#|
(list-of-overlapping-flapjacks
 0 list-of-flapjacks) = list-of-list-of-overlapping-flapjacks
|#
; DESIGN STRATEGY: use template for ListOfFlapjack (LOF)
(define (list-of-overlapping-flapjacks counter lof)
  (cond [(empty? lof) empty]
        [else
         (if (< counter (length lof))
             (cons
              (overlaps-with (from-index lof counter) lof)
              (list-of-overlapping-flapjacks (+ 1 counter) lof))
             empty)]))



; no-overlap-list : PosInt Flapjack ListOfFlapjack -> ListOfFlapjack
; GIVEN: An index value, flapjack and list of flapjacks
; RETURNS: List of flapjacks that do not overlap
; EXAMPLES:
#|
(no-overlap-list
 0
 (first list-of-flapjacks) list-of-flapjacks) = (list (make-flapjack 20 4 4.2))
|#
; DESIGN STRATEGY: use simpler functions


(define (no-overlap-list counter f lof)
  (if (not (overlapping? f lof))
      (cons f (list-of-non-overlapping-flapjacks (+ 1 counter) lof))
      (list-of-non-overlapping-flapjacks (+ 1 counter) lof)))


; list-of-non-overlapping-flapjacks : PosInteger LOF -> LOLOF
; GIVEN: A list of flapjacks and a counter with value ranging from 0 to
; length of the list of flapjacks
; RETURNS: A list of the flapjacks in the given list that do not overlap with
; any other flapjacks in the list
; EXAMPLES:
#|
(list-of-non-overlapping-flapjacks
 0 list-of-flapjacks) = (list (make-flagjack 20 4 4.2))
|#
; DESIGN STRATEGY: use template for ListOfFlapjack (LOF)

(define (list-of-non-overlapping-flapjacks counter lof)
  (cond [(empty? lof) empty]
        [else
         (if (< counter (length lof))
             (no-overlap-list counter (from-index lof counter) lof)
             empty)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN FUNCTIONS 


; Initial counter is the default value used for iterating over list of flipjacks
(define initial-counter 0)


; overlapping-flapjacks : ListOfFlapjack -> ListOfListOfFlapjack
; GIVEN : A list of Flapjacks
; RETURNS: a list of the same length whose i-th element is a list of flapjacks
;  in the given list that overlap with the i-th flapjack in the given list
; EXAMPLES:
; (overlapping-flapjacks empty)  =>  empty
; (overlapping-flapjacks
;    list-of-flapjacks) = list-of-list-of-overlapping-flapjacks
; DESIGN STRATEGY: combine simpler functions

(define (overlapping-flapjacks lof)
 (list-of-overlapping-flapjacks initial-counter lof))


;; examples for test
(define list-of-list-of-overlapping-flapjacks
  (list
   (list (make-flapjack -10 2 5)
         (make-flapjack  -3  0 4))
   (list (make-flapjack -10  2 5)
         (make-flapjack  -3  0 4)
         (make-flapjack   4 -2 4.6))
   (list (make-flapjack  -3  0 4)
         (make-flapjack   4 -2 4.6)
         (make-flapjack 7.2  6 5))
   (list (make-flapjack   4 -2 4.6)
         (make-flapjack 7.2  6 5))
   (list (make-flapjack  20  4 4.2))))


;; TESTS

(begin-for-test
  (check-equal?
   (overlapping-flapjacks list-of-flapjacks)
   list-of-list-of-overlapping-flapjacks
   " list of overlapping flapjacks")
  (check-equal?
   (overlapping-flapjacks empty)
   empty
   " list of overlapping flapjacks is empty"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;non-overlapping-flapjacks : ListOfFlapjack -> ListOfFlapjack
;GIVEN: a list of flapjacks
;RETURNS: a list of flapjacks in the given list that do not overlap with any
; other flapjacks in the list
; EXAMPLES:
; -- (non-overlapping-flapjacks empty) = empty
; -- (non-overlapping-flapjacks
;      list-of-flapjacks) = list-of-flapjacks-nonoverlapping
; DESIGN STRATEGY: combine simpler functions

(define (non-overlapping-flapjacks lof)
  (list-of-non-overlapping-flapjacks initial-counter lof))


;; example for test
(define list-of-flapjacks-nonoverlapping
  (list (make-flapjack 20 4 4.2)))

;; TESTS

(begin-for-test
  (check-equal?
   (non-overlapping-flapjacks list-of-flapjacks)
   list-of-flapjacks-nonoverlapping
   "list of nonoverlapping flapjacks")
  (check-equal?
   (non-overlapping-flapjacks empty)
   empty
   "list of nonoverlapping flapjacks is empty"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; flapjacks-in-skillet: ListOfFlapjack Skillet -> ListOfFlapjack
; GIVEN: A list of flapjacks and  a skillet
; RETURNS: a list of the given flapjacks that fit entirely within the skillet
; EXAMPLES:
; -- (flapjacks-in-skillet empty skillet-1) = empty
; -- (flapjacks-in-skillet
;       list-of-flapjacks skillet-1) = list-of-flapjacks-inside-skillet
; DESIGN STRATEGY: used template for ListOfFlapjack (LOF)

(define (flapjacks-in-skillet lof s)
  (cond [(empty? lof) empty]
        [else
         (if (inside-of? (first lof) s)
             (cons (first lof) (flapjacks-in-skillet (rest lof) s))
             (flapjacks-in-skillet (rest lof) s))]))

;; constants for testing if flapjacks are entirely inside a skillet
(define list-of-flapjacks-inside-skillet
  (list (make-flapjack  -3  0 4)
        (make-flapjack   4 -2 4.6)
        (make-flapjack 7.2  6 5)))

;; Tests
(begin-for-test
  (check-equal?
   (flapjacks-in-skillet list-of-flapjacks skillet-1)
   list-of-flapjacks-inside-skillet
   "list of flapjacks inside skillet")
  
  (check-equal?
   (flapjacks-in-skillet empty skillet-1)
   empty
   "empty list is returned as list of flapjacks is empty"))