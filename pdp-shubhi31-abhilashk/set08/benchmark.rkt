;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname benchmark) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "flight.rkt")
(require "q1.rkt")

;;; all-airports : ListOfFlight -> ListOfString
;;; RETURNS: A list of all airports served by some flight
;;;     in the given list.
    
(define (all-airports flights)
  (all-airports-loop (append (map departs flights)
                             (map arrives flights))
                     empty))
    
;;; all-airports-loop : ListOfString ListOfString -> ListOfString
;;; GIVEN: two lists of strings
;;; WHERE: the second list is a set (no element occurs twice)
;;; RETURNS: the set union of the given lists
    
(define (all-airports-loop names airports)
  (if (empty? names)
      airports
      (all-airports-loop (rest names)
                         (if (member (first names) airports)
                             airports
                             (cons (first names) airports)))))
    
;;; visit-all-pairs-of-airports :
;;;     (String String ListOfFlight -> X) ListOfFlight -> ListOfX
;;; GIVEN: a function whose arguments are suitable for can-get-there?
;;; RETURNS: a list of the results obtained by calling that function
;;;     on all pairs of airports served by the given flights,
;;;     passing the given list of flights as a third argument.
    
(define (visit-all-pairs-of-airports visitor flights)
  (let ((airports (all-airports flights)))
    (apply append
           (map (lambda (ap1)
                  (map (lambda (ap2)
                         (visitor ap1 ap2 flights))
                       airports))
                airports))))
    
;;; make-stress-test0 : NonNegInt -> ListOfFlight
;;; GIVEN: a non-negative integer n
;;; RETURNS: a list of 2n flights connecting n+1 airports
    
(define (make-stress-test0 n)
  (if (= n 0)
      empty
      (let* ((name1 (string-append "NoWays " (number->string (+ n n))))
             (name2 (string-append "NoWays " (number->string (+ n n 1))))
             (ap1 (string-append "AP" (number->string n)))
             (ap2 (string-append "AP" (number->string (+ n 1))))
             (t1 (make-UTC (remainder (* 107 n) 24)
                           (remainder (* 223 n) 60)))
             (t2 (make-UTC (remainder (* 151 n) 24)
                           (remainder (* 197 n) 60)))
             (t3 (make-UTC (remainder (* 163 n) 24)
                           (remainder (* 201 n) 60)))
             (t4 (make-UTC (remainder (* 295 n) 24)
                           (remainder (* 183 n) 60)))
             (f1 (make-flight name1 ap1 ap2 t1 t2))
             (f2 (make-flight name2 ap1 ap2 t3 t4)))
        (cons f1 (cons f2 (make-stress-test0 (- n 1)))))))
    
;;; benchmark0 : NonNegInt -> List
;;; GIVEN: a non-negative integer parameter n
;;; RETURNS: a list of length n^2 showing the travel time for
;;;     every pair of airports in a stress test of size Theta(n)
;;; EFFECT: prints the total running time
    
(define (benchmark0 n)
  (let ((flights (make-stress-test0 n)))
    (time (visit-all-pairs-of-airports
           (lambda (ap1 ap2 flights)
             (if (can-get-there? ap1 ap2 flights)
                 (list ap1 ap2 (travel-time ap1 ap2 flights))
                 (list ap1 ap2 -1)))
           flights))))


;(time (airport-tree "AP1" (make-stress-test0 10)))
;(time (airport-tree "AP1" (make-stress-test0 11)))
;(time (airport-tree "AP1" (make-stress-test0 12)))
;(time (airport-tree "AP1" (make-stress-test0 13)))

(define (foo n)
  (let ((ignored (benchmark0 n))) n))


(foo 100)
;(foo 12)
;(foo 14)
;(foo 16)
;(foo 18)
;(foo 20)
;(foo 22)
;(foo 24)
;(foo 26)
;(foo 28)
;(foo 30)
;(foo 32)
;(foo 34)
;(foo 36)
;(foo 38)
;(foo 40)
;(foo 42)
;(foo 44)
;(foo 46)
;(foo 48)
;(foo 50)