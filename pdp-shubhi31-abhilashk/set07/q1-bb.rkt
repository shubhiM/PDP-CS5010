#lang racket

(require rackunit)
(require rackunit/text-ui)
(require racket/sandbox)
(require "flight.rkt")
(require "q1.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Black-box tests.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Examples given in problem set.

(define panAmFlights '())

(define deltaFlights
  (let ((u make-UTC)) ; so u can abbreviate make-UTC
    (list
     (make-flight "Delta 0121" "LGA" "MSP" (u 11 00) (u 14 09))
     (make-flight "Delta 1609" "MSP" "DEN" (u 20 35) (u 22 52))
     (make-flight "Delta 5703" "DEN" "LAX" (u 14 04) (u 17 15))
     (make-flight "Delta 2077" "LAX" "PDX" (u 17 35) (u 20 09))
     (make-flight "Delta 2163" "MSP" "PDX" (u 15 00) (u 19 02)))))

;;; New tests, not based on examples in the problem set.

;;; itinerary=? : ListOfFlight ListOfFlight -> Boolean
;;; RETURNS: true iff the two itineraries are equal
;;; EXAMPLES: see tests below

(define (itinerary=? it1 it2)
  (and (= (length it1) (length it2))
       (andmap (lambda (bool) bool)
               (map flight=? it1 it2))))

(define syntheticFlights
  (list (make-flight "1010" "UVW" "XYZ" (make-UTC 23 59) (make-UTC  4 20)) ;420
        (make-flight "2020" "UVW" "XYZ" (make-UTC  3 00) (make-UTC  7 15))
        (make-flight "3030" "UVW" "XYZ" (make-UTC 19 45) (make-UTC  0  0))
        (make-flight "0100" "RST" "XYZ" (make-UTC 10 00) (make-UTC 17 55))
        (make-flight "0101" "RST" "UVW" (make-UTC  7 10) (make-UTC 12 05)) ;500
        (make-flight "0102" "RST" "UVW" (make-UTC 11 59) (make-UTC  4 50))
        (make-flight "0103" "RST" "UVW" (make-UTC 15 15) (make-UTC 20 20))
        (make-flight "0201" "OPQ" "RST" (make-UTC  6 20) (make-UTC  9 00)) ;230
        (make-flight "0202" "OPQ" "RST" (make-UTC  9 05) (make-UTC 11 30))
        (make-flight "0203" "OPQ" "RST" (make-UTC 14 30) (make-UTC 17 01))
        (make-flight "0204" "OPQ" "UVW" (make-UTC 10 00) (make-UTC 16 58))
        (make-flight "0301" "LMN" "OPQ" (make-UTC  8 19) (make-UTC 11 30)) ;300
        (make-flight "0302" "LMN" "OPQ" (make-UTC 23 00) (make-UTC  2 15))
        (make-flight "0401" "IJK" "LMN" (make-UTC  5 45) (make-UTC 11 35)) ;600
        (make-flight "0402" "IJK" "LMN" (make-UTC  9 35) (make-UTC 15 25))
        (make-flight "0403" "IJK" "LMN" (make-UTC 13 30) (make-UTC 19 15))
        (make-flight "0404" "IJK" "LMN" (make-UTC 23 20) (make-UTC  5 45))
        (make-flight "0501" "FGH" "IJK" (make-UTC 10 48) (make-UTC 12 15)) ;120
        (make-flight "0502" "FGH" "IJK" (make-UTC 14 20) (make-UTC 15 45))
        (make-flight "0503" "FGH" "IJK" (make-UTC 18 30) (make-UTC 19 50))
        (make-flight "0504" "FGH" "UVW" (make-UTC 20 30) (make-UTC 13 00))
        (make-flight "0601" "CDE" "FGH" (make-UTC  7 45) (make-UTC  8 40)) ;100
        (make-flight "0602" "CDE" "FGH" (make-UTC  9 45) (make-UTC 10 44))
        (make-flight "0603" "CDE" "FGH" (make-UTC 16 45) (make-UTC 17 50))
        (make-flight "0604" "CDE" "FGH" (make-UTC 18 45) (make-UTC 19 48))))

(define synthetic-airports
  (list "CDE" "FGH" "IJK" "LMN" "OPQ" "RST" "UVW" "XYZ"))

(define time-limit-s 20)
(define memory-limit-mb 128)

(define tests
  (test-suite
   "q1"
   
   (test-case
    "Test #1"
    (with-limits time-limit-s memory-limit-mb
                 (check-equal? (can-get-there? "06N" "JFK" panAmFlights)
                               false
                               "but you can't get anywhere from 06N")
                 ))
   (test-case
    "Test #2"
    (with-limits time-limit-s memory-limit-mb
                 (check-equal? (can-get-there? "JFK" "JFK" panAmFlights)
                               true
                               "but the trivial itinerary goes from JFK to JFK")
                 ))
   (test-case
    "Test #3"
    (with-limits time-limit-s memory-limit-mb
                 (check-equal? (can-get-there? "06N" "LAX" deltaFlights)
                               false
                               "but you can't get to LAX from 06N")
                 ))
   (test-case
    "Test #4"
    (with-limits time-limit-s memory-limit-mb
                 (check-equal? (can-get-there? "LAX" "06N" deltaFlights)
                               false
                               "but you can't get to 06N from LAX")
                 ))
   (test-case
    "Test #5"
    (with-limits time-limit-s memory-limit-mb
                 (check-equal? (can-get-there? "LGA" "PDX" deltaFlights)
                               true
                               "but you can get to PDX from LGA")
                 ))
   (test-case
    "Test #6"
    (with-limits time-limit-s memory-limit-mb
                 (check-equal? (fastest-itinerary "JFK" "JFK" panAmFlights)
                               empty
                               "wrong itinerary for trivial case")
                 ))
   (test-case
    "Test #7"
    (with-limits time-limit-s memory-limit-mb
                 (check-equal? (let ((it (fastest-itinerary "LGA" "PDX" deltaFlights))
                                     (f1 (make-flight "Delta 0121"
                                                      "LGA" "MSP"
                                                      (make-UTC 11 00) (make-UTC 14 09)))
                                     (f2 (make-flight "Delta 2163"
                                                      "MSP" "PDX"
                                                      (make-UTC 15 00) (make-UTC 19 02))))
                                 (list (flight=? (first it) f1)
                                       (flight=? (second it) f2)
                                       (= (length it) 2)))
                               (list true true true)
                               "returned wrong answer for LGA to PDX on Delta")
                 ))
   (test-case
    "Test #8"
    (with-limits time-limit-s memory-limit-mb
                 (check-equal? (travel-time "JFK" "JFK" panAmFlights)
                               0
                               "nonzero travel time for trivial travel from JFK to JFK")
                 ))
   (test-case
    "Test #9"
    (with-limits time-limit-s memory-limit-mb
                 (check-equal? (fastest-itinerary "JFK" "JFK" panAmFlights)
                               empty
                               "wrong itinerary for flying from JFK to JFK on Pan Am")
                 ))
   (test-case
    "Test #10"
    (with-limits time-limit-s memory-limit-mb
                 (check-equal? (travel-time "LGA" "PDX" deltaFlights)
                               482
                               "wrong travel time for LGA to PDX")
                 ))
   ;;; New tests, not based on examples in the problem set.
   (test-case
    "Test #11"
    (with-limits (* time-limit-s 36) memory-limit-mb
                 (check-equal? (map (lambda (ap1)
                                      (map (lambda (ap2)
                                             (can-get-there? ap1 ap2 syntheticFlights))
                                           synthetic-airports))
                                    synthetic-airports)
                               (list (list true true true true true true true true)
                                     (list false true true true true true true true)
                                     (list false false true true true true true true)
                                     (list false false false true true true true true)
                                     (list false false false false true true true true)
                                     (list false false false false false true true true)
                                     (list false false false false false false true true)
                                     (list false false false false false false false true))
                               "can-get-there? fails on some synthetic airports")
                 ))
   (test-case
    "Test #12"
    (with-limits (* time-limit-s 36) memory-limit-mb
                 (check-equal? (letrec ((map-rest (lambda (f lst)
                                                    (if (null? lst)
                                                        empty
                                                        (cons (f lst)
                                                              (map-rest f (rest lst)))))))
                                       (map-rest (lambda (airports)
                                                   (let ((ap1 (first airports)))
                                                     (map (lambda (ap2)
                                                            (travel-time ap1 ap2 syntheticFlights))
                                                          airports)))
                                                 synthetic-airports))
                               (list (list 0 55 150 570 990 1395 1095 1755)    ; CDE
                                     (list    0  80 507 927 1332  990 1650)    ; FGH
                                     (list        0 345 730 1061 1648 1705)    ; IJK
                                     (list            0 191  522 1078 1135)    ; LMN
                                     (list                0  145  418  695)    ; OPQ
                                     (list                     0  295  475)    ; RST
                                     (list                          0  255)    ; UVW
                                     (list                               0)))  ; XYZ
                 ))
   (test-case
    "Test #13"
    (with-limits time-limit-s memory-limit-mb
                 (check-equal?
                  (itinerary=?
                   (fastest-itinerary "CDE" "RST" syntheticFlights)
                   (list (make-flight "0602" "CDE" "FGH" (make-UTC  9 45) (make-UTC 10 44))
                         (make-flight "0501" "FGH" "IJK" (make-UTC 10 48) (make-UTC 12 15))
                         (make-flight "0403" "IJK" "LMN" (make-UTC 13 30) (make-UTC 19 15))
                         (make-flight "0302" "LMN" "OPQ" (make-UTC 23 00) (make-UTC  2 15))
                         (make-flight "0201" "OPQ" "RST" (make-UTC  6 20) (make-UTC  9 00))))
                  true
                  "wrong itinerary for CDE to RST")
                 ))
   (test-case
    "Test #14"
    (with-limits time-limit-s memory-limit-mb
                 (check-equal?
                  (itinerary=?
                   (fastest-itinerary "CDE" "UVW" syntheticFlights)
                   (list (make-flight "0604" "CDE" "FGH" (make-UTC 18 45) (make-UTC 19 48))
                         (make-flight "0504" "FGH" "UVW" (make-UTC 20 30) (make-UTC 13  0))))
                  true
                  "wrong itinerary for CDE to UVW")
                 ))
   (test-case
    "Test #15"
    (with-limits time-limit-s memory-limit-mb
                 (check-equal?
                  (itinerary=?
                   (fastest-itinerary "IJK" "XYZ" syntheticFlights)
                   (list (make-flight "0403" "IJK" "LMN" (make-UTC 13 30) (make-UTC 19 15))
                         (make-flight "0302" "LMN" "OPQ" (make-UTC 23 00) (make-UTC  2 15))
                         (make-flight "0201" "OPQ" "RST" (make-UTC  6 20) (make-UTC  9 00))
                         (make-flight "0100" "RST" "XYZ" (make-UTC 10 00) (make-UTC 17 55))))
                  true
                  "wrong itinerary for IJK to XYZ")
                 ))))

(run-tests tests 'verbose)
