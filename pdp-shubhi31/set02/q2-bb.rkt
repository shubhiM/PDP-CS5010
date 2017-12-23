#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "q2.rkt")

;;; Black-box testing.

;;; Help functions for black-box testing.

;;; bb-accepts? : ListOfLegalInput -> Boolean
;;; Given: a list of LegalInput
;;; Returns: true iff the machine should accept that sequence

(define (bb-accepts? inputs)
  (bb-accepts-from-state? (initial-state 3.14159) inputs))

;;; bb-viable? : ListOfLegalInput -> Boolean
;;; Given: a list of LegalInput
;;; Returns: true iff that sequence is a viable prefix
;;;     (i.e. a prefix of some sequence that would be accepted)

(define (bb-viable? inputs)
  (bb-viable-from-state? (initial-state "nineteen") inputs))

;;; bb-accepts-from-state? : State ListOfLegalInput -> Boolean
;;; Given: a state and a list of LegalInput
;;; Returns: true iff the machine, starting in that state,
;;;     should accept that sequence

(define (bb-accepts-from-state? s inputs)
  (if (empty? inputs)
      (accepting-state? s)
      (bb-accepts-from-state? (next-state s (first inputs))
                              (rest inputs))))

;;; bb-viable-from-state? : ListOfLegalInput -> Boolean
;;; Given: a state and a list of LegalInput
;;; Returns: true iff that sequence is a viable prefix
;;;     for the machine starting from the given state
;;;     (i.e. a prefix of some sequence that would be accepted)

(define (bb-viable-from-state? s inputs)
  (if (empty? inputs)
      (not (rejecting-state? s))
      (bb-viable-from-state? (next-state s (first inputs))
                             (rest inputs))))

;;; Black-box tests proper.

(define tests
  (test-suite
   "q2"

   (test-case
    "Test #1"
    (check-equal? (bb-accepts? '())
                  false
                  "machine should not accept empty sequence"))

   (test-case
    "Test #2"
    (check-equal? (bb-viable? '())
                  true
                  "machine should not start out in a rejecting state"))

   (test-case
    "Test #3"
    (check-equal? (bb-accepts? (list "e"))
                  false
                  "machine should accept e only at end"))

   (test-case
    "Test #4"
    (check-equal? (bb-viable? (list "e"))
                  false
                  "machine should reject sequences starting with e"))

   (test-case
    "Test #5"
    (check-equal? (bb-accepts? (list "p"))
                  false
                  "machine should not accept p by itself"))

   (test-case
    "Test #6"
    (check-equal? (bb-viable? (list "p"))
                  true
                  "machine should not reject sequences starting with p"))

   (test-case
    "Test #7"
    (check-equal? (bb-accepts? (list "s"))
                  false
                  "machine should not accept s by itself"))

   (test-case
    "Test #8"
    (check-equal? (bb-viable? (list "s"))
                  true
                  "machine should not reject sequences starting with s"))

   (test-case
    "Test #9"
    (check-equal? (bb-accepts? (list "d"))
                  true
                  "machine should accept d all by itself"))

   (test-case
    "Test #10"
    (check-equal? (bb-viable? (list "d"))
                  true
                  "machine should not reject sequences starting with d"))

   (test-case
    "Test #11"
    (check-equal? (bb-accepts? (list "d" "d" "e"))
                  true
                  "machine should accept dde"))

   (test-case
    "Test #12"
    (check-equal? (bb-viable? (list "d" "d" "e"))
                  true
                  "machine should not reject sequences starting with dde"))

   (test-case
    "Test #13"
    (check-equal? (bb-accepts? (list "s" "d"))
                  true
                  "machine should accept sd"))

   (test-case
    "Test #14"
    (check-equal? (bb-viable? (list "s" "d"))
                  true
                  "machine should not reject sequences starting with sd"))

   (test-case
    "Test #15"
    (check-equal? (bb-accepts? (list "s" "d" "d"))
                  true
                  "machine should accept sdd"))

   (test-case
    "Test #16"
    (check-equal? (bb-viable? (list "s" "d" "d"))
                  true
                  "machine should not reject sequences starting with sdd"))

   (test-case
    "Test #17"
    (check-equal? (bb-accepts? (list "s" "p"))
                  false
                  "machine should not accept sp"))

   (test-case
    "Test #18"
    (check-equal? (bb-viable? (list "s" "p"))
                  true
                  "machine should not reject sequences starting with sp"))

   (test-case
    "Test #19"
    (check-equal? (bb-accepts? (list "s" "p" "d"))
                  true
                  "machine should accept spd"))

   (test-case
    "Test #20"
    (check-equal? (bb-viable? (list "s" "p" "d"))
                  true
                  "machine should not reject sequences starting with spd"))

   (test-case
    "Test #21"
    (check-equal? (bb-accepts? (list "d" "p" "d" "p" "d"))
                  false
                  "machine should not accept dpdpd"))

   (test-case
    "Test #22"
    (check-equal? (bb-viable? (list "d" "p" "d" "p" "d"))
                  false
                  "machine should reject sequences starting with dpdpd"))

   (test-case
    "Test #23"
    (check-equal? (bb-viable? (list "s" "s"))
                  false
                  "machine should reject sequences starting with ss"))

   (test-case
    "Test #24"
    (check-equal? (bb-accepts? (list "d" "p" "d" "d" "e"))
                  true
                  "machine should accept dpdde"))

   (test-case
    "Test #25"
    (check-equal? (bb-viable? (list "d" "p" "d" "d" "e"))
                  true
                  "machine should not reject sequences starting with dpdde"))

   (test-case
    "Test #26"
    (check-equal? (bb-accepts? (list "d" "p" "d" "d" "e" "e"))
                  false
                  "machine should not accept dpddee"))

   (test-case
    "Test #27"
    (check-equal? (bb-viable? (list "d" "p" "d" "d" "e" "e"))
                  false
                  "machine should reject sequences starting with dpddee"))

   (test-case
    "Test #28"
    (check-equal? (bb-viable? (list "d" "d" "s"))
                  false
                  "machine should reject sequences starting with dds"))

   (test-case
    "Test #29"
    (check-equal? (bb-viable? (list "d" "d" "p" "p"))
                  false
                  "machine should reject sequences starting with ddpp"))

   (test-case
    "Test #30"
    (check-equal? (bb-viable? (list "d" "d" "p" "s"))
                  false
                  "machine should reject sequences starting with ddps"))

   (test-case
    "Test #31"
    (check-equal? (bb-viable? (list "d" "d" "p" "e"))
                  false
                  "machine should reject sequences starting with ddpe"))

   (test-case
    "Test #32"
    (check-equal? (bb-viable? (list "s" "e"))
                  false
                  "machine should reject sequences starting with se"))

   (test-case
    "Test #33"
    (check-equal? (bb-viable? (list "s" "p" "p"))
                  false
                  "machine should reject sequences starting with spp"))

   (test-case
    "Test #34"
    (check-equal? (bb-viable? (list "s" "p" "s"))
                  false
                  "machine should reject sequences starting with sps"))

   (test-case
    "Test #35"
    (check-equal? (bb-viable? (list "s" "p" "e"))
                  false
                  "machine should reject sequences starting with spe"))

   (test-case
    "Test #36"
    (check-equal? (bb-viable? (list "d" "d" "p" "d" "s"))
                  false
                  "machine should reject sequences starting with ddpds"))

   ))

(run-tests tests 'verbose)
