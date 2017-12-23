#lang racket

(require rackunit)
(require rackunit/text-ui)
(require 2htdp/image)
(require 2htdp/universe)
(require "flight.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Black-box tests.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tests
  (test-suite
   "q1"
   
   (test-case
    "Test #1"
    (check-equal? (UTC-hour (make-UTC 0 59))
                  0
                  "UTC-hour doesn't work on (make-UTC 0 59)")
    )
   (test-case
    "Test #2"
    (check-equal? (UTC-minute (make-UTC 0 59))
                  59
                  "UTC-minute doesn't work on (make-UTC 0 59)")
    )
   (test-case
    "Test #3"
    (check-equal? (UTC-hour (make-UTC 23 0))
                  23
                  "UTC-hour doesn't work on (make-UTC 23 0)")
    )
   (test-case
    "Test #4"
    (check-equal? (UTC-minute (make-UTC 23 0))
                  0
                  "UTC-minute doesn't work on (make-UTC 23 0)")
    )
   (test-case
    "Test #5"
    (check-equal? (UTC=? (make-UTC 12 13) (make-UTC 12 13))
                  true
                  "UTC=? doesn't return true when it should")
    )
   (test-case
    "Test #6"
    (check-equal? (UTC=? (make-UTC 14 15) (make-UTC 14 16))
                  false
                  "UTC=? doesn't return false when the minute is different")
    
    )
   (test-case
    "Test #7"
    (check-equal? (UTC=? (make-UTC 14 15) (make-UTC 13 15))
                  false
                  "UTC=? doesn't return false when the hour is different")
    )
   (test-case
    "Test #8"
    (check-equal? (let* ((u0 (make-UTC 0 0))
                         (u1 (make-UTC 23 59))
                         (f (make-flight "Braniff 278" "SF" "LA" u0 u1)))
                    (list (flight-name f)
                          (departs f)
                          (arrives f)
                          (UTC-hour (departs-at f))
                          (UTC-minute (departs-at f))
                          (UTC-hour (arrives-at f))
                          (UTC-minute (arrives-at f))))
                  (list "Braniff 278" "SF" "LA" 0 0 23 59)
                  "wrong answer for Braniff 278")
    )
   (test-case
    "Test #9"
    (check-equal? (let* ((u1 (make-UTC 0 59))
                         (u2 (make-UTC 23 01))
                         (u3 (make-UTC 0 59))
                         (u4 (make-UTC 23 1))
                         (f1 (make-flight "United 3865" "MSP" "PDX" u1 u2))
                         (f2 (make-flight "United 3865" "MSP" "PDX" u3 u4)))
                    (flight=? f1 f2))
                  true
                  "flight=? should have returned true")
    )
   (test-case
    "Test #10"
    (check-equal? (let* ((u1 (make-UTC 0 58))
                         (u2 (make-UTC 23 01))
                         (u3 (make-UTC 0 59))
                         (u4 (make-UTC 23 1))
                         (f1 (make-flight "United 3865" "MSP" "PDX" u1 u2))
                         (f2 (make-flight "United 3865" "MSP" "PDX" u3 u4)))
                    (flight=? f1 f2))
                  false
                  "but 58 is different from 59")
    )
   (test-case
    "Test #11"
    (check-equal? (let* ((u1 (make-UTC 1 59))
                         (u2 (make-UTC 23 01))
                         (u3 (make-UTC 0 59))
                         (u4 (make-UTC 23 1))
                         (f1 (make-flight "United 3865" "MSP" "PDX" u1 u2))
                         (f2 (make-flight "United 3865" "MSP" "PDX" u3 u4)))
                    (flight=? f1 f2))
                  false
                  "but 1 is different from 0")
    )
   (test-case
    "Test #12"
    (check-equal? (let* ((u1 (make-UTC 0 59))
                         (u2 (make-UTC 23 01))
                         (u3 (make-UTC 0 59))
                         (u4 (make-UTC 22 1))
                         (f1 (make-flight "United 3865" "MSP" "PDX" u1 u2))
                         (f2 (make-flight "United 3865" "MSP" "PDX" u3 u4)))
                    (flight=? f1 f2))
                  false
                  "but 22 is not 23")
    )
   (test-case
    "Test #13"
    (check-equal? (let* ((u1 (make-UTC 0 59))
                         (u2 (make-UTC 23 01))
                         (u3 (make-UTC 0 59))
                         (u4 (make-UTC 23 7))
                         (f1 (make-flight "United 3865" "MSP" "PDX" u1 u2))
                         (f2 (make-flight "United 3865" "MSP" "PDX" u3 u4)))
                    (flight=? f1 f2))
                  false
                  "but 1 is not 7")
    )
   (test-case
    "Test #14"
    (check-equal? (let* ((u1 (make-UTC 0 59))
                         (u2 (make-UTC 23 01))
                         (u3 (make-UTC 0 59))
                         (u4 (make-UTC 23 1))
                         (f1 (make-flight "United 3865" "MSP" "PdX" u1 u2))
                         (f2 (make-flight "United 3865" "MSP" "PDX" u3 u4)))
                    (flight=? f1 f2))
                  false
                  "but PdX is not PDX")
    )
   (test-case
    "Test #15"
    (check-equal? (let* ((u1 (make-UTC 0 59))
                         (u2 (make-UTC 23 01))
                         (u3 (make-UTC 0 59))
                         (u4 (make-UTC 23 1))
                         (f1 (make-flight "United 3865" "MSP" "PDX" u1 u2))
                         (f2 (make-flight "United 3865" "LAX" "PDX" u3 u4)))
                    (flight=? f1 f2))
                  false
                  "but LAX is not MSP")
    )
   (test-case
    "Test #16"
    (check-equal? (let* ((u1 (make-UTC 0 59))
                         (u2 (make-UTC 23 01))
                         (u3 (make-UTC 0 59))
                         (u4 (make-UTC 23 1))
                         (f1 (make-flight "United 3864" "MSP" "PDX" u1 u2))
                         (f2 (make-flight "United 3865" "MSP" "PDX" u3 u4)))
                    (flight=? f1 f2))
                  false
                  "but 3864 is not 3865")
    )))

(run-tests tests 'verbose)
