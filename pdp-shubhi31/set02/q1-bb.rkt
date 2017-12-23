#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "q1.rkt")

(define tests
  (test-suite
   "q1"

   ;;; Tests of normal character insertion.

   (test-case
    "Test #1"
     (check-equal? (edit (make-editor "" "") "a")
                   (make-editor "a" "")
                   "incorrect insertion of letter into empty editor"))
   (test-case
    "Test #2"
     (check-equal? (edit (make-editor "a" "") "c")
                   (make-editor "ac" "")
                   "incorrect insertion of letter after single character"))
   (test-case
    "Test #3"
     (check-equal? (edit (make-editor "" "b") "a")
                   (make-editor "a" "b")
                   "incorrect insertion of letter before single character"))
   (test-case
    "Test #4"
     (check-equal? (edit (make-editor "a" "b") "c")
                   (make-editor "ac" "b")
                   "incorrect insertion of letter between characters"))

   ;;; Tests of ignoring tab and return characters.

   (test-case
    "Test #5"
     (check-equal? (edit (make-editor "ac" "b") "\t")
                   (make-editor "ac" "b")
                   "should have ignored tab character"))
   (test-case
    "Test #6"
     (check-equal? (edit (make-editor "ac" "b") "\r")
                   (make-editor "ac" "b")
                   "should have ignored return character"))

   ;;; Tests of "left" and "right" keys.

   (test-case
    "Test #7"
     (check-equal? (edit (make-editor "" "") "left")
                   (make-editor "" "")
                   "left key should leave empty editor unchanged"))
   (test-case
    "Test #8"
     (check-equal? (edit (make-editor "a" "") "left")
                   (make-editor "" "a")
                   "left key misbehaved on single-character editor"))
   (test-case
    "Test #9"
     (check-equal? (edit (make-editor "" "b") "left")
                   (make-editor "" "b")
                   "left key should have left editor unchanged"))
   (test-case
    "Test #10"
     (check-equal? (edit (make-editor "a" "b") "left")
                   (make-editor "" "ab")
                   "left key should have moved cursor to left"))
   (test-case
    "Test #11"
     (check-equal? (edit (make-editor "" "") "right")
                   (make-editor "" "")
                   "right key should leave empty editor unchanged"))
   (test-case
    "Test #12"
     (check-equal? (edit (make-editor "a" "") "right")
                   (make-editor "a" "")
                   "right key should have left editor unchanged"))
   (test-case
    "Test #13"
     (check-equal? (edit (make-editor "" "b") "right")
                   (make-editor "b" "")
                   "right key misbehaved on single-character editor"))
   (test-case
    "Test #14"
     (check-equal? (edit (make-editor "a" "b") "right")
                   (make-editor "ab" "")
                   "right key should have moved cursor to right"))

   ;;; Tests of ignoring other keys.

   (test-case
    "Test #15"
     (check-equal? (edit (make-editor "" "") "up")
                   (make-editor "" "")
                   "up key should be ignored"))
   (test-case
    "Test #16"
     (check-equal? (edit (make-editor "a" "") "down")
                   (make-editor "a" "")
                   "down key should be ignored"))
   ))

(run-tests tests 'verbose)