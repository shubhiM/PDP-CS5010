;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; q2.rkt : Implements string-last function which returns last string
;; from a given non empty string


(require rackunit)
(require "extras.rkt")

(provide string-last)

;; DATA DEFINITIONS: none

;; string-last: String -> String
;; GIVEN: a string s
;; WHERE: length of given string is greater than 0
;; RETURNS: last string from the string s
;; EXAMPLES
;;   (string-last "cat") -> t
;;   (string-last "a") -> a
;; DESIGN STRATEGY: combine simpler functions

(define
  (string-last s)
  (string-ith s (- (string-length s) 1)))

;; TESTS:
(begin-for-test
  (check-equal?
   (string-last "cat")
   "t"
   "string-last should return last string as 't' from given string cat")
  (check-equal?
   (string-last "a")
   "a"
   "string-last should return last string as 'a' from given string a")
  (check-equal?
   (string-last "name\t")
   "\t"
   "string-last should return last string as '\t' from given string name\t")
  (check-equal?
   (string-length
    (string-last "a"))
   1
   "string-last should return last string with length equal to 1"))
