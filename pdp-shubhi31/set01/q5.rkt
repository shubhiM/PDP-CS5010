;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; q5.rkt: Implements function string-remove-last which removes the last
;; string from a given string

(require rackunit)
(require "extras.rkt")

(provide string-remove-last)

;; DATA DEFINITIONS: none

;; string-remove-last : String -> String
;; GIVEN: A string s
;; WHERE: given string cannot be an empty string
;; RETURNS: given string with last string removed from it
;; EXAMPLES:
;;    (string-remove-last "name") -> "nam"
;;    (string-remove-last "n") -> ""
;; DESIGN STRATEGY : combine simpler functions

(define
  (string-remove-last s)
  (substring s 0 (- (string-length s) 1)))

;; TESTS:
(begin-for-test
  (check-equal?
   (string-remove-last "north")
   "nort"
   "string-remove-last should return nort for given string north")
  (check-equal?
   (string-remove-last "a")
   ""
   "string-remove-last should return '' for given string a"))