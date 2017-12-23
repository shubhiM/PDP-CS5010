;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; q3.rkt : Implements string insert function which inserts an underscore
;; at the given index position in the string


(require rackunit)
(require "extras.rkt")

(provide string-insert)

;; DATA DEFINITIONS: none

;; string-insert : String PositiveInteger -> String
;; GIVEN: A string s and index i
;; WHERE: value of i is between 0 to length of string (inclusive)
;; RETURNS: A string with "_" substituted at the specified index i
;; EXAMPLES:
;;    (string-insert "northeastern" 3) -> nor_theastern
;;    (string-insert "a", 0) -> "_a"
;;    (string-insert "a", 1) -> "a_"
;; DESIGN STRATEGY: combine simpler functions

(define
   (string-insert s i)
   (string-append
    (substring s 0 i) "_" (substring  s i)))

;; TESTS:
(begin-for-test
  (check-equal?
   (string-insert "north" 3)
   "nor_th"
   "string-insert should return nor_th for given string 'north' and index 3")
  (check-equal?
   (string-insert "a" 0)
   "_a"
   "string-insert should return _a for given string 'a' and index 0")
  (check-equal?
   (string-insert "" 0)
   "_"
   "string-insert should return _ for given string '' and index 0")
  (check-equal?
   (string-insert "a" 1)
   "a_"
   "string-insert should return a_ for given string 'a' and index 1"))