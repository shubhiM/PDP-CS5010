;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; q1.rkt : Implements cvolume and csurface functions which computes
;; value of the volume and surface area.


(require rackunit)
(require "extras.rkt")

(provide cvolume)
(provide csurface)

;; DATA DEFINITIONS: none

;; cvolume : PosReal -> PosReal
;; GIVEN: a side s of the cube in cm
;; RETURNS: volume of the cube in cm^3
;; EXAMPLES:
;;     (cvolume 2) -> 8
;;     (cvolume 3) -> 27
;; DESIGN STRATEGY: combine simpler functions

(define
  (cvolume s)
  (expt s 3))

;; TESTS
(begin-for-test
  (check-equal? (cvolume 1) 1
                "cube with 1 cm side should return 1 cm^3 volume")
  (check-equal? (cvolume 0.5) 0.125
                "cube with 0.5 cm side should return 0.125 cm^3 volume"))


;; DATA DEFINITIONS: none

;; csurface : PosReal -> PosReal
;; GIVEN: a side s of the cube in cm
;; RETURNS: surface area of the cube in cm^2
;; EXAMPLES:
;;     (csurface 2) -> 24
;;     (csurface 3) -> 54
;; DESIGN STRATEGY: combine simpler functions

(define
  (csurface s)
  ( * 6 (sqr s)))

;; TESTS
(begin-for-test
  (check-equal? (csurface 1) 6
                "cube with 1 cm side should return 6 cm^2 surface area")
  (check-equal? (csurface 0.5) 1.5
                "cube with 0.5 cm side should return 1.5 cm^2 surface area"))


