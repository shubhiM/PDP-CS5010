;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; q4.rkt: Refractors profit function and defines the constants

(require rackunit)
(require "extras.rkt")

(provide profit)

; the base price of show ticket
(define BASE-TICKET-PRICE 5.0)

; the number of people attending the show at base ticket price
(define BASE-ATTENDANCE 120)

; the fixed cost of running the show
(define FIXED-COST 180)

; the variable cost of running the show per attendee
(define VARIABLE-COST 0.04)

; the change in the number of people corresponding to change in ticket price
(define AVG-CHANGE-IN-ATTENDANCE 15)

; the change in the ticket price
(define CHANGE-IN-TICKET_PRICE 0.1)

;; DATA DEFINITIONS: None
;; attendees : PosReal -> Integer
;; GIVEN: price of the ticket in USD
;; RETURNS : number of people attending
;; EXAMPLES:
;;      (attendees 7) -> -180
;;      (attendees 5.5) -> 45
;; DESIGN STRATEGY: combine simpler functions


(define (attendees ticket-price)
  (- BASE-ATTENDANCE (*
                      (- ticket-price BASE-TICKET-PRICE)
                      (/ AVG-CHANGE-IN-ATTENDANCE CHANGE-IN-TICKET_PRICE))))


;; TESTS:
(begin-for-test
  (check-equal?
   (attendees 7)
   -180
   "attendees should return -180 for ticket price equal to 7 dollars")
  (check-equal?
   (attendees 5.5)
   45
   "attendees should return 45 for ticket price equal to 5.5 dollars"))


;; DATA DEFINITIONS: None
;; revenue : PosReal -> Real
;; GIVEN: price of the ticket in USD
;; RETURNS : revenue earned in USD
;; EXAMPLES:
;;      (revenue 7) -> -1260
;;      (revenue 5.5) -> 247.5
;; DESIGN STRATEGY: combine simpler functions

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

;;TESTS:
(begin-for-test
  (check-equal?
   (revenue 7)
   -1260
   "revenue should return -1260 for ticket price equal to 7 dollars")
  (check-equal?
   (revenue 5.5)
   247.5
   "revenue should return 247.5 for ticket price equal to 5.5 dollars"))


;; DATA DEFINITIONS: None
;; cost : PosReal -> Real
;; GIVEN: price of the ticket in USD
;; RETURNS : cost incurred in the event in USD
;; EXAMPLES:
;;      (cost 7) -> 172.8
;;      (cost 5.5) -> 181.8
;; DESIGN STRATEGY : combine simpler functions

(define (cost ticket-price)
  (+ FIXED-COST (* VARIABLE-COST (attendees ticket-price))))

;; TESTS:

(begin-for-test
  (check-equal?
   (cost 7)
   172.8
   "cost should return 172.8 for ticket price equal to 7 dollars")
  (check-equal?
    (cost 5.5)
    181.8
    "cost should return 181.8 for ticket price equal to 5.5 dollars"))

;; DATA DEFINITIONS: None
;; profit : PosReal -> Real
;; GIVEN: price of the ticket in USD
;; RETURNS : profit earned in USD, profit in negative indicates loss
;; EXAMPLES:
;;    (profit 7) -> -1432.8
;;    (profit 5.5) -> 65.7
;; DESIGN STRATEGY: combine simpler functions

(define (profit ticket-price)
  (- (revenue ticket-price) (cost ticket-price)))

;; TESTS:
(begin-for-test
  (check-equal?
   (profit 7)
   -1432.8
   "profit should return -1432.8 for ticket price equal to 7 dollars")
  (check-equal?
   (profit 5.5)
   65.7
   "profit should return 65.7 for ticket price equal to 5.5 dollars"))