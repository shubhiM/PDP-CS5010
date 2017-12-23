;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname flight) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; This file contains the solution of Question 1 of Problem Set 06
;; Flight & UTC Data Defintions

(require rackunit)
(require "extras.rkt")

(check-location "06" "flight.rkt")

(provide make-UTC
         UTC-hour
         UTC-minute
         UTC=?
         make-flight
         flight-name
         departs
         arrives
         departs-at
         arrives-at
         flight=?)


;; DATA DEFINITION

(define-struct UTC [hour minute])

;; A UTC is a (make-UTC NonNegInt NonNegInt)
;;
;; INTERPRETATION:
;; hour is the hour part of a time in UTC and it is less than 24
;; minute is the minute part of a time in UTC and it is less than 60
;;
;; utc-fn : UTC -> ??
;;
;; TEMPLATE:
;; (define (utc-fn u)
;;   (...
;;     (UTC-hour u)
;;     (UTC-minute u))
;;
;; EXAMPLE:
;; (make-UTC 15 06)

(define UTC-uno (make-UTC 15 06))


(define-struct flight [name departs arrives departs-at arrives-at])

;; A Flight is (make-flight String String String UTC UTC)
;;
;; INTERPRETATION:
;; name is the name of a flight
;; departs is the name of the airport from which the flight departs
;; arrives is the name of the airport at which the flight arrives
;; departs-at is the time of departure in UTC
;; arrives-at is the time of arrival in UTC
;;
;; flight-fn : Flight -> ??
;;
;; TEMPLATE:
;; (define (flight-fn f)
;;   (...
;;     (flight-name f)
;;     (flight-departs f)
;;     (flight-arrives f)
;;     (flight-departs-at (utc-fn f))
;;     (flight-arrives-at (utc-fn f)))
;;
;; EXAMPLE:
;; (make-flight "United 448" "BOS" "DEN" (make-UTC 20 03) (make-UTC 00 53))

(define flight-uno (make-flight "United 448" "BOS" "DEN"
                                (make-UTC 20 03) (make-UTC 00 53)))
(define flight-dos (make-flight "Delta 0121" "LGA" "MSP"
                                (make-UTC 11 00) (make-UTC 14 09)))
(define flight-tres (make-flight "Delta 1609" "MSP" "DEN"
                                (make-UTC 20 35) (make-UTC 22 52)))


;; FUNCTIONS
          
;; UTC=?           : UTC UTC -> Boolean
;; GIVEN           : two UTC times
;; RETURNS         : true iff they have the same hour and minute parts
;; EXAMPLES        :
;;       (UTC=? (make-UTC 15 31) (make-UTC 15 31))  =>  true
;;       (UTC=? (make-UTC 15 31) (make-UTC 14 31))  =>  false
;;       (UTC=? (make-UTC 15 31) (make-UTC 15 32))  =>  false
;; STRATEGY               : Using Template for UTC


(define (UTC=? UTC-one UTC-two)
  (equal? UTC-one UTC-two))

(begin-for-test
  (check-equal? (UTC=? (make-UTC 15 31) (make-UTC 15 31))
                #true "The Output should be #true"))

(begin-for-test
  (check-equal? (UTC=? (make-UTC 15 31) (make-UTC 15 32))
                #false "The Output should be #false"))

(begin-for-test
  (check-equal? (UTC=? (make-UTC 15 31) (make-UTC 14 31))
                #false "The Output should be #false"))


;; departs    : Flight -> String
;; GIVEN      : a flight
;; RETURNS    : the name of the airport from which the flight departs
;; EXAMPLE    : (departs flight-uno)  =>  "BOS"
;; STRATEGY   : Combine simpler function

(define
  (departs flight)
  (flight-departs flight))

(begin-for-test
  (check-equal? (departs flight-uno)
                "BOS" "The Output for flight-uno should be 'BOS'"))

(begin-for-test
  (check-equal? (departs flight-dos)
                "LGA" "The Output for flight-dos should be 'LGA'"))


;; arrives    : Flight -> String
;; GIVEN      : a flight
;; RETURNS    : the name of the airport at which the flight arrives
;; EXAMPLE    : (arrives flight-uno)  =>  "DEN"

(define
  (arrives flight)
  (flight-arrives flight))

(begin-for-test
  (check-equal? (arrives flight-uno)
                "DEN" "The Output for flight-uno should be 'DEN'"))

(begin-for-test
  (check-equal? (arrives flight-dos)
                "MSP" "The Output for flight-dos should be 'MSP'"))


;; departs-at : Flight -> UTC
;; GIVEN      : a flight
;; RETURNS    : the time (in UTC, see above) at which the flight departs
;; EXAMPLE    : (departs-at flight-uno)  =>  (make-UTC 20 03)

(define
  (departs-at flight)
  (flight-departs-at flight))

(begin-for-test
  (check-equal? (departs-at flight-uno)
                (make-UTC 20 03)
                "The Output for flight-uno should be (make-UTC 20 03)"))

(begin-for-test
  (check-equal? (departs-at flight-dos)
                (make-UTC 11 00)
                "The Output for flight-dos should be (make-UTC 11 00)"))

        
;; arrives-at : Flight -> UTC
;; GIVEN      : a flight
;; RETURNS    : the time (in UTC, see above) at which the flight arrives
;; EXAMPLE    : (arrives-at flight-uno)  =>  (make-UTC 00 53)

(define
  (arrives-at flight)
  (flight-arrives-at flight))

(begin-for-test
  (check-equal? (arrives-at flight-uno)
                (make-UTC 00 53)
                "The Output for flight-uno should be (make-UTC 00 53)"))

(begin-for-test
  (check-equal? (arrives-at flight-dos)
                (make-UTC 14 09)
                "The Output for flight-dos should be (make-UTC 14 09)"))


;; flight=?   : Flight Flight -> Boolean
;; GIVEN      : two flights
;; RETURNS    : true if and only if the two flights have the same
;;              name, depart from the same airport, arrive at the same
;;              airport, depart at the same time, and arrive at the same time
;; EXAMPLES:
;;     (flight=? flight-uno flight-uno)  => #true
;;     (flight=? (make-flight "United 448" "BOS" "DEN"
;;                            (make-UTC 20 00) (make-UTC 00 55))
;;               flight-dos) => #false

(define (flight=? flight-one flight-two)
  (equal? flight-one flight-two))

(begin-for-test
  (check-equal? (flight=? flight-uno flight-uno)
                #true
                "The flight-uno flight-uno are same)"))

(begin-for-test
  (check-equal? (flight=? flight-uno flight-dos)
                #false
                "The flight-uno flight-two are not same"))

