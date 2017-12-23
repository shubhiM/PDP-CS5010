;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; q2.rkt: Implements a finite state machine that can accept strings
; with (d* | d* p d* | s d* | s d* p d) (d | d e) regular expression.

(require rackunit)
(require "extras.rkt")

(provide initial-state)
(provide next-state)
(provide accepting-state?)
(provide rejecting-state?)


;; CONSTANTS

;; LegalInput specific Constants
(define D "d")
(define P "p")
(define E "e")
(define S "s")

;; State specific constants
(define ST "ST")
(define D1 "D1")
(define D2 "D2")
(define PP "P")
(define SS "S")
(define EE "E")
(define ER "ER")


;; DATA DEFINITIONS:


; A LegalInput to the finite state machine(FSM) is one of the
; --  D
; --  P
; --  E
; --  S
; INTERPRETATION: A legal input is a one letter string which when
;  entered can change the state of the machine.

; TEMPLATE:
; legal-input-fn : LegalInput -> ??
#|
(define (legal-input-fn input)
 (cond
   [(String=? input D)    ...]
   [(String=? input P) ...]
   [(String=? input E)  ...]
   [(String=? input S)  ...]))  
|#  


; A State of the finite state machine(FSM) is one of the
; -- ST
; -- D1
; -- D2
; -- PP
; -- SS
; -- EE
; -- ER
; INTERPRETATION:
; -- ST is the start/initial state of the FSM.
; -- D1 is one of the final states in FSM that expects 'd' as input.
; -- D2 is one of the final states in FSM that expects 'd' as input.
; -- PP is the intermediate state in the FSM that expects 'p' as input.
; -- SS is the intermediate state in the FSM that expects 's' as input.
; -- EE is one of the final states in the FSM that expects 'e' as input.
; -- ER is the error state, which is reached in case of invalid input.

; TEMPLATE:
; state-fn : State -> ??
#|
(define (state-fn state)
 (cond
   [(String=? state ST)    ...]
   [(String=? state D1) ...]
   [(String=? state D2)  ...]
   [(String=? state PP)  ...]
   [(String=? state SS)  ...]
   [(String=? state EE)  ...]
   [(String=? state ER)  ...]))  
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helper functions
;; Define transition from different states in the FSM


;; next-state-from-st : LegalInput -> State
;; GIVEN: legal input to the machine
;; RETURNS: next state of the ST state of the machine based on the input
;; EXAMPLES:
;; -- (next-state-from-st D) = D1
;; -- (next-state-from-st P) = PP
;; DESIGN STRATEGY: cases based on machine input

(define (next-state-from-st legal-input)
  (cond [ (string=? legal-input D) D1]
        [ (string=? legal-input P) PP]
        [ (string=? legal-input S) SS]
        [else ER]))



;; next-state-from-d1 : LegalInput -> State
;; GIVEN: legal input to the machine
;; RETURNS: next state of the D1 state of the machine based on the input
;; EXAMPLES:
;; -- (next-state-from-d1 D) = D1
;; -- (next-state-from-d1 P) = PP
;; DESIGN STRATEGY: cases based on machine input

(define (next-state-from-d1 legal-input)
  (cond [ (string=? legal-input D) D1]
        [ (string=? legal-input E) EE]
        [ (string=? legal-input P) PP]
        [else ER]))


;; next-state-from-d2 : LegalInput -> State
;; GIVEN: legal input to the machine
;; RETURNS: next state of the D2 state of the machine based on the input
;; EXAMPLES:
;; -- (next-state-from-d2 D) = D2
;; -- (next-state-from-d2 P) = ER
;; DESIGN STRATEGY: cases based on machine input

(define (next-state-from-d2 legal-input)
  (cond [ (string=? legal-input D) D2]
        [ (string=? legal-input E) EE]
        [else ER]))


;; next-state-from-pp : LegalInput -> State
;; GIVEN: legal input to the machine
;; RETURNS: next state of the PP state of the machine based on the input
;; EXAMPLES:
;; -- (next-state-from-pp D) = D2
;; -- (next-state-from-pp P) = ER
;; DESIGN STRATEGY: cases based on machine input

(define (next-state-from-pp legal-input)
  (cond [ (string=? legal-input D) D2]
        [else ER]))

;; next-state-from-ss : LegalInput -> State
;; GIVEN: legal input to the machine
;; RETURNS: next state of the SS state of the machine based on the input
;; EXAMPLES:
;; -- (next-state-from-ss D) = D1
;; -- (next-state-from-ss P) = PP
;; DESIGN STRATEGY: cases based on machine input

(define (next-state-from-ss legal-input)
  (cond [ (string=? legal-input D) D1]
        [ (string=? legal-input P) PP]
        [else ER]))

;; next-state-from-ee : LegalInput -> State
;; GIVEN: legal input to the machine
;; RETURNS: next state of the EE state of the machine based on the input
;; EXAMPLES:
;; -- (next-state-from-ee D) = ER
;; -- (next-state-from-ee P) = ER
;; DESIGN STRATEGY: cases based on machine input

(define (next-state-from-ee legal-input) ER)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; next-state : State LegalInput -> State
; GIVEN: A state of the machine and machine input
; RETURNS: the state the machine should enter if it is
;   in the given state and sees the given input.
; EXAMPLES:
#|
 -- (next-state (initial-state 20) S) = SS
 -- (next-state (initial-state 20) P) = PP
 -- (next-state (initial-state 20) D) = D1
 -- (next-state (initial-state 20) E) = ER
 -- (next-state (next-state (initial-state 20) D) D) = D1
 -- (next-state (next-state (initial-state 20) D) E) = EE
 -- (next-state (next-state (initial-state 20) P) P) = ER
 -- (next-state (next-state (initial-state 20) S) S) = ER
 -- (next-state (next-state (initial-state 20) P) D) = D2
 -- (next-state (next-state (initial-state 20) S) D) = D1
 -- (next-state (next-state (initial-state 20) D) S) = ER
 -- (next-state (next-state (initial-state 20) D) P) = ER
 -- (next-state (next-state (next-state (initial-state 20) S) D) E) = EE
 -- (next-state (next-state (next-state (initial-state 20) S) P) D) = D2
 -- (next-state (next-state (next-state (initial-state 20) P) D) D) = D2
 -- (next-state (next-state (next-state (initial-state 20) P) D) E) = EE
 -- (next-state (next-state (next-state (initial-state 20) P) D) P) = ER
|#
; DESIGN STRATEGY: Cases based on the state of the machine.

(define (next-state state legal-input)
  (cond [ (string=? state ST) (next-state-from-st legal-input)]
        [ (string=? state D1) (next-state-from-d1 legal-input)]
        [ (string=? state D2) (next-state-from-d2 legal-input)]
        [ (string=? state PP) (next-state-from-pp legal-input)]
        [ (string=? state SS) (next-state-from-ss legal-input)]
        [ (string=? state EE) (next-state-from-ee legal-input)]
        [else ER]))



; accepting-state? : State -> Boolean
; GIVEN: A state of the machine
; RETURNS: true iff the given state is the final state.
; EXAMPLES:
; -- (accepting-state? (initial-state 20)) = false
; -- (accepting-state? (next-state (initial-state 20) D)) = true
; -- (accepting-state? (next-state (initial-state 20) S)) = false
; -- (accepting-state? (next-state (initial-state 20) P)) = false
; -- (accepting-state? (next-state (initial-state 20) D)) = true
; -- (accepting-state? (next-state (initial-state 20) E)) = false
; DESIGN STRATEGY: combine simpler functions

(define (accepting-state? state)
  (or
   (string=? state D1)
   (string=? state D2)
   (string=? state EE)))


; rejecting-state? : State -> Boolean
; GIVEN: A state of the machine
; RETURNS: true iff there is no path from the given state to an accepting
;  state.
; EXAMPLES:
; -- (rejecting-state? SS) = false
; -- (rejecting-state? D1) = false
; -- (rejecting-state? D2) = false
; -- (rejecting-state? EE) = true
; -- (rejecting-state? PP) = false
; -- (rejecting-state? ST) = false
; -- (rejecting-state? ER) = true
; DESIGN STRATEGY: combine simpler functions

(define (rejecting-state? state)
  (not
   (or
    (accepting-state? state)
    (accepting-state? (next-state state D))
    (accepting-state? (next-state state E))
    (accepting-state? (next-state state S))
    (accepting-state? (next-state state P)))))



; DATA DEFINITIONS: none
; initial-state : Number -> State
; GIVEN: Any number
; RETURNS: A representation of the initial state of the machine.
;   The given number is ignored.
; EXAMPLES:
; -- (initial-state 32) = ST
; -- (initial-state 0) = ST
; -- (initial-state -1) = ST
; -- (initial-state 0.5) = ST
; DESIGN STRATEGY: combine simpler functions

(define (initial-state number) ST)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS
(begin-for-test
  (check-equal?
   (accepting-state? (next-state (initial-state 20) S))
   false              
   "Machine does not recognizes 's' pattern.")

  (check-equal?
   (accepting-state? (next-state (initial-state 20) P))
   false              
   "Machine does not recognizes 'p' pattern.")

  (check-equal?
   (accepting-state? (next-state (initial-state 20) D))
   true              
   "Machine recognizes 'd' pattern.")

  (check-equal?
   (accepting-state? (next-state (initial-state 20) E))
   false              
   "Machine does  not recognizes 'e' pattern.")

  (check-equal?
   (accepting-state? (next-state (next-state (initial-state 20) D) D))
   true              
   "Machine recognizes 'dd' pattern.")

  (check-equal?
   (accepting-state? (next-state (next-state (initial-state 20) D) E))
   true              
   "Machine recognizes 'de' pattern.")

  (check-equal?
   (accepting-state? (next-state (next-state (initial-state 20) P) D))
   true              
   "Machine recognizes 'pd' pattern.")

  (check-equal?
   (accepting-state? (next-state (next-state (initial-state 20) S) D))
   true              
   "Machine recognizes 'sd' pattern.")

  (check-equal?
   (accepting-state? (next-state (next-state (initial-state 20) D) S))
   false              
   "Machine does not recognizes 'ds' pattern.")

  (check-equal?
   (accepting-state? (next-state (next-state (initial-state 20) D) P))
   false              
   "Machine does not recognizes 'dp' pattern.")


  (check-equal?
   (accepting-state? (next-state (next-state (initial-state 20) P) P))
   false              
   "Machine does  not recognizes 'pp' pattern.")

  (check-equal?
   (accepting-state? (next-state (next-state (initial-state 20) S) S))
   false              
   "Machine does  not recognizes 'ss' pattern.")

  (check-equal?
   (accepting-state?
    (next-state (next-state (next-state (initial-state 20) P) D) D))
   true              
   "Machine recognizes 'pdd' pattern.")

  (check-equal?
   (accepting-state?
    (next-state (next-state (next-state (initial-state 20) P) D) E))
   true              
   "Machine recognizes 'pde' pattern.")

  (check-equal?
   (accepting-state?
    (next-state (next-state (next-state (initial-state 20) S) D) E))
   true              
   "Machine recognizes 'sde' pattern.")

  (check-equal?
   (accepting-state?
    (next-state (next-state (next-state (initial-state 20) S) P) D))
   true              
   "Machine recognizes 'spd' pattern.")

  (check-equal?
   (accepting-state?
    (next-state (next-state (next-state (initial-state 20) P) D) P))
   false              
   "Machine does not recognizes 'pdp' pattern.")
   
  (check-equal?
   (accepting-state?
    (next-state
     (next-state (next-state (next-state (initial-state 20) D) P) D) D))
   true              
   "Machine recognizes 'dpdd' pattern.")

  (check-equal?
   (accepting-state?
    (next-state
     (next-state (next-state (next-state (initial-state 20) D) P) D) E))
   true              
   "Machine recognizes 'dpde' pattern.")

  (check-equal?
   (accepting-state?
    (next-state
     (next-state (next-state (next-state (initial-state 20) P) D) D) E))
   true              
   "Machine recognizes 'pdde' pattern.")

  (check-equal?
   (accepting-state?
    (next-state
     (next-state (next-state (next-state (initial-state 20) S) D) D) E))
   true              
   "Machine recognizes 'sdde' pattern.")

  (check-equal?
   (accepting-state?
    (next-state
     (next-state
      (next-state (next-state (next-state (initial-state 20) S) D) P) D) D))
   true              
   "Machine recognizes 'sdpdd' pattern.")

  (check-equal?
   (accepting-state?
    (next-state
     (next-state
      (next-state
       (next-state
        (next-state (next-state (initial-state 20) S) P) D) D) D) E))
   true              
   "Machine recognizes 'spdde' pattern.")

  (check-equal?
   (accepting-state?
    (next-state
     (next-state
      (next-state
       (next-state
        (next-state (next-state (initial-state 20) P) D) E) D) D) E))
   false              
   "Machine does  not recognizes 'pdedde' pattern.")
  
  (check-equal?
   (rejecting-state? ST)
   false              
   "There is a path from ST state to one of the accepting states")

  (check-equal?
   (rejecting-state? SS)
   false              
   "There is a path from SS state to one of the accepting states")

  (check-equal?
   (rejecting-state? EE)
   false              
   "EE itself is an accepting state so it cannot be a rejecting state")

  (check-equal?
   (rejecting-state? D1)
   false              
   "There is a path from D1 state to one of the accepting states")

  (check-equal?
   (rejecting-state? D2)
   false              
   "There is a path from D2 state to one of the accepting states")

  (check-equal?
   (rejecting-state? PP)
   false              
   "There is a path from PP state to one of the accepting states")

  (check-equal?
   (rejecting-state? ER)
   true              
   "There is no path from ER state to one of the accepting states"))