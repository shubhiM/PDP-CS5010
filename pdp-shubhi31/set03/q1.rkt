;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; q1.rkt: Potrays an animation that displays two doodads moving around in a
; rectangular enclosure

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)


(provide animation)
(provide initial-world)
(provide world-after-tick)
(provide world-after-key-event)
(provide world-paused?)
(provide world-doodad-star)
(provide world-doodad-square)
(provide doodad-x)
(provide doodad-y)
(provide doodad-vx)
(provide doodad-vy)
(provide doodad-color)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-location "03" "q1.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WORLD CONSTANTS

(define PAUSED false)

;; Canvas specific constants
(define CANVAS-WIDTH 601)
(define CANVAS-HEIGHT 449)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))


;; Doodad common constants
(define OUTLINE-MODE "solid")

;; Star doodad specific constants
(define INITIAL-X-CORD-STAR-DOODAD 125)
(define INITIAL-Y-CORD-STAR-DOODAD 120)
(define INITIAL-X-VEL-STAR-DOODAD 10)
(define INITIAL-Y-VEL-STAR-DOODAD 12)
(define INITIAL-COLOR-STAR-DOODAD "gold")
(define INNER-RADIUS 10)
(define OUTER-RADIUS 50)
(define POINTS 8)


;; Square doodad specific constants
(define INITIAL-X-CORD-SQUARE-DOODAD 460)
(define INITIAL-Y-CORD-SQUARE-DOODAD 350)
(define INITIAL-X-VEL-SQUARE-DOODAD -13)
(define INITIAL-Y-VEL-SQUARE-DOODAD -9)
(define INITIAL-COLOR-SQUARE-DOODAD "gray")
(define LENGHT-OF-SIDE 71)


;; Image for star doodad
(define STAR-DOODAD-IMAGE
  (radial-star
   POINTS
   INNER-RADIUS
   OUTER-RADIUS
   OUTLINE-MODE
   INITIAL-COLOR-STAR-DOODAD))


;; Image for square doodad
(define SQUARE-DOODAD-IMAGE
  (square
   LENGHT-OF-SIDE
   OUTLINE-MODE
   INITIAL-COLOR-SQUARE-DOODAD))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; DATA DEFINITIONS

;; A KeyEvent is a predefined scalar data type in the 2htdp/universe in Racket
;; It represents key board events as strings. All key events are strings but all
;; strings are not key events


;; Constants for StarColor
(define GOLD "gold")
(define GREEN "green")
(define BLUE "blue")

;; A StarColor is one of the
;; -- GOLD
;; -- GREEN
;; -- BLUE
;; Interpretation:
;; GOLD, GREEN and BLUE are the colors that a star shaped doodad
;; changes through during its core bounce cycle in the given order
;; TEMPLATE:

; star-color-fn : String -> ??

#|
(define (star-color-fn c)
  (cond
    [(string=? GOLD c) ...]
    [(string=? GREEN c) ...]
    [(string=? BLUE c) ...]))
|#


;; Constants for SquareColor

(define GRAY "gray")
(define OLIVE-DRAB "olivedrab")
(define KHAKI "khaki")
(define ORANGE "orange")
(define CRIMSON "crimson")

;; A SquareColor is one of the
;; -- GRAY
;; -- OLIVE-DRAB
;; -- KHAKI
;; -- ORANGE
;; -- CRIMSON
;; INTERPRETATION:
;; GRAY, OLIVE-DRAB, KHAKI, ORANGE and CRIMSON are the colors that a star
;; shaped doodad changes through during its core bounce cycle in the given
;; order

;; TEMPLATE:

;; square-color-fn: String -> ??

#|
(define (square-color-fn c)
  (cond
    [(string=? GRAY c) ...]
    [(string=? OLIVE-DRAB c) ...]
    [(string=? KHAKI c) ...]
    [(string=? ORANGE c) ...]
    [(string=? CRIMSON c) ...]))

|#

(define-struct doodad ( x y vx vy color))

; A Doodad is the structure
; -- (make-doodad Integer Integer Integer Integer String)
; INTERPRETATION:
; -- x is the x-coordinate of the doodad in pixels
; -- y is the y-coordinate of the doodad in pixels
; -- vx is the number of the pixels doodad moves in x direction on each tick
; -- vy is the number of the pixels doodad moves in y direction on each tick
; -- color is the color of the doodad after each core bounce

;;TEMPLATE:

;; doodad-fn Doodad -> ??

#|
(define (doodad-fn d)
  (..
   (doodad-x d)
   (doodad-y d)
   (doodad-vx d)
   (doodad-vy d)
   (doodad-color d)))
|#

  
(define-struct world (doodad-star doodad-square paused?))
; A World is a 
; -- (make-world Doodad Doodad Boolean)
; INTERPRETATION:
; -- doodat-star is the doodad of shape of a radial star
; -- doodad-square is the doodad of shape of a square
; -- paused? is a boolean value that represents if the world is unpaused or not.
; --   True denotes paused
; --   False denotes unpaused

;; TEMPLATE:

; world-fn : World -> ??
#|
(define (world-fn w)
  (..
   (world-doodad-star w)
   (world-doodad-square w)
   (world-paused? w)))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; COMMON TESTS CONSTANTS
;; To be used as test constants across multiple functions

(define doodad-star-1 (make-doodad 0 100 20 600 GOLD))
(define doodad-square-1 (make-doodad 0 100 20 600 GRAY))

(define initial-doodad-star (make-doodad 125 120 10 12 GOLD))
(define initial-doodad-square (make-doodad 460 350 -13 -9 GRAY))
 
(define world-paused
  (make-world
   doodad-star-1
   doodad-square-1
   true))

(define world-not-paused
  (make-world
   doodad-star-1
   doodad-square-1
   false))

(define paused-key-event " ")
(define non-paused-key-event "q")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WISHLIST


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS

;; Data Definition: Refer to the data definition of the StarColor
;; next-color-star-doodad : StarColor -> StarColor
;; RETURNS: the next color in the core bounce cycle of the star shaped doodad
;; EXAMPLES:
;; -- (next-color-star-doodad GOLD) = GREEN
;; -- (next-color-star-doodad GREEN) = BLUE
;; -- (next-color-star-doodad BLUE) = GOLD
;; DESIGN STRATEGY: use template of StarColor

(define (next-color-star-doodad c)
  (cond [(string=? c GOLD) GREEN]
        [(string=? c GREEN) BLUE]
        [(string=? c BLUE) GOLD]))

;; TESTS
(begin-for-test
  (check-equal?
   (next-color-star-doodad GOLD)
   GREEN
   "Star doodad should turn gold to green in course of the core bounce event")

 (check-equal?
   (next-color-star-doodad GREEN)
   BLUE
   "Star doodad should turn green to blue in course of the core bounce event")

 (check-equal?
   (next-color-star-doodad BLUE)
   GOLD
   "Star doodad should turn blue to gold in course of the core bounce event"))
   
 
;; Data Definition: Refer to the data definition of the SquareColor
;; next-color-square-doodad : SquareColor -> SquareColor
;; RETURNS: the next color in the core bounce cycle of the square shaped doodad
;; EXAMPLES:
;; -- (next-color-square-doodad  GRAY) = OLIVE-DRAB
;; -- (next-color-square-doodad  OLIVE-DRAB) = KHAKI
;; -- (next-color-square-doodad  KHAKI) = ORANGE
;; -- (next-color-square-doodad  ORANGE) = CRIMSON
;; -- (next-color-square-doodad  CRIMSON) = GRAY
;; DESIGN STRATEGY: use template of SquareColor

(define (next-color-square-doodad c)
  (cond [(string=? c GRAY) OLIVE-DRAB]
        [(string=? c OLIVE-DRAB) KHAKI]
        [(string=? c KHAKI) ORANGE]
        [(string=? c ORANGE) CRIMSON]
        [(string=? c CRIMSON) GRAY]))

;; TESTS
(begin-for-test
  (check-equal?
   (next-color-square-doodad GRAY)
   OLIVE-DRAB
   "Star doodad should turn gold to green in course of the core bounce event")

 (check-equal?
   (next-color-square-doodad OLIVE-DRAB)
   KHAKI
   "Star doodad should turn green to blue in course of the core bounce event")

 (check-equal?
   (next-color-square-doodad KHAKI)
   ORANGE
   "Star doodad should turn blue to gold in course of the core bounce event")

 (check-equal?
   (next-color-square-doodad ORANGE)
   CRIMSON
   "Star doodad should turn blue to gold in course of the core bounce event")

  (check-equal?
   (next-color-square-doodad CRIMSON)
   GRAY
   "Star doodad should turn blue to gold in course of the core bounce event"))


;; Data Definition: none
;; in-x-range? :  Integer -> Boolean
;; GIVEN: An integer representing x coordinate in pixels
;; RETURNS: true if x lies within the canvas boundaries.
;; EXAMPLES:
;; -- (in-x-range? 0) = true
;; -- (in-x-range? 601) = false
;; -- (in-x-range? -1) = false
;; -- (in-x-range? 50) = true
;; DESIGN STRATEGY : combine simpler functions

(define (in-x-range? x)
  (and (>= x 0) (< x CANVAS-WIDTH)))

;; TESTS
(begin-for-test
  (check-equal?
   (in-x-range? 0)
   true
   "in-x-range? should return true as 0 is in x-range")

 (check-equal?
   (in-x-range? 601)
   false
   "in-x-range? should return false as 601 is out of x-range")

 (check-equal?
   (in-x-range? -1)
   false
   "in-x-range? should return false as negative values are out of x-range")

 (check-equal?
   (in-x-range? 50)
   true
   "in-x-range? should return true as 50 is in range of x-range"))



;; Data Definition:none
;; in-y-range? : Integer -> Boolean
;; GIVEN: An integer representing y coordinate in pixels
;; RETURNS: true if y lies within the canvas boundaries.
;; EXAMPLES:
;; -- (in-y-range? 0) = true
;; -- (in-y-range? 449) = false
;; -- (in-y-range? -1) = false
;; -- (in-y-range? 50) = true
;; DESIGN STRATEGY : combine simpler functions

(define (in-y-range? y)
  (and (>= y 0) (< y CANVAS-HEIGHT)))

;; TESTS
(begin-for-test
  (check-equal?
   (in-y-range? 0)
   true
   "in-y-range? should return true as 0 is in y-range")

 (check-equal?
   (in-y-range? 449)
   false
   "in-y-range? should return false as 449 is out of y-range")

 (check-equal?
   (in-y-range? -1)
   false
   "in-y-range? should return false as negative values are out of y-range")

 (check-equal?
   (in-y-range? 50)
   true
   "in-y-range? should return true as 50 is in range of y-range"))

;; Data Definition:  none
;; doodad-core-bounce? Integer Integer -> Boolean
;; GIVEN: tentative x and tentative y values for the doodad
;; RETURNS: true if any one or both of the doodad's x and y coordinates
;; are out of range on the canvas
;; EXAMPLES:
;; -- (doodad-core-bounce? 0 0) = false
;; -- (doodad-core-bounce? 601 429) = true
;; -- (doodad-core-bounce? 600 449) = true
;; -- (doodad-core-bounce? -1 448) = true
;; -- (doodad-core-bounce? 600 -1) = true
;; DESIGN STRATEGY: combine simpler functions

(define (doodad-core-bounce? x y)
  (or
   (not (in-x-range? x))
   (not (in-y-range? y))))

;; TESTS
(begin-for-test
  (check-equal?
   (doodad-core-bounce? 0 0)
   false
   "core bounce cannot happen as doodad at (0, 0) is inside the canvas.")

 (check-equal?
   (doodad-core-bounce? 601 429)
   true
   "core bounce will happen as doodad at (601, 429) has x outside of canvas")

 (check-equal?
   (doodad-core-bounce? 600 449)
   true
   "core bounce will happen as doodad at (601, 429) has y outside of canvas")

 (check-equal?
   (doodad-core-bounce? -1 448)
   true
   "core bounce will happen as doodad at (-1, 448) has x outside of canvas")

 (check-equal?
   (doodad-core-bounce? 600 -1)
   true
   "core bounce will happen as doodad at (600, -1) has y outside of canvas"))



;; Data Definition: Refer to the data definition of the Doodad
;; tentative-x : Doodad -> Integer
;; RETURNS: summation of the x and vx of the given doodad in pixels
;; EXAMPLES:
;; -- (tentative-x (make-doodad 0 20 30 40 "grey")) = 30
;; -- (tentative-x (make-doodad 0 20 -1 40 "grey")) = -1
;; STRATEGY: combine simpler functions

(define (tentative-x d)
  (+ (doodad-x d) (doodad-vx d)))


;; TESTS
(begin-for-test
  (check-equal?
   (tentative-x (make-doodad 0 20 30 40 "grey"))
   30
   "tentative-x should return 30")

 (check-equal?
   (tentative-x (make-doodad 0 20 -1 40 "grey"))
   -1
   "tentative-x should return -1"))


;; Data Definition: Refer to the data definition of the Doodad
;; tentative-y : Doodad -> Integer
;; RETURNS: summation of the y and vy of the given doodad in pixels
;; EXAMPLES:
;; -- (tentative-y (make-doodad 0 20 30 40 "grey")) = 60
;; -- (tentative-y (make-doodad 0 20 -1 -80 "grey")) = -60
;; STRATEGY: combine simpler functions

(define (tentative-y d)
  (+ (doodad-y d) (doodad-vy d)))

;; TESTS
(begin-for-test
  (check-equal?
   (tentative-y (make-doodad 0 20 30 40 "grey"))
   60
   "tentative-y should return 60")

 (check-equal?
   (tentative-y (make-doodad 0 20 -1 -80 "grey"))
   -60
   "tentative-y should return -60"))

;; Data Definition: none
;; doodad-x-pos-after-the-tick: Integer -> Integer
;; GIVEN : tentative value of x-coordinates of a doodad after the tick
;; RETURNS: final x-coordinates of a doodad after the tick
;; EXAMPLES:
;; -- (doodad-x-pos-after-the-tick 20) = 20
;; -- (doodad-x-pos-after-the-tick -5) = 5
;; -- (doodad-x-pos-after-the-tick -1000) = 200
;; -- (doodad-x-pos-after-the-tick 2000) = 208
;; DESIGN STRATEGY: cases based on x

(define (doodad-x-pos-after-the-tick x)
  (cond [(in-x-range? x) x]
        [( < x 0 ) (doodad-x-pos-after-the-tick (abs x))]
        [else
         (doodad-x-pos-after-the-tick
          (- (- CANVAS-WIDTH 1)
             (- x (- CANVAS-WIDTH 1))))]))

;; TESTS
(begin-for-test
  (check-equal?
   (doodad-x-pos-after-the-tick 20)
   20
   "doodad-x-pos-after-the-tick should return 20")

  (check-equal?
   (doodad-x-pos-after-the-tick -5)
   5
   "doodad-x-pos-after-the-tick should return 5")
 
 (check-equal?
   (doodad-x-pos-after-the-tick -1000)
   200
   "doodad-x-pos-after-the-tick should return 200")
 
 (check-equal?
   (doodad-x-pos-after-the-tick 2000)
   400
   "doodad-x-pos-after-the-tick should return 208"))


;; Data Definition: none
;; doodad-y-pos-after-the-tick: Integer -> Integer
;; GIVEN : tentative value of y-coordinates of a doodad after the tick
;; RETURNS: final y-coordinates of a doodad after the tick
;; EXAMPLES:
;; -- (doodad-y-pos-after-the-tick 20) = 20
;; -- (doodad-y-pos-after-the-tick -5) = 5
;; -- (doodad-y-pos-after-the-tick: -1000) = 104
;; -- (doodad-y-pos-after-the-tick: 2000) = 350
;; DESIGN STRATEGY: cases based on y

(define (doodad-y-pos-after-the-tick y)
  (cond [(in-y-range? y) y]
        [(< y 0 ) (doodad-y-pos-after-the-tick (abs y))]
        [else
         (doodad-y-pos-after-the-tick
          (- (- CANVAS-HEIGHT 1)
             (- y (- CANVAS-HEIGHT 1))))]))

;; TESTS
(begin-for-test
  (check-equal?
   (doodad-y-pos-after-the-tick 20)
   20
   "doodad-y-pos-after-the-tick should return 20")

  (check-equal?
   (doodad-y-pos-after-the-tick -5)
   5
   "doodad-y-pos-after-the-tick should return 5")
 
 (check-equal?
   (doodad-y-pos-after-the-tick -1000)
   104
   "doodad-y-pos-after-the-tick should return 104")
 
 (check-equal?
   (doodad-y-pos-after-the-tick 2000)
   208
   "doodad-y-pos-after-the-tick should return 208"))


;; Data Definition: none
;; doodad-x-vel-after-the-tick : Integer Integer -> Integer
;; GIVEN: x and vx
;; -- x is the tentative x-coordinates
;; -- vx is the velocity in x direction
;; RETURNS: the final velocity of the doodad in x direction following the tick
;; EXAMPLES:
;;  -- (doodad-x-vel-after-the-tick 0 20) = 20
;;  -- (doodad-x-vel-after-the-tick 601 20) = -20
;; DESIGN STRATEGY: cases based on x

(define (doodad-x-vel-after-the-tick x vx)
  (cond [(in-x-range? x) vx]
        [else (* -1 vx)]))

;; TESTS
(begin-for-test
  (check-equal?
   (doodad-x-vel-after-the-tick 0 20)
   20
   "doodad-x-vel-after-the-tick should return 20 when x is 0 and vx is 20")

  (check-equal?
   (doodad-x-vel-after-the-tick 601 20)
   -20
   "doodad-x-vel-after-the-tick should return -20 when x is 601 and vx is 20"))


;; Data Definition: none
;; doodad-y-vel-after-the-tick : Integer Integer -> Integer
;; GIVEN: y and vy
;; -- y is the tentative y-coordinates
;; -- vy is the velocity in y direction
;; RETURNS: the final velocity of the doodad in y direction following the tick
;; EXAMPLES:
;;  -- (doodad-y-vel-after-the-tick 0 0) = 0
;;  -- (doodad-y-vel-after-the-tick 449 40) = -40
;; DESIGN STRATEGY: cases based on y

(define (doodad-y-vel-after-the-tick y vy)
  (cond [(in-y-range? y) vy]
        [else (* -1 vy)]))

;; TESTS
(begin-for-test
  (check-equal?
   (doodad-y-vel-after-the-tick 0 0)
   0
   "doodad-x-vel-after-the-tick should return 0 when x is 0 and vx is 0")

  (check-equal?
   (doodad-y-vel-after-the-tick 449 40)
   -40
   "doodad-x-vel-after-the-tick should return 449 when x is 601 and vx is 40"))


;; Data Definition: Refer to the definition of the Doodad
;; doodad-star-color-after-the-tick : Doodad -> String
;; RETURNS: new color of the given star doodad if core bounce event happens
;; else returns the same color
;; EXAMPLES:
;;  (doodad-star-color-after-the-tick doodad-star-to-be-core-bounced) = GREEN
;;  (doodad-star-color-after-the-tick doodad-star-not-to-be-core-bounced) = GRAY
;; DESIGN STRATEGY: combine simpler functions

(define (doodad-star-color-after-the-tick d)
  (if (doodad-core-bounce? (tentative-x d) (tentative-y d))
      (next-color-star-doodad (doodad-color d))
      (doodad-color d)))

(define doodad-star-to-be-core-bounced (make-doodad 0 100 20 600 GOLD))
(define doodad-star-not-to-be-core-bounced (make-doodad 0 100 20 100 GOLD))

;; TESTS:
(begin-for-test
  (check-equal?
   (doodad-star-color-after-the-tick doodad-star-to-be-core-bounced)
   GREEN
   "doodad star will change the color from gold to green.")

  (check-equal?
   (doodad-star-color-after-the-tick doodad-star-not-to-be-core-bounced)
   GOLD
   "doodad star will not change the color"))


;; Data Definition: Refer to the definition of the Doodad
;; doodad-square-color-after-the-tick: Doodad -> String
;; RETURNS: new color of the given square doodad if core bounce event happens
;; else returns the same color
;; EXAMPLES:
;; (doodad-square-color-after-the-tick
;;   doodad-square-to-be-core-bounced) = GREEN
;; (doodad-square-color-after-the-tick
;;    doodad-square-not-to-be-core-bounced) = GRAY
;; DESIGN STRATEGY: combine simpler functions

(define (doodad-square-color-after-the-tick d)
  (if (doodad-core-bounce? (tentative-x d) (tentative-y d))
      (next-color-square-doodad (doodad-color d))
      (doodad-color d)))

(define doodad-square-to-be-core-bounced (make-doodad 0 100 20 600 GRAY))
(define doodad-square-not-to-be-core-bounced (make-doodad 0 100 20 100 GRAY))

;; TESTS:
(begin-for-test
  (check-equal?
   (doodad-square-color-after-the-tick doodad-square-to-be-core-bounced)
   OLIVE-DRAB
   "doodad square will change the color from gray to olivedrab.")

  (check-equal?
   (doodad-square-color-after-the-tick doodad-square-not-to-be-core-bounced)
   GRAY
   "doodad square will not change the color"))


;; Data Definition: Refer to the definition of the Doodad
;; doodad-star-after-the-tick : Doodad -> Doodad
;; RETURNS: the state of the given star shaped doodad after the tick in an
;; unpaused world
;; EXAMPLES:
;; -- doodad core bounced
;; (doodad-star-after-the-tick
;;    doodad-star-1-before-tick) = doodad-star-1-after-tick
;; -- doodad core not bounced
;; (doodad-star-after-the-tick
;;    doodad-star-2-before-tick) = doodad-star-2-after-tick
;; DESIGN STRATEGY: combine simpler functions

(define (doodad-star-after-the-tick d)
  (make-doodad
   (doodad-x-pos-after-the-tick (tentative-x d))
   (doodad-y-pos-after-the-tick (tentative-y d))
   (doodad-x-vel-after-the-tick (tentative-x d) (doodad-vx d))
   (doodad-y-vel-after-the-tick (tentative-y d) (doodad-vy d))
   (doodad-star-color-after-the-tick d)))


;; constants for tests
;; doodad-star to be core bounced after the tick
(define doodad-star-1-before-tick doodad-star-1)
(define doodad-star-1-after-tick (make-doodad 20 196 20 -600 GREEN))

;; doodad-star not to be core bounced after the tick
(define doodad-star-2-before-tick (make-doodad 0 100 20 100 GOLD))
(define doodad-star-2-after-tick (make-doodad 20 200 20 100 GOLD))

;; TESTS:
(begin-for-test
  (check-equal?
   (doodad-star-after-the-tick doodad-star-1-before-tick)
   doodad-star-1-after-tick
   "doodad star 1 will get core bounced after the tick.")

  (check-equal?
   (doodad-star-after-the-tick doodad-star-2-before-tick)
   doodad-star-2-after-tick
   "doodad star 2 will not get core bounced after the tick."))


;; Data Definition: Refer to the definition of the Doodad
;; doodad-square-after-the-tick : Doodad -> Doodad
;; RETURNS: the state of the given square shaped doodad after the tick in an
;; unpaused world
;; EXAMPLES:
;; -- doodad core bounced
;; (doodad-square-after-the-tick
;;    doodad-square-1-before-tick) = doodad-square-2-before-tick
;; -- doodad core not bounced
;; (doodad-square-after-the-tick
;;    doodad-square-2-before-tick) = doodad-square-2-after-tick
;; DESIGN STRATEGY: combine simpler functions

(define (doodad-square-after-the-tick d)
  (make-doodad
   (doodad-x-pos-after-the-tick (tentative-x d))
   (doodad-y-pos-after-the-tick (tentative-y d))
   (doodad-x-vel-after-the-tick (tentative-x d) (doodad-vx d))
   (doodad-y-vel-after-the-tick (tentative-y d) (doodad-vy d))
   (doodad-square-color-after-the-tick d)))


;; constants for tests
;; doodad-square to be core bounced after the tick
(define doodad-square-1-before-tick doodad-square-1)
(define doodad-square-1-after-tick (make-doodad 20 196 20 -600 OLIVE-DRAB))

;; doodad-square not to be core bounced after the tick
(define doodad-square-2-before-tick (make-doodad 0 100 20 100 GRAY))
(define doodad-square-2-after-tick (make-doodad 20 200 20 100 GRAY))

;; TESTS:
(begin-for-test
  (check-equal?
   (doodad-square-after-the-tick doodad-square-1-before-tick)
   doodad-square-1-after-tick
   "doodad square-1 will get core bounced after the tick")

  (check-equal?
   (doodad-square-after-the-tick doodad-square-2-before-tick)
   doodad-square-2-after-tick
   "doodad square-2 will not get core bounced after the tick"))


;; Data Definition: Refer to the KeyEvent data definition
;; is-pause-key-event? : KeyEvent -> Boolean
;; RETURNS: true iff the key event passed is a space bar.
;; EXAMPLES:
;; -- (is-pause-key-event? paused-key-event) = true
;; -- (is-pause-key-event? non-paused-key-event) = false
;; DESIGN STRATEGY: combine simpler functions

(define (is-pause-key-event? ke) (key=? ke " "))


;; paused-key-event and non-paused-key-event are defined in
;; common test constants

;; TESTS:
(begin-for-test
  (check-equal?
   (is-pause-key-event? paused-key-event)
   true
   "is-pause-key-event? should return true for ' ' key event")

  (check-equal?
   (is-pause-key-event? non-paused-key-event)
   false
   "is-pause-key-event? should return false for non pause key event"))



;; Data Definition: Refer to the definition of the World
;; world-with-paused-toggled : World -> World
;; RETURNS: the world just like the given one but with paused
;; toggled
;; EXAMPLES:
#| -- (world-with-paused-toggled
         world-paused) = (make-world
                            (world-doodad-star w)
                            (world-doodad-square w)
                            (not (world-paused? w)))
   -- (world-with-paused-toggled
         world-not-paused) = (make-world
                                (world-doodad-star w)
                                (world-doodad-square w)
                                (not (world-paused? w)))
|#
;; DESIGN STRATEGY: combine simpler functions

(define (world-with-paused-toggled w)
  (make-world
   (world-doodad-star w)
   (world-doodad-square w)
   (not (world-paused? w))))


;; world-paused and world-not-paused are defined in common test
;; constants


;; TESTS:
(begin-for-test
  (check-equal?
   (world-with-paused-toggled world-paused)
   (make-world
    (world-doodad-star world-paused)
    (world-doodad-square world-paused)
    (not (world-paused? world-paused)))
   "un-pauses the paused world")
  (check-equal?
   (world-with-paused-toggled world-not-paused)
   (make-world
    (world-doodad-star world-not-paused)
    (world-doodad-square world-not-paused)
    (not (world-paused? world-not-paused)))
   "pauses the un-paused world"))


;; Data Definition: Refer to the definition of the Doodad
;; place-doodad-square : Doodad Scene -> Image
;; RETURNS: A scene that potrays the given square shaped doodad
;; EXAMPLES:
#|
(place-doodad-square initial-doodad-square EMPTY-CANVAS) =
    (place-image
     SQUARE-DOODAD-IMAGE
     INITIAL-X-CORD-SQUARE-DOODAD
     INITIAL-Y-CORD-SQUARE-DOODAD
     EMPTY-CANVAS)
|#
;;(place-doodad-square initial-doodad-square EMPTY-CANVAS) = SQUARE-DOODAD-IMAGE
;; DESIGN STRATEGY: combine simpler functions

(define (place-doodad-square d s)
  (place-image
   (square LENGHT-OF-SIDE OUTLINE-MODE (doodad-color d))
   (doodad-x d)
   (doodad-y d)
   s))

;; TESTS
(begin-for-test
  (check-equal?
   (place-doodad-square initial-doodad-square EMPTY-CANVAS)
   (place-image
    SQUARE-DOODAD-IMAGE
    INITIAL-X-CORD-SQUARE-DOODAD
    INITIAL-Y-CORD-SQUARE-DOODAD
    EMPTY-CANVAS)
   "place-doodad-square should put doodad in the empty canvas"))


;; Data Definition: Refer to the definition of the Doodad
;; place-doodad-star : Doodad Scene -> Image
;; RETURNS: A scene that potrays the given star shaped doodad
;; EXAMPLES:
#|
(place-doodad-star
   initial-doodad-star EMPTY-CANVAS) = (place-image
                                          STAR-DOODAD-IMAGE
                                          INITIAL-X-CORD-STAR-DOODAD
                                          INITIAL-Y-CORD-STAR-DOODAD
                                          EMPTY-CANVAS)
|#
;; DESIGN STRATEGY: combine simpler functions

(define (place-doodad-star d s)
  (place-image
  (radial-star POINTS INNER-RADIUS OUTER-RADIUS OUTLINE-MODE (doodad-color d))
   (doodad-x d)
   (doodad-y d)
   s))


;; TESTS
(begin-for-test
  (check-equal?
   (place-doodad-star initial-doodad-star EMPTY-CANVAS)
   (place-image
    STAR-DOODAD-IMAGE
    INITIAL-X-CORD-STAR-DOODAD
    INITIAL-Y-CORD-STAR-DOODAD
    EMPTY-CANVAS)
   "place-doodad-star should put doodad in the empty canvas"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN FUNCTIONS

;; Start the program with animation function

;; Data Definitions: none
;; initial-world : Any -> World
;; GIVEN: Any value (ignored)
;; RETURNS: the initial state of the world. The initial state of the world
;; potrays a radial-star and a square doodad moving in an enclosed canvas.
;; EXAMPLES:
;; -- (initial-world -1) = initial-world-1
;; -- (initial-world 0)  = initial-world-1
;; -- (initial-world -0.5) = initial-world-1
;; Design Strategy: combine simpler functions

(define (initial-world w)
  (make-world
   (make-doodad
    INITIAL-X-CORD-STAR-DOODAD
    INITIAL-Y-CORD-STAR-DOODAD
    INITIAL-X-VEL-STAR-DOODAD
    INITIAL-Y-VEL-STAR-DOODAD
    INITIAL-COLOR-STAR-DOODAD)
   (make-doodad
    INITIAL-X-CORD-SQUARE-DOODAD
    INITIAL-Y-CORD-SQUARE-DOODAD
    INITIAL-X-VEL-SQUARE-DOODAD
    INITIAL-Y-VEL-SQUARE-DOODAD
    INITIAL-COLOR-SQUARE-DOODAD)
   PAUSED))

;; TESTS:


(define
  initial-world-1
  (make-world
   initial-doodad-star
   initial-doodad-square
   PAUSED))

(begin-for-test
  (check-equal?
   (initial-world -1)
   initial-world-1
   "initial world should return a world with a star and square doodad in
      unpaused state")

  (check-equal?
   (doodad-color (world-doodad-star initial-world-1))
   GOLD
   "color of the doodad-star in initial world should be Gold")

  (check-equal?
   (doodad-color (world-doodad-square initial-world-1))
   GRAY
   "color of the doodad-square in initial world should be Gold")

  (check-equal?
   (world-paused? initial-world-1)
   false
   "initial world should be in unpaused state"))


;; Data Definitions: Refer to the definition of the world
;; world-after-tick : World -> World
;; RETURNS: the world that should follow the given world after the tick
;; EXAMPLES:
;; -- (world-after-tick world-paused) = world-paused
#|
   -- (world-after-tick
         world-not-paused) = (make-world
                                (doodad-star-after-the-tick
                                (world-doodad-star world-not-paused))
                                (doodad-square-after-the-tick
                                (world-doodad-square world-not-paused))
                                (world-paused? world-not-paused))
|#
;; DESIGN STRATEGY: cases based on w

(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world
       (doodad-star-after-the-tick (world-doodad-star w))
       (doodad-square-after-the-tick (world-doodad-square w))
       (world-paused? w))))

;; TESTS:

(begin-for-test
  (check-equal?
   (world-after-tick world-paused)
   world-paused
   "world after tick will remain the same if world is paused")

  (check-equal?
   (world-after-tick world-not-paused)
   (make-world
    (doodad-star-after-the-tick (world-doodad-star world-not-paused))
    (doodad-square-after-the-tick (world-doodad-square world-not-paused))
    (world-paused? world-not-paused))
   "world after tick will remain the same if world is paused"))


;; Data Definitions: Refer to the World and KeyEvent definition
;; world-after-key-event: World KeyEvent -> World
;; GIVEN : current state of the world and a key event.
;; RETURNS: the world that should follow given world after the key event.
;;  -- Pauses/un-pauses the world after spacebar key event and ignores
;;  -- other key events.
;; EXAMPLES:
#|
key event is a paused key event
(world-after-key-event world-paused paused-key-event) = world-not-paused
(world-after-key-event world-not-paused paused-key-event) = world-paused

key event is not a paused key event
(world-after-key-event world-paused non-paused-key-event) = world-paused
(world-after-key-event world-not-paused non-paused-key-event) = world-not-paused
|#
;; DESIGN STRATEGY: cases based on ke

(define (world-after-key-event w ke)
  (if (is-pause-key-event? ke)
      (world-with-paused-toggled w)
      w))

;;TESTS:

(begin-for-test
  (check-equal?
   (world-after-key-event world-paused paused-key-event)
   world-not-paused
   "world-after-key-event will return non-paused-world when a paused
      key event is applied on a paused world")

  (check-equal?
   (world-after-key-event world-not-paused paused-key-event)
   world-paused
   "world-after-key-event will return paused-world when a paused
      key event is applied on a non-paused world")

  (check-equal?
   (world-after-key-event world-paused non-paused-key-event)
   world-paused
   "world-after-key-event will return paused world when a non paused
      key event is applied as world remains unaffected")

  (check-equal?
   (world-after-key-event world-not-paused non-paused-key-event)
   world-not-paused
   "world-after-key-event will return non paused world when a non paused
      key event is applied as world remains unaffected"))



;; Data Defintions: Refer to the World data definition
;; world-to-scene : World -> Scene
;; RETURNS: A scene the portrays the given world
;; EXAMPLES:
;; -- (world-to-scene intial-world-1) = initial-image-of-world
;; DESIGN STRATEGY: combine simpler functions
(define (world-to-scene w)
  (place-doodad-star
   (world-doodad-star w)
   (place-doodad-square (world-doodad-square w) EMPTY-CANVAS)))

(define
  initial-image-of-world
  (place-image
   STAR-DOODAD-IMAGE
   INITIAL-X-CORD-STAR-DOODAD
   INITIAL-Y-CORD-STAR-DOODAD
   (place-image
    SQUARE-DOODAD-IMAGE
    INITIAL-X-CORD-SQUARE-DOODAD
    INITIAL-Y-CORD-SQUARE-DOODAD
    EMPTY-CANVAS)))
   
;; TESTS
(begin-for-test
  (check-equal?
   (world-to-scene initial-world-1)
   initial-image-of-world
   "world-to-scene should return initial image of the world at the begining"))


;; animation : PosReal -> World
;; GIVEN: the speed of the animation, in seconds per tick
;; EFFECT: runs the animation, starting with intial world
;; RETURNS: the final state of the world
;; Design Strategy: combine simpler functions

(define (animation speed)
  (big-bang (initial-world speed)
            (on-tick world-after-tick speed)
            (on-draw world-to-scene)
            (on-key world-after-key-event)))