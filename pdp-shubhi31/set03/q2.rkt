;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; q2.rkt: Adds jerkless dragging and new features in the world potrayed
; through q1.rkt

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
(provide world-after-mouse-event)
(provide doodad-after-mouse-event)
(provide doodad-selected?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-location "03" "q2.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
(define STAR-DOODAD-SELECTED false)
(define INNER-RADIUS 10)
(define OUTER-RADIUS 50)
(define HALF-STAR-WIDTH 50)
(define POINTS 8)


;; Square doodad specific constants
(define INITIAL-X-CORD-SQUARE-DOODAD 460)
(define INITIAL-Y-CORD-SQUARE-DOODAD 350)
(define INITIAL-X-VEL-SQUARE-DOODAD -13)
(define INITIAL-Y-VEL-SQUARE-DOODAD -9)
(define INITIAL-COLOR-SQUARE-DOODAD "gray")
(define LENGHT-OF-SIDE 71)
(define HALF-SQUARE-WIDTH 35.5)
(define SQUARE-DOODAD-SELECTED false)

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

;; Image of circle to be displayed on the selected doodad
(define CIRCLE (circle 3 "solid" "black"))

;; MOUSE EVENTS
(define BUTTON-UP "button-up")
(define BUTTON-DOWN "button-down")
(define DRAG "drag")

;; Default values for x and y coordinates on mouse selection
;; when nothing is selected
(define INITIAL-SELECTED-X 0)
(define INITIAL-SELECTED-Y 0)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

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

;; square-color-fn : String -> ??

#|
(define (square-color-fn c)
  (cond
    [(string=? GRAY c) ...]
    [(string=? OLIVE-DRAB c) ...]
    [(string=? KHAKI c) ...]
    [(string=? ORANGE c) ...]
    [(string=? CRIMSON c) ...]))

|#


(define STAR "star")
(define SQUARE "square")

;; A DoodadType is one of the
;; -- STAR
;; -- SQUARE
;; INTERPRETATION:
;; STAR is the star shaped doodad
;; SQUARE is the square shaped doodad

;; doodad-type-fn : String -> ??
;; TEMPLATE:
#|
(define (doodad-type-fn t)
  (cond
    [(string=? STAR) ...]
    [(string=? SQUARE) ...]))

|#


(define-struct doodad ( x y vx vy color selected? type selected-x selected-y))

; A Doodad is the structure
; (make-doodad
;    Integer Integer Integer Integer String Boolean DoodadType Integer Integer)
; INTERPRETATION:
; -- x is the x-coordinate of the doodad in pixels
; -- y is the y-coordinate of the doodad in pixels
; -- vx is the number of the pixels doodad moves in x direction on each tick
; -- vy is the number of the pixels doodad moves in y direction on each tick
; -- color is the color of the doodad after each core bounce
; -- selected? is the boolean which is true when the doodad is selected using
; -- type is the itemization data of DoodadType which represents the type of
; doodad i.e. STAR OR SQUARE
; -- selected-x is the value of x in pixels when the doodad is selected,
;    it defauts to 0 for unselected doodads
; -- selected-y is the value of y in pixels when the doodad is selected
;    it defaults to 0 for unselected doodads.

;;TEMPLATE:

;; doodad-fn : Doodad -> ??

#|
(define (doodad-fn d)
  (..
   (doodad-x d)
   (doodad-y d)
   (doodad-vx d)
   (doodad-vy d)
   (doodad-color d)
   (doodad-selected? d)
   (doodad-type d)
   (doodad-selected-x d)
   (doodad-selected-y d)))
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


;; A MouseEvent is a scalar datatype defined in the 2htdp/universe in racket.
;; It represents mouse events including mouse movements and clicks by user.
;; All mouse events are represented by strings

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; common test examples
;; To be used as test constants across multiple functions

(define SELECTED-X 10)
(define SELECTED-Y 80)


;; Both of the star doodads are unselected doodads
(define doodad-star-1
  (make-doodad
   0 100 20 600 GOLD false STAR INITIAL-SELECTED-X INITIAL-SELECTED-Y))

(define doodad-square-1
  (make-doodad
   0 100 20 600 GRAY false SQUARE INITIAL-SELECTED-X INITIAL-SELECTED-Y))


;; Both of the star doodads are selected doodads
(define doodad-star-selected-1
  (make-doodad
   0 100 20 600 GOLD true STAR SELECTED-X SELECTED-Y))

(define doodad-square-selected-1
  (make-doodad
   0 100 20 600 GRAY true SQUARE SELECTED-X SELECTED-Y))



;; Both of the doodads are unselected and represent intital state
(define initial-doodad-star
  (make-doodad
   125 120 10 12 GOLD false STAR INITIAL-SELECTED-X INITIAL-SELECTED-Y))

(define initial-doodad-square
  (make-doodad
   460 350 -13 -9 GRAY false SQUARE INITIAL-SELECTED-X INITIAL-SELECTED-Y))

;; World in the paused state
(define world-paused
  (make-world
   doodad-star-1
   doodad-square-1
   true))

;; world in the non-paused state
(define world-not-paused
  (make-world
   doodad-star-1
   doodad-square-1
   false))

;; world in the paused state with both of the doodads selected
(define world-paused-selected
  (make-world
   doodad-star-selected-1
   doodad-square-selected-1
   true))

;; World in the non-paused state with both of the doodads selected
(define world-not-paused-selected
  (make-world
   doodad-star-selected-1
   doodad-square-selected-1
   false))

(define paused-key-event " ")
(define non-paused-key-event "q")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WISHLIST-1


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS


;; Data Defintions: Refer to the StarColor definition
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
   
 
;; Data Defintions: Refer to the SquareColor definition
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



;; Data Definitions : none
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


;; Data Definitions : none
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


;; Data Definitions : none
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


;; Data Definitions : Refer to the Doodad data definition.
;; tentative-x : Doodad -> Integer
;; RETURNS: summation of the x and vx of the given doodad in pixels.
;;  This value represents the possible next value of x coordinates of the doodad
;;  in pixels.
;; EXAMPLES:
;; -- (tentative-x (make-doodad 0 20 30 40 "grey")) = 30
;; -- (tentative-x (make-doodad 0 20 -1 40 "grey")) = -1
;; STRATEGY: combine simpler functions

(define (tentative-x d)
  (+ (doodad-x d) (doodad-vx d)))


;; TESTS
(begin-for-test
  (check-equal?
   (tentative-x
    (make-doodad
     0 20 30 40 "grey" false SQUARE INITIAL-SELECTED-X INITIAL-SELECTED-Y))
   30
   "tentative-x should return 30")

 (check-equal?
   (tentative-x
    (make-doodad
     0 20 -1 40 "grey" false SQUARE INITIAL-SELECTED-X INITIAL-SELECTED-Y))
   -1
   "tentative-x should return -1"))


;; Data Definitions : Refer to the Doodad data definition.
;; tentative-y : Doodad -> Integer
;; RETURNS: summation of the y and vy of the given doodad in pixels
;;  This value represents the possible next value of y coordinates of the doodad
;;  in pixels.
;; EXAMPLES:
;; -- (tentative-y (make-doodad 0 20 30 40 "grey")) = 60
;; -- (tentative-y (make-doodad 0 20 -1 -80 "grey")) = -60
;; STRATEGY: combine simpler functions

(define (tentative-y d)
  (+ (doodad-y d) (doodad-vy d)))

;; TESTS
(begin-for-test
  (check-equal?
   (tentative-y
    (make-doodad
     0 20 30 40 "grey" false SQUARE INITIAL-SELECTED-X INITIAL-SELECTED-Y))
   60
   "tentative-y should return 60")

 (check-equal?
   (tentative-y
    (make-doodad
     0 20 -1 -80 "grey" false SQUARE INITIAL-SELECTED-X INITIAL-SELECTED-Y))
   -60
   "tentative-y should return -60"))


;; Data Definitions : none
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


;; Data Definitions : none
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


;; Data Definitions : none
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


;; Data Definitions : none
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


;; Data Definitions : Refer to the Doodad data definition
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


;; Both of the doodad's are unselected
(define doodad-star-to-be-core-bounced
  (make-doodad
   0 100 20 600 GOLD false STAR INITIAL-SELECTED-X INITIAL-SELECTED-Y))
(define doodad-star-not-to-be-core-bounced
  (make-doodad
   0 100 20 100 GOLD false STAR INITIAL-SELECTED-X INITIAL-SELECTED-Y))

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


;; Data Definitions : Refer to the Doodad data definition
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

;; Both of the square doodads are unselected
(define doodad-square-to-be-core-bounced
  (make-doodad
   0 100 20 600 GRAY false SQUARE INITIAL-SELECTED-X INITIAL-SELECTED-Y))
(define doodad-square-not-to-be-core-bounced
  (make-doodad
   0 100 20 100 GRAY false SQUARE INITIAL-SELECTED-X INITIAL-SELECTED-Y))

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


;; Data Definitions : Refer to the Doodad data definition
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
  (if (not (doodad-selected? d))
      (make-doodad
       (doodad-x-pos-after-the-tick (tentative-x d))
       (doodad-y-pos-after-the-tick (tentative-y d))
       (doodad-x-vel-after-the-tick (tentative-x d) (doodad-vx d))
       (doodad-y-vel-after-the-tick (tentative-y d) (doodad-vy d))
       (doodad-star-color-after-the-tick d)
       (doodad-selected? d)
       (doodad-type d)
       (doodad-selected-x d)
       (doodad-selected-y d))
      d))

;; constants for tests
;; doodad-star to be core bounced after the tick

;; Both of the doodads are unselected 
(define doodad-star-1-before-tick doodad-star-1)
(define doodad-star-1-after-tick
  (make-doodad
   20 196 20 -600 GREEN false STAR INITIAL-SELECTED-X INITIAL-SELECTED-Y))

;; Both of the doodads are unselected
;; doodad-star not to be core bounced after the tick
(define doodad-star-2-before-tick
  (make-doodad
   0 100 20 100 GOLD false STAR INITIAL-SELECTED-X INITIAL-SELECTED-Y))
(define doodad-star-2-after-tick
  (make-doodad
   20 200 20 100 GOLD false STAR INITIAL-SELECTED-X INITIAL-SELECTED-Y))

;; TESTS:
(begin-for-test
  (check-equal?
   (doodad-star-after-the-tick doodad-star-1-before-tick)
   doodad-star-1-after-tick
   "doodad star 1 will get core bounced after the tick.")

  (check-equal?
   (doodad-star-after-the-tick doodad-star-2-before-tick)
   doodad-star-2-after-tick
   "doodad star 2 will not get core bounced after the tick.")
  
  (check-equal?
   (doodad-star-after-the-tick doodad-star-selected-1)
   doodad-star-selected-1
   "same star doodad will get returned"))


;; Data Definitions : Refer to the Doodad data definition
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
  (if (not (doodad-selected? d))
      (make-doodad
       (doodad-x-pos-after-the-tick (tentative-x d))
       (doodad-y-pos-after-the-tick (tentative-y d))
       (doodad-x-vel-after-the-tick (tentative-x d) (doodad-vx d))
       (doodad-y-vel-after-the-tick (tentative-y d) (doodad-vy d))
       (doodad-square-color-after-the-tick d)
       (doodad-selected? d)
       (doodad-type d)
       (doodad-selected-x d)
       (doodad-selected-y d))
      d))


;; Doodads are unselected
;; constants for tests
;; doodad-square to be core bounced after the tick
(define doodad-square-1-before-tick doodad-square-1)
(define doodad-square-1-after-tick
  (make-doodad
   20 196 20 -600
   OLIVE-DRAB false SQUARE INITIAL-SELECTED-X INITIAL-SELECTED-Y))

;; doodad-square not to be core bounced after the tick
(define doodad-square-2-before-tick
  (make-doodad
   0 100 20 100 GRAY false SQUARE INITIAL-SELECTED-X INITIAL-SELECTED-Y))
(define doodad-square-2-after-tick
  (make-doodad
   20 200 20 100 GRAY false SQUARE INITIAL-SELECTED-X INITIAL-SELECTED-Y))

;; TESTS:
(begin-for-test
  (check-equal?
   (doodad-square-after-the-tick doodad-square-1-before-tick)
   doodad-square-1-after-tick
   "doodad square-1 will get core bounced after the tick")

  (check-equal?
   (doodad-square-after-the-tick doodad-square-2-before-tick)
   doodad-square-2-after-tick
   "doodad square-2 will not get core bounced after the tick")

  (check-equal?
   (doodad-square-after-the-tick doodad-square-selected-1)
   doodad-square-selected-1
   "same square doodad will get returned"))


;; Data Definitions : Refer to the KeyEvent data definition
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



;; Data Definitions : Refer to the World data definition
;; world-with-paused-toggled : World -> World
;; RETURNS: the world just like the given one but with paused
;; toggled
;; EXAMPLES:
#| -- (world-with-paused-toggled world-paused) = (make-world
                                            (world-doodad-star w)
                                            (world-doodad-square w)
                                            (not (world-paused? w)))
   -- (world-with-paused-toggled world-not-paused) = (make-world
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


;; Data Definition: Refer to the Doodad data definition
;; doodad-square-image : Doodad -> Image
;; GIVEN: A doodad of shape of a square
;; RETURNS: Image of the square shaped doodad
;; EXAMPLES:
;; -- (doodad-square-image initial-doodad-square) = SQUARE-DOODAD-IMAGE
;; DESIGN STRATEGY: combine simpler functions

(define (doodad-square-image d)
  (square LENGHT-OF-SIDE OUTLINE-MODE (doodad-color d)))


;; Data Definitions: Refer to the Doodad definition
;; place-doodad-square : Doodad Scene -> Image
;; RETURNS: A scene that potrays the given square shaped doodad in the given
;;  scene.
;; EXAMPLES:
#|
(place-doodad-square initial-doodad-square EMPTY-CANVAS) =
    (place-image
     SQUARE-DOODAD-IMAGE
     INITIAL-X-CORD-SQUARE-DOODAD
     INITIAL-Y-CORD-SQUARE-DOODAD
     EMPTY-CANVAS)
|#
;; DESIGN STRATEGY: combine simpler functions

(define (place-doodad-square d s)
  (if (doodad-selected? d)
      (place-image
       CIRCLE
       (doodad-selected-x d)
       (doodad-selected-y d)
       (place-image
        (square LENGHT-OF-SIDE OUTLINE-MODE (doodad-color d))
        (doodad-x d)
        (doodad-y d)
        s))
  (place-image
   (doodad-square-image d)
   (doodad-x d)
   (doodad-y d)
   s)))

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


;; Data Definition: Refer to the Doodad data definition
;; doodad-star-image : Doodad -> Image
;; GIVEN: A doodad of shape of a star
;; RETURNS: Image of the star shaped doodad
;; EXAMPLES:
;; -- (doodad-star-image initial-doodad-star) = STAR-DOODAD-IMAGE
;; DESIGN STRATEGY: combine simpler functions
(define (doodad-star-image d)
  (radial-star POINTS INNER-RADIUS OUTER-RADIUS OUTLINE-MODE (doodad-color d)))


;; Data Definition: Refer to the definition of Doodad
;; place-doodad-star : Doodad Scene -> Image
;; RETURNS: A scene that potrays the given star shaped doodad
;; EXAMPLES:

#|
(place-doodad-star initial-doodad-star EMPTY-CANVAS) =
   (place-image
     STAR-DOODAD-IMAGE
     INITIAL-X-CORD-STAR-DOODAD
     INITIAL-Y-CORD-STAR-DOODAD
     EMPTY-CANVAS)
|#
;; DESIGN STRATEGY: combine simpler functions

(define (place-doodad-star d s)
  (if (doodad-selected? d)
      (place-image
       CIRCLE
       (doodad-selected-x d)
       (doodad-selected-y d)
       (place-image (doodad-star-image d) (doodad-x d) (doodad-y d) s))
  (place-image
   (doodad-star-image d)
   (doodad-x d)
   (doodad-y d)
   s)))



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


;; Data Definition : Refer to the definition of the Doodad
;; doodad-star-after-key-event-c : Doodad -> Doodad
;; RETURNS: The given doodad
;; EXAMPLES:
;; -- (doodad-star-after-key-event-c
;; --  doodad-star-selected-1 "c") = doodad-star-after-color-change
;; DESIGN STRATEGY: combine simpler functions

(define doodad-star-after-color-change
  (make-doodad
       (doodad-x doodad-star-selected-1)
       (doodad-y doodad-star-selected-1)
       (doodad-vx doodad-star-selected-1)
       (doodad-vy doodad-star-selected-1)
       (next-color-star-doodad (doodad-color doodad-star-selected-1))
       (doodad-selected? doodad-star-selected-1)
       (doodad-type doodad-star-selected-1)
       (doodad-selected-x doodad-star-selected-1)
       (doodad-selected-y doodad-star-selected-1)))


(define (doodad-star-after-key-event-c d)
  (if (doodad-selected? d)
      (make-doodad
       (doodad-x d)
       (doodad-y d)
       (doodad-vx d)
       (doodad-vy d)
       (next-color-star-doodad (doodad-color d))
       (doodad-selected? d)
       (doodad-type d)
       (doodad-selected-x d)
       (doodad-selected-y d))
      d))


;; Data Definition : Refer to the definition of the Doodad
;; doodad-square-after-key-event-c : Doodad -> Doodad
;; RETURNS: The given doodad
;; EXAMPLES:
;; -- (doodad-square-after-key-event-c
;; --  doodad-square-selected-1 "c") = doodad-square-after-color-change
;; DESIGN STRATEGY: combine simpler functions


(define doodad-square-after-color-change
  (make-doodad
       (doodad-x doodad-square-selected-1)
       (doodad-y doodad-square-selected-1)
       (doodad-vx doodad-square-selected-1)
       (doodad-vy doodad-square-selected-1)
       (next-color-square-doodad (doodad-color doodad-square-selected-1))
       (doodad-selected? doodad-square-selected-1)
       (doodad-type doodad-square-selected-1)
       (doodad-selected-x doodad-square-selected-1)
       (doodad-selected-y doodad-square-selected-1)))


(define (doodad-square-after-key-event-c d)
  (if (doodad-selected? d)
      (make-doodad
       (doodad-x d)
       (doodad-y d)
       (doodad-vx d)
       (doodad-vy d)
       (next-color-square-doodad (doodad-color d))
       (doodad-selected? d)
       (doodad-type d)
       (doodad-selected-x d)
       (doodad-selected-y d))
      d))

;; Data Definition : Refer to the definition of the World
;; world-after-key-event-c : World -> World
;; RETURNS: The world that should follow the given world after the key event
;; "c" occurs
;; EXAMPLES:
;; -- (world-after-key-event-c
;; --    world-paused-selected) = world-paused-chaged-after-key-event-c
;; DESIGN STRATEGY: combine simpler functions

(define world-paused-chaged-after-key-event-c
  (make-world
   doodad-star-after-color-change
   doodad-square-after-color-change
   true))

(define (world-after-key-event-c w)
  (make-world
   (doodad-star-after-key-event-c (world-doodad-star w))
   (doodad-square-after-key-event-c (world-doodad-square w))
   (world-paused? w)))


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
    INITIAL-COLOR-STAR-DOODAD
    STAR-DOODAD-SELECTED
    STAR
    INITIAL-SELECTED-X
    INITIAL-SELECTED-Y)
   (make-doodad
    INITIAL-X-CORD-SQUARE-DOODAD
    INITIAL-Y-CORD-SQUARE-DOODAD
    INITIAL-X-VEL-SQUARE-DOODAD
    INITIAL-Y-VEL-SQUARE-DOODAD
    INITIAL-COLOR-SQUARE-DOODAD
    SQUARE-DOODAD-SELECTED
    SQUARE
    INITIAL-SELECTED-X
    INITIAL-SELECTED-Y)
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


;; Data Definitions: Refer to the World data definition
;; world-after-tick : World -> World
;; RETURNS: the world that should follow the given world after the tick
;; EXAMPLES:
;; -- (world-after-tick world-paused) = world-paused
#|
   -- (world-after-tick world-not-paused) =
         (make-world
           (doodad-star-after-the-tick (world-doodad-star world-not-paused))
           (doodad-square-after-the-tick (world-doodad-square world-not-paused))
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


;; Data Definition: Refer to the World and KeyEvent data definition
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
  (cond [(is-pause-key-event? ke) (world-with-paused-toggled w)]
        [(key=? ke "c") (world-after-key-event-c w)]
        [else w]))


;; test specific examples/constants
(define key-event-c "c")

(define world-paused-after-key-event-c
  (make-world
   (make-doodad 0 100 20 600 "green" true "star" 10 80)
   (make-doodad 0 100 20 600 "olivedrab" #true "square" 10 80)
   true))

(define world-not-paused-after-key-event-c
  (make-world
   (make-doodad 0 100 20 600 "gold" false "star" 0 0)
   (make-doodad 0 100 20 600 "gray" false "square" 0 0) false))

  
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
      key event is applied as world remains unaffected")

  (check-equal?
   (world-after-key-event world-paused-selected key-event-c)
   world-paused-after-key-event-c
   "world-after-key-event will return")
  
  (check-equal?
   (world-after-key-event world-not-paused key-event-c)
   world-not-paused-after-key-event-c
   "world-after-key-event will return"))


;; Data Definitions : Refer to the World Data Definition
;; world-to-scene : World -> Scene
;; RETURNS: A scene the portrays the given world
;; EXAMPLES:
;; -- (world-to-scene intial-world-1) = initial-image-of-world
;; DESIGN STRATEGY: combine simpler functions

(define (world-to-scene w)
  (place-doodad-star
   (world-doodad-star w)
   (place-doodad-square (world-doodad-square w) EMPTY-CANVAS)))


;; test specific examples/constants

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

(define
  image-of-world-paused-selected
  (world-to-scene world-paused-selected))
  
;; TESTS
(begin-for-test
  (check-equal?
   (world-to-scene initial-world-1)
   initial-image-of-world
   "world-to-scene should return initial image of the world at the begining")
  
  (check-equal?
   (world-to-scene world-paused-selected)
   image-of-world-paused-selected
   "world-to-scene should return image of the world in paused state with
      selected doodads"))


;; Data Definitions : none
;; animation : PosReal -> World
;; GIVEN: the speed of the animation, in seconds per tick
;; EFFECT: runs the animation, starting with intial world
;; RETURNS: the final state of the world
;; Design Strategy: combine simpler functions

(define (animation speed)
  (big-bang (initial-world speed)
            (on-tick world-after-tick speed)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))


;; WISHLIST - 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HELPER FUNCTIONS FOR NEW FEATURE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Data Definitions: Refer to the Doodad data definition
;; mouse-within-star-doodad? : Doodad Integer Integer -> Boolean
;; RETURNS: true if the mouse coordinates lie within the doodad boundary
;; EXAMPLES:
;; --  (mouse-within-star-doodad? doodad-star-1 SELECTED-X SELECTED-Y) = true
;; DESIGN STRATEGY: combine simpler functions
(define (mouse-within-star-doodad? d mx my)
  (and
    (<=
     (- (doodad-x d) HALF-STAR-WIDTH)
     mx
     (+ (doodad-x d) HALF-STAR-WIDTH))
    (<=
     (- (doodad-y d) HALF-STAR-WIDTH)
     my
     (+ (doodad-y d) HALF-STAR-WIDTH))))



;; Data Definitions: Refer to the Doodad data definition
;; mouse-within-square-dooadad? : Doodad Integer Integer -> Boolean
;; RETURNS: true if the mouse coordinates lie within the doodad boundary
;; EXAMPLES:
;; (mouse-within-square-doodad?
;;    doodad-square-selected-1 SELECTED-X SELECTED-Y) = true
;; DESIGN STRATEGY: combine simpler functions

(define (mouse-within-square-doodad? d mx my)
  (and
    (<=
     (- (doodad-x d) HALF-SQUARE-WIDTH)
     mx
     (+ (doodad-x d) HALF-SQUARE-WIDTH))
    (<=
     (- (doodad-y d) HALF-SQUARE-WIDTH)
     my
     (+ (doodad-y d) HALF-SQUARE-WIDTH))))


;; Data Definitions: Refer to the Doodad data definition
;; mouse-within-doodad? : Doodad Integer Integer -> Boolean
;; GIVEN: A Doodad and x and y coordinates of the mouse
;; RETURNS: true if the given mouse coordinates lies within the doodad's
;; boundaries
;; EXAMPLES:
;; (mouse-within-doodad? doodad-square-selected-1 SELECTED-X SELECTED-Y) = true
;; (mouse-within-doodad? doodad-star-selected-1 SELECTED-X SELECTED-Y) = true
;; DESIGN STRATEGY: cases based on doodad type (DoodadType)

(define (mouse-within-doodad? d mx my)
  (cond
    [(string=? (doodad-type d) STAR) (mouse-within-star-doodad? d mx my)]
    [(string=? (doodad-type d) SQUARE) (mouse-within-square-doodad? d mx my)]))


;; Data Definitions: Refer to the Doodad data definition
;; doodad-after-grabbed : Doodad Integer Integer -> Doodad
;; GIVEN: Doodad and x and y coordinates of the mouse
;; RETURNS: The given doodad with selected? as true
;; EXAMPLES:
;; (doodad-after-grabbed
;;    doodad-star-1 SELECTED-X SELECTED-Y) = doodad-star-selected-1
;; (doodad-after-grabbed
;;    doodad-square-1 SELECTED-X SELECTED-Y) = doodad-square-selected-1
;; DESIGN STRATEGY: combine simpler functions

(define (doodad-after-grabbed d mx my)
  (make-doodad
   (doodad-x d)
   (doodad-y d)
   (doodad-vx d)
   (doodad-vy d)
   (doodad-color d)
   true
   (doodad-type d)
   mx
   my))


;; Data Definitions: Refer to the Doodad data definition
;; doodad-after-toggled-selected : Doodad Integer Integer -> Doodad
;; GIVEN: doodad and x and y of the mouse
;; RETURNS: The given doodad with toggled valued for selected
;; EXAMPLES:
;; doodad-after-toggled-selected
;;   (world-doodad-star world-paused)
;;   SELECTED-X SELECTED-Y) = doodad-star-selected-1
;; DESIGN STRATEGY: combine simpler functions

(define (doodad-after-toggled-selected d mx my)
  (make-doodad
   (doodad-x d)
   (doodad-y d)
   (doodad-vx d)
   (doodad-vy d)
   (doodad-color d)
   (not (doodad-selected? d))
   (doodad-type d)
   (if (doodad-selected? d) INITIAL-SELECTED-X mx)
   (if (doodad-selected? d)  INITIAL-SELECTED-Y my)))

;;TESTS

(begin-for-test
  (check-equal?
   (doodad-after-toggled-selected
    (world-doodad-star world-paused) SELECTED-X SELECTED-Y)
   doodad-star-selected-1
   "The doodad will get selected when mouse event button down gets triggered."))


;; Data Definitions: Refer to the Doodad data definition
;; doodad-after-mouse-button-down : Doodad Integer Integer -> Doodad
;; GIVEN: A doodad and x and y coordinates of mouse
;; RETURNS: The doodad that should follow the diven doodad on mouse button down
;; at the given mouse coordinates
;; EXAMPLES:
;; (doodad-after-mouse-button-down
;;    doodad-star-1 SELECTED-X SELECTED-Y) = doodad-star-selected-1
;; (doodad-after-mouse-button-down
;;    doodad-square-1 SELECTED-X SELECTED-Y) = doodad-square-selected-1
;; DESIGN STRATEGY: case based on the mouse coordinates


(define (doodad-after-mouse-button-down d mx my)
  (if (mouse-within-doodad? d mx my) (doodad-after-grabbed d mx my) d))


;; Data Definitions: Refer to the Doodad data definition
;; doodad-after-mouse-button-up : Doodad Integer Integer -> Doodad
;; GIVEN: A doodad and x and y coordinates of the mouse
;; RETURNS: The doodad that should follow the existing doodad after mouse
;; is released
;; EXAMPLES:
;; (doodad-after-mouse-button-up
;;    doodad-star-selected-1 SELECTED-X SELECTED-Y) = doodad-star-1
;; (doodad-after-mouse-button-up
;;    doodad-square-selected-1 SELECTED-X SELECTED-Y) = doodad-square-1
;; DESIGN STRATEGY: case based on selected? in doodad


(define (doodad-after-mouse-button-up d mx my)
  (if (doodad-selected? d)
      (doodad-after-toggled-selected d mx my)
      d))


;; Data Definition: Refer to the Doodad data definition
;; doodad-after-mouse-drag : Doodad Integer Integer -> Doodad
;; GIVEN: A doodad and x and y coordinates of the mouse
;; RETURNS: The doodad that should follow the given doodad after the  mouse
;; has been dragged by the user.
;; EXAMPLES:
;; (doodad-after-mouse-drag
;;    doodad-star-selected-1
;;    SELECTED-X SELECTED-Y) = doodad-star-dragged-to-10-80
;; DESIGN STRATEGY: case based on selected? in doodad

(define (doodad-after-mouse-drag d mx my)
  (if (doodad-selected? d)
      (make-doodad
       (- mx (- (doodad-selected-x d) (doodad-x d)))
       (- my (- (doodad-selected-y d) (doodad-y d)))
       (doodad-vx d)
       (doodad-vy d)
       (doodad-color d)
       (doodad-selected? d)
       (doodad-type d)
       mx
       my)
      d))


;; Data Definition: Refer to the Doodad data definition
;; doodad-after-mouse-event: Doodad Integer Integer MouseEvent -> Doodad
;; GIVEN: A doodad, x and y coordinates of mouse and mouse events
;; RETURNS: The doodad that should follow the given doodad after the mouse event
;; EXAMPLES:
;; (doodad-after-mouse-event
;;    doodad-star-1 SELECTED-X SELECTED-Y BUTTON-DOWN) = doodad-star-selected-1
;; (doodad-after-mouse-event
;;    doodad-star-selected-1 SELECTED-X SELECTED-Y BUTTON-UP) = doodad-star-1
;; (doodad-after-mouse-event
;;    doodad-star-selected-1
;;    SELECTED-X SELECTED-Y DRAG) = doodad-star-dragged-to-10-80
;; (doodad-after-mouse-event
;;    doodad-star-1 SELECTED-X SELECTED-Y "move") = doodad-star-1
;; DESIGN STRATEGY: cases based on the mev


(define doodad-star-dragged-to-10-80
   (make-doodad 10 80 20 600 "gold" true "star" 10 80))
  
(define (doodad-after-mouse-event d mx my mev)
  (cond [(mouse=? mev BUTTON-DOWN) (doodad-after-mouse-button-down d mx my)]
        [(mouse=? mev BUTTON-UP) (doodad-after-mouse-button-up d mx my)]
        [(mouse=? mev DRAG) (doodad-after-mouse-drag d mx my)]
        [else d]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NEW FEATURE : MOUSE EVENT

;; Data Definition: Refer to the World and MouseEvent data definition
;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: The current state of the world, x and y coordinates of mouse and
;; mouse event
;; RETURNS: the world that should follow the given world after the mouse event
;; EXAMPLES:

#|
;; world is paused and nothing is selected
(world-after-mouse-event
 world-paused
 INITIAL-SELECTED-X INITIAL-SELECTED-Y BUTTON-DOWN) = world-paused

;; world  is paused and we have something selected in the world
(world-after-mouse-event
 world-paused-selected SELECTED-X SELECTED-Y BUTTON-UP) = world-paused-selected
(world-after-mouse-event
 world-paused-selected SELECTED-X SELECTED-Y DRAG) = world-dragged-by-30-40

;; world is not paused and nothing is selected
(world-after-mouse-event
 world-not-paused
 INITIAL-SELECTED-X INITIAL-SELECTED-Y BUTTON-DOWN) = world-not-paused

|#

;; world is not paused and something is selected

#|
(world-after-mouse-event
 world-not-paused-selected INITIAL-SELECTED-X INITIAL-SELECTED-Y BUTTON-DOWN)
(world-after-mouse-event
 world-not-paused-selected INITIAL-SELECTED-X INITIAL-SELECTED-Y BUTTON-UP)
(world-after-mouse-event
 world-not-paused-selected INITIAL-SELECTED-X INITIAL-SELECTED-Y DRAG)
|#

;; DESIGN STRATEGY: combine simpler functions

(define (world-after-mouse-event w mx my mev)
  (make-world
   (doodad-after-mouse-event (world-doodad-star w) mx my mev)
   (doodad-after-mouse-event (world-doodad-square w) mx my mev)
   (world-paused? w)))

;; constants for tests

(define world-dragged-by-10-80
  (make-world
   (make-doodad 0 100 20 600 "gold" true "star" 10 80)
   (make-doodad 0 100 20 600 "gray" true "square" 10 80)
   true))

(define world-unselected
  (make-world
   (make-doodad 0 100 20 600 "gold" false "star" 0 0)
   (make-doodad 0 100 20 600 "gray" false "square" 0 0)
   true))


;; TESTS
(begin-for-test
  (check-equal?
   (world-after-mouse-event
    world-paused INITIAL-SELECTED-X INITIAL-SELECTED-Y BUTTON-DOWN)
   world-paused
   "paused world will not change on mouse button down as nothing is selected")

  (check-equal?
   (world-after-mouse-event
    world-paused INITIAL-SELECTED-X INITIAL-SELECTED-Y BUTTON-UP)
   world-paused
   "paused world will not change on mouse button up as nothing is selected")

  (check-equal?
   (world-after-mouse-event
    world-paused INITIAL-SELECTED-X INITIAL-SELECTED-Y DRAG)
   world-paused
   "paused world will not change on mouse drag as nothing is selected")

  (check-equal?
   (world-after-mouse-event
    world-not-paused INITIAL-SELECTED-X INITIAL-SELECTED-Y BUTTON-DOWN)
   world-not-paused
   "Non-paused World won't change on mouse button down as nothing is selected")

  (check-equal?
   (world-after-mouse-event
    world-not-paused INITIAL-SELECTED-X INITIAL-SELECTED-Y BUTTON-UP)
   world-not-paused
   "Non-paused World won't change on mouse button up as nothing is selected")

  (check-equal?
   (world-after-mouse-event
    world-not-paused INITIAL-SELECTED-X INITIAL-SELECTED-Y DRAG)
   world-not-paused
   "Non-paused World will not change on drag as nothing is selected")
  
  (check-equal?
   (world-after-mouse-event
    world-not-paused INITIAL-SELECTED-X INITIAL-SELECTED-Y "move")
   world-not-paused
   "Non-paused World will not change on move as move is to be ignored.")

  (check-equal?
   (world-after-mouse-event
    world-paused-selected SELECTED-X SELECTED-Y BUTTON-DOWN)
   world-paused-selected
   "paused World in which both doodads are selected will not change
      on mouse button down.")

  (check-equal?
   (world-after-mouse-event
    world-paused-selected SELECTED-X SELECTED-Y BUTTON-UP)
   world-unselected
   "doodads will get unselected on mouse button up")
  
  (check-equal?
   (world-after-mouse-event
    world-paused-selected SELECTED-X SELECTED-Y DRAG)
   world-dragged-by-10-80
   "both doodads will get dragged to mouse x and y coordinated in the
       paused World in which both doodads are selected."))