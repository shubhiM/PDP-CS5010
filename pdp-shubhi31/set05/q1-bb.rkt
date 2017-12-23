#lang racket

(require rackunit)
(require rackunit/text-ui)
(require 2htdp/image)
(require 2htdp/universe)
(require "q1.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Black-box tests.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We don't know how the Doodad and World data types are reqresented,
;;; so we can't use check-equal? on values of those data types.
;;; To simplify black-box testing, we convert values of those data
;;; types to lists of their observable properties, which can then
;;; be compared using check-equal?
;;;
;;; We've got a special problem with colors, because they can be
;;; represented so many ways.  We'll leave colors out of the lists
;;; and use special tests for colors instead.

;;; doodad-as-list : Doodad -> List
;;; Given a doodad, returns a partial representation as a list.
;;; Examples: see tests below.

(define (doodad-as-list d)
  (list 'doodad
        (doodad-x d)
        (doodad-y d)
        (doodad-vx d)
        (doodad-vy d)
        (doodad-selected? d)))

;;; world-as-list : World -> List
;;; Given a world, returns a partial representation as a list.
;;; Examples: see tests below.

(define (world-as-list w)
  (list 'world
        (world-paused? w)
        (map doodad-as-list
             (insertion-sort doodad<
                             (world-doodads-star w)))
        (map doodad-as-list
             (insertion-sort doodad<
                             (world-doodads-square w)))))

;;; world-after-ticks : World Int -> World
;;; Given a world and an integer n,
;;; returns the world as it would be after n more ticks.

(define (world-after-ticks w n)
  (if (= n 0)
      w
      (world-after-ticks (world-after-tick w)
                         (- n 1))))

;;; world-after-ticks-and-key-events : World ListOfX -> World
;;; Given a world and a list evts,
;;; where every element of evts is a non-negative integer or a KeyEvent,
;;; returns the world as it would be after the ticks and key events
;;; indicated by the given list, going left to right.

(define (world-after-ticks-and-key-events w evts)
  (cond ((empty? evts)
         w)
        ((number? (first evts))
         (world-after-ticks-and-key-events
          (world-after-ticks w (first evts))
          (rest evts)))
        (else
         (world-after-ticks-and-key-events
          (world-after-key-event w (first evts))
          (rest evts)))))

;;; world-doodad-1star : World -> Doodad
;;; Given a world, returns its first star-like doodad,
;;; but throws an exception if it has zero or more than one.

(define (world-doodad-1star w)
  (if (= 1 (length (world-doodads-star w)))
      (first (world-doodads-star w))
      (error 'world-doodad-1star "not one star" (world-doodads-star w))))

;;; world-doodad-1square : World -> Doodad
;;; Given a world, returns its first square doodad,
;;; but throws an exception if it has zero or more than one.

(define (world-doodad-1square w)
  (if (= 1 (length (world-doodads-square w)))
      (first (world-doodads-square w))
      (error 'world-doodad-1square "not one star" (world-doodads-square w))))

;;; color-after-ticks : World Int Function -> Color
;;; Given a world w, an integer n, and a selector function f
;;; where f is world-doodad-1star or world-doodad-1square,
;;; returns the color of the doodad specified by f as it
;;; would be in (world-after-ticks w n).

(define (color-after-ticks w n f)
  (doodad-color (f (world-after-ticks w n))))

;;; colors-are-equal? : Color Color -> Boolean
;;; Given two values for which image-color? returns true,
;;; returns true iff the colors might be equal.
;;; Note: Comparing strings to structures is hard,
;;; so we give up here and say they might be equal.

(define (colors-are-equal? c1 c2)
  (cond ((symbol? c1)
         (colors-are-equal? (symbol->string c1) c2))
        ((symbol? c2)
         (colors-are-equal? c1 (symbol->string c2)))
        ((and (string? c1) (string? c2))
         (string-ci=? c1 c2))
        ((and (not (string? c1))
              (not (string? c2)))
         (equal? c1 c2))
        (else true)))

;;; colors-not-equal? : Color Color -> Boolean
;;; Given two values for which image-color? returns true,
;;; returns true iff the colors might not be equal.
;;; Note: Comparing strings to structures is hard,
;;; so we give up here and say they might not be equal.

(define (colors-not-equal? c1 c2)
  (cond ((symbol? c1)
         (colors-not-equal? (symbol->string c1) c2))
        ((symbol? c2)
         (colors-not-equal? c1 (symbol->string c2)))
        ((and (string? c1) (string? c2))
         (not (string-ci=? c1 c2)))
        ((and (not (string? c1))
              (not (string? c2)))
         (not (equal? c1 c2)))
        (else true)))

;;; core-bounce-log : World Int Function -> ListOfInt
;;; Given a world w, a limit on how many ticks to explore,
;;; and a selector function (world-doodad-1star or world-doodad-1square),
;;; returns a list of tick numbers for which the doodad specified
;;; by f undergoes a Core Bounce.

(define (core-bounce-log w limit f)
  (local ((define (loop w n log)
            (if (>= n limit)
                (reverse log)
                (let* ((w2 (world-after-tick w))
                       (vx1 (doodad-vx (f w)))
                       (vy1 (doodad-vy (f w)))
                       (vx2 (doodad-vx (f w2)))
                       (vy2 (doodad-vy (f w2))))
                  (loop w2
                    (+ n 1)
                    (if (and (= vx1 vx2) (= vy1 vy2))
                        log
                        (cons n log)))))))
         (loop w 0 '())))

;;; doodad< : Doodad Doodad -> Boolean
;;; Given two doodads,
;;; returns true iff the first is less than the second
;;; in some arbitrary but fixed canonical ordering

(define (doodad< d1 d2)
  (cond ((< (doodad-x d1) (doodad-x d2))
         true)
        ((> (doodad-x d1) (doodad-x d2))
         false)
        ((< (doodad-y d1) (doodad-y d2))
         true)
        ((> (doodad-y d1) (doodad-y d2))
         false)
        ((< (doodad-vx d1) (doodad-vx d2))
         true)
        ((> (doodad-vx d1) (doodad-vx d2))
         false)
        ((< (doodad-vy d1) (doodad-vy d2))
         true)
        ((> (doodad-vy d1) (doodad-vy d2))
         false)
        (else
         false)))

;;; doodad-as-list< : ListOfX ListOfX -> Boolean
;;; Given the list representations of two doodads
;;; as produced by doodad-as-list,
;;; returns true iff the doodad represented by the first list
;;; is less than the doodad repreented by the second list
;;; in the ordering used by doodad< above.

(define (doodad-as-list< d1 d2)
  (cond ((< (second d1) (second d2))
         true)
        ((> (second d1) (second d2))
         false)
        ((< (third d1) (third d2))
         true)
        ((> (third d1) (third d2))
         false)
        ((< (fourth d1) (fourth d2))
         true)
        ((> (fourth d1) (fourth d2))
         false)
        ((< (fifth d1) (fifth d2))
         true)
        ((> (fifth d1) (fifth d2))
         false)
        (else
         false)))

;;; insertion-sort : (X -> Boolean) ListOfX -> ListOfX
;;; Given a less-than relation on X and a ListOfX,
;;; returns the list sorted according to the given ordering.

(define (insertion-sort < xs)
  (cond ((empty? xs)
         xs)
        (else
         (insert <
                 (first xs)
                 (insertion-sort < (rest xs))))))



;;; insert : (X -> Boolean) X ListOfX -> ListOfX
;;; Given a less-than relation on X, an X, and a sorted ListOfX,
;;; returns the sorted list obtained by inserting X into the given list.

(define (insert < x xs)
  (cond ((empty? xs)
         (list x))
        ((< x (first xs))
         (cons x xs))
        (else
         (cons (first xs)
               (insert < x (rest xs))))))

;;; core-bounce-special-log : World Int Function -> ListOfInt
;;; Given a world w, a limit on how many ticks to explore,
;;; and a selector function (world-doodad-1star or world-doodad-1square),
;;; returns a list of tick numbers for which the doodad specified
;;; by f undergoes a Core Bounce in which both velocity components
;;; undergo a change of sign.

(define (core-bounce-special-log w limit f)
  (local ((define (loop w n log)
            (if (>= n limit)
                (reverse log)
                (let* ((w2 (world-after-tick w))
                       (vx1 (doodad-vx (f w)))
                       (vy1 (doodad-vy (f w)))
                       (vx2 (doodad-vx (f w2)))
                       (vy2 (doodad-vy (f w2))))
                  (loop w2
                    (+ n 1)
                    (if (and (not (= vx1 vx2)) (not (= vy1 vy2)))
                        (cons n log)
                        log))))))
         (loop w 0 '())))



(define tests
  (test-suite
   "q1"
   
   ;;; Black-box tests adapted from Problem Set 03, q1.rkt
   
   (test-case
    "Test #1"
    (check-equal? (world-as-list (initial-world 0))
                  (list 'world
                        false
                        '((doodad 125 120 10 12 #f))
                        '((doodad 460 350 -13 -9 #f)))))
   
   (test-case
    "Test #2"
    (check-equal? (world-as-list
                   (world-after-key-event (initial-world 0) " "))
                  (list 'world
                        true
                        '((doodad 125 120 10 12 #f))
                        '((doodad 460 350 -13 -9 #f)))))
   
   (test-case
    "Test #3"
    (check-equal? (world-as-list
                   (world-after-tick
                    (world-after-key-event (initial-world 0) " ")))
                  (list 'world
                        true
                        '((doodad 125 120 10 12 #f))
                        '((doodad 460 350 -13 -9 #f)))))
   
   (test-case
    "Test #4"
    (check-equal? (world-as-list
                   (world-after-key-event
                    (world-after-tick
                     (world-after-key-event (initial-world 0) " "))
                    " "))
                  (list 'world
                        false
                        '((doodad 125 120 10 12 #f))
                        '((doodad 460 350 -13 -9 #f)))))
   
   (test-case
    "Test #5"
    (check-equal? (world-as-list
                   (world-after-tick (initial-world 0)))
                  (list 'world
                        false
                        '((doodad 135 132 10 12 #f))
                        '((doodad 447 341 -13 -9 #f)))))
   
   (test-case
    "Test #6"
    (check-equal? (world-as-list
                   (world-after-ticks (initial-world 0) 27))
                  (list 'world
                        false
                        '((doodad 395 444 10 12 #f))
                        '((doodad 109 107 -13 -9 #f)))))
   
   (test-case
    "Test #7"
    (check-equal? (world-as-list
                   (world-after-ticks (initial-world 0) 28))
                  (list 'world
                        false
                        '((doodad 405 440 10 -12 #f))
                        '((doodad  96  98 -13 -9 #f)))))
   
   (test-case
    "Test #8"
    (check-equal? (world-as-list
                   (world-after-ticks (initial-world 0) 35))
                  (list 'world
                        false
                        '((doodad 475 356 10 -12 #f))
                        '((doodad   5  35 -13 -9 #f)))))
   
   (test-case
    "Test #9"
    (check-equal? (world-as-list
                   (world-after-ticks (initial-world 0) 36))
                  (list 'world
                        false
                        '((doodad 485 344 10 -12 #f))
                        '((doodad   8  26 13 -9 #f)))))
   
   (test-case
    "Test #10"
    (check-equal? (core-bounce-log (initial-world 0) 200 world-doodad-1star)
                  (list 27 47 64 102 107 139 167 176)
                  "core bounces computed incorrectly for star doodad"))
   
   (test-case
    "Test #11"
    (check-equal? (colors-are-equal?
                   (color-after-ticks (initial-world 0)  0 world-doodad-1star)
                   (color-after-ticks (initial-world 0) 27 world-doodad-1star))
                  true
                  "star color should be gold at first"))
   
   (test-case
    "Test #12"
    (check-equal? (colors-not-equal?
                   (color-after-ticks (initial-world 0) 27 world-doodad-1star)
                   (color-after-ticks (initial-world 0) 28 world-doodad-1star))
                  true
                  "star color should change from gold to green"))
   
   (test-case
    "Test #13"
    (check-equal? (colors-not-equal?
                   (color-after-ticks (initial-world 0) 47 world-doodad-1star)
                   (color-after-ticks (initial-world 0) 48 world-doodad-1star))
                  true
                  "star color should change from green to blue"))
   
   (test-case
    "Test #14"
    (check-equal? (colors-not-equal?
                   (color-after-ticks (initial-world 0) 64 world-doodad-1star)
                   (color-after-ticks (initial-world 0) 65 world-doodad-1star))
                  true
                  "star color should change from blue to gold"))
   
   (test-case
    "Test #15"
    (check-equal? (colors-are-equal?
                   (color-after-ticks (initial-world 0)  0 world-doodad-1star)
                   (color-after-ticks (initial-world 0) 65 world-doodad-1star))
                  true
                  "star color should be gold after 65 ticks"))
   
   (test-case
    "Test #16"
    (check-equal? (core-bounce-log (initial-world 0) 200 world-doodad-1square)
                  (list 35 38 81 88 127 138 173 188)
                  "core bounces computed incorrectly for square doodad"))
   
   (test-case
    "Test #17"
    (check-equal? (colors-are-equal?
                   (color-after-ticks (initial-world 0)  0 world-doodad-1square)
                   (color-after-ticks (initial-world 0) 35 world-doodad-1square))
                  true
                  "square color should be gray at first"))
   
   (test-case
    "Test #18"
    (check-equal? (colors-not-equal?
                   (color-after-ticks (initial-world 0) 35 world-doodad-1square)
                   (color-after-ticks (initial-world 0) 36 world-doodad-1square))
                  true
                  "square color should change from gray to olive drab"))
   
   (test-case
    "Test #19"
    (check-equal? (colors-not-equal?
                   (color-after-ticks (initial-world 0) 38 world-doodad-1square)
                   (color-after-ticks (initial-world 0) 39 world-doodad-1square))
                  true
                  "square color should change from olive drab to khaki"))
   
   (test-case
    "Test #20"
    (check-equal? (colors-not-equal?
                   (color-after-ticks (initial-world 0) 81 world-doodad-1square)
                   (color-after-ticks (initial-world 0) 82 world-doodad-1square))
                  true
                  "square color should change from khaki to orange"))
   
   (test-case
    "Test #21"
    (check-equal? (colors-not-equal?
                   (color-after-ticks (initial-world 0) 88 world-doodad-1square)
                   (color-after-ticks (initial-world 0) 89 world-doodad-1square))
                  true
                  "square color should change from orange to crimson"))
   
   (test-case
    "Test #22"
    (check-equal? (colors-not-equal?
                   (color-after-ticks (initial-world 0) 127 world-doodad-1square)
                   (color-after-ticks (initial-world 0) 128 world-doodad-1square))
                  true
                  "square color should change from crimson to gray"))
   
   (test-case
    "Test #23"
    (check-equal? (colors-are-equal?
                   (color-after-ticks (initial-world 0)   0 world-doodad-1square)
                   (color-after-ticks (initial-world 0) 128 world-doodad-1square))
                  true
                  "square color should be gray after 128 ticks"))
   
   (test-case
    "Test #24"
    (check-equal?
     (core-bounce-special-log (initial-world 0) 5000 world-doodad-1star)
     (list 587 2267 3947)
     "both of the star's velocity components should change sign after 588 ticks"))
   
   (test-case
    "Test #25"
    (check-equal? (colors-are-equal?
                   (color-after-ticks (initial-world 0)  30 world-doodad-1star)
                   (color-after-ticks (initial-world 0) 588 world-doodad-1star))
                  true
                  "star should be green after 588 ticks"))
   
   ;;; Black-box tests adapted from Problem Set 03, q2.rkt
   
   (test-case
    "Test #26"
    (check-equal? (world-as-list (initial-world 0))
                  (world-as-list
                   (world-after-mouse-event
                    (initial-world 0) 200 220 "button-down"))
                  "button-down outside doodads should not select anything"))
   
   (test-case
    "Test #27"
    (check-equal? (world-as-list (initial-world 0))
                  (list 'world
                        false
                        '((doodad 125 120 10 12 #f))
                        '((doodad 460 350 -13 -9 #f)))
                  "doodads should be unselected at first"))
   
   (test-case
    "Test #28"
    (check-equal? (world-as-list
                   (world-after-mouse-event
                    (initial-world 0) 125 120 "button-down"))
                  (list 'world
                        false
                        '((doodad 125 120 10 12 #t))
                        '((doodad 460 350 -13 -9 #f)))
                  "star should be selected by button-down"))
   
   (test-case
    "Test #29"
    (check-equal? (world-as-list
                   (world-after-mouse-event
                    (world-after-mouse-event
                     (initial-world 0) 125 120 "button-down")
                    345 220 "drag"))
                  (list 'world
                        false
                        '((doodad 345 220 10 12 #t))
                        '((doodad 460 350 -13 -9 #f)))
                  "star should have been dragged"))
   
   (test-case
    "Test #30"
    (check-equal? (world-as-list
                   (world-after-mouse-event
                    (world-after-mouse-event
                     (world-after-mouse-event
                      (initial-world 0) 125 120 "button-down")
                     440 360 "drag")
                    440 360 "button-up"))
                  (list 'world
                        false
                        '((doodad 440 360 10 12 #f))
                        '((doodad 460 350 -13 -9 #f)))
                  "star should have been dragged and unselected"))
   
   (test-case
    "Test #31"
    (check-equal? (world-as-list
                   (world-after-mouse-event
                    (world-after-mouse-event
                     (world-after-mouse-event
                      (world-after-mouse-event
                       (world-after-mouse-event
                        (world-after-mouse-event
                         (initial-world 0) 125 120 "button-down")
                        440 360 "drag")
                       440 360 "button-up")
                      440 360 "button-down")
                     150 200 "drag")
                    150 200 "button-up"))
                  (list 'world
                        false
                        '((doodad 150 200 10 12 #f))
                        '((doodad 170 190 -13 -9 #f)))
                  "both doodads should have been dragged"))
   
   (test-case
    "Test #32"
    (check-equal? (let* ((c1 (doodad-color
                              (first (world-doodads-star (initial-world 0)))))
                         (c2 (doodad-color
                              (first (world-doodads-square (initial-world 0)))))
                         (w (world-after-key-event
                             (world-after-mouse-event
                              (world-after-mouse-event
                               (world-after-mouse-event
                                (world-after-mouse-event
                                 (world-after-mouse-event
                                  (initial-world 0) 125 120 "button-down")
                                 440 360 "drag")
                                440 360 "button-up")
                               440 360 "button-down")
                              150 200 "drag")
                             "c")))
                    (list (colors-not-equal?
                           c1 (doodad-color (first (world-doodads-star w))))
                          (colors-not-equal?
                           c2 (doodad-color (first (world-doodads-square w))))))
                  (list true true)
                  "both doodads should have changed colors"))
   
   ;;; Black-box tests that are new for Problem Set 04.
   
   (test-case
    "Test #33"
    (check-equal? (doodad-age (world-doodad-1star (initial-world 0)))
                  0
                  "star-like doodad should start out with age 0"))
   
   (test-case
    "Test #34"
    (check-equal? (doodad-age (world-doodad-1square (initial-world 0)))
                  0
                  "square doodad should start out with age 0"))
   
   (test-case
    "Test #35"
    (check-equal? (doodad-age (world-doodad-1star
                               (world-after-ticks (initial-world 0) 273)))
                  273
                  "star-like doodad should have age 273 after 273 ticks"))
   
   (test-case
    "Test #36"
    (check-equal? (doodad-age (world-doodad-1square
                               (world-after-ticks (initial-world 0) 34)))
                  34
                  "star-like doodad should have age 34 after 34 ticks"))
   
   (test-case
    "Test #37"
    (check-equal? (world-as-list
                   (world-after-key-event (initial-world 0) "."))
                  (list 'world false empty empty)
                  "Both doodads should have disappeared."))
   
   (test-case
    "Test #38"
    (check-equal? (world-as-list
                   (world-after-ticks-and-key-events
                    (initial-world 192)
                    (list 25 "q" 50 "q" 30 "." 45 "t" 100 ".")))
                  (list 'world
                        false
                        empty
                        '((doodad 335 133 13 9 #f)))
                  "world wrong after these events: 25 q 50 q 30 . 45 5 100 ."))
   
   (test-case
    "Test #39"
    (check-equal? (doodad-age
                   (world-doodad-1star
                    (world-after-ticks-and-key-events
                     (initial-world 192)
                     (list 95 "q" "t" "t" 36 "t" "q" 8 "." 12 "." 15))))
                  35
                  "wrong age for dynamically created star doodad"))
   
   (test-case
    "Test #40"
    (check-equal? (world-as-list
                   (world-after-ticks-and-key-events
                    (initial-world 0)
                    (list 1 "t" 1 "q" 1 "t" 1 "q" 1 "t" 1 "q"
                          1 "t" 1 "q" 1 "t" 1 "q")))
                  (list 'world
                        false
                        (insertion-sort
                         doodad-as-list<
                         (list (list 'doodad
                                     (+ 125 (* 10 10))
                                     (+ 120 (* 10 12))
                                     10 12 #f)
                               (list 'doodad
                                     (+ 125 (* 9 -12))
                                     (+ 120 (* 9 10))
                                     -12 10 #f)
                               (list 'doodad
                                     (+ 125 (* 7 -10))
                                     (+ 120 (* 7 -12))
                                     -10 -12 #f)
                               (list 'doodad
                                     (+ 125 (* 5 12))
                                     (+ 120 (* 5 -10))
                                     12 -10 #f)
                               (list 'doodad
                                     (+ 125 (* 3 10))
                                     (+ 120 (* 3 12))
                                     10 12 #f)
                               (list 'doodad
                                     (+ 125 (* 1 -12))
                                     (+ 120 (* 1 10))
                                     -12 10 #f)))
                        (insertion-sort
                         doodad-as-list<
                         (list (list 'doodad
                                     (+ 460 (* 10 -13))
                                     (+ 350 (* 10 -9))
                                     -13 -9 #f)
                               (list 'doodad
                                     (+ 460 (* 8 9))
                                     (+ 350 (* 8 -13))
                                     9 -13 #f)
                               (list 'doodad
                                     (+ 460 (* 6 13))
                                     (+ 350 (* 6 9))
                                     13 9 #f)
                               (list 'doodad
                                     (+ 460 (* 4 -9))
                                     (+ 350 (* 4 13))
                                     -9 13 #f)
                               (list 'doodad
                                     (+ 460 (* 2 -13))
                                     (+ 350 (* 2 -9))
                                     -13 -9 #f)
                               (list 'doodad
                                     (+ 460 (* 0 9))
                                     (+ 350 (* 0 -13))
                                     9 -13 #f))))
                  "new doodads aren't going in the right directions"))
   
   (test-case
    "Test #41"
    (check-equal? (let* ((w
                          (world-after-ticks-and-key-events
                           (initial-world 0)
                           (list 100 "t" 100 "q" 100 "t" 100 "q" 100 "t" 100 "q"
                                 100 "t" 100 "q" 100 "t" 100 "q")))
                         (stars (world-doodads-star w))
                         (squares (world-doodads-square w))
                         (stars (insertion-sort doodad< stars))
                         (squares (insertion-sort doodad< squares))
                         (colors1 (map doodad-color stars))
                         (colors2 (map doodad-color squares))
                         (c100 (list-ref colors1 0))
                         (c101 (list-ref colors1 1))
                         (c102 (list-ref colors1 2))
                         (c103 (list-ref colors1 3))
                         (c104 (list-ref colors1 4))
                         (c105 (list-ref colors1 5))
                         (c200 (list-ref colors2 0))
                         (c201 (list-ref colors2 1))
                         (c202 (list-ref colors2 2))
                         (c203 (list-ref colors2 3))
                         (c204 (list-ref colors2 4))
                         (c205 (list-ref colors2 5)))
                    (list (colors-are-equal? c100 c101)
                          (colors-are-equal? c101 c103)
                          (colors-are-equal? c103 c104)
                          (colors-not-equal? c101 c102)
                          (colors-not-equal? c104 c105)
                          (colors-not-equal? c200 c201)
                          (colors-not-equal? c201 c202)
                          (colors-not-equal? c202 c203)
                          (colors-are-equal? c203 c204)
                          (colors-not-equal? c204 c205)))
                  '(#t #t #t #t #t #t #t #t #t #t)
                  "new doodads don't change color correctly"))
   
   (test-case
    "Test #42"
    (check-equal? (world-as-list
                   (world-after-mouse-event
                    (world-after-mouse-event
                     (world-after-mouse-event
                      (world-after-ticks-and-key-events
                       (initial-world 10295)
                       (list 10 "t" "t" "t" "t"))
                      125 120 "button-down")
                     379 421 "drag")
                    379 421 "button-up"))
                  '(world
                    #f
                    ((doodad 225 240 10 12 #f)
                     (doodad 379 421 -12 10 #f)
                     (doodad 379 421 -10 -12 #f)
                     (doodad 379 421 10 12 #f)
                     (doodad 379 421 12 -10 #f))
                    ((doodad 330 260 -13 -9 #f)))
                  "new doodads aren't being dragged correctly"))
   
   (test-case
    "Test #43"
    (check-equal? (let* ((w
                          (world-after-key-event
                           (world-after-mouse-event
                            (world-after-mouse-event
                             (world-after-ticks-and-key-events
                              (initial-world 10295)
                              (list 10 "t" "t" "t" "t"))
                             125 120 "button-down")
                            379 421 "drag")
                           "c"))
                         (stars (world-doodads-star w))
                         (stars (insertion-sort doodad< stars))
                         (colors1 (map doodad-color stars))
                         (c100 (list-ref colors1 0))
                         (c101 (list-ref colors1 1))
                         (c102 (list-ref colors1 2))
                         (c103 (list-ref colors1 3))
                         (c104 (list-ref colors1 4)))
                    (list (colors-not-equal? c100 c101)
                          (colors-are-equal? c101 c102)
                          (colors-are-equal? c101 c103)
                          (colors-are-equal? c101 c104)))
                  '(#t #t #t #t)
                  "multiple doodads aren't responding to c key event"))
   
   (test-case
    "Test #44"
    (check-equal? (let* ((w
                          (world-after-key-event
                           (world-after-mouse-event
                            (world-after-mouse-event
                             (world-after-ticks-and-key-events
                              (initial-world 10295)
                              (list 10 "t" "t" " " "t" "t"))
                             125 120 "button-down")
                            379 421 "drag")
                           "c"))
                         (stars (world-doodads-star w))
                         (stars (insertion-sort doodad< stars))
                         (colors1 (map doodad-color stars))
                         (c100 (list-ref colors1 0))
                         (c101 (list-ref colors1 1))
                         (c102 (list-ref colors1 2))
                         (c103 (list-ref colors1 3))
                         (c104 (list-ref colors1 4)))
                    (list (colors-not-equal? c100 c101)
                          (colors-are-equal? c101 c102)
                          (colors-are-equal? c101 c103)
                          (colors-are-equal? c101 c104)))
                  '(#t #t #t #t)
                  "multiple doodads aren't responding to c key event (paused)"))
   ))

(run-tests tests 'verbose)
