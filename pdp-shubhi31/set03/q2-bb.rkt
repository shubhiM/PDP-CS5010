#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "q2.rkt")
(require rackunit)
(require 2htdp/image)
;(require 2htdp/universe)


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
        (doodad-x d) (doodad-y d) (doodad-vx d) (doodad-vy d)
        (doodad-selected? d)))

;;; world-as-list : World -> List
;;; Given a world, returns a partial representation as a list.
;;; Examples: see tests below.

(define (world-as-list w)
  (list 'world
        (world-paused? w)
        (doodad-as-list (world-doodad-star w))
        (doodad-as-list (world-doodad-square w))))

;;; world-after-ticks : World Int -> List
;;; Given a world and an integer n,
;;; returns the world as it would be after n more ticks.

(define (world-after-ticks w n)
  (if (= n 0)
      w
      (world-after-ticks (world-after-tick w)
                         (- n 1))))

;;; color-after-ticks : World Int Function -> Color
;;; Given a world w, an integer n, and a selector function f
;;; where f is world-doodad-star or world-doodad-square,
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
;;; and a selector function (world-doodad-star or world-doodad-square),
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

;;; core-bounce-special-log : World Int Function -> ListOfInt
;;; Given a world w, a limit on how many ticks to explore,
;;; and a selector function (world-doodad-star or world-doodad-square),
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
   "q2"
   
   (test-case
    "Test #1"
    (check-equal? (world-as-list (initial-world 0))
                  (world-as-list
                   (world-after-mouse-event
                    (initial-world 0) 200 220 "button-down"))
                  "button-down outside doodads should not select anything"))
   (test-case
    "Test #2"
    (check-equal? (world-as-list (initial-world 0))
                  (list 'world
                        false
                        '(doodad 125 120 10 12 #f)
                        '(doodad 460 350 -13 -9 #f))
                  "doodads should be unselected at first"))
   (test-case
    "Test #3"
    (check-equal? (world-as-list
                   (world-after-mouse-event
                    (initial-world 0) 125 120 "button-down"))
                  (list 'world
                        false
                        '(doodad 125 120 10 12 #t)
                        '(doodad 460 350 -13 -9 #f))
                  "star should be selected by button-down"))
   (test-case
    "Test #4"
    (check-equal? (world-as-list
                   (world-after-mouse-event
                    (world-after-mouse-event
                     (initial-world 0) 125 120 "button-down")
                    345 220 "drag"))
                  (list 'world
                        false
                        '(doodad 345 220 10 12 #t)
                        '(doodad 460 350 -13 -9 #f))
                  "star should have been dragged"))
   (test-case
    "Test #5"
    (check-equal? (world-as-list
                   (world-after-mouse-event
                    (world-after-mouse-event
                     (world-after-mouse-event
                      (initial-world 0) 125 120 "button-down")
                     440 360 "drag")
                    440 360 "button-up"))
                  (list 'world
                        false
                        '(doodad 440 360 10 12 #f)
                        '(doodad 460 350 -13 -9 #f))
                  "star should have been dragged and unselected"))
   (test-case
    "Test #6"
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
                        '(doodad 150 200 10 12 #f)
                        '(doodad 170 190 -13 -9 #f))
                  "both doodads should have been dragged"))
   (test-case
    "Test #7"
    (check-equal? (let* ((c1 (doodad-color
                              (world-doodad-star (initial-world 0))))
                         (c2 (doodad-color
                              (world-doodad-square (initial-world 0))))
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
                           c1 (doodad-color (world-doodad-star w)))
                          (colors-not-equal?
                           c2 (doodad-color (world-doodad-square w)))))
                  (list true true)
                  "both doodads should have changed colors"))
   (test-case
    "Test #8"
    (check-equal? (let* ((w (world-after-ticks (initial-world 0) 28))
                         (d (world-doodad-star w))
                         (d (doodad-after-mouse-event d 405 440 "button-down"))
                         (d (doodad-after-mouse-event d 275 167 "drag")))
                    (doodad-as-list d))
                  '(doodad 275 167 10 -12 #t)
                  "star should have been dragged to (275,167)"))
   (test-case
    "Test #9"
    (check-equal? (let* ((w (world-after-ticks (initial-world 0) 28))
                         (d (world-doodad-square w))
                         (d (doodad-after-mouse-event d 120 123 "button-down"))
                         (d (doodad-after-mouse-event d 310 280 "drag")))
                    (doodad-as-list d))
                  '(doodad 286 255 -13 -9 #t)
                  "square should have been dragged to (286,255)"))
   
   ;; Regression tests
   (test-case
    "Test #10"
    (check-equal? (world-as-list
                   (world-after-key-event
                    (world-after-tick
                     (world-after-key-event (initial-world 0) " "))
                    " "))
                  (list 'world
                        false
                        '(doodad 125 120 10 12 #f)
                        '(doodad 460 350 -13 -9 #f))))
   (test-case
    "Test #11"
    (check-equal? (world-as-list
                   (world-after-tick (initial-world 0)))
                  (list 'world
                        false
                        '(doodad 135 132 10 12 #f)
                        '(doodad 447 341 -13 -9 #f))))
   (test-case
    "Test #12"
    (check-equal? (world-as-list
                   (world-after-ticks (initial-world 0) 28))
                  (list 'world
                        false
                        '(doodad 405 440 10 -12 #f)
                        '(doodad  96  98 -13 -9 #f))))
   (test-case
    "Test #13"
    (check-equal? (core-bounce-log (initial-world 0) 200 world-doodad-star)
                  (list 27 47 64 102 107 139 167 176)
                  "core bounces computed incorrectly for star doodad"))
   (test-case
    "Test #14"
    (check-equal? (core-bounce-log (initial-world 0) 200 world-doodad-square)
                  (list 35 38 81 88 127 138 173 188)
                  "core bounces computed incorrectly for square doodad"))
   (test-case
    "Test #15"
    (check-equal?
     (core-bounce-special-log (initial-world 0) 5000 world-doodad-star)
     (list 587 2267 3947)
     "both of the star's velocity components should change sign after 588 ticks"))
   ))


(run-tests tests 'verbose)
