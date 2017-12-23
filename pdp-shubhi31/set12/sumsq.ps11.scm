(define-library ($main)
  (export $main)
  (import (scheme base))
  (begin

   (define false #f)

   (define true #t)

   (define ($main . args)
     (apply main. args))

   (define main.
     (lambda (n. iters.) (if (= iters. 0) (- 0 1) (if (= iters. 1) (sumSquares. n.) ((lambda (ignored.) (main. n. (- iters. 1))) (sumSquares. n.))))))

   (define sumSquares.
     (lambda (n.) (sumSquaresLoop. n. 0)))

   (define sumSquaresLoop.
     (lambda (n. sum.) (if (< n. 2) (+ sum. (* n. n.)) (sumSquaresLoop2. (- n. 1) (+ sum. (* n. n.))))))

   (define sumSquaresLoop2.
     (lambda (n. sum.) (if (< n. 2) (- sum. (* n. n.)) (sumSquaresLoop. (- n. 1) (- sum. (* n. n.))))))


  ))

(import (scheme base)
        (scheme process-context)
        (scheme write)
        ($main))

(write (apply $main (map string->number (cdr (command-line)))))
(newline)
