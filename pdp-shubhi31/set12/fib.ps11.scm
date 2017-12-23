(define-library ($main)
  (export $main)
  (import (scheme base))
  (begin

   (define false #f)

   (define true #t)

   (define ($main . args)
     (apply main. args))

   (define main.
     (lambda (n. iters.) (if (= iters. 0) (- 0 1) (if (= iters. 1) (fib. n.) ((lambda (ignored.) (main. n. (- iters. 1))) (fib. n.))))))

   (define fib.
     (lambda (n.) (if (< n. 2) n. (+ (fib. (- n. 1)) (fib. (- n. 2))))))


  ))

(import (scheme base)
        (scheme process-context)
        (scheme write)
        ($main))

(write (apply $main (map string->number (cdr (command-line)))))
(newline)
