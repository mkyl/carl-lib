#lang racket/base

(require rackunit)

(define lang-tests
  (test-suite
   "CaRL Language Tests"
 
   (check-equal? (+ 1 1) 2 "Simple addition")
 
   (check-equal? (* 1 2) 2 "Simple multiplication")
 
   (test-case
    "List has length 4 and all elements even"
    (let ([lst (list 2 4 6 8)])
      (check = (length lst) 4)
      (for-each
        (lambda (elt)
          (check-pred even? elt))
      lst)))))

(provide lang-tests)