#lang racket/base

(require rackunit
    carl-lib/ground
    carl-lib/unit-table
    carl-lib/embed
    math/matrix)

(define embed-tests
    (test-suite
     "Embedding (Summary Function) Tests"
     (test-case
     	"one covariate"
     	(check-equal? (embed one-covariate) one-covariate-matrix))
     (test-case
     	"two covariates"
     	(check-equal? (embed two-covariates) two-covariates-matrix))
     (test-case
        "treatments binarized"
        (check-equal? (embed two-treatments) two-treatments-matrix))))

(define one-covariate
	(list (pre-row (atom '() 'a 1)
				   (list (atom '() 'b 1))
				   (list (atom '() 'c 1)))))

(define one-covariate-matrix
	(matrix [[1 1 1]]))

(define two-covariates
    (list (pre-row (atom '('y1) 'a 1)
                   (list (atom '('t1) 'b 0) (atom '('t2) 'b 0))
                   (list (atom '('z1) 'c 1) (atom '('z2) 'c 0)))))

(define two-covariates-matrix
    (matrix [[1 0 1/2]]))

(define two-treatments
    (list (pre-row (atom '('y1) 'a 1)
                   (list (atom '('t1) 'b 0) (atom '('t2) 'b 0))
                   (list (atom '('z1) 'c 1) (atom '('z1) 'd 0)))))

(define two-treatments-matrix
    (matrix [[1 0 1 0]]))

(provide embed-tests)
