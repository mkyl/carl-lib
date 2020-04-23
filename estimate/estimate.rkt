#lang racket/base

(require
	math/matrix
	racket/list
	math/statistics)

(define (estimate unit-table)
	(let*
		([data (matrix->list* unit-table)]
		 [treated (filter (lambda (r) (= (first r) 1)) data)]
		 [untreated (filter (lambda (r) (= (first r) 0)) data)]
		 [Y_T (mean (map (lambda (r) (first (rest r))) treated))]
		 [Y_UT (mean (map (lambda (r) (first (rest r))) untreated))])
	  (- Y_T Y_UT)))

(provide estimate)
