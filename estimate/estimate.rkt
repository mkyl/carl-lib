#lang racket/base

(require
	racket/list
	math/matrix
	math/array
	math/statistics
    racket/string
    racket/file
    racket/port
    racket/system
    csv-writing
    "misc.rkt")

(define (estimate-exact-match unit-table)
	(let*
		([data (matrix->list* unit-table)]
		 [groups (stratify data)]
         [groups (filter good-groups groups)]
		 [ns (map length groups)]
		 [n (apply + ns)]
		 [weights (map (λ (g) (/ (length g) n)) groups)]
		 [diffs (map diff-of-avgs groups)]
		 [w_diffs (map * diffs weights)]
		 [ate (apply + w_diffs)])
	  ate))

(define good-groups
    (λ(g) (rational? (diff-of-avgs g))))

(define (stratify rs) 
	(let* ([g (group-by cddr rs)]
		   [gg (filter (λ (s) (< 1 (length (remove-duplicates (map second s))))) g)])
		gg))

(define (diff-of-avgs data)
	(let*
		([treated (filter (lambda (r) (= (second r) 1)) data)]
		 [untreated (filter (lambda (r) (= (second r) 0)) data)]
		 [Y_T (mean (map car treated))]
		 [Y_UT (mean (map car untreated))])
	  (- Y_T Y_UT)))

(define (write-csv table file)
    (call-with-output-file file
        (lambda (out)
            (display-table table out))
        #:exists 'replace))

(define (estimate-k-means unit-table)
    (if (= (matrix-num-cols unit-table) 2)
        (estimate-exact-match unit-table)
    (let ([t-and-y (submatrix unit-table (::) ( :: 2))])
        (define covars-matrix
            (submatrix unit-table (::) ( :: 2 #f)))
        (define covars-list
            (vector->list (matrix->vector* covars-matrix)))
        (define centroids
            (k-means covars-list 2))
        (define c (λ(x) (argmin (λ(y) (euclidean-distance x y)) centroids)))
        (define closest (map c covars-list))
        (define keys (map (λ(x) (index-of centroids x)) closest))
        (define target
            (matrix-augment (list t-and-y (->col-matrix keys))))
        (estimate-exact-match target))))

(define estimate estimate-k-means)
(provide estimate)
