#lang racket/base

(require
	racket/list
	math/matrix
	math/array
	math/statistics)

(define (estimate unit-table)
	(let*
		([data (matrix->list* unit-table)]
		 [groups (stratify data)]
		 [ns (map length groups)]
		 [n (apply + ns)]
		 [weights (map (λ (g) (/ (length g) n)) groups)]
		 [diffs (map diff-of-avgs groups)]
		 [w_diffs (map * diffs weights)]
		 [ate (apply + w_diffs)])
	  ate))

(define (stratify rs) 
	; racket is beautiful
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

(provide estimate)
